#' @include model.R
NULL


#' Customize the Lange model
#'
#' `r .doc_customize_description('Lange', 'lange', 'Lange2008')`
#'
#' `r .doc_customize_call('the Lange model', 'lange')`
#'
#' ```{r, eval = FALSE}
#' model("lange",
#'
#'       # ==== onset ==== #
#'
#'       dd_onset_start_date = '01-01',
#'       dd_onset_base = 5,
#'       dd_onset_threshold = 110,
#'
#'       # ==== onset + development ====
#'
#'       tfly = 19.5,
#'
#'       # ==== development ==== #
#'
#'       dd_base_stages = c(10.6, 8.2, 9.9, 3.2),
#'       dd_threshold_stages = c(51.8, 204.4, 57.7, 238.5),
#'
#'       model_end_date = '12-31',
#'
#'       # ==== mortality ==== #
#'
#'       first_lethal_date = '09-01',
#'       tlethal = 0
#' )
#' ```
#'
#' @param dd_onset_start_date The date, when the degree days start to sum up ('MM-DD').
#' @param dd_onset_base Base temperature to calculate degree days to trigger the onset.
#' @param dd_onset_threshold Degree days that are required to trigger the onset of
#' infestation. Additionally, the maximum temperature must exceed `tfly`.
#'
#' @param tfly Minimum temperature that beetles need to fly.
#'
#' @param dd_base_stages Base temperatures to calculate degree days for the
#' different stages in the following order: egg, larva, pupa, juvenile adult.
#' @param dd_threshold_stages Thermal thresholds for the different stages in the
#' following order: egg, larva, pupa, juvenile adult.
#' @param model_end_date Date when the model ends (no further development will
#' be modeled).
#'
#' @param first_lethal_date Date before which no mortality will be modeled.
#' @param tlethal Temperature threshold below which white stages (egg, larva, pupa) will die.
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.lange.customize
#' @seealso [model()], [phenology()], [`model.lange.apply`]
#' @family model customizations
#'
#' @encoding UTF-8
NULL


#' Use the Lange model
#'
#' This page describes the usage of the the Lange model with [phenology()].
#' The model specific inputs are listed and its basic functionality is explained.
#' The model was published by \insertCite{Lange2008;textual}{barrks} for
#' *Ips typographus*.
#'
#' In `barrks`, [phenology()] is used to apply a model. The following code
#' illustrates which inputs are required to apply the Lange model and which additional
#' parameters are available.
#'
#' ```{r, eval = FALSE}
#' phenology("lange", ..., tmin, tmean, tmax)
#'
#' # calculate submodels separately
#' phenology("lange", ..., .submodels = 'onset', tmean, tmax)
#' phenology("lange", ..., .submodels = 'mortality', tmin)
#' phenology("lange", ..., .submodels = 'development',
#'           .onset, .diapause = NULL, .mortality = NULL,
#'           tmean, tmax)
#' ```
#'
#' @section Functioning:
#'
#' `r .doc_functioning_pre('lange', 'the Lange model')`
#'
#' - **Onset**: The onset of swarming is triggerd when the degree days of the maximum temperature reach
#' a specific threshold and the maximum temperature exceeds the minimum flight temperature
#' according to \insertCite{Annila1969;textual}{barrks}.
#' - **Development**: The development is calculated using stage-specific
#' temperature sums and thresholds \insertCite{Wermelinger1998}{barrks}.
#' A new generation starts its development when the last generation finished its
#' development and the maximum temperature exceeds the minimum flight temperature.
#' - **Diapause**: The Lange model does not have a diapause submodel implemented.
#' - **Mortality**: White stages (egg to pupa) die when the minimum temperature
#' falls below a specific threshold.
#'
#' `r .doc_functioning_post('lange')`
#'
#'
#' @param tmin,tmean,tmax Daily minimum/mean/maximum temperatures in °C.
#' @param .submodels,.onset,.diapause,.mortality,... `r .doc_phenology_dots()`
#'
#' @return `r .doc_return_pheno()`
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @name model.lange.apply
#' @seealso [model()], [phenology()], [`model.lange.customize`]
#' @family phenology applications
#'
#' @encoding UTF-8
NULL




.lange_create_calc_teff <- function(stage) {

  function(.params,
           tmean,
           .storage = NULL,
           .quiet = FALSE) {

    # use storage if requested
    if(is.character(.storage)) return(.use_storage())

    teff <- tmean - .params$dd_base_stages[[stage]]
    return(terra::ifel(teff > 0, teff, 0))
  }

}

.lange_calc_teff_egg <- .lange_create_calc_teff(1)
.lange_calc_teff_larva <- .lange_create_calc_teff(2)
.lange_calc_teff_pupa <- .lange_create_calc_teff(3)
.lange_calc_teff_adult <- .lange_create_calc_teff(4)


.lange_develop_generation <- function(.params,
                                        .onset,
                                        .diapause,
                                        .mortality,
                                        teff_egg,
                                        teff_larva,
                                        teff_pupa,
                                        teff_adult,
                                        fly,
                                        period,
                                        .last = NULL,
                                        .storage = NULL,
                                        .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage(.skip = c('period')))

  teff <- list(teff_egg, teff_larva, teff_pupa, teff_adult)
  num_lyrs <- terra::nlyr(teff_egg)
  total_dev <- sum(.params$dd_threshold_stages)

  if(!is.null(.params$model_end_date)) {

    end_date <- .get_date_of_year(teff_egg, .params$model_end_date)
    end_lyr <- .lyr_from_date(teff_egg, .params$model_end_date)

    teff <- purrr::map(teff, \(t) {
      if(all(terra::time(t) > end_date)) t <- t * 0
      else if(any(terra::time(t) > end_date)) {

        t <- c(t[[1:end_lyr]], t[[(end_lyr + 1):terra::nlyr(t)]] * 0)
      }

      return(t)
    })

  }


  teff <- purrr::map(teff, \(t) t * period)

  # get last temperature sum from backup
  if(!is.null(.last)) teff[[1]] <- teff[[1]] + terra::ifel(.last == -1, 0, .last) * total_dev


  # calculate cumulative development

  current_dev <- .template_rst(teff_egg)[[1]]
  limits <- cumsum(.params$dd_threshold_stages)


  classes <- matrix(c(0, limits[1], 1,
                      limits[1], limits[2], 2,
                      limits[2], limits[3], 3,
                      limits[3], limits[4], 4),
                    ncol = 3, byrow = TRUE)

  dev <- terra::rast(purrr::map(1:num_lyrs, \(i) {

    add_dev <- terra::ifel(current_dev < limits[1], teff[[1]][[i]],
                           terra::ifel(current_dev < limits[2], teff[[2]][[i]],
                                       terra::ifel(current_dev < limits[3], teff[[3]][[i]], teff[[4]][[i]])))
    current_dev <<- current_dev + add_dev

    return(current_dev)
  }))

  dev <- dev / total_dev

  if(!is.null(.mortality)) {
    kill <- .trigger_rst(.mortality & dev < limits[3] / total_dev & period)
    dev <- terra::ifel(kill, -2, dev)
  }

  terra::time(dev) <- terra::time(teff_egg)

  return(terra::ifel(dev == 0, -1, dev))
}


.lange_calc_development <- function(.params,
                                    .onset,
                                    .diapause,
                                    .mortality,
                                    teff_egg,
                                    teff_larva,
                                    teff_pupa,
                                    teff_adult,
                                    fly,
                                    .storage = NULL,
                                    .quiet = FALSE) {

  # only temperatures after onset and before hibernation account for development
  period_gen <- .onset

  # init variables
  out <- list()
  generation <- 1

  # walk through generations
  while(generation) {

    .msg(4, .quiet, 'generation ', generation)

    # storage path for current generation
    if(is.null(.storage)) storage_gen <- NULL
    else storage_gen <- file.path(.storage, paste0('generation', generation))

    # calculate development of current generation
    dev <- .lange_develop_generation(.params, .onset, .diapause, .mortality,
                                     teff_egg,
                                     teff_larva,
                                     teff_pupa,
                                     teff_adult,
                                     fly, period_gen, NULL,
                                     .storage = storage_gen,
                                     .quiet = .quiet)


    # save filial generations
    out[[paste0('gen_', generation)]] <- dev

    # only account for temperatures after development of last generation is finished
    if(is.null(.diapause)) period_gen <- .trigger_rst(fly * (dev >= 1))
    else period_gen <- .trigger_rst(fly * (dev >= 1) * (!.diapause))

    # break if no development will happen
    if(sum(terra::values(period_gen), na.rm = TRUE) == 0) break

    generation <- generation + 1
  }

  return(out)
}



# register model with default parameters
.create_model('lange',
             list(
               params = list(

                 dd_onset_start_date = '01-01',
                 dd_onset_base = 5,
                 dd_onset_threshold = 110,

                 tfly = 19.5,
                 model_end_date = '12-31',

                 dd_base_stages = c(10.6, 8.2, 9.9, 3.2),
                 dd_threshold_stages = c(51.8, 204.4, 57.7, 238.5),

                 first_lethal_date = '09-01',
                 tlethal = 0
               ),

               onset = list(
                 setup = list(dd_onset = .calc_dd_onset_func.tmean(),
                              fly = .calc_fly),
                 compute = .calc_onset_fly_dd_func()
               ),

               development = list(
                 setup = list(teff_egg = .lange_calc_teff_egg,
                              teff_larva = .lange_calc_teff_larva,
                              teff_pupa = .lange_calc_teff_pupa,
                              teff_adult = .lange_calc_teff_adult,
                              fly = .calc_fly),
                 compute = .lange_calc_development
               ),

               mortality = list(
                 compute = .calc_tmin_mortality
               )
             )
)
