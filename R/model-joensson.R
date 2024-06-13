#' @include model-phenips.R
NULL



#' Customize the Jönsson model
#'
#' `r .doc_customize_description('Jönsson', 'joensson', 'Jonsson2011')`
#'
#' @usage
#' model("joensson",
#'
#'       # ==== onset ====
#'
#'       dd_onset_start_date = '01-01',
#'       dd_onset_base = 5,
#'       dd_onset_threshold = 120,
#'
#'       # ==== onset + development ====
#'
#'       tfly = 20,
#'
#'       # ==== development ====
#'
#'       model_end_date = '12-31',
#'
#'       dd_development_base = 5,
#'       dd_total_dev_lower = 625,
#'       dd_total_dev_upper = 750,
#'       dev_start = 0,
#'       dev_end = 1,
#'       dev_mortal_min = NULL,
#'       dev_mortal_max = NULL,
#'
#'       # ==== diapause ====
#'
#'       daylength_dia = 19.3,
#'       tdia_min = 15
#' )
#'
#' @param dd_onset_start_date The date, when the degree days start to sum up ('MM-DD').
#' @param dd_onset_base Base temperature to calculate degree days to trigger the onset.
#' @param dd_onset_threshold Degree days that are required to trigger the onset of
#' infestation. Additionally, the maximum temperature must exceed `tfly`.
#'
#' @param tfly Minimum temperature that beetles need to fly.
#'
#' @param model_end_date Date when the model ends.
#'
#' @param dd_development_base Base temperature to calculate degree days for development.
#' @param dd_total_dev_lower,dd_total_dev_upper Lower/upper limit of degree days
#' that are required for a generation to fully develop
#' @param dev_start,dev_end `r .doc_dev_start_end()`
#' @param dev_mortal_min,dev_mortal_max Minimum/maximum share in the total
#' development of white stages (egg, larva, pupa). During these stages, the
#' beetles could die caused by a mortality event.
#'
#' @param daylength_dia,tdia_min When the daylength falls below `daylength_dia`
#' and the average daily temperature falls below `tdia_min`, diapause
#' will be initiated. The default value for the critical daylength was set to
#' 19.3 hours according to \insertCite{Schroeder2017;textual}{barrks} who
#' examined the photoperiodic diapause induction in Sweden. If the model is used
#' for other regions, this value should be adjusted.
#'
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.joensson.customize
#' @seealso [model()], [phenology()], [`model.joensson.apply`]
#' @family {model customizations}
#'
#' @encoding UTF-8
NULL


#' Use the Jönnson model
#'
#' This page describes the usage of the the Jönsson model with [phenology()].
#' The model specific inputs are listed and its basic functionality is explained.
#' The Jönsson model was published by \insertCite{Jonsson2011;textual}{barrks} and
#' parametrized for *Ips typographus* in southern Sweden.
#'
#' @section Functioning:
#'
#' `r .doc_functioning_pre('joensson', 'the Jönsson model')`
#'
#' - **Onset**: The onset of swarming is triggerd when the degree days of the maximum temperature reach
#' a specific threshold and the maximum temperature exceeds the minimum flight temperature.
#' The onset of infestation is triggered seven days later to account
#' for a pre-oviposition period.
#' - **Development**: The development progresses proportional to the degree days
#' of the mean temperature. To account for varying sun exposures, two different
#' thermal thresholds are defined that reflect the lower and the upper limit
#' of development. A generations starts swarming when it has finished its
#' development and the maximum temperature exceeds the minimum flight temperature.
#' Seven days later, the development of a new generation starts.
#' - **Diapause**: The diapause is initiated when the daylength falls below a
#' threshold. It is recommended to adjust the daylength threshold when applying elsewhere
#' (e.g. values from literature). \insertCite{Jonsson2011;textual}{barrks} proposes a model to
#' calculate the daylength threshold based on long-term climate data.
#' - **Mortality**: The Jönsson model does not have a mortality submodel implemented.
#'
#' `r .doc_functioning_post('joensson')`
#'
#' @usage
#'
#' phenology("joensson", ..., tmean, tmax, daylength, mode = 'fast')
#'
#' # calculate submodels separately
#' phenology("joensson", ..., .submodels = 'onset', tmax)
#' phenology("joensson", ..., .submodels = 'diapause', tmax, daylength)
#' phenology("joensson", ..., .submodels = 'development',
#'           .onset, .diapause = NULL, .mortality = NULL,
#'           tmean, mode = 'fast')
#'
#' @param ... `r .doc_phenology_dots()`
#' @param tmean,tmax Daily mean/maximum temperatures in °C.
#' @param daylength Length of the day in hours. Can be created with
#' [create_daylength_rst()] or [create_daylength_rst()].
#' @param mode Can be `'fast'` (default) or `'slow'`. Determines if the lower
#' (`'fast'`) or upper (`'slow'`) limit for the development of  genertion
#' should be used.
#'
#' @return `r .doc_return_pheno()`
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @name model.joensson.apply
#' @seealso [model()], [phenology()], [`model.joensson.customize`]
#' @family {phenology applications}
#'
#' @encoding UTF-8
NULL



joensson_calc_onset <- function(.params,
                               .storage = NULL,
                               .quiet = FALSE,
                               .last = NULL,
                               fly,
                               dd_onset) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  if(is.null(dd_onset)) return(.last)

  .msg(4, .quiet, 'calculate onset of swarming')
  onset <- .trigger_rst(dd_onset >= .params$dd_onset_threshold & fly)
  terra::time(onset) <- terra::time(dd_onset)

  .msg(4, .quiet, 'set onset of infestation to 7 days after swarming')
  first_date <- min(terra::time(onset))
  doy_offset <- lubridate::yday(first_date) - 1
  onset_doy <- terra::which.lyr(onset) + doy_offset
  out <- create_onset(onset, onset_doy + 7)

  # an onset in a backup will trigger the onset too
  if(!is.null(.last)) out <- out | .last

  return(out)
}



joensson_calc_diapause <- function(.params,
                                 .storage = NULL,
                                 .quiet = FALSE,
                                 .last = NULL,
                                 daylength,
                                 tmean) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # search the longest day of the current year
  dates <- terra::time(daylength)
  year <- format(dates[[1]], '%Y')
  longest_day <- as.Date(paste0(year, '-06-21'))

  # condition to start diapause
  .msg(4, .quiet, 'check diapause condition')
  cond <- (daylength < .params$daylength_dia & tmean < .params$tdia_min)

  # when is the condition met after the longest day?
  out <- terra::rast(purrr::map(terra::as.list(cond), function(rst) {
    if(terra::time(rst) < longest_day) return(rst * 0)
    return(rst)
  }))

  terra::time(out) <- terra::time(tmean)

  # an diapause in a backup will trigger the diapause too
  if(!is.null(.last)) out <- out | .last

  return(.trigger_rst(out))
}


joensson_calc_teff <- function(.params,
                             tmean,
                             .storage = NULL,
                             .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  .msg(4, .quiet, 'calculate effective temperature')

  return(terra::clamp(tmean - .params$dd_development_base, 0))
}



joensson_calc_development <- function(.params,
                                    .onset,
                                    .diapause,
                                    .mortality,
                                    teff,
                                    fly,
                                    mode = 'fast',
                                    .storage = NULL,
                                    .quiet = FALSE) {


  .msg(4, .quiet, 'calculate development of ', mode, ' beetles')

  if(is.null(.diapause)) .diapause <- as.logical(.onset * 0)

  # original tmean is needed for backup
  teff_org <- teff

  # depending on the calculation mode, dd_total_dev is chosen
  .params$dd_total_dev <- switch(mode,
                                 'fast' = .params$dd_total_dev_lower,
                                 'slow' = .params$dd_total_dev_upper)

  # only temperatures after onset account for development
  period_gen <- .onset
  #teff <- teff * .onset

  # init variables
  first_date <- min(terra::time(teff))
  doy_offset <- lubridate::yday(first_date) - 1
  development <- list()
  generation <- 1
  last_dev <- NULL

  # walk through generations
  while(generation) {

    .msg(4, .quiet, 'compute generation ', generation)

    # storage path for current generation
    if(is.null(.storage)) storage_gen <- NULL
    else storage_gen <- file.path(.storage, paste0('generation', generation))

    dev <- phenips_develop_generation(.params, .onset, .diapause, .mortality,
                                      teff, fly, period_gen,
                                      .storage = storage_gen, .last = NULL, .quiet)

    # save results
    development[[paste0('gen_', generation)]] <- dev

    # the next generation starts 7 days after the current finished development
    period_gen <- terra::which.lyr(fly * (dev >= 1) * (!.diapause))
    period_gen <- create_onset(dev, period_gen + 7 + doy_offset)

    # break if no development will happen
    if(sum(terra::values(period_gen), na.rm = TRUE) == 0) break

    last_dev <- dev
    generation <- generation + 1
  }

  return(development)
}



# register model with default parameters
.create_model('joensson',
             list(
               params = list(
                 model_end_date = '12-31',

                 dd_total_dev_lower = 625,
                 dd_total_dev_upper = 750,
                 dd_onset_base = 5,
                 dd_development_base = 5,
                 dev_start = 0,
                 dev_end = 1,
                 dev_mortal_min = NULL,
                 dev_mortal_max = NULL,

                 dd_onset_start_date = '01-01',
                 tfly = 20,
                 dd_onset_threshold = 120,

                 daylength_dia = 19.3,
                 tdia_min = 15
               ),

               onset = list(
                 setup = list(dd_onset = .calc_dd_onset_tmax,
                              fly = .calc_fly),
                 compute = joensson_calc_onset
               ),

               development = list(
                 setup = list(teff = joensson_calc_teff,
                              fly = .calc_fly),
                 compute = joensson_calc_development
               ),

               diapause = list(
                 compute = joensson_calc_diapause
               )
             )
)
