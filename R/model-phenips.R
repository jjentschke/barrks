#' @include model.R
NULL



#' Customize PHENIPS
#'
#' `r .doc_customize_description('PHENIPS', 'phenips', 'Baier2007')`
#'
#' @usage
#' model("phenips",
#'
#'       # ==== onset ====
#'
#'       dd_onset_start_date = '04-01',
#'       dd_onset_base = 8.3,
#'       dd_onset_threshold = 140,
#'
#'       # ==== onset + development ====
#'
#'       tfly = 16.5,
#'
#'       # ==== development ====
#'
#'       dd_development_base = 8.3,
#'       dd_total_dev = 557,
#'       dev_start = 0,
#'       dev_end = 1,
#'       dev_sister_brood = 0.5,
#'       dev_mortal_min = NULL,
#'       dev_mortal_max = 0.6,
#'
#'       topt = 30.4,
#'       tlow = 8.3,
#'       tup = 38.9,
#'
#'       func_btmean = \(tmean, rad) { -0.173 + 0.0008518 * rad + 1.054 * tmean},
#'       func_btmax = \(tmax, rad) { 1.656 + 0.002955 * rad + 0.534 * tmax + 0.01884 * tmax ^ 2 },
#'       func_btdiff = \(btmax) { (-310.667 + 9.603 * btmax) / 24 },
#'
#'       # ==== diapause ====
#'
#'       daylength_dia = 14.5,
#'
#'       # ==== mortality ====
#'
#'       model_end_date = '10-31'
#' )
#'
#' @param dd_onset_start_date The date, when the degree days start to sum up ('MM-DD').
#' @param dd_onset_base Base temperature to calculate degree days to trigger the onset.
#' @param dd_onset_threshold Degree days that are required to trigger the onset of
#' infestation. Additionally, the maximum temperature must exceed `tfly`.
#'
#' @param tfly Minimum temperature that beetles need to fly.
#'
#' @param dd_onset_base Base temperature to calculate degree days for calculating
#' the beetles development.
#' @param dd_total_dev Degree days that are required for a generation to fully
#' develop
#' @param dev_start,dev_end `r .doc_dev_start_end()`
#' @param dev_sister_brood Share in the total development when a sister brood
#' will be established.
#' @param dev_mortal_min,dev_mortal_max Minimum/maximum share in the total
#' development of white stages (egg, larva, pupa). During these stages, the
#' beetles could die caused by a mortality event.
#'
#' @param topt Temperature for optimal development.
#' @param tlow,tup Temperature below/above which no development happens.
#' @param func_btmean,func_btmax,func_btdiff Functions to caclulate the
#' effective bark temperature (see \insertCite{Baier2007;nobrackets}{barrks},
#' equations A.3 to A5). Each parameter will be passed as SpatRaster:
#'
#' - `tmean`: mean air temperature
#' - `tmax`: maximum air temperature
#' - `rad`: radiation
#' - `btmax`: maximum bark temperature
#'
#'
#' @param daylength_dia When the daylength falls below this threshold, diapause
#' will be initiated.
#'
#' @param model_end_date Date when the model ends and all white stages (egg, larva, pupa) die.
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.phenips.customize
#' @seealso [model()], [phenology()], [`model.phenips.apply`]
#' @family {model customizations}
#'
#' @encoding UTF-8
NULL



#' Use PHENIPS
#'
#' This page describes the usage of PHENIPS with [phenology()].
#' The model specific inputs are listed and its basic functionality is explained.
#' PHENIPS was published by \insertCite{Baier2007;textual}{barrks} and
#' parametrized at the Kalkalpen National Park in Austria for *Ips typographus*.
#'
#' @section Functioning:
#'
#' `r .doc_functioning_pre('phenips', 'PHENIPS')`
#'
#' - **Onset**: The onset is triggered when the degree days of the maximum temperature reach
#' a specific threshold and the maximum temperature exceeds the minimum flight temperature.
#' - **Development**: The beetles develop according to a slightly modified version
#' of the optimum curve described by \insertCite{Wermelinger1998;textual}{barrks}
#' depending on the bark temperature. The bark temperature is modeled based on mean
#' and maximum temperature, global radiation and sun exposure. A new generation will
#' emerge when the last generation is fully developed and the maximum temperature
#' exceeds the minimum flight temperature.
#' - **Diapause**: The diapause is initiated when the daylength falls below a threshold.
#' - **Mortality**: White stages (egg to pupa) die on a fixed date.
#'
#' `r .doc_functioning_post('phenips')`
#'
#' @usage
#'
#' phenology("phenips", ..., tmean, tmax, rad, daylength,
#'           exposure = 'sunny', sister_broods = TRUE)
#'
#' # calculate submodels separately
#' phenology("phenips", ..., .submodels = 'onset', tmax)
#' phenology("phenips", ..., .submodels = 'diapause', daylength)
#' phenology("phenips", ..., .submodels = 'mortality', tmax)
#' phenology("phenips", ..., .submodels = 'development',
#'           .onset, .diapause = NULL, .mortality = NULL,
#'           tmean, tmax, rad,
#'           exposure = 'sunny', sister_broods = TRUE)
#'
#' @param ... `r .doc_phenology_dots()`
#' @param tmean,tmax Daily mean/maximum temperatures in °C.
#' @param rad Daily radiation in W * h / m^2.
#' @param daylength Length of the day in hours. Can be created with
#' [create_daylength_rst()] or [create_daylength_rst()].
#' @param exposure Specifies the sun exposure. Can be `'sunny'` (default) or `'shaded'`.
#' @param sister_broods Set `FALSE` if sister broods should not be calculated.
#'
#' @return `r .doc_return_pheno()`
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @name model.phenips.apply
#' @seealso [model()], [phenology()], [`model.phenips.customize`]
#' @family {phenology applications}
#'
#' @encoding UTF-8
NULL





phenips_calc_btmean <- function(.params,
                                .storage = NULL,
                                .quiet = FALSE,
                                tmean,
                                rad,
                                exposure = 'sunny') {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # if tree is sunny incorporate radiation into temperatures
  if(exposure == 'sunny') return(.params$func_btmean(tmean, rad))

  return(tmean)
}



phenips_calc_btmax <- function(.params,
                               .storage = NULL,
                               .quiet = FALSE,
                                tmax,
                                rad,
                                exposure = 'sunny') {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # if tree is sunny incorporate radiation into temperatures
  if(exposure == 'sunny') return(.params$func_btmax(tmax, rad))

  return(tmax)
}



phenips_calc_teff <- function(.params,
                              .storage = NULL,
                              .quiet = FALSE,
                              btmean,
                              btmax,
                              exposure = 'semi-shaded') {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  teff <- terra::ifel(btmean < .params$tlow | btmean > .params$tup, 0, btmean)

  # if the maximum temperature rises above `topt`, the linear relation should be corrected
  diff <- .params$func_btdiff(btmax)
  teff <- teff - terra::ifel(btmax > .params$topt & diff > 0, diff, 0)

  # substract the temperature threshold to get the effective temperature
  teff <- teff - .params$dd_development_base

  # only return non-negative values
  return(terra::ifel(teff > 0, teff, 0))
}




phenips_calc_diapause <- function(.params,
                                  .storage = NULL,
                                  .quiet = FALSE,
                                  daylength) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # find the longest day as the diapause can only start later
  dates <- terra::time(daylength)
  year <- format(dates[[1]], '%Y')
  longest_day <- as.Date(paste0(year, '-06-21'))

  # find days which fall below a specific daylength after the longest day of the year
  out <- purrr::map(terra::as.list(daylength < .params$daylength_dia), function(rst) {
    if(terra::time(rst) < longest_day) return(rst * 0)
    return(rst)
  })

  # the first day found will trigger the diapause
  return(.trigger_rst(terra::rast(out)))
}



phenips_calc_mortality <- function(.params,
                                   .storage = NULL,
                                   .quiet = FALSE,
                                   tmin = NULL,
                                   tmean = NULL,
                                   tmax = NULL,
                                   rad = NULL) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  rsts <- list(tmin, tmean, tmax, rad)

  keys <- purrr::map_lgl(rsts, \(x) !is.null(x))
  if(!any(keys)) stop('model `phenips` needs tmin, tmean, tmax or rad to calculate mortality!')

  rst <- rsts[[which(keys)[1]]]

  return(.fixed_day_mortality(rst, .params$model_end_date))
}



# TODO: function is used by many models --> change function name?

phenips_develop_generation <- function(.params,
                                       .onset,
                                       .diapause,
                                       .mortality,
                                       teff,
                                       fly,
                                       period,
                                       .last = NULL,
                                       .storage = NULL,
                                       .quiet = FALSE) {


  # use storage if requested
  if(is.character(.storage)) return(.use_storage(.skip = c('period')))

  if(!is.null(.params$model_end_date)) {

    end_date <- .get_date_of_year(teff, .params$model_end_date)
    if(all(terra::time(teff) > end_date)) teff <- teff * 0
    else if(any(terra::time(teff) > end_date)) {

      end_lyr <- .lyr_from_date(teff, .params$model_end_date)
      teff <- c(teff[[1:end_lyr]], teff[[(end_lyr + 1):terra::nlyr(teff)]] * 0)
    }
  }


  teff <- teff * period

  # get last temperature sum from backup
  if(!is.null(.last)) teff[[1]] <- teff[[1]] + terra::ifel(.last == -1, 0, .last) * .params$dd_total_dev


  # calculate cumulative development
  dev <- cumsum(teff / .params$dd_total_dev)

  if(!is.null(.mortality) & !(is.null(.params$dev_mortal_min) & is.null(.params$dev_mortal_max))) {

    kill <- (.mortality & period)

    while(TRUE) {

      kill_ <- kill
      if(!is.null(.params$dev_mortal_min)) kill_ <- (kill_ & dev > .params$dev_mortal_min)
      if(!is.null(.params$dev_mortal_max)) kill_ <- (kill_ & dev < .params$dev_mortal_max)

      if(sum(terra::values(kill_), na.rm = TRUE) == 0) break

      lyr <- terra::which.lyr(kill_)

      trigger_kill <- .trigger_rst(kill_)



      dev <- terra::ifel(trigger_kill, 0, dev)

      new_period <- .trigger_rst((!.diapause) & fly & c(trigger_kill[[1]] & FALSE, trigger_kill)[[1:terra::nlyr(trigger_kill)]])
      dev <- dev + cumsum(new_period * teff / .params$dd_total_dev)

      kill <- terra::app(c(lyr, kill), \(x) {
        if(is.na(x[[1]])) return(rep(NA, length(x) - 1))
        x[x[[1]] + 1] <- FALSE
        return(x[2:length(x)])
      })
    }
  }

  return(terra::ifel(dev == 0, -1, dev))
}



phenips_calc_development <- function(.params,
                                     .onset,
                                     .diapause,
                                     .mortality,
                                     sister_broods = TRUE,
                                     teff,
                                     fly,
                                     .storage = NULL,
                                     .quiet = FALSE) {

  if(isTRUE(sister_broods)) sister_broods <- .params$dev_sister_brood
  if(isFALSE(sister_broods)) sister_broods <- NULL

  if(is.null(.diapause)) .diapause <- as.logical(.template_rst(teff))



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
    else storage_gen <- file.path(.storage, paste0('gen_', generation))

    # calculate development of current generation
    dev_raw <- phenips_develop_generation(.params, .onset, .diapause, .mortality, teff, fly, period_gen, NULL,
                              .storage = storage_gen, .quiet = .quiet)


    # calculate development of sister brood(s)
    dev_sis <- purrr::map(sister_broods, \(sis) {
      period_sis <- .trigger_rst(fly * (dev_raw > sis) * (!.diapause))

      if(is.null(.storage)) storage_sis <- NULL
      else storage_sis <- file.path(.storage, paste0('gen_', generation + 0.5))

      d <- phenips_develop_generation(.params, .onset, .diapause, .mortality, teff, fly, period_sis, NULL,
                                      .storage = storage_sis, .quiet = .quiet)

      return(d)
    })
    names(dev_sis) <- as.character(sister_broods)

    dev <- terra::ifel(dev_raw == -1, -1,
                       terra::clamp((dev_raw - .params$dev_start) / (.params$dev_end - .params$dev_start), 0))
    out[[paste0('gen_', generation)]] <- dev
    out[[paste0('gen_', generation, '_raw')]] <- dev_raw

    # save sister broods
    purrr::walk(sister_broods, \(sis) {

      vals <- terra::values(dev_sis[[as.character(sis)]], FALSE)

      if(sum(vals[vals >= 0], na.rm = TRUE) > 0) {

        out[[paste0('gen_', generation + 0.5)]] <<- terra::ifel(dev_sis[[as.character(sis)]] == -1, -1,
                                                                terra::clamp((dev_sis[[as.character(sis)]] - .params$dev_start) / (.params$dev_end - .params$dev_start), 0))
        out[[paste0('gen_', generation + 0.5, '_raw')]] <<- dev_sis[[as.character(sis)]]
      }

    })

    # only account for temperatures after development of last generation is finished
    period_gen <- .trigger_rst(fly * (dev >= 1) * (!.diapause))

    # break if no development will happen
    if(sum(terra::values(period_gen), na.rm = TRUE) == 0) break

    generation <- generation + 1
  }

  return(out)
}



# register model with default parameters
.create_model('phenips',
             list(
               params = list(
                 dd_onset_start_date = '04-01',
                 dd_onset_base = 8.3,
                 dd_development_base = 8.3,
                 dd_total_dev = 557,
                 dd_onset_threshold = 140,

                 topt = 30.4,
                 tlow = 8.3,
                 tup = 38.9,
                 tfly = 16.5,

                 dev_start = 0,
                 dev_end = 1,
                 dev_sister_brood = 0.5,
                 dev_mortal_min = NULL,
                 dev_mortal_max = 0.6,
                 daylength_dia = 14.5,
                 model_end_date = '10-31',

                 func_btmean = \(tmean, rad) { -0.173 + 0.0008518 * rad + 1.054 * tmean},
                 func_btmax = \(tmax, rad) { 1.656 + 0.002955 * rad + 0.534 * tmax + 0.01884 * tmax ^ 2 },
                 func_btdiff = \(btmax) { (-310.667 + 9.603 * btmax) / 24 }
               ),

               onset = list(
                 setup = list(dd_onset = .calc_dd_onset_tmax,
                              fly = .calc_fly),
                 compute = .calc_onset_fly_dd
               ),

               development = list(
                 setup = list(btmean = phenips_calc_btmean,
                              btmax = phenips_calc_btmax,
                              teff = phenips_calc_teff,
                              fly = .calc_fly),
                 compute = phenips_calc_development
               ),

               diapause = list(
                 compute = phenips_calc_diapause
               ),

               mortality = list(
                 compute = phenips_calc_mortality
               )
             )
)


