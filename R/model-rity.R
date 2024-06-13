#' @include model.R model-phenips.R
NULL



#' Customize RITY
#'
#' `r .doc_customize_description('RITY (also called RITY-2)', 'rity', 'Ogris2019')`
#'
#' @usage
#' model("rity",
#'
#'       # ==== onset ====
#'
#'       dd_onset_start_date = '03-07',
#'       dd_onset_base = 8.3,
#'       dd_onset_threshold = 155.6,
#'
#'       # ==== onset + development ====
#'
#'       tfly = 14.5,
#'
#'       # ==== development ====
#'
#'       dd_development_base = 8.3,
#'       dd_total_dev = 557,
#'       dev_start = 0,
#'       dev_end = 1,
#'       dev_sister_brood = 0.5,
#'       dev_mortal_min = NULL,
#'       dev_mortal_max = 0.8,
#'
#'       func_ftmin = function(tmin) { 1.44 + 0.82 * tmin },
#'       func_ftmean = function(tmean) { 0.50 + 0.81 * tmean },
#'       func_ftmax = function(tmax) { 1.03 + 0.86 * tmax },
#'
#'       func_btmin = function(ftmin) { 0.56 + 0.99 * ftmin },
#'       func_btmean = function(ftmean) { -0.48 + 1.03 * ftmean },
#'       func_btmax = function(ftmax) { 0.03 + 0.99 * ftmax },
#'
#'       dt_low = 8.3,
#'       dt_up = 38.9,
#'       topt = 30.4,
#'       tmax = 40.9958913,
#'       alpha = 0.02876507,
#'       beta = 3.5922336,
#'       gamma = 1.24657367,
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
#' @param dd_onset_base Base temperature to calculate degree days for development.
#' @param dd_total_dev Degree days that are required for a generation to fully
#' develop
#' @param dev_start,dev_end `r .doc_dev_start_end()`
#' @param dev_sister_brood Share in the total development, when a sister brood
#' will be established.
#' @param dev_mortal_min,dev_mortal_max Minimum/maximum share in the total
#' development of white stages (egg, larva, pupa). During these stages, the
#' beetles could die caused by a mortality event.
#'
#' @param func_ftmean,func_ftmax,func_atdiff Functions to caclulate the
#' air temperature in forest stands (see \insertCite{Ogris2019;nobrackets}{barrks},
#' equations 1 - 3). Each parameter will be passed as SpatRaster:
#'
#' - `tmin`: min air temperature
#' - `tmean`: mean air temperature
#' - `tmax`: maximum air temperature
#'
#' @param func_btmean,func_btmax,func_btdiff Functions to caclulate the
#' bark temperature (see \insertCite{Ogris2019;nobrackets}{barrks},
#' equations 4 - 6). Each parameter will be passed as SpatRaster:
#'
#' - `ftmin`: min air temperature in forest stands
#' - `ftmean`: mean air temperature in forest stands
#' - `ftmax`: maximum air temperature in forest stands
#'
#' @param dt_low,dt_up,topt,tmax,alpha,beta,gamma Parameters to calculate
#' the effective bark temperature (see \insertCite{Ogris2019;nobrackets}{barrks},
#' equations 7 - 9).
#'
#' @param daylength_dia When the daylength falls below this threshold, diapause
#' will be initiated.
#'
#' @param model_end_date Date when the model ends and all white stages (egg, larva, pupa) die.
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.rity.customize
#' @seealso [model()], [phenology()], [`model.rity.apply`]
#' @family {model customizations}
#'
#' @encoding UTF-8
NULL


#' Use RITY
#'
#' This page describes the usage of RITY with [phenology()].
#' The model specific inputs are listed and its basic functionality is explained.
#' RITY (also called RITY-2) was published by \insertCite{Ogris2019;textual}{barrks} and
#' parametrized for *Ips typographus* in Slovenia.
#'
#' @section Functioning:
#'
#' `r .doc_functioning_pre('rity', 'RITY')`
#'
#' - **Onset**: See [PHENIPS][model.phenips.apply].
#' - **Development**: Based on [PHENIPS][model.phenips.apply] with a few modifications:
#'   - The optimum curve is calculated according to
#'     \insertCite{Wermelinger1998;textual}{barrks} without
#'     simplification.
#'   - The minimum, mean or maximum bark temperature can be used to calculate
#'     the development. These temperatures depend only on the respective air temperatures.
#' - **Diapause**: See [PHENIPS][model.phenips.apply].
#' - **Mortality**: See [PHENIPS][model.phenips.apply].
#'
#' `r .doc_functioning_post('rity')`
#'
#' @usage
#'
#' phenology("rity", ..., tmin = NULL, tmean = NULL, tmax, daylength, mode = 'max')
#'
#' # calculate submodels separately
#' phenology("rity", ..., .submodels = 'onset', tmax)
#' phenology("rity", ..., .submodels = 'diapause', daylength)
#' phenology("rity", ..., .submodels = 'mortality', tmax)
#' phenology("rity", ..., .submodels = 'development',
#'           .onset, .diapause = NULL, .mortality = NULL,
#'           tmin = NULL, tmean = NULL, tmax = NULL, mode = 'max')
#'
#' @param tmin,tmean,tmax Daily minimum/mean/maximum temperatures in °C.
#' For the `development` submodel, the parameter that is obligatory depends on
#' `mode`.
#' @param daylength Length of the day in hours. Can be created with
#' [create_daylength_rst()] or [create_daylength_rst()].
#' @param mode Specifies which temperature should be used to calculate the
#' development. Can be `min`, `mean` or `max`.
#' @param .submodels,.onset,.diapause,.mortality,... `r .doc_phenology_dots()`
#'
#' @return `r .doc_return_pheno()`
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @name model.rity.apply
#' @seealso [model()], [phenology()], [`model.rity.customize`]
#' @family {phenology applications}
#'
#' @encoding UTF-8
NULL



#' Calculate the effective bark temperature
#' @noRd
rity_calc_teff <- function(.params,
                           .storage = NULL,
                           .quiet = FALSE,
                           tmin,
                           tmean,
                           tmax,
                           mode = 'max') {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  bt <- switch(
    mode,
    min = .params$func_btmin(.params$func_ftmin(tmin)),
    mean = .params$func_btmean(.params$func_ftmean(tmean)),
    max = .params$func_btmax(.params$func_ftmax(tmax))
  )


  linear <- bt - .params$dt_low
  nonlinear <- (.params$topt - .params$dt_low) * (exp(.params$alpha * bt) - exp(.params$alpha * .params$tmax - (.params$tmax - bt) / .params$beta) - .params$gamma)

  out <- terra::ifel(bt <= .params$dt_low, 0,
                     terra::ifel(bt <= .params$topt, linear,
                                 terra::ifel(bt < .params$dt_up, nonlinear, 0)))

  return(out)
}



# register model with default parameters
.create_model('rity',
             list(params = list(

                    dd_onset_start_date = '03-07',
                    dd_onset_base = 8.3,
                    dd_development_base = 8.3,
                    dd_total_dev = 557,
                    dd_onset_threshold = 155.6,

                    dev_start = 0,
                    dev_end = 1,
                    dev_sister_brood = 0.5,
                    dev_mortal_min = NULL,
                    dev_mortal_max = 0.6,

                    model_end_date = '10-31',

                    tfly = 14.5,

                    daylength_dia = 14.5,

                    dt_low = 8.3,
                    dt_up = 38.9,
                    topt = 30.4,
                    tmax = 40.9958913,
                    alpha = 0.02876507,
                    beta = 3.5922336,
                    gamma = 1.24657367,

                    func_ftmin = function(tmin) { 1.44 + 0.82 * tmin },
                    func_ftmean = function(tmean) { 0.50 + 0.81 * tmean },
                    func_ftmax = function(tmax) { 1.03 + 0.86 * tmax },

                    func_btmin = function(atmin) { 0.56 + 0.99 * atmin },
                    func_btmean = function(atmean) { -0.48 + 1.03 * atmean },
                    func_btmax = function(atmax) { 0.03 + 0.99 * atmax }
                  ),

                  onset = model('phenips')$onset,

                  development = list(
                    setup = list(teff = rity_calc_teff,
                                 fly = .calc_fly),
                    compute = phenips_calc_development
                  ),

                  diapause = model('phenips')$diapause,
                  mortality = model('phenips')$mortality
             )
)

