#' @include model-rity.R
NULL



#' Customize CHAPY
#'
#' `r .doc_customize_description('CHAPY', 'chapy', 'Ogris2020')`
#'
#' @usage
#' model("chapy",
#'
#'       # ==== onset ====
#'
#'       dd_onset_start_date = '03-09',
#'       dd_onset_base = 7.4,
#'       dd_onset_threshold = 216.5,
#'
#'       # ==== onset + development ====
#'
#'       tfly = 15.6,
#'
#'       # ==== development ====
#'
#'       dd_development_base = 7.4,
#'       dd_total_dev = 635.4,
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
#'       func_btmin = function(atmin) { 0.56 + 0.99 * atmin },
#'       func_btmean = function(atmean) { -0.48 + 1.03 * atmean },
#'       func_btmax = function(atmax) { 0.03 + 0.99 * atmax },
#'
#'       dt_low = 7.4,
#'       dt_up = 39.4,
#'       topt = 30,
#'       tmax = 41.97,
#'       alpha = 0.031,
#'       beta = 5.3,
#'       gamma = 1.25,
#'
#'       # ==== diapause ====
#'
#'       daylength_dia = 13.6,
#'
#'       # ==== mortality ====
#'
#'       model_end_date = '12-31'
#' )
#'
#' @param dd_onset_start_date The date, when the degree days start to sum up ('MM-DD').
#' @param dd_onset_base Base temperature to calculate degree days to trigger the onset.
#' @param dd_onset_threshold Degree days that are required to trigger the onset of
#' infestation. Additionally, the maximum temperature must exceed `tfly`.
#'
#' @param tfly Minimum temperature that beetles need to fly.
#'
#' @param dd_development_base Base temperature to calculate degree days for development.
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
#' the effective bark temperature (see \insertCite{Ogris2020;nobrackets}{barrks},
#' equations A.7 - A.9).
#'
#' @param daylength_dia When the daylength falls below this threshold, diapause
#' will be initiated.
#'
#' @param model_end_date Date when the model ends and all white stages (egg, larva, pupa) die.
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.chapy.customize
#' @seealso [model()], [phenology()], [`model.chapy.apply`]
#' @family {model customizations}
#'
#' @encoding UTF-8
NULL


#' Use CHAPY
#'
#' This page describes the usage of CHAPY with [phenology()].
#' The model specific inputs are listed and its basic functionality is explained.
#' CHAPY was published by \insertCite{Ogris2020;textual}{barrks} and
#' parametrized for *Pityogenes chalcographus* in Slovenia.
#'
#' @section Functioning:
#'
#' The functioning of CHAPY is identical to [RITY][model.rity.apply] but it is
#' has a different [parametrization][model.chapy.customize].
#'
#' @usage
#'
#' phenology("chapy", ..., tmin = NULL, tmean = NULL, tmax, daylength, mode = 'max')
#'
#' # calculate submodels separately
#' phenology("chapy", ..., .submodels = 'onset', tmax)
#' phenology("chapy", ..., .submodels = 'diapause', daylength)
#' phenology("chapy", ..., .submodels = 'mortality', tmax)
#' phenology("chapy", ..., .submodels = 'development',
#'           .onset, .diapause = NULL, .mortality = NULL,
#'           tmin = NULL, tmean = NULL, tmax = NULL, mode = 'max')
#'
#'
#' @param tmin,tmean,tmax Daily minimum/mean/maximum air temperatures in °C.
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
#' @name model.chapy.apply
#' @seealso [model()], [phenology()], [`model.chapy.customize`]
#' @family {phenology applications}
#'
#' @encoding UTF-8
NULL




# register model with default parameters
.create_model('chapy',
             list(
               params = list(
                 dd_onset_start_date = '03-09',
                 dd_onset_base = 7.4,
                 dd_development_base = 7.4,
                 dd_onset_threshold = 216.5,
                 dd_total_dev = 635.4,

                 tfly = 15.6,
                 dt_low = 7.4,
                 dt_up = 39.4,
                 topt = 30,
                 tmax = 41.97,
                 alpha = 0.031,
                 beta = 5.3,
                 gamma = 1.25,

                 dev_start = 0,
                 dev_end = 1,
                 dev_sister_brood = 0.5,
                 dev_mortal_min = NULL,
                 dev_mortal_max = 0.8,
                 model_end_date = '12-31',


                 daylength_dia = 13.6,


                 func_ftmin = function(tmin) { 1.44 + 0.82 * tmin },
                 func_ftmean = function(tmean) { 0.50 + 0.81 * tmean },
                 func_ftmax = function(tmax) { 1.03 + 0.86 * tmax },

                 func_btmin = function(atmin) { 0.56 + 0.99 * atmin },
                 func_btmean = function(atmean) { -0.48 + 1.03 * atmean },
                 func_btmax = function(atmax) { 0.03 + 0.99 * atmax }
               ),

               onset = model('rity')$onset,
               development = model('rity')$development,
               diapause = model('rity')$diapause,
               mortality = model('rity')$mortality
             )
)
