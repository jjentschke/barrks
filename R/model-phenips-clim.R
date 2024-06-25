#' @include model-phenips.R
NULL


#' Customize PHENIPS-Clim
#'
#' In barrks, [model()] is used to customize a model. Here, the parameters are
#' described that can be used to customize PHENIPS-Clim. The model
#' is currently unpublished. This manual will be updated as soon as a
#' publication is available. Look [here][model.phenips_clim.customize] to find
#' out how to apply the model.
#'
#' `r .doc_customize_call('PHENIPS-Clim', 'phenips-clim')`
#'
#' ```{r, eval = FALSE}
#' model("phenips-clim",
#'
#'       # ==== onset ====
#'
#'       dd_onset_start_date = '03-01',
#'       dd_onset_base = 12,
#'       onset_func = function(tmax, dd_tmax) 0.564071 * tmax + 0.006434 * dd_tmax - 12.37046 > 0,
#'       onset_add_dd = c('0.1' = 0, '0.5' = 90, '0.9' = 190),
#'
#'       # ==== development ====
#'
#'       model_end_date = '12-31',
#'       tfly = 16.5,
#'
#'       dd_total_dev = 557,
#'
#'       dev_oviposition = c('0.1' = 0.1,
#'                           '0.5' = 0.15,
#'                           '0.9' = 0.26),
#'       dev_end = 1,
#'       dev_sister_brood = 0.3,
#'
#'       dev_mortal_min = NULL,
#'       dev_mortal_max = 0.6,
#'
#'       topt = 30.4,
#'
#'       func_btmean = function(tmean, rad) { -0.173 + 0.0008518 * rad + 1.054 * tmean},
#'       func_btmax = function(tmax, rad) { 1.656 + 0.002955 * rad + 0.534 * tmax + 0.01884 * tmax ^ 2 },
#'       func_btdiff = function(tmax) { (-310.667 + 9.603 * tmax) / 24 },
#'
#'       dev_rates = phenips_clim_get_dev_rates(),
#'
#'       # ==== diapause ====
#'
#'       first_diapause_date = '08-12',
#'       diapause_thermal_func = function(daylength, tmax) 0.8619156 * daylength + 0.5081128 * tmax - 23.63691 > 0,
#'       daylength_dia = 14.5,
#'
#'       # ==== mortality ====
#'
#'       tlethal = -5
#' )
#' ```
#'
#' @param dd_onset_start_date The date, when the degree days start to sum up ('MM-DD').
#' @param dd_onset_base Base temperature to calculate degree days to trigger the onset.
#' @param onset_func Function with the SpatRasters `tmax` (maximum temperature)
#' and `dd_tmax` (degree days of maximum temperature) as parameters.
#' The function should return `TRUE` when the base onset is
#' triggered. See `onset_add_dd` for the actual onset of infestation.
#' @param onset_add_dd Vector of options to calculate the actual onset of
#' infestation. The vector should be named after the share of beetles that
#' already started breeding when the onset is triggered (choose an option via
#' `phenology(..., onset_mode = [option])` when applying the
#' model). The values specify the degree days that are required starting at the
#' first positive return value of `onset_func`.
#' @param model_end_date Date when the model ends.
#' @param tfly Minimum temperature that beetles need to fly.
#' @param dd_total_dev Degree days that are required for a generation to fully
#' develop
#' @param dev_oviposition Named numeric vector of shares in the total development
#' when the oviposition is finished. The vector should be named after the share
#' of beetles that should be taken into account (choose an option via
#' `phenology(..., oviposition_mode = [option])` when applying the
#' model).
#' @param dev_end Share in total development when the juvenile beetle's
#' development ends. Usable if the development above this threshold should
#' account for mating, oviposition etc.
#' @param dev_sister_brood Share in the total development, when a sister brood
#' will be established.
#'
#' @param dev_mortal_min,dev_mortal_max Minimum/maximum share in the total
#' development of white stages (egg, larva, pupa). During these stages, the
#' beetles could die caused by a mortality event.
#'
#' @param topt Temperature for optimal development.
#'
#' @param func_btmean,func_btmax,func_btdiff Functions to calculate the
#' bark temperatures (see \insertCite{Baier2007;nobrackets}{barrks},
#' equations A.3 to A.5). Each parameter will be passed as SpatRaster:
#'
#' - `tmean`: mean air temperature
#' - `tmax`: maximum air temperature
#' - `rad`: radiation
#' - `btmax`: maximum bark temperature
#'
#' @param dev_rates Data frame that specifies the development rates per day depending
#' on the mean temperature and the temperature amplitude. Column names are the
#' mean temperatures and row names the temperature amplitudes both with one
#' decimal place.
#' base onset (see `onset_func`) to trigger the actual onset.
#'
#' @param first_diapause_date Date before which an initiation of the diapause is
#' impossible ('MM-DD').
#' @param diapause_thermal_func Function to calculate the initiation
#' of the diapause if the model was applied using `phenology(..., diapause_mode = 'thermal')`.
#' The diapause will be initiated the last time when the function returns `TRUE`.
#' @param daylength_dia When the daylength falls below this threshold, diapause
#' will be initiated if the model was applied using
#' `phenology(..., diapause_mode = 'photoperiodic')`.
#'
#' @param tlethal Temperature threshold below which white stages will die.
#'
#'
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.phenips_clim.customize
#' @seealso [model()], [phenology()], [`model.phenips_clim.apply`]
#' @family {model customizations}
#'
#' @encoding UTF-8
NULL



#' Use PHENIPS-Clim
#'
#' This page describes the usage of PHENIPS-Clim with [phenology()].
#' The model specific inputs are listed and its basic functionality is explained.
#' PHENIPS-Clim is not published yet. This manual will be updated when a
#' publication is available. It was parametrized for *Ips typographus* in southern Germany.
#'
#' In `barrks`, [phenology()] is used to apply a model. The following code
#' illustrates which inputs are required to apply PHENIPS-Clim and which additional
#' parameters are available.
#'
#' ```{r, eval = FALSE}
#' phenology("phenips-clim", ..., tmin, tmean, tmax, rad, daylength,
#'           sister_broods = TRUE, scenario = 'max', exposure = NULL,
#'           onset_mode = NULL, oviposition_mode = NULL, diapause_mode = NULL)
#'
#' # calculate submodels separately
#' phenology("phenips-clim", ..., .submodels = 'onset', tmax, scenario = 'max', onset_mode = NULL)
#' phenology("phenips-clim", ..., .submodels = 'diapause', tmax, daylength, scenario = 'max', diapause_mode = NULL)
#' phenology("phenips-clim", ..., .submodels = 'mortality', tmin)
#' phenology("phenips-clim", ..., .submodels = 'development',
#'           .onset, .diapause = NULL, .mortality = NULL,
#'           tmin, tmean, tmax, rad, sister_broods = TRUE,
#'           scenario = 'max', exposure = NULL, oviposition_mode = NULL)
#' ```
#'
#' @section Functioning:
#'
#' `r .doc_functioning_pre('phenips-clim', 'PHENIPS-Clim')`
#'
#' - **Onset**: A base onset is triggered by a logistic model that relates to the
#' maximum temperature and the respective degree days. Beginning from the base onset,
#' a specific level of degree days (depending on the share of individuals that
#' should be accounted for)  and maximum air temperature must be reached to
#' trigger the actual onset.
#' - **Development**: While the bark temperature and the emergence of new
#' generations are determined according to [PHENIPS][model.phenips.apply], the
#' calculation of the beetles' development rates is refined. Rather than implying
#' a constant development within a day, temperature fluctuations are incorporated
#' by taking the daily temperature amplitude into account. Additionally, the
#' first part of development represents the pre-oviposition period and will
#' not appear in the resulting output.
#' - **Diapause**: The diapause can be initiated due to the photoperiod according
#' to [PHENIPS][model.phenips.apply] or by a logistic model that depends on the
#' daylength and the maximum temperature and accounts for beetles that reproduce
#' even on shorter days if the temperatures are favorable. In the second case,
#' PHENIPS-Clim detects a reproductive arrest, due to adverse abiotic parameters,
#' and not an actual diapause as the output can be adjusted, if conditions improve
#' and allow for further reproduction later in the season.
#' - **Mortality**: White stages (egg to pupa) die when the minimum temperature
#' falls below a specific threshold.
#'
#' `r .doc_functioning_post('phenips_clim')`
#'
#' @param ... `r .doc_phenology_dots()`
#' See [phenology()] for details.
#' @param tmin,tmean,tmax Daily minimum/mean/maximum temperatures in °C. `tmin`
#' is optional. If available it will be used to
#' calculate the temperature amplitude. If not, `(tmax - tmean) * 2` will be
#' used as amplitude.
#' @param rad Daily radiation in W * h / m^2.
#' @param daylength Length of the day in hours. Can be created with
#' [create_daylength_rst()] or [create_daylength_rst()].
#' @param sister_broods Set `FALSE` to disable the calculation of sister broods.
#' @param scenario Choose a scenario to use a suitable combination of parameters
#' for specific situations. The scenario defines a default value for each value
#' that can be overwritten by specifying a value for the respective parameter.
#' The following scenarios are available:
#'
#' - mean: `list(exposure = 'sunny', onset_mode = 0.5, diapause_mode = 'photoperiodic', oviposition_mode = 0.5)`
#' - max: `list(exposure = 'sunny', onset_mode = 0.1, diapause_mode = 'thermal', oviposition_mode = 0.1)`
#'
#' @param exposure Specifies the sun exposure. Can be `'sunny'`(default) or `'shaded'`.
#' @param onset_mode Share of beetles that are already infesting trees necessary to
#' trigger the onset. Must be `0.1`, `0.5` or `0.9` if not customized.
#' @param oviposition_mode Share of beetles that should have finished oviposition
#' to trigger the beginning of the development. Must be `0.1`, `0.5` or `0.9` if not customized.
#' @param diapause_mode Determines how the diapause is initiated. Can be one of
#' the following options:
#'
#' - `'photoperiodic'`: The diapause is initiated when the daylength falls below
#'   a specific threshold.
#' - `'thermal'`: The diapause is initiated by a logistic model that depends on
#'   the daylength and the maximum temperature.
#'
#' Share of beetles that already stopped reproducing necessary to
#' trigger the diapause. Must be `thermal` or `'photoperiodic'` if not customized.
#' If `'photoperiodic'` is chosen, the diapause is controlled by a daylength
#' threshold (see parameter daylength_dia [here][model.phenips_clim.customize]).
#'
#'
#' @name model.phenips_clim.apply
#' @seealso [model()], [phenology()], [`model.phenips_clim.customize`]
#' @family {phenology applications}
#'
#' @encoding UTF-8
NULL



phenips_clim_use_scenario <- function(arg, scenario, value = NULL) {

  if(!is.null(value)) return(value)

  return(
    switch(scenario,
           mean = list(exposure = 'sunny', onset_mode = 0.5, diapause_mode = 'photoperiodic', oviposition_mode = 0.5),
           max = list(exposure = 'sunny', onset_mode = 0.1, diapause_mode = 'thermal', oviposition_mode = 0.1))[[arg]]
  )
}



phenips_clim_calc_teff <- function(.params,
                                   .storage = NULL,
                                   .quiet = FALSE,
                                   btmean,
                                   btmax,
                                   rad,
                                   tmin = NULL,
                                   scenario = 'max',
                                   exposure = NULL) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  exposure <- phenips_clim_use_scenario('exposure', scenario, exposure)

  teffs <- .params$dev_rates * .params$dd_total_dev


  # calculate temperature amplitude (use tmin if available)
  .msg(4, .quiet, 'calculate amplitude')
  if(is.null(tmin)) amplitude <- btmax - btmean
  else amplitude <- (btmax - tmin) / 2

  minrow <- min(as.numeric(rownames(teffs)))
  maxrow <- max(as.numeric(rownames(teffs)))
  mincol <- min(as.numeric(colnames(teffs)))
  maxcol <- max(as.numeric(colnames(teffs)))

  amplitude <- terra::clamp(round(amplitude * 10), minrow, maxrow)
  btmean <- terra::clamp(round(btmean * 10), mincol, maxcol)

  # get the respective values from the development rates matrix
  .msg(4, .quiet, 'get effective temperature from temperature/amplitude')

  data <- terra::sds(terra::as.int(amplitude), terra::as.int(btmean))

  out <- terra::app(data, \(x) {
    if(is.na(x[[1]]) | is.na(x[[2]])) return(NA)
    return(teffs[as.character(x[[1]]), as.character(x[[2]])])
  })

  # set dates
  terra::time(out) <- terra::time(btmean)

  return(out)
}



phenips_clim_calc_onset <- function(.params,
                                    .storage = NULL,
                                    .quiet = FALSE,
                                    .last = NULL,
                                    tmax,
                                    dd_onset,
                                    scenario = 'max',
                                    onset_mode = NULL) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())


  onset_mode <- phenips_clim_use_scenario('onset_mode', scenario, onset_mode)

  # check if onset_mode has a valid value
  if(!as.character(onset_mode) %in% names(.params$onset_add_dd)) {
    stop('`onset_mode` must be one of: ',
         paste(names(.params$onset_add_dd), collapse = ', '))
  }


  # calculate the base onset
  .msg(4, .quiet, 'calculate the base onset')
  out <- .params$onset_func(tmax, dd_onset)

  add_dd <- .params$onset_add_dd[[as.character(onset_mode)]]
  .msg(4, .quiet, 'add ', add_dd, ' degree days to get the onset of the first ', onset_mode * 100, ' % beetles')

  if(add_dd > 0) {

    # calculate the effective max temperature sum from the base onset
    lyr <- terra::which.lyr(out)
    lyr <- terra::ifel(is.na(lyr), terra::nlyr(dd_onset), lyr)
    dd_tmax_diff <- dd_onset - terra::app(c(lyr, dd_onset), \(x) x[[x[[1]] + 1]])

    # wait for an additional tmaxsum according to `onset_mode`
    out <- (dd_tmax_diff >= add_dd)
  }

  # the maximum temperature must exceed the minimum flight temperature
  out <- .trigger_rst(out & tmax > .params$tfly)
  # an onset in a backup will trigger the onset too
  if(!is.null(.last)) out <- out | .last

  # set dates
  terra::time(out) <- terra::time(tmax)

  return(out)
}



phenips_clim_calc_diapause <- function(.params,
                                       .storage = NULL,
                                       .quiet = FALSE,
                                       daylength,
                                       tmax,
                                       scenario = 'max',
                                       diapause_mode = NULL) {


  # use storage if requested
  if(is.character(.storage)) return(.use_storage(.update_all = TRUE))


  diapause_mode <- phenips_clim_use_scenario('diapause_mode', scenario, diapause_mode)

  # check if diapause_mode has a valid value
  valid_keys <- c('thermal', 'photoperiodic')
  if(!diapause_mode %in% valid_keys)
    stop('`diapause_mode` must be one of: ', paste(valid_keys, collapse = ', '))

  if(diapause_mode == 'photoperiodic') return(phenips_calc_diapause(.params,
                                                                     .storage,
                                                                     .quiet,
                                                                     daylength))

  # set earliest diapause date for current year
  first_diapause_lyr <- .lyr_from_date(tmax, .params$first_diapause_date)

  # trigger diapause when the diapause_func returns the last TRUE
  if(length(first_diapause_lyr) == 0) diapause <- tmax * 0
  else {
    nlyr <- terra::nlyr(daylength)
    diapause_cond <- .params$diapause_thermal_func(daylength, tmax)
    diapause_start <- nlyr + 1 - terra::which.lyr(diapause_cond[[nlyr:1]])
    diapause_start <- terra::ifel(diapause_start < first_diapause_lyr, NA, diapause_start)
    diapause <- terra::rast( purrr::map(1:nlyr, \(l) terra::ifel(diapause_start <= l, 1, 0)) )
  }

  terra::time(diapause) <- terra::time(tmax)

  return(as.logical(diapause))
}



phenips_clim_calc_mortality <- function(.params,
                                        .storage = NULL,
                                        .quiet = FALSE,
                                        tmin) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())


  res <- tmin < .params$tlethal

  dates <- terra::time(res)
  year <- lubridate::year(dates)
  end_lyr <- which(dates == paste0(year, '-', .params$model_end_date))

  if(length(end_lyr) > 0) res[[end_lyr]] <- as.logical(res[[end_lyr]] * 0 + 1)

  return(res)
}


phenips_clim_calc_development <- function(.params,
                                          .onset,
                                          .diapause,
                                          .mortality,
                                          sister_broods = TRUE,
                                          scenario = 'max',
                                          oviposition_mode = NULL,
                                          teff,
                                          fly,
                                          .storage = NULL,
                                          .quiet = FALSE) {

  oviposition_mode <- phenips_clim_use_scenario('oviposition_mode', scenario, oviposition_mode)

  .params$dev_start <- .params$dev_oviposition[as.character(oviposition_mode)]
  .params$dev_end <- .params$dev_start + 1
  if(!is.null(.params$dev_mortal_min))
     .params$dev_mortal_min <- .params$dev_start + (.params$dev_end - .params$dev_start) * .params$dev_mortal_min
  if(!is.null(.params$dev_mortal_max))
    .params$dev_mortal_max <- .params$dev_start + (.params$dev_end - .params$dev_start) * .params$dev_mortal_max

  phenips_calc_development(.params,
                           .onset,
                           .diapause,
                           .mortality,
                           sister_broods,
                           teff,
                           fly,
                           .storage,
                           .quiet)
}




#' Get development rates for PHENIPS-Clim
#'
#' @returns Returns a matrix that determines the development in relation to the
#' mean temperature and the temmperature amplitude. Col names indicate the mean
#' temperature and row names the temperature amplitude in °C * 10.
#'
#' @export

# TODO: good solution?

phenips_clim_get_dev_rates <- function() {

  df <- readr::read_csv(system.file('extdata/dev-rates-phenips-clim.csv', package = 'barrks'),
                         show_col_types = FALSE)
  out <- as.matrix(df)
  rownames(out) <- rownames(df)
  colnames(out) <- colnames(df)

  return(out)
}


# register model with default parameters
.create_model('phenips-clim',
              list(
                params = list(

                  dd_onset_start_date = '03-01',
                  dd_onset_base = 12,

                  onset_func = \(tmax, dd_tmax) 0.564071 * tmax + 0.006434 * dd_tmax - 12.37046 > 0,
                  onset_add_dd = c('0.1' = 0, '0.5' = 90, '0.9' = 190),

                  model_end_date = '12-31',

                  tfly = 16.5,
                  dd_total_dev = 557,

                  dev_oviposition = c('0.1' = 0.1,
                                      '0.5' = 0.15,
                                      '0.9' = 0.26),
                  dev_end = 1,
                  dev_sister_brood = 0.3,
                  dev_mortal_min = NULL,
                  dev_mortal_max = 0.6,

                  topt = 30.4,

                  func_btmean = function(tmean, rad) { -0.173 + 0.0008518 * rad + 1.054 * tmean},
                  func_btmax = function(tmax, rad) { 1.656 + 0.002955 * rad + 0.534 * tmax + 0.01884 * tmax ^ 2 },
                  func_btdiff = function(tmax) { (-310.667 + 9.603 * tmax) / 24 },

                  dev_rates = phenips_clim_get_dev_rates(),

                  first_diapause_date = '08-12',
                  diapause_thermal_func = \(daylength, tmax) 0.8619156 * daylength + 0.5081128 * tmax - 23.63691 > 0,
                  daylength_dia = 14.5,

                  tlethal = -5
                ),


                onset = list(
                  setup = list(dd_onset = .calc_dd_onset_tmax),
                  compute = phenips_clim_calc_onset
                ),

                development = list(
                  setup = list(btmean = phenips_calc_btmean,
                               btmax = phenips_calc_btmax,
                               teff = phenips_clim_calc_teff,
                               fly = .calc_fly),
                  compute = phenips_clim_calc_development
                ),

                diapause = list(
                  compute = phenips_clim_calc_diapause
                ),

                mortality = list(
                  compute = phenips_clim_calc_mortality
                )
              )
)


