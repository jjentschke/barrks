


.fixed_day_mortality <- function(template, date) {

  out <- .template_rst(template)

  lyr <- .lyr_from_date(template, date)
  if(length(lyr) > 0) out[[lyr]] <- out[[lyr]] * 0 + 1

  return(as.logical(out))
}


.calc_tmin_mortality <- function(.params,
                                 .storage = NULL,
                                 .quiet = FALSE,
                                 tmin) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # get first mortality date of the current year
  first_lethal_date <- .get_date_of_year(tmin, .params$first_lethal_date)
  first_lethal_lyr <- .lyr_from_date(tmin, .params$first_lethal_date)

  # mortality condition
  if(length(first_lethal_lyr) == 0) {
    if(all(terra::time(tmin) < first_lethal_date)) out <- tmin * 0
    else out <- (tmin <= .params$tlethal)
  }
  else {
    out <- (tmin[[first_lethal_lyr:terra::nlyr(tmin)]] <= .params$tlethal)
    out <- c(tmin[[1:(first_lethal_lyr - 1)]] * 0, out)
  }

  terra::time(out) <- terra::time(tmin)

  return(as.logical(out))
}


.calc_dd_onset_func <- function(param_start_date = 'dd_onset_start_date',
                                param_base = 'dd_onset_base') {

  function(.params,
           .storage = NULL,
           .quiet = FALSE,
           .last = NULL,
           t) {

    if(is.character(.storage)) return(.use_storage())

    # find start date of the current year
    dates <- terra::time(t)
    year <- format(dates[[1]], '%Y')
    start_date <- as.Date(paste0(year, '-', .params[[param_start_date]]))

    # calculate temperature above .params[[param_base]]
    t <- t - .params[[param_base]]
    t <- terra::ifel(t > 0, t, 0)

    # ignore dates before the start date
    first <- which(dates < start_date)
    second <- which(dates >= start_date)

    if(length(first) > 0) {
      if(length(second) > 0) t <- c(t[[first]] * 0, t[[second]])
      else t <- t[[first]] * 0
    }

    # if a backup was recovered, add its temperature sum to the first new result
    if(!is.null(.last)) t[[1]] <- t[[1]] + .last

    # build cumulative sum
    return(cumsum(t))
  }
}

.calc_dd_onset_func.tmax <- function(param_start_date = 'dd_onset_start_date',
                                     param_base = 'dd_onset_base') {

  function(.params,
           .storage = NULL,
           .quiet = FALSE,
           .last = NULL,
           tmax) {

    if(is.character(.storage)) return(.use_storage())

    f <- .calc_dd_onset_func(param_start_date, param_base)
    f(.params,
      .storage = NULL,
      .quiet = FALSE,
      .last = NULL,
      tmax)
  }
}

.calc_dd_onset_func.tmean <- function(param_start_date = 'dd_onset_start_date',
                                      param_base = 'dd_onset_base') {

  function(.params,
           .storage = NULL,
           .quiet = FALSE,
           .last = NULL,
           tmean) {

    if(is.character(.storage)) return(.use_storage())

    f <- .calc_dd_onset_func(param_start_date, param_base)
    f(.params,
      .storage = NULL,
      .quiet = FALSE,
      .last = NULL,
      tmean)
  }
}


.calc_onset_fly_dd_func <- function(param_dd_threshold = 'dd_onset_threshold') {

  function(.params,
           .storage = NULL,
           .quiet = FALSE,
           .last = NULL,
           fly,
           dd_onset) {

    # use storage if requested
    if(is.character(.storage)) return(.use_storage())

    # check onset condition to trigger the onset
    out <- .trigger_rst(dd_onset >= .params[[param_dd_threshold]] & fly)
    # an onset in a backup will trigger the onset too
    if(!is.null(.last)) out <- out | .last

    return(out)
  }
}


.calc_fly <- function(.params,
                      tmax,
                      .storage = NULL,
                      .quiet = NULL) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  return(tmax > .params$tfly)
}
