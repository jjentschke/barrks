


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


.calc_dd_onset <- function(.params,
                           .quiet = FALSE,
                           .last = NULL,
                           t) {

  # find start date of the current year
  dates <- terra::time(t)
  year <- format(dates[[1]], '%Y')
  start_date <- as.Date(paste0(year, '-', .params$dd_onset_start_date))

  # calculate temperature above .params$dd_onset_base
  t <- t - .params$dd_onset_base
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

.calc_dd_onset_tmax <- function(.params, .storage = NULL, .quiet = FALSE, .last = NULL, tmax) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  .calc_dd_onset(.params, .quiet, .last, tmax)
}

.calc_dd_onset_tmean <- function(.params, .storage = NULL, .quiet = FALSE, .last = NULL, tmean) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  .calc_dd_onset(.params, .quiet, .last, tmean)
}



.calc_onset_fly_dd <- function(.params,
                               .storage = NULL,
                               .quiet = FALSE,
                               .last = NULL,
                               fly,
                               dd_onset) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # check onset condition to trigger the onset
  out <- .trigger_rst(dd_onset >= .params$dd_onset_threshold & fly)
  # an onset in a backup will trigger the onset too
  if(!is.null(.last)) out <- out | .last

  return(out)
}



.calc_fly <- function(.params,
                      tmax,
                      .storage = NULL,
                      .quiet = NULL) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  return(tmax > .params$tfly)
}
