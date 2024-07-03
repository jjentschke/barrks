


#' Create phenological events (onset/diapause/mortality)
#'
#' Generate onset, diapause or mortality manually to be able to run [phenology()]
#' with observed or arbitrary inputs.
#'
#' @param template SpatRaster or data frame that determines the
#' spatial and temporal extent of the result. If a single-layer SpatRaster was
#' passed, the temporal extent should be defined by using `dates`.
#' @param doys Numeric vector, (multi-layer) SpatRaster or data frame that
#' specifies the days of year when the event is triggered. Vectors will define
#' the events globally whereas SpatRasters allow spatially explict definitions.
#' For the creation of events based on stations, data frames are used.
#' In that case, the field `station` specifies the station name and `doy` indicates the
#' respective day of year.
#' @param stations If `template` is a SpatRaster and `doys` is a data frame,
#' stations should be passed to define which cells are affected. See [stations_create()] for details.
#' @param dates Dates to define the temporal extent of the output if `template`
#' is a single-layer SpatRaster.
#' @param .quiet `r .doc_quiet()`
#'
#' @returns A logical multi-layer SpatRaster. Each layer represents one date.
#'
#' @examples
#' \donttest{
#' # load sample data
#' d <- barrks_data('stations')
#'
#' # create onset, diapause, mortality
#' on <- create_onset(d, lubridate::yday('2015-04-15'))
#' dia <- create_diapause(d, lubridate::yday('2015-08-15'))
#' mort <- create_mortality(d, lubridate::yday('2015-11-15'))
#'
#' # claculate phenologe
#' p <- phenology('phenips-clim', d, .quiet = TRUE,
#'                .onset = on, .diapause = dia, .mortality = mort)
#'
#' # plot development
#' plot_development_diagram(p, .lwd = 4)
#' }
#' @name create_events
NULL

#' @order 1
#' @describeIn create_events Create a onset.
#' @export

create_onset <- function(template,
                         doys = NULL,
                         stations = NULL,
                         dates = NULL,
                         .quiet = FALSE) {
  .create_events(template, doys, stations, TRUE, dates, .quiet)
}

#' @order 2
#' @describeIn create_events Create a diapause.
#' @export

create_diapause <- function(template,
                            doys = NULL,
                            stations = NULL,
                            dates = NULL,
                            .quiet = FALSE) {
  .create_events(template, doys, stations, TRUE, dates, .quiet)
}

#' @order 3
#' @describeIn create_events Create mortality events.
#' @export

create_mortality <- function(template,
                             doys = NULL,
                             stations = NULL,
                             dates = NULL,
                             .quiet = FALSE) {
  .create_events(template, doys, stations, FALSE, dates, .quiet)
}





.create_events <- function(template,
                          doys = NULL,
                          stations = NULL,
                          trigger = FALSE,
                          dates = NULL,
                          .quiet = FALSE) {


  if(is.null(doys)) {

    if('SpatRaster' %in% class(template)) if(terra::nlyr(template) == 1) {
      template <- .template_rst(template)[[1]] * 1:length(dates)
      terra::time(template) <- dates
    }

    return(as.logical(.template_rst(template)))
  }

  ndoys <- ifelse('SpatRaster' %in% class(doys), terra::nlyr(doys), length(doys))

  if(!is.data.frame(doys) & ndoys > 1) {

    if('SpatRaster' %in% class(doys)) doys <- as.list(doys)

    out <- purrr::map(doys, .progress = .get_pb(.quiet), \(doy) .create_events(template, doy))
    out <- purrr::reduce(out, \(a, b) a | b)
  }
  else {
    if(is.data.frame(template)) stations <- .extract_stations(template)

    if('SpatRaster' %in% class(template) & !is.null(dates)) {
      template <- .template_rst(template)[[1]] * 1:length(dates)
      terra::time(template) <- dates
    }

    out <- .template_rst(template)
    dates <- as.Date(terra::time(out))
    doys_rst <- lubridate::yday(dates)


    if(is.numeric(doys)) {

      # set doys globally

      val <- out[[doys_rst == doys]] + 1
      out[[which(doys_rst == doys)]] <- val
    }
    else {

      if(is.data.frame(doys)) {

        # set doys for stations

        all_doys <- unique(doys$doy)

        for(doy in all_doys) {

          station_names <- doys[doys$doy == doy, 'station']
          lyr <- which(doys_rst == doy)
          cells <- stations[station_names]

          out[[lyr]][cells] <- out[[lyr]][cells] + 1
        }
      }
      else {

        # set doys from raster

        all_doys <- unique(terra::values(doys, FALSE))

        for(doy in all_doys) {
          if(is.na(doy)) next

          lyr <- which(doys_rst == doy)
          if(length(lyr) > 0) out[[lyr]] <- terra::ifel(doys == doy, 1, out[[lyr]])
        }
      }
    }
  }


  if(trigger) out <- .trigger_rst(out)

  return(as.logical(out))
}



