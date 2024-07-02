

#' Create rasters that indicate sunrise and sunset
#'
#' Generate a list of two multi-layer SpatRasters for a given template that
#' indicate sunrise and sunset for the respective cells.
#' The package `suncalc` is required to use this function.
#'
#' @param template (Multi-layer) SpatRaster that determines the
#' spatial extent of the result.
#' @param dates Dates that should be processed. If not specified, the dates of
#' `template` are used through [terra::time()].
#' @param crs Coordinate reference system with longitude/latitude metrics.
#' It is used to project the raster coordinates to be able to retrieve longitude
#' and latitude.
#' @param tz Timezone of the results.
#' @param .quiet `r .doc_quiet()`
#'
#' @returns A list with the elements `sunrise` and `sunset` which are
#' both multi-layer SpatRasters. The values indicate the respective time in
#' minutes. Each layer represents one date.
#'
#' @examplesIf rlang::is_installed("suncalc")
#' \donttest{
#' # calculate suntimes, use barrks_data()$tmin as template
#' st <- create_suntimes_rsts(barrks_data()[[1]])
#'
#' # plot results on May 1st, 2015
#' terra::plot(st$sunrise[[terra::time(st$sunrise) == '2015-05-01']])
#' terra::plot(st$sunset[[terra::time(st$sunset) == '2015-05-01']])
#' }
#' @seealso [create_suntimes_df()]
#'
#' @export

create_suntimes_rsts <- function(template,
                                 dates = terra::time(template),
                                 crs = 'EPSG:4258',
                                 tz = Sys.timezone(),
                                 .quiet = FALSE) {

  if(!requireNamespace('suncalc', quietly = TRUE)) stop('package suncalc required!')

  if(is.null(dates)) return(NULL)

  template <- terra::setValues(template[[1]], 0)

  sunrise <- c()
  sunset <- c()

  coords <- terra::vect(terra::crds(template))
  terra::crs(coords) <- terra::crs(template)
  coords_prj <- terra::project(coords, crs)

  data <- purrr::map(as.Date(dates), .progress = .get_pb(.quiet), \(date) {

    sun <- suncalc::getSunlightTimes(data = data.frame(lat = terra::geom(coords_prj)[, 'y'],
                                                       lon = terra::geom(coords_prj)[, 'x'],
                                                       date = rep(date, length(coords_prj))),
                                     tz = tz)

    sr <- 60 * as.numeric(format(sun$sunrise, '%H')) + as.numeric(format(sun$sunrise, '%M'))
    ss <- 60 * as.numeric(format(sun$sunset, '%H')) + as.numeric(format(sun$sunset, '%M'))


    return(list(terra::setValues(template, sr),
                terra::setValues(template, ss)))
  })

  sunrise <- terra::rast(purrr::map(data, \(element) element[[1]]))
  sunset <- terra::rast(purrr::map(data, \(element) element[[2]]))

  terra::time(sunrise) <- dates
  terra::time(sunset) <- dates

  return(list(sunrise = sunrise, sunset = sunset))
}



#' Create a data frame of sunrises and sunsets
#'
#' Generate a data frame that specifies sunrises and sunsets for different
#' coordinates and dates.
#' The package `suncalc` is required to use this function.
#'
#' @param coords Data frame with the fields `station`, `lat` and `lon`. Defines
#' the latitude and longitude for the respective stations.
#' @param dates Dates that should be processed.
#' @param tz Timezone of the results.
#' @param .quiet `r .doc_quiet()`
#'
#' @returns A data frame with the columns `date`, `station` and `sunrise` and
#' `sunset`. The values of sunrise and sunset indicate the respective time in
#' minutes.
#'
#' @examplesIf rlang::is_installed("suncalc")
#' \donttest{
#' date_start <- as.Date('2020-01-01')
#' date_end <- as.Date('2020-12-31')
#'
#' st <- create_suntimes_df(barrks_data('station_coords'),
#'                          seq(date_start, date_end, by = 'day'))
#'
#' # print results of station 'Freiburg'
#' head(st[st$station == 'Freiburg',], 10)
#' }
#' @seealso [create_suntimes_rsts()]
#'
#' @export

create_suntimes_df <- function(coords,
                               dates,
                               tz = Sys.timezone(),
                               .quiet = FALSE) {

  if(!requireNamespace('suncalc', quietly = TRUE)) stop('package suncalc required!')

  purrr::map_dfr(as.Date(dates), .progress = .get_pb(.quiet), \(date) {
    purrr::map_dfr(coords$station, \(stat) {
      sun_times <- suncalc::getSunlightTimes(date = date,
                                             lat = dplyr::filter(coords, coords$station == stat)$lat,
                                             lon = dplyr::filter(coords, coords$station == stat)$lon,
                                             tz = tz)

      sunrise <- sun_times$sunrise
      sunset <- sun_times$sunset

      return(data.frame(date = date,
                        station = stat,
                        sunrise = 60 * as.numeric(format(sunrise, '%H')) + as.numeric(format(sunrise, '%M')),
                        sunset = 60 * as.numeric(format(sunset, '%H')) + as.numeric(format(sunset, '%M'))))
    })
  })
}


