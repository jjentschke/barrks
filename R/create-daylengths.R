
#' Create day length rasters
#'
#' Generate a multi-layer SpatRaster of day lengths for a given template.
#' The package `geosphere` is required to use this function.
#'
#' @param template (Multi-layer) SpatRaster that determines the
#' spatial extent of the result.
#' @param dates Dates that should be processed. If not specified, the dates of
#' `template` are used through [terra::time()].
#' @param crs Coordinate reference system with longitude/latitude metrics.
#' It is used to project the raster coordinates to be able to retrieve the
#' latitude.
#' @param .quiet `r .doc_quiet()`
#'
#' @returns A multi-layer SpatRaster. Each layer represents one date.
#'
#' @examplesIf rlang::is_installed("geosphere")
#' \donttest{
#' # calculate day length, use barrks_data()$tmin as template
#' dl <- create_daylength_rst(barrks_data()$tmin)
#'
#' # plot day length on May 1st, 2015
#' terra::plot(dl[[terra::time(dl) == '2015-05-01']])
#' }
#' @seealso [create_daylength_df()]
#'
#' @export

create_daylength_rst <- function(template,
                                 dates = terra::time(template),
                                 crs = 'EPSG:4258',
                                 .quiet = FALSE) {

  if(!requireNamespace('geosphere', quietly = TRUE)) stop('package geosphere required!')

  dates <- as.Date(dates)

  template <- template[[1]] * 0 + 1

  # retrieve coordinates from raster cells and project to long/lat crs
  coords <- terra::vect(terra::crds(template, na.rm = FALSE))
  terra::crs(coords) <- terra::crs(template)
  coords_prj <- terra::project(coords, crs)

  # calculate daylengths for all dates
  out <- terra::rast(purrr::map(dates, .progress = .get_pb(.quiet), function(date) {

    daylengths <- as.double(geosphere::daylength(terra::geom(coords_prj)[, 'y'], date))
    return(terra::setValues(template, daylengths) * template)
  }))

  # adjust dates of output
  terra::time(out) <- dates
  names(out) <- paste0('daylengths-', dates)

  return(out)
}



#' Create a data frame of day lengths
#'
#' Generate a data frame of day lengths for given latitudes.
#' The package `geosphere` is required to use this function.
#'
#' @param lat (Named) vector of latitudes. The names of the vector indicate the
#' respective stations. If no names are provided, numbers are used instead.
#' @param lat Data frame with the fields `station` and `lat`. Defines
#' the latitude for the respective stations.
#' @param dates Dates that should be processed.
#' @param .quiet `r .doc_quiet()`
#'
#' @returns A data frame with the columns `date`, `station` and `daylength`.
#'
#' @examplesIf rlang::is_installed("geosphere")
#' # dates of interest
#' date_start <- as.Date('2020-01-01')
#' date_end <- as.Date('2020-12-31')
#'
#'
#' # calculate day length
#' dl <- create_daylength_df(barrks_data('station_coords'),
#'                           seq(date_start, date_end, by = 'day'))
#'
#' # print day lengths of station 'Freiburg'
#' head(dl[dl$station == 'Freiburg',], 10)
#' @seealso [create_daylength_rst()]
#'
#' @export

create_daylength_df <- function(lat,
                                dates,
                                .quiet = FALSE) {

  if(!requireNamespace('geosphere', quietly = TRUE)) stop('package geosphere required!')

  # set names if no names are provided
  if(is.null(names(lat))) names(lat) <- 1:length(lat)

  # calculate daylengths for all dates
  purrr::map_dfr(as.Date(dates), .progress = .get_pb(.quiet), \(date) {

    return(data.frame(date = rep(date, nrow(lat)),
                      station = lat$station,
                      daylength = as.double(geosphere::daylength(lat$lat, date))))

  })
}
