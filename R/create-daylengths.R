
#' Create daylength rasters
#'
#' Generate a multi-layer SpatRaster of daylengths for a given template.
#' The package [geosphere] is required to use this function.
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
#' @return Returns a multi-layer SpatRaster. Each layer represents one date.
#'
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



#' Create a data frame of daylengths
#'
#' Generate a data frame of daylengths for given latitudes.
#' The package [geosphere] is required to use this function.
#'
#' @param lat (Named) vector of latitudes. The names of the vector indicate the
#' respective stations. If no names are provided, numbers are used instead.
#' @param dates Dates that should be processed.
#' @param .quiet `r .doc_quiet()`
#'
#' @return A data frame with the columns `date`, `station` and `daylength`.
#'
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

    return(data.frame(date = rep(date, length(lat)),
                      station = names(lat),
                      daylength = as.double(geosphere::daylength(lat, date))))

  })
}
