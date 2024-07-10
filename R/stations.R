

#' Work with stations
#'
#' In `barrks`, stations are references to specific raster cells. Thus, they
#' can be used to extract point-related data from a phenology. Look
#' [here][analyse.phenology] to find out which station-based functions are
#' available to analyse a phenology.
#'
#' @param pheno `r .doc_pheno()`
#' @param stations Stations created with `stations_create()` or obtained by
#' [prop_stations()].
#' @param station_names Character vector that specifies the names of the stations.
#' @param cells Numbers of the cells that should be represented by the stations.
#'
#' @returns
#'
#' * `stations_create()`: A named numeric vector.
#' * `stations_assign()`: A phenology object (see [phenology()]).
#' * `stations_names()`: A character vector.
#' * `stations_cells()`: A numeric vector.
#'
#' @examples
#' \donttest{
#' # calculate phenology
#' p <- phenology('phenips-clim', barrks_data(), .quiet = TRUE)
#'
#' # create stations and assign them to the phenology object
#' stations <- stations_create(c('station a', 'station b'),
#'                             c(234, 345))
#' p <- stations_assign(p, stations)
#'
#' # plot the development of 'station b'
#' plot_development_diagram(p, 'station b', .lwd = 4, .legend_lty = FALSE)
#' }
#' @name stations
NULL


#' @describeIn stations Create stations.
#'
#' @order 1
#' @export

stations_create <- function(station_names, cells) {

  names(cells) <- station_names
  return(cells)
}

#' @describeIn stations Assign stations to a phenology. Returns the phenology
#' that was passed with respective stations assigned.
#'
#' @order 2
#' @export

stations_assign <- function(pheno, stations) {
  pheno$stations <- stations
  return(pheno)
}



#' @describeIn stations Get the names of stations.
#'
#' @order 3
#' @export
stations_names <- function(stations) {
  return(names(stations))
}


#' @describeIn stations Get the raster cells of stations.
#'
#' @order 4
#' @export
stations_cells <- function(stations) {
  return(stations)
}


.extract_stations <- function(df) {

  stations <- unique(df$station)
  out <- 1:length(stations)
  names(out) <- stations

  return(out)
}


