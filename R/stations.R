


#' Assign stations to a phenology
#'
#' Assign stations to a phenology.
#'
#' @param pheno `r .doc_pheno()`
#' @param stations Use the return value of [stations_create()] here.
#'
#' @return Returns the phenology that was passed to the function with stations assigned.
#'
#' @seealso [stations_create()]
#'
#' @export

stations_assign <- function(pheno, stations) {
  pheno$stations <- stations
  return(pheno)
}


#' Build stations
#'
#' Build stations that can be used to extract point data from a phenology.
#'
#' @param station_names Character vector that specifies the names of the stations.
#' @param cells Numbers of the cells that should be represented by the stations.
#'
#' @return Returns stations that can be assigned to a phenology object or used for
#' `get_..._df`-functions.
#'
#' @seealso [stations_assign()]
#'
#' @export

stations_create <- function(station_names, cells) {

  names(cells) <- station_names
  return(cells)
}

#' Get the names of stations
#'
#' Get the names of stations.
#'
#' @param stations Stations created with [stations_create()] or obtained by
#' [prop_stations()].
#'
#' @export
stations_names <- function(stations) {
  return(names(stations))
}

#' Get the raster cells of stations
#'
#' Get the raster cells of stations.
#'
#' @param stations Stations created with [stations_create()] or obtained by
#' [prop_stations()].
#'
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


