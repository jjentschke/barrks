


#' Load sample data
#'
#' The package comes with sample data that allow the application of all models
#' available. The following datasets are available:
#'
#' - `raster` Contains a list of raster weather datasets for a sample area.
#'   The data was taken from Deutscher Wetterdienst (DWD).
#' - `stations` Contains sample station weather data for some cities in Germany.
#'   The data was taken from Deutscher Wetterdienst (DWD).
#'   Missing global radiation values were replaced by the mean value of the other stations.
#'
#' @param dataset Choose the dataset that should be returned.
#'
#' @source
#' https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/
#' https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical
#' https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/solar
#'
#' @export

barrks_data <- function(dataset = 'raster') {

  if(dataset == 'raster') {

    path <- system.file('extdata/sample-data', package = 'barrks')
    return(.process_data(path))
  }

  if(dataset == 'stations') return(station_data)
}

