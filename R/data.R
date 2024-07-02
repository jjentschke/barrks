


#' Load sample data
#'
#' The package comes with sample data that allow the application of all models
#' available. The following data sets are available:
#'
#' - `raster` Contains a list of raster weather datasets for a sample area.
#'   The data was taken from Deutscher Wetterdienst (DWD).
#' - `stations` Contains sample station weather data for some cities in Germany.
#'   The data was taken from Deutscher Wetterdienst (DWD).
#'   Missing global radiation values were replaced by the mean value of the other stations.
#' - `station_coords` Contains the coordinates (longitude/latitude) of the
#'   stations that are included in the `stations` data set.
#'   The data was taken from Deutscher Wetterdienst (DWD).
#'
#' @source
#'
#' * \href{https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/}{https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/}
#' * \href{https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical}{https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical}
#' * \href{https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/solar}{https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/solar}
#'
#' @param dataset Choose the data set that should be returned.
#'
#' @returns The respective data set. Can be a list of SpatRasters (for
#' `dataset = 'raster'`) or a data frame.
#'
#' @examples
#' # plot first layer of the minimum temperature of the sample raster data
#' terra::plot(barrks_data()$tmin[[1]])
#'
#' # print the first lines of the sample station data
#' head(barrks_data('stations'), 10)
#'
#' # print the coordinates of the sample stations
#' barrks_data('station_coords')
#' @export

barrks_data <- function(dataset = 'raster') {

  if(dataset == 'raster') {

    path <- system.file('extdata/sample-data', package = 'barrks')

    out <- .process_data(path, .ext = 'nc')

    out$tmin <- out$tmin / 10
    out$tmean <- out$tmean / 10
    out$tmax <- out$tmax / 10
    out$daylength <- out$daylength / 100

    return(out)
  }

  if(dataset == 'stations') return(station_data)
  if(dataset == 'station_coords') return(station_coords)
}

