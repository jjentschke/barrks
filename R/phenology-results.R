



#' Make a numeric generations raster categorical
#'
#' Make a numeric generations raster categorical. Useful when mathematical
#' operations were performed with generations rasters (use
#' [`get_generations_rst(..., categorical = FALSE)`][get_generations_rst] to get numeric generations
#' rasters).
#'
#' @param rst A numeric SpatRaster that represents bark beetle generations.
#' Sister broods are defined by adding `0.5` to the respective generation.
#' @param colors,labels Vectors of colors/labels starting from zero generations followed
#' consecutively by elements for the respective generations (including sister broods).
#'
#' @returns A categorical SpatRaster.
#'
#'
#' @examples
#' \donttest{
#' # calculate phenology with different models
#' p1 <- phenology('phenips-clim', barrks_data(), .quiet = TRUE)
#' p2 <- phenology('phenips', barrks_data(), .quiet = TRUE)
#'
#' # get the generation as numerical rasters to allow mathematical operations
#' gens1 <- get_generations_rst(p1, categorical = FALSE)
#' gens2 <- get_generations_rst(p2, categorical = FALSE)
#'
#' # calculate the maximum generations from the 2 models
#' gens_max <- max(gens1, gens2)
#' # categorize the results
#' gens_max_cat <- categorize_generations_rst(gens_max)
#'
#' # plot the categorized raster
#' terra::plot(gens_max_cat)
#' # plot the uncategorized raster
#' terra::plot(gens_max)
#' }
#' @export

categorize_generations_rst <- function(rst,
                                       colors = barrks_colors(),
                                       labels = barrks_labels()) {

  if(is.null(rst)) return(NULL)
  if(all(is.na(terra::values(rst)))) return(rst)


  n_vals <- min(length(labels), length(colors))
  max_gen <- n_vals %/% 2 + 0.5 * (n_vals %% 2)

  max_gen_rst <- max(terra::values(rst), na.rm = TRUE)

  if(max_gen_rst > max_gen)
    stop('not enough labels/colors passed, raster contains cells with ', max_gen_rst, ' generations')

  out <- rst * 0
  values <- 1:n_vals


  if(max_gen >= 1) generations <- c(0, seq(1, max_gen, 0.5))
  else generations <- 0


  # transform to integer raster
  purrr::walk(values, function(i) {
    out <<- out + terra::ifel(rst == generations[[i]], i, 0)
  })

  # set levels
  df_levels <- data.frame(id = values, category = labels[1:n_vals], cat = labels[1:n_vals])
  purrr::walk(1:terra::nlyr(out), function(i) levels(out[[i]]) <<- df_levels)


  # set colors
  if(!is.null(colors)) {
    coltab <- data.frame(value = c(), color = c())

    purrr::walk(values, \(i) {
      coltab <<- rbind(coltab, c(value = i, col = colors[[i]]))
    })

    purrr::walk(1:terra::nlyr(out), \(i) terra::coltab(out[[i]]) <<- coltab)
  }

  names(out) <- names(rst)

  return(out)
}





#' Get generations
#'
#' Find out how many generations are present (or have reached a development
#' threshold).
#'
#' @param pheno `r .doc_pheno()`
#' @param stations `r .doc_stations()`
#' @param dates `r .doc_dates()`
#' @param threshold Threshold of the beetle development to account for a generation.
#' @param generations Numeric vector that determines which generations should be
#' included in the result.
#' @param categorical Set `FALSE` if the SpatRaster that is returned
#' should be numeric. Otherwise, it will be categorical.
#' @param colors,labels Vectors of colors/labels starting from zero generations followed
#' consecutively by elements for the respective generations (including sister broods).
#'
#' @returns
#'
#' * `get_generations_rst()`: A multi-layer SpatRaster.
#' * `get_hibernating_generations_rst()`: A SpatRaster. Only available
#'   if the model's end date has been reached. Otherwise all values will be `NA`.
#' * `get_generations_df()`: A data frame.
#' * `get_hibernating_generations_df()`: A data frame. Only available if the
#'   model's end date has been reached. Otherwise all values will be `NA`.
#'
#' @examples
#' \donttest{
#' # calculate phenology
#' p <- phenology('phenips-clim', barrks_data(), .quiet = TRUE)
#'
#' # get the generations raster
#' gens <- get_generations_rst(p)
#'
#' # plot the generations raster
#' terra::plot(gens)
#' }
#' @name get_generations
NULL

#' @rdname get_generations
#' @order 1
#' @export

get_generations_rst <- function(pheno,
                                dates = prop_last_date(pheno),
                                threshold = 0,
                                generations = prop_hatched_generations(pheno),
                                categorical = TRUE,
                                colors = barrks_colors('raster'),
                                labels = barrks_labels('raster')) {

  lyrs <- prop_dates(pheno) %in% as.Date(dates)
  out <- 0 * pheno$development$gen_1[[lyrs]]

  purrr::walk(generations, \(generation) {
    if(!paste0('gen_', generation) %in% names(pheno$development)) return()
    out <<- terra::ifel(pheno$development[[paste0('gen_', generation)]][[lyrs]] >= threshold, generation, out)
  })

  names(out) <- paste0('generations-', terra::time(out))

  if(categorical) out <- categorize_generations_rst(out, colors, labels)

  return(out)
}




#' @rdname get_generations
#' @order 2
#' @export

get_generations_df <- function(pheno,
                               stations = prop_stations(pheno),
                               dates = prop_dates(pheno),
                               threshold = 0,
                               generations = prop_hatched_generations(pheno)) {

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  rst <- get_generations_rst(pheno, dates, threshold, generations, categorical = FALSE)
  return(.rsts2df(list(generation = rst), stations))
}






#' @rdname get_generations
#' @order 3
#' @export

get_hibernating_generations_rst <- function(pheno,
                                            categorical = TRUE,
                                            colors = barrks_colors('raster'),
                                            labels = barrks_labels('raster')) {

  if(categorical) return(categorize_generations_rst(pheno$hibernating_generations, colors, labels))
  return(pheno$hibernating_generations)
}



#' @rdname get_generations
#' @order 4
#' @export

get_hibernating_generations_df <- function(pheno,
                                           stations = prop_stations(pheno)) {

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  rst <- get_hibernating_generations_rst(pheno, categorical = FALSE)
  return(.rsts2df(list(hibernating_generation = rst), stations))
}



#' Get the beetles development
#'
#' Get the beetles development of specific generations.A value of -1
#' implies that the generation is not present yet.
#'
#' @param pheno `r .doc_pheno()`
#' @param stations `r .doc_stations()`
#' @param dates `r .doc_dates()`
#' @param generation `r .doc_generation()` `get_development_df()` allows
#' multiple generations here.
#'
#' @returns
#'
#' * `get_development_rst()`: A multi-layer SpatRaster.
#' * `get_development_df()`: A data frame which contains a field for
#'    each generation  (`gen_1`, `gen_1.5`, `gen_2`, `gen_2.5`, ...) requested.
#'
#' @examples
#' \donttest{
#' # calculate station-based phenology
#' p <- phenology('phenips-clim', barrks_data('stations'), .quiet = TRUE)
#'
#' # print the development data frame of station 'Freiburg'
#' df <- get_development_df(p, 'Freiburg')
#' df[,4:ncol(df)] <- round(df[,4:ncol(df)], 3) # round results
#' df
#' }
#' @name get_development
NULL

#' @rdname get_development
#' @order 1
#' @export

get_development_rst <- function(pheno,
                                generation,
                                dates = prop_dates(pheno)) {

  if(length(generation) > 1) stop('`generation` should be of length 1.')
  if(!generation %in% prop_hatched_generations(pheno)) stop('Generation ', generation, ' is not available.')

  out <- pheno$development[[paste0('gen_', generation)]]
  return(out[[which(terra::time(out) %in% as.Date(dates))]])
}

#' @rdname get_development
#' @order 2
#' @export

get_development_df <- function(pheno,
                               stations = prop_stations(pheno),
                               generation = prop_hatched_generations(pheno),
                               dates = prop_dates(pheno)) {

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  rsts <- purrr::map(generation, \(g) get_development_rst(pheno, g, dates))
  names(rsts) <- paste0('gen_', generation)

  return(.rsts2df(rsts, stations))
}



#' Get onset, diapause or mortality
#'
#' Get onset, diapause or mortality as day of year or raw output. Note that
#' multiple mortality events are possible over the season.
#'
#' @param pheno `r .doc_pheno()`
#' @param stations `r .doc_stations()`
#' @param as_doy If `TRUE`, the day(s) of year will be returned. If `FALSE` the
#' phenological events will be returned in a raw format. Then, the return
#' value could be used as input for [phenology()]/[bso_phenology()]
#' (parameters `.onset`, `.diapause` and `.mortality`).
#' @param dates `r .doc_dates()`
#'
#' @returns
#'
#' * `get_onset_rst()`, `get_diapause_rst()`, `get_mortality_rst()`: A
#'   (multi-layer) SpatRaster.
#' * `get_onset_df()`, `get_diapause_df()`, `get_mortality_df()`: A data frame.
#'
#' @examples
#' \donttest{
#' # calculate phenology
#' p <- phenology('phenips-clim', barrks_data(), .quiet = TRUE)
#'
#' # plot onset, diapause, mortality
#' get_onset_rst(p) |> terra::plot()
#' get_diapause_rst(p) |> terra::plot()
#' get_mortality_rst(p)[[1]] |> terra::plot()
#'
#' }
#'
#'
#' @name get_events
#' @aliases get_onset get_diapause get_mortality
NULL

#' @describeIn get_events Returns a (multi-layer) SpatRaster of the onset.
#' @order 1
#' @export

get_onset_rst <- function(pheno,
                          as_doy = TRUE,
                          dates = prop_dates(pheno)) {

  if(is.null(pheno$onset)) return(NULL)

  onset <- pheno$onset[[terra::time(pheno$onset) %in% as.Date(dates)]]

  if(as_doy) return(.get_doy_rst(onset))
  return(onset)
}

#' @describeIn get_events Returns a data frame of the onset.
#' @order 2
#' @export

get_onset_df <- function(pheno,
                         stations = prop_stations(pheno),
                         as_doy = TRUE,
                         dates = prop_dates(pheno)) {

  if(is.null(pheno$onset)) return(NULL)

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  out <- .rsts2df(list(onset_doy = get_onset_rst(pheno, as_doy, dates)), stations)
  if(as_doy) out$onset_date <- .doy2date(out$onset, prop_year(pheno))

  return(out)
}

#' @describeIn get_events Returns a (multi-layer) SpatRaster of the diapause.
#' @order 3
#' @export

get_diapause_rst <- function(pheno,
                             as_doy = TRUE,
                             dates = prop_dates(pheno)) {

  if(is.null(pheno$diapause)) return(NULL)

  diapause <- pheno$diapause[[terra::time(pheno$diapause) %in% as.Date(dates)]]

  if(as_doy) return(.get_doy_rst(diapause))
  return(diapause)
}

#' @describeIn get_events Returns a data frame of the diapause.
#' @order 4
#' @export

get_diapause_df <- function(pheno,
                            stations = prop_stations(pheno),
                            as_doy = TRUE,
                            dates = prop_dates(pheno)) {

  if(is.null(pheno$diapause)) return(NULL)

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  out <- .rsts2df(list(diapause_doy = get_diapause_rst(pheno, as_doy, dates)), stations)
  if(as_doy) out$diapause_date <- .doy2date(out$diapause, prop_year(pheno))

  return(out)
}


#' @describeIn get_events Returns a (multi-layer) SpatRaster of the mortality.
#' @order 5
#' @export

get_mortality_rst <- function(pheno,
                              as_doy = TRUE,
                              dates = prop_dates(pheno)) {

  if(is.null(pheno$mortality)) return(NULL)

  mortality <- pheno$mortality[[terra::time(pheno$mortality) %in% as.Date(dates)]]

  if(as_doy) return(.get_doy_rst(mortality, FALSE))
  return(mortality)
}

#' @describeIn get_events Returns a data frame of the mortality.
#' @order 6
#' @export

get_mortality_df <- function(pheno,
                             stations = prop_stations(pheno),
                             as_doy = TRUE,
                             dates = prop_dates(pheno)) {

  if(is.null(pheno$mortality)) return(NULL)

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  out <- .rsts2df(list(mortality_doy = get_mortality_rst(pheno, as_doy, dates)), stations)
  if(as_doy) out$mortality_date <- .doy2date(out$mortality, prop_year(pheno))

  return(out)
}


#' Get (preprocessed) input data
#'
#' The function returns a list that contains the input data of the phenology as
#' well as some intermediate results that are needed as preprocessed inputs for the
#' model. The result can be used as input for `phenology()` to avoid redundant
#' calculations.
#'
#' @param pheno `r .doc_pheno()`
#'
#' @returns A list of SpatRasters.
#'
#' @examples
#' \donttest{
#' # setup phenology
#' p <- phenology('phenips-clim', barrks_data(), .setup_only = TRUE, .quiet = TRUE)
#'
#' # get the (preprocessed) input data
#' inputs <- get_input_data(p)
#'
#' # print the names to show which input data is available
#' names(inputs)
#' }
#' @export

get_input_data <- function(pheno) {
  return(pheno$data)
}







.get_doy_rst <- function(x, first_only = TRUE) {

  z <- x * 1
  dates <- terra::time(z)
  first_doy <- lubridate::yday(min(dates))

  res <- NULL

  for(i in 1:terra::nlyr(z)) {

    doy <- terra::app(terra::which.lyr(z), \(y) {
      as.numeric(strftime(dates[y], format = "%j"))
    })

    if(is.null(res)) res <- doy
    else res <- c(res, doy)

    z <- z & .trigger_rst(c(z[[1]] * 0, z)[[1:terra::nlyr(z)]])

    if(sum(terra::values(z), na.rm = TRUE) == 0 | first_only) break
  }

  return(res)
}




.doy2date <- function(doy, year) {
  first_day <- as.Date(paste0(year, '-01-01'))
  return(first_day + doy - 1)
}

