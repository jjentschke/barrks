



#' Analyse a phenology
#'
#' Here, all functions are listed that are available to analyse the results of
#' a [phenology()]-call.
#'
#' @details
#'
#' Get phenology properties:
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^prop_'), '()]', collapse = '\n')`
#'
#' Get phenology results:
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^get_'), '()]', collapse = '\n')`
#'
#' Plot phenology results:
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^plot_'), '()]', collapse = '\n')`
#'
#' @name analyse.phenology
NULL



#' Make a numeric generations raster categorical
#'
#' Make a numeric generations raster categorical.
#'
#' @param rst A numeric SpatRaster that represents bark beetle generations.
#' Sister broods are defined by adding `0.5` to the respective generation.
#' @param colors,labels Vectors of colors/labels starting from zero generations followed
#' consecutively by elements for the respective generations (including sister broods).
#'
#' @return A categorical SpatRaster.
#'
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
#' @name get_generations
NULL

#' @describeIn get_generations Returns a multi-layer SpatRaster of generations.
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




#' @describeIn get_generations Returns a data frame of generations.
#' @order 2
#' @export

get_generations_df <- function(pheno,
                               stations = prop_stations(pheno),
                               dates = prop_dates(pheno),
                               threshold = 0) {

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  rst <- get_generations_rst(pheno, dates, threshold, categorical = FALSE)
  return(.rsts2df(list(generation = rst), stations))
}






#' @describeIn get_generations Returns a SpatRaster of the generations that are
#' able to hibernate (only available if the model's end date has been reached).
#' @order 3
#' @export

get_hibernating_generations_rst <- function(pheno,
                                            categorical = TRUE,
                                            colors = barrks_colors('raster'),
                                            labels = barrks_labels('raster')) {

  if(categorical) return(categorize_generations_rst(pheno$hibernating_generations, colors, labels))
  return(pheno$hibernating_generations)
}



#' @describeIn get_generations Returns a data frame of the generations that are
#' able to hibernate (only available if the model's end date has been reached).
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
#' Get the beetles development of specific generations.
#'
#' @param pheno `r .doc_pheno()`
#' @param stations `r .doc_stations()`
#' @param dates `r .doc_dates()`
#' @param generation `r .doc_generation()`
#'
#' @name get_development
NULL

#' @describeIn get_development Returns a multi-layer SpatRaster. A value of -1
#' implies that the generation is not present yet.
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

#' @describeIn get_development Multiple generations are allowed as value for
#' `generation`. Returns a data frame which contains a field for
#' each generation  (`gen_1`, `gen_1.5`, `gen_2`, `gen_2.5`, ...). A value of -1
#' implies that the generation is not present yet.
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
#' Get onset, diapause or mortality as day of year or raw output.
#'
#' @param pheno `r .doc_pheno()`
#' @param stations `r .doc_stations()`
#' @param as_doy If `TRUE`, the day of year will be returned. If `FALSE` the
#' phenological events will be returned in a raw format. Then, the return
#' value could be used as input for [phenology()]/[bso_phenology()]
#' (parameters `.onset`, `.diapause` and `.mortality`).
#' @param dates `r .doc_dates()`
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

    z <- terra::app(c(doy, z), \(y) {
      r <- y[-1]
      r[y[1] - first_doy + 1] <- FALSE
      r
    })

    if(sum(terra::values(z), na.rm = TRUE) == 0 | first_only) break
  }

  return(res)
}




.doy2date <- function(doy, year) {
  first_day <- as.Date(paste0(year, '-01-01'))
  return(first_day + doy - 1)
}

