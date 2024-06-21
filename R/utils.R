

# declare environment for global variables

env_barrks <- new.env(parent = emptyenv())




.process_data <- function(.data = NULL,
                         .win = NULL,
                         .dates = NULL,
                         .ext = 'tif',
                         ...) {

  if(is.character(.data)) {
    files <- list.files(.data, paste0('\\.', .ext, '$'))

    .data <- purrr::map(files, \(file) terra::rast(file.path(.data, file), win = .win))

    rst_names <- stringr::str_extract(files, paste0('(.*)\\.', .ext, '$'), 1)
    names(.data) <- rst_names
  }

  if(is.data.frame(.data)) {
    args_data <- .df2rsts(.data)
    stations <- .extract_stations(.data)
    out <- c(list(...), args_data)
  }
  else {
    if(is.list(.data)) {
      out <- .data
      additional <- list(...)
      purrr::walk(names(additional), \(key) out[key] <<- additional[key] )
    } else out <- list(...)
    stations <- NULL
  }

  if(!is.null(.dates)) {
    out <- purrr::map(out, \(x) {
      if('SpatRaster' %in% class(x)) if(!any(is.na(terra::time(x)))) {
        lyrs <- as.Date(terra::time(x)) %in% as.Date(.dates)
        if(any(lyrs) > 0) return(x[[lyrs]])
        else return(NULL)
      }

      return(x)
    })
  }

  return(out)
}






.df2rsts <- function(df, cols = colnames(df)) {

  dates <- as.Date(unique(df$date))
  stations <- .extract_stations(df)
  nstations <- length(stations)
  template <- terra::rast(nrows = nstations, ncols = 1, nlyrs = length(dates))
  terra::time(template) <- dates

  cols <- cols[!(cols %in% c('date', 'station'))]

  out <- purrr::map(cols, function(col) {

    x <- template
    lyrs <- terra::nlyr(x)

    purrr::walk(1:length(stations), function(i) {
      station <- names(stations)[i]
      df_st <- df[df$station == station,]
      tmp <- df_st[order(df_st$date),][[col]]
      x[i] <<- c(tmp, rep(NA, lyrs - length(tmp)))
    })

    return(x)
  })

  names(out) <- cols

  return(out)
}


.rsts2df <- function(rsts, stations) {

  labels <- names(stations)
  if(is.null(labels)) labels <- paste0('s', 1:length(stations))

  dfs <- purrr::map(names(rsts), function(key) {

    rst <- rsts[[key]]
    dates <- terra::time(rst)
    doys <- lubridate::yday(dates)
    nlyr <- terra::nlyr(rst)


    purrr::map_dfr(1:length(stations), function(i) {

      vals <- as.double(rst[stations[[i]]])

      out <- data.frame(station = rep(labels[[i]], nlyr))
      out$date <- dates
      out$doy <- doys
      out[[key]] <- vals

      return(out)

    })
  })

  df <- purrr::reduce(dfs, dplyr::full_join, by = c('station', 'date', 'doy'))
  if(all(is.na(df$date))) df <- dplyr::select(df, - 'date')
  if(all(is.na(df$doy))) df <- dplyr::select(df, - 'doy')

  return(df)
}




.template_rst <- function(x) {
  if(is.data.frame(x)) {
    x <- x[,c('date', 'station')]
    x$dummy <- 0

    data <- .df2rsts(x)
    return(data[[1]])
  }

  return(x * 0)
}



.trigger_rst <- function(rst) {

  inf <- terra::nlyr(rst) + 1

  # terra::app is working different for one layer?!
  pos <- terra::app(c(rst, as.logical(rst[[1]] * 0)), function (x) {
    if(all(is.na(x))) return(NA)

    y <- match(TRUE, x)
    if(is.na(y)) return(Inf)
    return(y)
  })

  out <- terra::rast(
    purrr::map(1:terra::nlyr(rst), function(i) { pos <= i })
  )

  terra::time(out) <- terra::time(rst)

  return(out)
}


.get_date_of_year <- function(rst, date) {

  dates <- terra::time(rst)
  year <- format(dates[[1]], '%Y')

  return(as.Date(paste0(year, '-', date)))
}



.lyr_from_date <- function(rst, date) {

  dates <- terra::time(rst)
  which(dates == .get_date_of_year(rst, date))
}



.storage_sub <- function(storage, sub) {
  if(is.null(storage)) return(NULL)
  return(file.path(storage, sub))
}


