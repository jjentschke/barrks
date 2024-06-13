



.use_storage <- function(.path = '',
                        .dates = NULL,
                        .add_days = c(),
                        .update_all = FALSE,
                        .check_modified = .update_all,
                        .skip = NULL,
                        .ext = '.tif',
                        .datatype = 'FLT8S'
                        ) {

  args <- as.list(sys.frame(-1), all.names = TRUE)
  .func <- sys.function(sys.parent())
  storage <- file.path(args$.storage, .path)
  .quiet <- args$.quiet

  if(is.null(.dates)) .dates <- .dates2update(storage, .update_all, .skip, .ext, args)
  else  .dates <- as.Date(.dates)

  purrr::walk(.add_days, \(d) .dates <<- unique(c(.dates, .dates + d)))

  if(!length(.dates)) return(.restore_output(storage, .quiet))

  if('.last' %in% names(args)) {
    first_new_date <- min(.dates)
    last_old_file <- list.files(storage, paste0(as.character(first_new_date - 1), .ext, '$'), full.names = TRUE)

    if(!length(last_old_file)) args_new <- args
    else {
      last_rsts <- terra::rast(last_old_file)
      last_time <- max(terra::time(last_rsts))

      args_new <- purrr::map(args, \(arg) {

        if(!'SpatRaster' %in% class(arg)) return(arg)

        keys <- (as.Date(terra::time(arg)) > last_time)
        return(arg[[keys]])
      })

      args_new$.last <- last_rsts[[terra::time(last_rsts) == last_time]]
    }
  }
  else {
    args_new <- purrr::map(args, \(arg) {

      if(!'SpatRaster' %in% class(arg)) return(arg)

      keys <- (as.Date(terra::time(arg)) %in% .dates)
      return(arg[[keys]])
    })
  }

  names(args_new) <- names(args)
  args_new$.storage <- TRUE

  return(.save_output(do.call(.func, args_new), #c(list(.params), args_new, list(.storage = TRUE, .quiet = .quiet))),
                     storage, .check_modified, .datatype, .quiet))#.quiet))
}



.dates2update <- function(.path,
                         .update_all = FALSE,
                         .skip = NULL,
                         .ext = '.tif',
                         args) {

  files <- list.files(.path, paste0(.ext, '$'), full.names = TRUE)

  if(!length(files)) dates_output <- c()
  else {
    dates_output <- as.Date(stringr::str_sub(stringr::str_extract(files, '-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}.'), 2, -2))
    mtime_output <- file.info(files)[['mtime']]
    names(mtime_output) <- dates_output
  }

  out <- c()

  purrr::walk(args[!names(args) %in% .skip], \(rst) {

    if(!'SpatRaster' %in% class(rst)) return()
    dates_input <- unique(as.Date(terra::time(rst)))

    if(.update_all | all(terra::sources(rst) == '')) {
      out <<- append(out, dates_input)
      return()
    }

    mtime_input <- purrr::map(dates_input, \(date) {

      srcs <- unique(terra::sources(rst[[as.Date(terra::time(rst)) == date]]))
      mtimes <- purrr::map_vec(srcs, \(src) file.info(src)[['mtime']] )

      return(min(mtimes))
    })

    names(mtime_input) <- dates_input

    purrr::walk(dates_input, \(date) {

      if(!date %in% dates_output) {
        out <<- append(out, date)
        return()
      }

      if(mtime_output[[as.character(date)]] < mtime_input[[as.character(date)]]) out <<- append(out, date)
    })
  })

  return(unique(out))
}


.save_raster <- function(x,
                        path,
                        label,
                        check_modified = FALSE,
                        datatype = 'FLT8S',
                        .quiet = FALSE) {

  dates <- unique(as.Date(terra::time(x)))

  .msg(5, .quiet, 'save to \'', path, '\'')

  purrr::walk(dates, .progress = .get_pb(.quiet), \(date) {

    f <- file.path(path, paste0(label, '-', date, '.tif'))
    keys <- (as.Date(terra::time(x)) == date)
    rst <- x[[keys]]

    if(check_modified & file.exists(f)) {
      rst_old <- terra::rast(f)

      if(all(terra::time(rst) == terra::time(rst_old)) &
         all(terra::values(rst) == terra::values(rst_old), na.rm = TRUE) &
         all(terra::values(is.na(rst)) == terra::values(is.na(rst)))) return()
    }

    terra::writeRaster(rst, f, overwrite = TRUE, datatype = datatype)
  })
}

.save_output <- function(x, path, check_modified = FALSE, datatype = 'FLT8S', .quiet = FALSE) {

  if(!dir.exists(path)) dir.create(path, recursive = TRUE)

  if(!is.null(x)) {
    label <- rev(strsplit(path, '/')[[1]])[1]


    if(is.list(x))
      purrr::walk(names(x), \(key) .save_raster(x[[key]],
                                               file.path(path, key),
                                               paste0(label, '_', key),
                                               check_modified,
                                               datatype,
                                               .quiet))
    else .save_raster(x, path, label, check_modified, datatype, .quiet)



  }


  return(.restore_output(path, .quiet))
}

.restore_output <- function(path, .quiet = FALSE) {

  .msg(5, .quiet, 'load from \'', path, '\'')

  files <- list.files(path, paste0('.tif', '$'), full.names = TRUE)
  out <- terra::rast(purrr::map(1:length(files), .progress = .get_pb(.quiet), \(i) terra::rast(files[[i]])))

  return(out)
}



