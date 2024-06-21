


#' Central function to calculate a phenology
#'
#' Calculate a phenology (or its subparts) with a specific model.
#'
#' @param .model A phenology model or a model name (see [model()],
#' [model_combine()]).
#' @param .data Data that will be passed to the model. It can be one of the following:
#'
#' - Character string: The raster data will be loaded from the path specified.
#' The files have to be named like the respective model inputs.
#' - Named list: Each element contains the input data according to its name.
#' - Data frame (station data): Should have the columns `date` and `station`
#' (name of the station). Additional columns have to be named like the
#' respective model inputs.
#' - Additionally, data can be passed through the \dots argument.
#'
#' Look at the model application manuals to find out which inputs are required
#' by a specific model: `r .doc_apply_models()`.
#'
#' @param .dates Vector of dates that the data should be restricted to.
#' @param .win SpatExtent to set a window (area of interest) if `.data` is a
#' path to load the raster data from.
#' @param .ext Extension of the files that should be used if `.data` is a path
#' to load the raster data from.
#' @param .onset,.diapause,.mortality Pass custom or precalculated phenological
#' events to the model. See [`create_events`] to find out how to create events
#' manually. Alternatively, the return value of [get_onset_rst()],
#' [get_diapause_rst()] or [get_mortality_rst()]
#' could be used (with `as_doy = FALSE`) to extract the respective phenological event from another phenology.
#' In that case, that phenology must match the temporal and spatial extent of the
#' other inputs.
#' @param .submodels Character vector. Specifies which submodels should be
#' calculated. Can be a subset of
#' `c('onset', 'diapause', 'mortality', 'development')`.
#' @param .setup_only If `TRUE` only the inputs will be preprocessed without
#' calculating any submodels. The preprocessed data can be used as input for
#' other [phenology()] calls and can be accessed via [get_input_data()].
#' @param .stations Assign stations to the phenology. See [stations_create()] for details.
#' @param .storage If set, the path specified here will be used to save the
#' (intermediate) results. If `phenology()` is called successively with a
#' growing amount of data, the calculations will continue where they stopped.
#' This can save calculation time especially for large raster inputs. Note that
#' this will only work of raster inputs and if `terra::sources()` is not empty.
#' Otherwise the results of the calculations will be saved but successive
#' calculations are not possible. If no input data is passed, the phenology
#' will be loaded from the storage.
#' @param .quiet `r .doc_quiet()`
#' @param ... Parameters that will be passed to the model. Must be named according
#' to the model inputs. See `.data` for alternative ways to pass data to the model.
#'
#' @return Returns a phenology as a list. Look [here][analyse.phenology] to find out how
#' a phenology can be analysed. It is not recommended to access the list elements directly.
#'
#' @seealso `r .doc_apply_models()`
#'
#' @order 1
#' @export



phenology <- function(.model,
                      .data = NULL,
                      .dates = NULL,
                      .win = NULL,
                      .ext = 'tif',
                      .onset = NULL,
                      .diapause = NULL,
                      .mortality = NULL,
                      .submodels = c('onset', 'diapause', 'mortality', 'development'),
                      .setup_only = FALSE,
                      .stations = NULL,
                      .storage = NULL,
                      .quiet = FALSE,
                      ...) {

  if(is.character(.model)) .model <- model(.model)

  purrr::walk(.submodels, \(s) {
    if(is.null(.model[[s]]$type)) return()
    if(.model[[s]]$type == 'bso') stop('model is not compatible with `phenology()`, use `bso_phenology()` instead')
  })

  .msg(1, .quiet, 'calculate phenology with model `', .model$name, '`')

  submodels_setup <- c('onset', 'diapause', 'mortality', 'development')
  submodels_setup <- submodels_setup[c(is.null(.onset), is.null(.diapause), is.null(.mortality), TRUE)]
  submodels_setup <- intersect(submodels_setup, .submodels)

  if(is.data.frame(.data)) .stations <- .extract_stations(.data)
  .data <- .setup(.model, .data, .win, .dates, .ext, .storage, .quiet, submodels_setup, .setup_only, ...)

  .development <- NULL
  dates_out <- NULL

  if(isFALSE(.setup_only)) {
    if(is.null(.onset) & 'onset' %in% .submodels) {
      .onset <- .calc_onset(.model, .data, .win, .dates, .ext, .storage, .quiet)
      if(!is.null(.onset)) dates_out <- terra::time(.onset)
    }
    if(is.null(.diapause) & 'diapause' %in% .submodels) {
      .diapause <- .calc_diapause(.model, .data, .win, .dates, .ext, .storage, .quiet)
      if(!is.null(.diapause)) dates_out <- terra::time(.diapause)
    }
    if(is.null(.mortality) & 'mortality' %in% .submodels) {
      .mortality <- .calc_mortality(.model, .data, .win, .dates, .ext, .storage, .quiet)
      if(!is.null(.mortality)) dates_out <- terra::time(.mortality)
    }

    if('development' %in% .submodels) {
      .development <- .calc_development(.model, .onset, .diapause, .mortality, .data, .win, .dates, .ext, .storage, .quiet)
      if(!is.null(.development$gen_1)) dates_out <- terra::time(.development$gen_1)
    }
  }


  out <- list(
    dates = dates_out,
    data = .data,
    onset = .onset,
    diapause = .diapause,
    mortality = .mortality,
    development = .development
  )

  if(!is.null(params(.model)$model_end_date)) {
    out$hibernating_generations <- .get_hibernation(out, params(.model)$model_end_date)
  }

  return(stations_assign(out, .stations))
}


#' Save a phenology
#'
#' Saves a phenology to a path.
#'
#' @param pheno A phenology, calculated with [phenology()].
#' @param .storage Path that defines where to save the phenology.
#' @param .submodels Which submodels should be saved.
#' @param .overwrite Should the overwirte an existing storage?
#' @param .ext Extension for raster files.
#' @param .quiet `r .doc_quiet()`
#'
#' @export

save_phenology <- function(pheno,
                           .storage,
                           .submodels = c('onset', 'diapause', 'mortality', 'development'),
                           .overwrite = FALSE,
                           .ext = '.tif',
                           .quiet = FALSE) {

  .msg(1, .quiet, 'save phenology to `', .storage, '`')

  purrr::walk(.submodels, \(s) {

    submodel <- pheno[[s]]

    if(is.null(submodel)) return()

    .msg(3, .quiet, 'submodel: ', s)

    if(s == 'development') {

      purrr::walk(names(submodel), .progress = .get_pb(.quiet), \(key) {

        path <- file.path(.storage, s, key)
        if(!dir.exists(path)) dir.create(path, recursive = TRUE)

        purrr::walk(terra::time(submodel[[key]]), \(date) {

          f <- file.path(path, paste0(key, '-', date, .ext))
          terra::writeRaster(submodel[[key]][[terra::time(submodel[[key]]) == date]], f, overwrite = .overwrite)
        })
      })


      if(!is.null(pheno$hibernating_generations)) {

        f <- file.path(.storage, paste0('hibernating_generations', .ext))
        terra::writeRaster(pheno$hibernating_generations, f, overwrite = .overwrite)
      }
    }
    else {
      path <- file.path(.storage, s)
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)

      purrr::walk(terra::time(submodel), .progress = .get_pb(.quiet), \(date) {

        f <- file.path(path, paste0(s, '-', date, .ext))
        terra::writeRaster(submodel[[terra::time(submodel) == date]], f, overwrite = .overwrite)
      })
    }
  })
}


#' Load a phenology
#'
#' Loads a phenology from a path.
#'
#' @param .storage Path where the phenology should be loaded from.
#' @param .submodels Which submodels should be loaded.
#' @param .ext Extension of the raster files.
#' @param .quiet `r .doc_quiet()`
#'
#' @export

load_phenology <- function(.storage,
                           .submodels = c('onset', 'diapause', 'mortality', 'development'),
                           .ext = '.tif',
                           .quiet = FALSE) {

  .msg(1, .quiet, 'load phenology from `', .storage, '`')

  dates <- NULL

  out <- purrr::map(.submodels, \(s) {

    path <- file.path(.storage, s)
    if(!dir.exists(path)) {
      .submodels <<- .submodels[.submodels != s]
      return()
    }

    .msg(3, .quiet, s)


    if(s == 'development') {

      dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)

      out_submodel <- purrr::map(dirs, \(dir) {
        terra::rast(list.files(file.path(path, dir), paste0(.ext, '$'), full.names = TRUE))
      })
      names(out_submodel) <- dirs

      dates <<- terra::time(out_submodel[[1]])

      return(out_submodel)
    }
    else {

      out_submodel <- terra::rast(list.files(path, paste0(.ext, '$'), full.names = TRUE))
      dates <<- terra::time(out_submodel)

      return(out_submodel)
    }
  })

  names(out) <- .submodels

  if('development' %in% .submodels) {

    f <- file.path(.storage, paste0('hibernating_generations', .ext))
    out$hibernating_generations <- terra::rast(f)
  }

  out$dates <- dates

  return(out)
}



.is_phenology <- function(pheno) {
  all(c('onset', 'diapause', 'mortality', 'development') %in% names(pheno))
}




.get_hibernation <- function(pheno, model_end_date) {

  if(is.null(pheno$development) | length(pheno$development) == 0) return(NULL)

  tmp <- .template_rst(pheno$development[[1]])

  end_lyr <- .lyr_from_date(tmp, model_end_date)

  if(length(end_lyr) == 0) lyr <- terra::setValues(tmp[[1]], NA)
  else lyr <- tmp[[1]] + end_lyr

  if(!is.null(pheno$mortality)) {

    lyr_mort <- terra::which.lyr(pheno$mortality)
    lyr <- terra::ifel(is.na(lyr_mort), lyr, lyr_mort)
  }

  out <- terra::setValues(tmp[[1]], NA)

  lyrs <- unique(terra::values(lyr, FALSE, na.rm = TRUE))
  dates <- terra::time(tmp)[lyrs]

  if(length(lyrs) > 0) {
    purrr::walk(1:length(lyrs), \(i) {

      gen <- get_generations_rst(pheno, dates[i], categorical = FALSE)

      out <<- terra::ifel(is.na(lyr), NA, terra::ifel(lyr == lyrs[i], gen, out))
    })
  }

  terra::time(out) <- NULL

  return(out)
}




.define_setup_func <- function(message = 'setup') {

  function(.model,
           .data = NULL,
           .win = NULL,
           .dates = NULL,
           .ext = 'tif',
           .storage = NULL,
           .quiet = FALSE,
           .submodels = c('onset', 'diapause', 'mortality', 'development'),
           .setup_only = FALSE,
           ...) {

    .model <- model(.model)

    .data <- .process_data(.data, .win, .dates, .ext, ...)

    if(is.function(.model$setup)) {

      # use model-specific setup function if available

      return(.model$setup(params(.model),
                          .data,
                          .win,
                          .dates,
                          .ext,
                          .storage,
                          .quiet,
                          .submodels))
    }


    # check if setup is needed

    check <- FALSE
    purrr::walk(.submodels, \(submodel_name) {
      submodel <- .model[[submodel_name]]

      if(is.list(submodel$setup)) {
        purrr::walk(names(submodel$setup), \(arg_name) {
          if(!arg_name %in% names(.data)) check <<- TRUE
        })
      }
    })

    # continue only if data is missing

    if(!check) return(.data)
    .msg(2, .quiet, message)

    # walk through submodels

    purrr::walk(.submodels, \(submodel_name) {
      submodel <- .model[[submodel_name]]

      # walk through setups of submodel

      if(is.list(submodel$setup)) {
        purrr::walk(names(submodel$setup), \(arg_name) {

          if(is.character(.setup_only) & !(arg_name %in% .setup_only)) return()

          # do not calculate parameter if it was passed with `.data`

          if(arg_name %in% names(.data)) return()

          .msg(3, .quiet, 'parameter ', arg_name)

          out <- .call_model_function(.model,
                                     submodel$setup[[arg_name]],
                                     .data,
                                     .storage_sub(.storage, arg_name),
                                     .quiet)

          if(!any(is.na(terra::time(out)))) names(out) <- paste0(arg_name, '-', terra::time(out))

          .data[[arg_name]] <<- out
        })
      }
    })

    return(.data)
  }
}



.setup <- .define_setup_func()



.define_model_function <- function(submodel, message = submodel) {

  function(.model,
           .data = NULL,
           .win = NULL,
           .dates = NULL,
           .ext = 'tif',
           .storage = NULL,
           .quiet = FALSE,
           ...) {

    .model <- model(.model)

    subm <- .model[[submodel]]

    if(is.null(subm)) return(NULL)

    .data <- .setup(.model, .data, .win, .dates, .ext, .storage, .quiet, submodel, ...)

    if(is.function(subm$func))
      return(subm$func(.model, .data, .win, .dates, .ext, .storage, .quiet))

    .msg(2, .quiet, message)


    out <- .call_model_function(.model,
                               subm$compute,
                               .data,
                               .storage_sub(.storage, submodel),
                               .quiet)

    if('SpatRaster' %in% class(out)) if(!any(is.na(terra::time(out)))) names(out) <- paste0(submodel, '-', terra::time(out))
    return(out)
  }
}



.calc_onset <- .define_model_function('onset')
.calc_diapause <- .define_model_function('diapause')
.calc_mortality <- .define_model_function('mortality')



.define_model_dev_function <- function(message = 'development') {

  function(.model,
           .onset,
           .diapause,
           .mortality,
           .data = NULL,
           .win = NULL,
           .dates = NULL,
           .ext = 'tif',
           .storage = NULL,
           .quiet = FALSE,
           ...) {


    .model <- model(.model)
    .data <- .setup(.model, .data, .win, .dates, .ext, .storage, .quiet, 'development', ...)


    if(is.function(.model$development$func))
      return(.model$development$func(.model, .onset, .diapause, .mortality,
                                     .data, .win, .dates, .ext, .storage, .quiet))

    .msg(2, .quiet, message)


    out <- .call_model_function(.model,
                               .model$development$compute,
                               .data,
                               .storage_sub(.storage, 'development'),
                               .quiet,
                               .onset = .onset,
                               .diapause = .diapause,
                               .mortality = .mortality)

    keys <- names(out)
    out <- purrr::map(keys, \(k) {
      x <- out[[k]]
      if('SpatRaster' %in% class(x)) names(x) <- paste0('development-', k, '-', terra::time(x))
      return(x)
    })
    names(out) <- keys

    return(out)
  }
}


.calc_development <- .define_model_dev_function()





.call_model_function <- function(.model,
                                .fun,
                                .data = NULL,
                                .storage = NULL,
                                .quiet = FALSE,
                                ...) {

  if(is.null(.fun)) return(NULL)

  # use only available arguments

  args <- c(.process_data(.data, ...),
            list(.params = params(.model),
                 .storage = .storage,
                 .quiet = .quiet))
  arg_names <- names(formals(.fun))

  if('...' %in% arg_names) arguments <- args
  else {
    arg_names <- arg_names[arg_names %in% names(args)]
    arguments <- args[arg_names]
  }

  # function call
  out <- do.call(.fun, c(arguments))

  return(out)
}




