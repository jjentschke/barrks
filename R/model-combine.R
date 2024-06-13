

#' Combine different (sub-)models
#'
#' Combine different (sub-)models.
#'
#' @param ... Phenology models, model names or lists with the keys `model` and
#' `submodels`. In the last case, only the submodels specified are used (one of
#' `'onset'`, `'diapause'`, `'mortality'` or `'development'`) of the respective
#' model. If multiple models are supplied for the same submodel, the last one
#' overwrites all others.
#'
#' @seealso
#'
#' - [model()], [phenology()]
#' - Customize (sub-)models: `r .doc_customize_models()`
#' - Use (sub-)models: `r .doc_apply_models()`
#'
#' @export

model_combine <- function(...) {

  get_combined_func <- function(submodel, fun) {
    function(.model,
             .data = NULL,
             .win = NULL,
             .dates = NULL,
             .ext = 'tif',
             .storage = NULL,
             .quiet = FALSE,
             ...) {

      # start from last model added to 'overwrite' other models

      for(i in length(.model$params$models):1) {

        m <- .model$params$models[[i]]

        if(!is.null(m[[submodel]])) {

          f <- .define_model_function(submodel, paste0(submodel, ' (', m$name, ')'))

          return(f(m,
                   .data[[paste0('model', i)]],
                   .win,
                   .dates,
                   .ext,
                   .storage,
                   .quiet,
                   ...))

          out <- .call_model_function(m,
                                     m[[submodel]]$compute,
                                     list(...)[[paste0('model', i)]],
                                     .storage,
                                     .quiet)


          names(out) <- paste0(submodel, '-', terra::time(out))

          return(out)
        }
      }
    }
  }


  args <- list(...)
  models <- list()
  model_end_date <- NULL

  purrr::walk(1:length(args), \(i) {

    is_model <- FALSE
    if(!is.character(args[[i]])) is_model <- TRUE
    else if(!any(names(args[[i]]) == 'model')) is_model <- TRUE


    use_submodel_development <- TRUE

    if(any(names(args[[i]]) == 'model')) {
      m <- model(args[[i]]$model)

      if(!is.null(args[[i]]$submodels)) {
        m <- m[c('name', 'params', args[[i]]$submodels)]
        if(!'development' %in% args[[i]]$submodels) use_submodel_development <- FALSE
      }
    }
    else m <- model(args[[i]])

    models[[length(models) + 1]] <<- m
    if(use_submodel_development) model_end_date <<- params(m)$model_end_date
  })


  m <- list(
    name = 'combined',

    params = list(
      models = models,
      model_end_date = model_end_date
    ),

    setup = function(.params,
                     .data = NULL,
                     .win = NULL,
                     .dates = NULL,
                     .ext = 'tif',
                     .storage = NULL,
                     .quiet = FALSE,
                     .submodels = c('onset', 'diapause', 'hibernation', 'development')) {

      out <- purrr::map(1:length(.params$models), \(i) {
        if(paste0('model', i) %in% names(.data)) d <- .data[[paste0('model', i)]]
        else d <- .data

        f <- .define_setup_func(paste0('setup (', .params$models[[i]]$name, ')'))
        f(.params$models[[i]], d, .win, .dates, .ext, .storage_sub(.storage, paste0('model', i)), .quiet)
      })

      names(out) <- paste0('model', 1:length(.params$models))
      return(out)
    },

    onset = list(func = get_combined_func('onset')),
    diapause = list(func = get_combined_func('diapause')),
    mortality = list(func = get_combined_func('mortality')),

    development = list(
      func = function(.model,
                      .onset,
                      .diapause,
                      .mortality,
                      .data = NULL,
                      .win = NULL,
                      .dates = NULL,
                      .ext = 'tif',
                      .storage = NULL,
                      .quiet = FALSE) {

        for(i in length(.model$params$models):1) {

          m <- .model$params$models[[i]]

          if(!is.null(m$development)) {
            f <- .define_model_dev_function(paste0('development (', m$name, ')'))

            return(f(m,
                     .onset,
                     .diapause,
                     .mortality,
                     .data[[paste0('model', i)]],
                     .win,
                     .dates,
                     .ext,
                     .storage,
                     .quiet))
          }
        }
      }
    )
  )
}

