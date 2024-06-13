#' @include utils.R utils-model-functions.R
NULL




env_barrks$models <- list()


.create_model <- function(name, model) {
  model$name <- name
  env_barrks$models[[name]] <- model
}


#' Get a phenology model
#'
#' Returns a (customized) phenology model.
#'
#' @param m Name of the model or the return value of another `model()`-call.
#' @param ... List of parameters to customize the model.
#'
#' @seealso Look at the customization manuals, to find out which parameters
#' can be customized for a specific model: `r .doc_customize_models()`.
#'
#' @export

model <- function(m, ...) {

  # get model from name
  if(is.character(m)) {
    if(!m %in% names(env_barrks$models)) stop('model \'', m, '\' not available')
    m <- env_barrks$models[[m]]
  }

  # overwrite parameters
  params <- list(...)
  purrr::walk(names(params), \(par_name) m$params[[par_name]] <<- params[[par_name]])

  return(m)
}


#' Get model parameters
#'
#' Returns the parameters of a model as list.
#'
#' @param m A phenology model
#'
#'
#' @export

params <- function(m) {

  return(model(m)$params)
}


#' List all models
#'
#' Get the names of all available models.
#'
#' @export

list_models <- function() {
  return(names(env_barrks$models))
}





