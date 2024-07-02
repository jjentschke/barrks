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
#' @returns A phenology model. Can be passed to [phenology()].
#'
#' @seealso Look at the customization manuals, to find out which parameters
#' can be customized for a specific model: `r .doc_customize_models()`.
#'
#' @examples
#' \donttest{
#' # customize the temperature beetles need to fly for PHENIPS-Clim
#' m <- model('phenips-clim', tfly = 16)
#'
#' # calculate phenology
#' phenology(m, barrks_data(), .quiet = TRUE)
#'
#' # plot generations
#' gens <- get_generations_rst(p)
#' terra::plot(gens)
#' }
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
#' @param m Name of the model or the return value of another `model()`-call.
#' @param ... List of parameters to customize the model.
#'
#' @examples
#' # print the first parameters of `phenips-clim`
#' head(params('phenips-clim'))
#' @export

params <- function(m, ...) {

  return(model(m, ...)$params)
}


#' List all models
#'
#' Get the names of all available models.
#'
#' @examples
#' # print all available models
#' list_models()
#' @export

list_models <- function() {
  return(names(env_barrks$models))
}





