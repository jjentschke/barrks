


.doc_quiet <- function() {
  return('If `TRUE`, messages are suppressed.')
}

.doc_stations <- function() {
  return('Pass a character vector to choose stations assigned to `pheno` by
          their names, or pass different stations. See [stations_create()] for details.')
}

.doc_station <- function(par = 'pheno') {
  return(paste0('Pass a character vector to choose a station assigned to `',
                par, '` by its name, or pass a different station. See [stations_create()] for details.'))
}

.doc_pheno <- function(type = '', fun = 'phenology') {
  return(paste0('A ', type, ' phenology (see [', fun, '()])'))
}

.doc_generation <- function() {
  return('Generation of interest. For sister broods, 0.5 should be added.')
}

.doc_dates <- function() {
  return('Select dates that should be present in the output.')
}

.doc_param_dev_mortal <- function() {
  return('The beetles are considered to be in
white stages (egg, larva, pupa) if their development exceeds `dev_mortal_min`
and subceeds `dev_mortal_max`. During these stages, the beetles could die
caused by a mortality event. `NULL` means that no lower/upper threshold is
defined.')
}

.doc_phenology_dots <- function() {
  return('See [phenology()] for a detailled description of the function.')
}

.doc_apply_models <- function() {
  paste0('[`model.', sub('-', '_', list_models()), '.apply`]', collapse = ', ')
}

.doc_customize_models <- function() {
  paste0('[`model.', sub('-', '_', list_models()), '.customize`]', collapse = ', ')
}

.doc_return_pheno <- function() {
  return('The function returns a phenology. Look [here][analyse.phenology] to find out how it can be analysed.')
}

.doc_functioning_pre <- function(m, n) {
  paste0('In the following, the basic functioning of ', n,
         ' is explained.')
}

.doc_functioning_post <- function(m) {
  paste0('Look [here][model.', m, '.customize] to find out how the model parameters ',
  'affect the actual calculations and which values are used by default.')
}

.doc_customize_description <- function(m, abbr, cite_key) {
  paste0('This page describes the parameters
          that can be used to customize ', m, '. The model was developed by
          \\insertCite{', cite_key, ';textual}{barrks}. Look [here][model.', abbr, '.apply] to find
          out how to apply the model.')
}

.doc_customize_call <- function(n, m) {
  paste0("
In `barrks`, [model()] is used to customize a model. The following code
illustrates which parameters are available for ", n, " and specifies their
default values.")
}

.doc_dev_start_end <- function() {
  return("Share in total development when the egg development starts and the
         juvenile beetle's development ends respectively. Usable if the development
         below/above these thresholds should account for mating, oviposition etc.")
}



