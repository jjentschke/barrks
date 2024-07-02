


.bso_generation_data <- function(pheno, generation) return(pheno$development[[paste0('gen_', generation)]])


#' Get individuals (BSO only)
#'
#' Get the number of individuals of a generation that are in a specific development stage.
#'
#' @param pheno `r .doc_pheno('BSO', 'bso_phenology')`
#' @param generation Generation of interest. For sister broods, 0.5 should be
#' added.
#' @param stage If it is a numeric, the individuals of the
#' slots specified will be retrieved. Otherwise it could be one of the following
#' values: `all`, `egg`, `larva`, `pupa`, `white` (egg + larva + pupa),
#' `maturation`, `preflight`, `reproduction`, `brown` (maturation + preflight +
#' reproduction)
#' @param dates `r .doc_dates()`
#' @param stations `r .doc_stations()`
#'
#' @returns
#' * `bso_get_individuals_rst()`: a multi-layer SpatRaster.
#' * `bso_get_individuals()`: a data frame.
#'
#' @examples
#' \donttest{
#' # This may take a few minutes...
#'
#' # calculate phenology
#' p <- bso_phenology('bso', barrks_data('stations'), .quiet = TRUE)
#'
#' # get the number of individuals of the hibernating generation that are waiting to fly
#' bso_get_individuals_df(p, 0, stage = 'preflight')
#' }
#' @name bso_get_individuals
NULL



#' @rdname bso_get_individuals
#' @order 1
#' @export

bso_get_individuals_rst <- function(pheno,
                                    generation,
                                    stage = 'all',
                                    dates = prop_dates(pheno)) {

  individuals <- .bso_generation_data(pheno, generation)$individuals

  label <- ''

  start_slots <- cumsum(pheno$meta$num_slots)
  slot_seqs <- purrr::map2((c(1, start_slots + 1)[1:length(start_slots)]), start_slots, \(a, b) a:b)
  slot_seqs <- c(slot_seqs, list(slot_seqs[[1]] + sum(pheno$meta$num_slots) + 1))
  names(slot_seqs) <- c(names(pheno$meta$num_slots), 'reproduction_next')

  if(is.character(stage)) {
    label <- paste0('-', stage)

    stage <- switch(stage,
                    all = unname(unlist((slot_seqs))),
                    white = unname(unlist((slot_seqs[c('egg', 'larva', 'pupa')]))),
                    brown = unname(unlist((slot_seqs[c('maturation', 'preflight', 'reproduction_next')]))),
                    stage)

    if(is.character(stage)) stage <- slot_seqs[[stage]]
  }

  keys <- which(dates %in% prop_dates(pheno))
  out <- terra::rast(purrr::map(individuals[keys], \(count) sum(count[[stage]])))

  terra::time(out) <- prop_dates(pheno)
  names(out) <- paste0('individuals', label, '-', terra::time(out))

  return(out)
}


#' @rdname bso_get_individuals
#' @order 2
#' @export

bso_get_individuals_df <- function(pheno,
                                   generation,
                                   stations = prop_stations(pheno),
                                   stage = 'all',
                                   dates = prop_dates(pheno)) {

  if(is.character(stations)) stations <- prop_stations(pheno)[stations]

  rst <- bso_get_individuals_rst(pheno, generation, stage, dates)

  return(.rsts2df(list(individuals = rst), stations))
}




#' Get flight of individuals (BSO only)
#'
#' Get the number of individuals that are flying.
#'
#' @param pheno `r .doc_pheno('BSO', 'bso_phenology')`
#' @param generation `r .doc_generation()`
#' @param stations `r .doc_stations()`
#' @param flight Specifies which flight of the respective generation should be
#' returned. Can be `1` (first flight) or `2` (second flight).
#' @param dates `r .doc_dates()`
#'
#' @returns
#' * `bso_get_flight_rst`: a multi-layer SpatRaster.
#' * `bso_get_flight_df`: a data frame.
#'
#' @examples
#' \donttest{
#' # This may take a few minutes...
#'
#' # calculate phenology
#' p <- bso_phenology('bso', barrks_data('stations'), .quiet = TRUE)
#'
#' # get the number of individuals of the hibernating generation on their first flight
#' bso_get_flight_df(p, 0, flight = 1)
#' }
#' @name bso_get_flight
NULL

#' @rdname bso_get_flight
#' @order 1
#' @export

bso_get_flight_rst <- function(pheno,
                               generation,
                               flight = 1,
                               dates = prop_dates(pheno)) {
  if(flight == 1) out <- .bso_generation_data(pheno, generation)$flight
  if(flight == 2) out <- .bso_generation_data(pheno, generation)$flight_2

  if(is.null(out)) return(NULL)

  return(out[[terra::time(out) %in% as.Date(dates)]])
}



#' @rdname bso_get_flight
#' @order 2
#' @export

bso_get_flight_df <- function(pheno,
                              generation,
                              stations = prop_stations(pheno),
                              flight = 1,
                              dates = prop_dates(pheno)) {

  rst <- bso_get_flight_rst(pheno, generation, flight, dates)

  if(!is.null(rst)) return(.rsts2df(list(flight = rst), stations))
}
