



#' Get phenology properties
#'
#' To examine a phenology, there are different functions to query its properties.
#'
#' @param pheno `r .doc_pheno()`
#'
#' @usage
#'
#' ## get the year the phenology was calculated for
#' prop_year(pheno)
#'
#' ## get all dates that are covered by the phenology
#' prop_dates(pheno)
#'
#' ## get the first date that is covered by the phenology
#' prop_first_date(pheno)
#'
#' ## get the last date that is covered by the phenology
#' prop_last_date(pheno)
#'
#' ## get all hatched generations as numeric vector
#' prop_hatched_generations(pheno)
#'
#' ## get all hatched filial generations as numeric vector
#' prop_filial_generations(pheno)
#'
#' ## get all hatched sister broods as numeric vector
#' prop_sister_broods(pheno)
#'
#' ## get the stations assigned to the phenology
#' prop_stations(pheno)
#'
#' @returns The requested property.
#'
#' @examples
#' \donttest{
#' # calculate phenology
#' p <- phenology('phenips-clim', barrks_data(), .quiet = TRUE)
#'
#' # print all generations that were hatched
#' prop_hatched_generations(p)
#' }
#' @name properties
#' @aliases prop_year prop_dates prop_first_date prop_last_date prop_hatched_generations prop_filial_generations prop_sister_broods prop_stations
NULL



#' @export
prop_year <- function(pheno) return(as.numeric(format(prop_dates(pheno)[1], '%Y')))

#' @export
prop_dates <- function(pheno) return(pheno$dates)

#' @export
prop_first_date <- function(pheno) return(utils::head(prop_dates(pheno), 1))

#' @export
prop_last_date <- function(pheno) return(utils::tail(prop_dates(pheno), 1))

#' @export
prop_hatched_generations <- function(pheno) {

  match <- stringr::str_extract(names(pheno$development), 'gen_([\\d.]*)$', 1)
  generations <- as.numeric(stats::na.omit(match))

  return(sort(generations[generations >= 0]))
}

#' @export
prop_filial_generations <- function(pheno) {

  generations <- prop_hatched_generations(pheno)
  return(generations[generations %% 1 == 0 & generations > 0])
}

#' @export
prop_sister_broods <- function(pheno) {

  generations <- prop_hatched_generations(pheno)
  return(generations[generations %% 1 == 0.5])
}

#' @export
prop_stations <- function(pheno) return(pheno$stations)



