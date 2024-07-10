

#' @describeIn phenology As BSO works a bit different than the other models, a seperate
#' phenology function is implemented for this model. Note that while the
#' onset and the development submodels are needed to be taken from BSO,
#' the diapause and the mortality submodels are compatible with other models.
#'
#' The function returns a BSO phenology as a list. Look [here][analyse.phenology.bso] to find out how
#' a BSO phenology can be analysed. It is not recommended to access the list elements directly.
#' To be able to use the functions that are
#' available for phenology objects returned by [phenology()], call [bso_translate_phenology()].
#' @order 2
#' @export

bso_phenology <- function(.model = 'bso',
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
    if(s %in% c('onset', 'development')) {
      t <- .model[[s]]$type
      error_msg <- 'model is not compatible with `bso_phenology()`, use `phenology()` instead'
      if(is.null(t)) stop(error_msg)
      if(t != 'bso') stop(error_msg)
    }
  })

  .msg(1, .quiet, 'calculate phenology with model `', .model$name, '`')


  submodels_setup <- c('onset', 'diapause', 'mortality', 'development')
  submodels_setup <- submodels_setup[c(is.null(.onset), is.null(.diapause), is.null(.mortality), TRUE)]
  submodels_setup <- intersect(submodels_setup, .submodels)

  dates_out <- NULL

  if(is.data.frame(.data)) .stations <- .extract_stations(.data)
  .data <- .setup(.model, .data, .win, .dates, .ext, .storage, .quiet, submodels_setup, ...)

  .development <- NULL

  if(!.setup_only) {
    if(is.null(.onset) & 'onset' %in% .submodels) {
      .onset <- .calc_onset(.model, .data, .win, .dates, .ext, .storage, .quiet)
      if(!is.null(.onset)) dates_out <- terra::time(.onset[[3]])
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
      if(!is.null(.development$gen_0$individuals)) dates_out <- terra::time(.development$gen_0$flight)
    }
  }

  out <- list(
    dates = dates_out,
    meta = list(n = .onset[[6]],
                num_slots = params(.model)$num_slots,
                psis = params(.model)$psis),
    n_slots = params(.model)$n_slots,
    model_end_date = params(.model)$model_end_date,
    data = .data,
    onset = .onset,
    diapause = .diapause,
    mortality = .mortality,
    development = .development
  )

  return(stations_assign(out, .stations))
}


#' Analyse a BSO generated phenology
#'
#' Here, all functions are listed that are available to analyse the results of
#' a [bso_phenology()] call.
#'
#' @details
#'
#' Get BSO phenology properties:
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^prop_'), '()]', collapse = '\n')`
#'
#' Get BSO phenology results (raster-based):
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^bso_get_.*_rst'), '()]', collapse = '\n')`
#'
#' Get BSO phenology results (station-based):
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^bso_get_.*_df'), '()]', collapse = '\n')`
#'
#' Plot BSO phenology results (station-based):
#'
#' `r paste0(' - [', lsf.str('package:barrks', pattern = '^bso_plot_'), '()]', collapse = '\n')`
#'
#' @seealso [analyse.phenology]
#' @name analyse.phenology.bso
NULL


#' Translate BSO generated phenology
#'
#' A BSO generated phenology cannot be analysed in the same way as other phenology objects.
#' To be able to use the functions that are available for phenology objects
#' returned by [phenology()], the BSO generated phenology should be translated.
#'
#' @param pheno `r .doc_pheno('BSO', 'bso_phenology')`
#' @param threshold Share of individuals that must have reached a specific
#' development in the BSO phenology to account for them in the corresponding
#' standard phenology.
#' @param .quiet `r .doc_quiet()`
#'
#' @returns Returns a standard phenology as a list. Look [here][analyse.phenology] to find out how
#' a phenology can be analysed. It is not recommended to access the list elements directly.
#'
#' @examples
#' \donttest{
#' # This may take a few minutes...
#'
#' # calculate and translate BSO phenology
#' p <- bso_phenology('bso', barrks_data('stations'), .quiet = TRUE)
#' pt <- bso_translate_phenology(p, .quiet = TRUE)
#'
#' # print the generations data frame of station 'Freiburg'
#' df <- get_generations_df(pt, 'Freiburg')
#' df
#' }
#' @export

bso_translate_phenology <- function(pheno,
                                    threshold = 0.1,
                                    .quiet = FALSE) {

  .msg(1, .quiet, 'translate bso-phenology to allow processing with standard functions')
  .msg(3, .quiet, 'use threshold of ', threshold * 100, ' %')

  generations <- prop_hatched_generations(pheno)

  num_slots <- pheno$meta$num_slots
  slots_total <- sum(num_slots)
  slots_total_sis <- sum(num_slots[c('maturation', 'preflight', 'reproduction')])

  development <- list()

  purrr::walk(generations, \(generation) {

    .msg(4, .quiet, 'generation ', generation)

    individuals <- pheno$development[[paste0('gen_', generation)]]$individuals

    res <- terra::rast(purrr::map(1:length(individuals), .progress = .get_pb(.quiet), \(day) {
      ind <- individuals[[day]][[(slots_total + 1):1]]
      x <- slots_total + 2 - terra::which.lyr(cumsum(ind) >= (pheno$meta$n * threshold))
      x <- terra::ifel(is.na(x), 0, x)
      terra::ifel(is.na(sum(ind)), NA, x)
    }))

    res <- terra::ifel(res == 0, -1, terra::clamp((res - num_slots['reproduction']) / (slots_total - sum(num_slots[c('reproduction', 'preflight')])), 0, 1))
    terra::time(res) <- prop_dates(pheno)

    development[[paste0('gen_', generation)]] <<- res


    if(generation > 0) {
      .msg(4, .quiet, 'generation ', generation + 0.5)

      psis <- pheno$meta$psis
      sister_breeders <- pheno$development[[paste0('gen_', generation - 1)]]$sister_breeders

      res_sis <- terra::rast(purrr::map(1:length(sister_breeders), .progress = .get_pb(.quiet), \(day) {
        ind <- sister_breeders[[day]][[(slots_total_sis + 1):1]]
        slots_total_sis + 2 - terra::which.lyr(cumsum(ind) * (psis + 1) / psis >= (pheno$meta$n * threshold))
      }))

      res_sis <- terra::ifel(is.na(res_sis), -1, 0)
      terra::time(res_sis) <- prop_dates(pheno)

      development[[paste0('gen_', generation + 0.5)]] <<- res_sis
    }
  })

  purrr::walk(names(development), \(g) {
    if(all(terra::values(development[[g]]) == -1, na.rm = TRUE)) development[[g]] <<- NULL
  })

  onset_individuals <- terra::rast(purrr::map(pheno$development$gen_0$individuals, \(x) x[[92]]))
  terra::time(onset_individuals) <- pheno$dates

  out <- list()

  out$dates <- pheno$dates
  out$data <- pheno$data
  out$development <- development
  out$onset <- (onset_individuals >= (pheno$meta$n * threshold))
  out$diapause <- pheno$diapause
  out$mortality <- pheno$mortality

  out$hibernating_generations <- .get_hibernation(out, pheno$model_end_date)

  return(stations_assign(out, prop_stations(pheno)))
}


