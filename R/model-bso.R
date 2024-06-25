#' @include model.R
NULL


#' Customize the BSO model
#'
#' `r .doc_customize_description('the BSO model', 'bso', 'Jakoby2019')`
#'
#' `r .doc_customize_call('the BSO model', 'bso')`
#'
#' ```{r, eval = FALSE}
#'
#' model("bso",
#'
#'       # ==== onset ====
#'
#'       dd_onset_start_date = '01-01',
#'       dd_onset_base = 5.124198,
#'       dd_onset_threshold = 100,
#'       slot_dia = 6,
#'
#'       # ==== onset + development ====
#'
#'       k = 2.853738e-02,
#'       alpha = c(2.549060e-05, 0.0000789, 1.009450e-05),
#'       tlo = c(-1.297644e+01, 4.760089e+00, -4.424628e+00),
#'       tup = c(3.600070e+01, 4.002483e+01, 3.999390e+01),
#'       tfly_min = 16.1064,
#'       tfly_max = 31.2901,
#'       pfly_max = 9.863263e-03,
#'       beta = 1.363763,
#'
#'       num_slots = c(
#'         'reproduction' = 11,
#'         'egg' = 18,
#'         'larva' = 45,
#'         'pupa' = 8,
#'         'maturation' = 8,
#'         'preflight' = 1
#'       ),
#'
#'       # ==== development ====
#'
#'       model_end_date = '12-30',
#'
#'       psis = 2.994450e-01,
#'       slot_sis = 4,
#'
#'       # ==== diapause ====
#'
#'       diapause_first = 210,
#'       diapause_last = 232,
#'       tdia_min = 1.645209e+01
#' )
#' ```
#'
#' @param dd_onset_start_date The date, when the degree days start to sum up ('MM-DD').
#' @param dd_onset_base Base temperature to calculate degree days to trigger the onset.
#' @param dd_onset_threshold Degree days that are required before the individuals
#' start regeneration feeding in `slot_dia` of the maturation stage. When the
#' regeneration feeding has finished, the onset is triggered.
#' @param slot_dia Maturation feeding slot where the individuals start regeneration feeding after
#' diapause.
#'
#' @param k Factor for the calculation of the phloem temperature.
#' @param alpha,tlo,tup Parameters used to calculate the transition probabilities
#' for each stage (except preflight) in the following order: development, maturation feeding,
#' reproduction.
#' @param tfly_min,tfly_max,pfly_max,beta Parameters used to calculate the
#' transition probabilities for the preflight stage.
#' @param num_slots Named vector that defines the number of slots for each stage.
#' The development stage is subdivided into the stages `egg`, `larva` and `pupa`.
#'
#' @param model_end_date Date when the model ends.
#' @param psis Probability that a sister brood will be established.
#' @param slot_sis Maturation feeding slot where the individuals start regeneration feeding before
#' they establish a sister brood.
#'
#' @param diapause_first The day of year when the diapause could start at the
#' earliest.
#' @param diapause_last The day of year when the diapause could start at the
#' latest.
#' @param tdia_min The diapause will be initiated when the average daily
#' temperature falls below that value.
#'
#' @references
#' \insertAllCited{}
#'
#' @name model.bso.customize
#' @seealso [model()], [phenology()], [`model.bso.apply`]
#' @family {model customizations}
#'
#' @encoding UTF-8
NULL


#' Use the BSO model
#'
#' This page describes the usage of the BSO model with [phenology()].
#' The model-specific inputs are listed and its basic functionality is explained.
#' The BSO model was published by \insertCite{Jakoby2019;textual}{barrks} and
#' parametrized for *Ips typographus* in Switzerland. Note that the onset and
#' the development submodel do not support the usage of a storage (except for
#' some precalculations).
#'
#' In `barrks`, [phenology()] is used to apply a model. The following code
#' illustrates which inputs are required to apply the BSO model and which additional
#' parameters are available.
#'
#' ```{r, eval = FALSE}
#' bso_phenology("bso", ..., tmin, tmax, sunrise, sunset,
#'               n = 1e+09, max_generations = 4)
#'
#' # calculate submodels separately
#' bso_phenology("bso", ..., .submodels = 'onset',
#'               tmin, tmax, sunrise, sunset, n = 1e+09)
#' bso_phenology("bso", ..., .submodels = 'diapause', tmin, tmax)
#' bso_phenology("bso", ..., .submodels = 'development',
#'               .onset, .diapause = NULL, .mortality = NULL,
#'               tmin, tmax, sunrise, sunset,
#'               max_generations = 4)
#' ```
#'
#' @section Functioning of the BSO:
#'
#' `r .doc_functioning_pre('bso', 'the BSO model')`
#'
#' - **Onset**: The onset of swarming will start when the degree days of the mean temperature reach
#' a specific threshold and regeneration feeding of the individuals has finished (Look at
#' development for details).
#' - **Development**: The development of single individuals is simulated.
#' The simulation of each individual is realized by passing a multitude of
#' slots that are grouped in stages. The hourly probability for an individual
#' to enter the next slot depends on the current stage and the phloem temperature.
#' The hourly temperature is derived from the minimum and maximum temperatures using
#' a sine interpolition. The hourly phloem temperature is calculated using
#' Newton's Law of Cooling (see \insertCite{Tran2007;nobrackets}{barrks}).
#' - **Diapause**: Specific photoperiod-related dates define when the diapause is initiated
#' at the earliest and at the latest. In between these dates, the diapause is
#' initiated when the mean temperature falls below a specific threshold.
#' - **Mortality**: The BSO model does not have a mortality submodel implemented.
#'
#' `r .doc_functioning_post('bso')`
#'
#' @param ... `r .doc_phenology_dots_bso()`
#' @param tmin,tmax Daily minimum/maximum air temperatures in °C.
#' @param sunrise,sunset Time of sunrise/sunset in minutes from midnight. Can
#' be created with [create_suntimes_rsts()] or [create_suntimes_df()].
#' @param n number of individuals to simulate.
#' @param max_generations maximum number of generations to calculate.
#'
#' @return `r .doc_return_pheno_bso()`
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @name model.bso.apply
#' @seealso [model()], [bso_phenology()], [`model.bso.customize`]
#' @family {phenology applications}
#'
#' @encoding UTF-8
NULL


bso_calc_thourly <- function(.params,
                             tmin,
                             tmax,
                             .storage = NULL,
                             .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  thourly <- terra::app(c(tmin, tmax), \(x) {

    tmn <- x[1:terra::nlyr(tmin)]
    tmx <- x[(terra::nlyr(tmin) + 1):(terra::nlyr(tmin) +terra::nlyr(tmax))]
    tmn2 <- c(tmn[2:length(tmn)], 0)

    out <- unlist(purrr::map(1:24, \(hour) {

      time <- hour/24

      b <- ceiling(time + 0.5)
      if(ceiling(time + 0.5) > 1) tmn_ <- tmn2
      else tmn_ <- tmn

      return((tmx + tmn_)/2 - ((tmx-tmn_)/2) * cos(2*pi*time))
    }))

    out <- out[unlist(purrr::map(1:365, \(x) x + 0:23 * 365))]

    return(out)
  })

  # set metadata (time, layer names)
  dates <- terra::time(tmin)
  times <- seq(as.POSIXlt(min(dates)),as.POSIXlt( max(dates) + 1), by = 'hour')
  terra::time(thourly) <- times[1:terra::nlyr(thourly)]
  names(thourly) <- paste0('temperature-', gsub(' ', '-', terra::time(thourly)))

  return(thourly)
}



bso_calc_tphloem <- function(.params,
                             thourly,
                             .storage = NULL,
                             .quiet = FALSE,
                             .last = NULL) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  if(is.null(.last)) .last <- thourly[[1]] * 0

  tphloem <- terra::rast(
    purrr::map(1:terra::nlyr(thourly), .progress = .get_pb(.quiet), \(i) {
      .last <<- .last + .params$k * (thourly[[i]] - .last)
    })
  )

  # set metadata
  terra::time(tphloem) <- terra::time(thourly)
  names(tphloem) <- paste0('phloem-temperature-', gsub(' ', '-', terra::time(tphloem)))

  return(tphloem)
}



bso_calc_dd <- function(.params,
                        thourly,
                        .storage = NULL,
                        .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  start_date <- .get_date_of_year(thourly, .params$dd_onset_start_date)

  tcorr <- thourly - .params$dd_onset_base
  tcorr <- terra::ifel(tcorr > 0, tcorr, 0)

  dates <- as.Date(terra::time(thourly))

  if(start_date > min(dates)) {
    if(start_date <= max(dates)){
      tcorr <- c(tcorr[[dates < start_date]] * 0, tcorr[[dates >= start_date]])
    }
    else tcorr <- tcorr * 0
  }

  dd <- cumsum(tcorr) / 24

  names(dd) <- paste0('daily-temperature-sum-', gsub(' ', '-', terra::time(dd)))

  return(dd)
}


bso_slots_total <- function(.params, stage = NULL){

  if(is.null(stage)) return(sum(.params$num_slots))

  i <- which(names(.params$num_slots) == stage)
  return(sum(.params$num_slots[1:i]))
}

bso_stage <- function(.params, slot) {

  slot <- (slot - 1) %% bso_slots_total(.params) + 1
  ifelse(slot <=  .params$num_slots[[1]], 1,
         ifelse(slot <= sum(.params$num_slots[1:4]), 2,
                ifelse(slot <= sum(.params$num_slots[1:5]), 3, 4)))
}



bso_calc_r_phloem <- function(.params, tphloem, j,
                              .storage = NULL,
                              .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  alpha <- .params$alpha[j]
  tlo <- .params$tlo[j]
  tup <- .params$tup[j]

  return(terra::ifel(tphloem > tlo & tphloem < tup,
                     terra::clamp(alpha * tphloem * (tphloem - tlo) * sqrt(tup - tphloem), 0), 0))
}

bso_calc_r_rep <- function(.params, tphloem, .storage = NULL, .quiet = FALSE) bso_calc_r_phloem(.params, tphloem, 1, .storage, .quiet)
bso_calc_r_dev <- function(.params, tphloem, .storage = NULL, .quiet = FALSE) bso_calc_r_phloem(.params, tphloem, 2, .storage, .quiet)
bso_calc_r_mat <- function(.params, tphloem, .storage = NULL, .quiet = FALSE) bso_calc_r_phloem(.params, tphloem, 3, .storage, .quiet)


bso_calc_r_fly <- function(.params,
                           thourly,
                           sunrise,
                           sunset,
                           .storage = NULL,
                           .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  times <- terra::time(thourly)
  dates <- as.Date(times)
  minutes <- 60 * as.numeric(format(times, '%H')) + as.numeric(format(times, '%M'))

  srise <- sunrise[[match(dates, terra::time(sunrise))]]
  sset <- sunset[[match(dates, terra::time(sunset))]]
  is_day <- (srise < minutes & sset >= minutes)

  tfly <- thourly - .params$tfly_min
  tfly <- terra::ifel(tfly >= 0 & thourly <= .params$tfly_max, tfly, 0)
  out <- terra::ifel(is_day, - (1 / (.params$beta ^ tfly) - 1) * .params$pfly_max, 0)

  terra::time(out) <- times

  return(out)
}



bso_calc_onset <- function(.params,
                           dd,
                           r_dev,
                           r_mat,
                           r_fly,
                           r_rep,
                           n = 1e+09,
                           .quiet = FALSE) {

  # when is the minimal annual thermal sum reached?
  sum_reached <- terra::which.lyr(dd >= .params$dd_onset_threshold)
  sum_reached <- floor(sum_reached / 24) * 24 - 12

  # generate regeneration start rasters
  start_steps <- unique(terra::values(sum_reached, FALSE))
  start_steps <- stats::na.omit(start_steps)
  start_regeneration <- dd * 0
  purrr::walk(start_steps, \(step) {
    start_regeneration[[step]] <<- terra::ifel(sum_reached == step, n, 0)
  })
  terra::time(start_regeneration) <- terra::time(dd)

  # perform regeneration feeding
  out <- bso_develop_generation(.params,
                                r_dev, r_mat, r_fly, r_rep,
                                start_regeneration,
                                bso_slots_total(.params, 'pupa') + .params$slot_dia,
                                .quiet = .quiet)

  out$n <- n

  return(out)
}


bso_calc_diapause <- function(.params,
                              tmin,
                              tmax,
                              .storage = NULL,
                              .quiet = FALSE) {

  # use storage if requested
  if(is.character(.storage)) return(.use_storage())

  # calculate firs/last date when diapause could be triggered
  dates <- terra::time(tmin)
  year <- format(dates[[1]], '%Y')
  date_first <- as.Date(paste0(year, '-01-01')) + .params$diapause_first - 1
  date_last <- as.Date(paste0(year, '-01-01')) + .params$diapause_last - 1


  # return TRUE after last diapause date or if mean temperature falls below threshold
  rst_F <- as.logical(tmin[[1]] * 0)
  rst_T <- as.logical(rst_F + 1)
  out <- purrr::map(terra::as.list((tmin + tmax) / 2 < .params$tdia_min), function(rst) {
    if(terra::time(rst) <= date_first) return(rst_F)
    if(terra::time(rst) >= date_last) return(rst_T)
    return(rst)
  })

  out <- terra::rast(out)
  terra::time(out) <- dates

  return(.trigger_rst(out))
}



bso_develop_generation <- function(.params,
                                   r_dev,
                                   r_mat,
                                   r_fly,
                                   r_rep,
                                   onset_hourly,
                                   onset_slot = 1,
                                   diapause_hourly = NULL,
                                   mortality_hourly = NULL,
                                   daily = TRUE,
                                   .quiet = FALSE) {

  # basic initialization
  slots_total <- bso_slots_total(.params)
  ncells <- terra::ncell(r_dev)
  cells_finished <- (slots_total * ncells + 1):((slots_total + 1) * ncells)
  template <- r_dev[[1]] * 0 * 1:(slots_total + 1)
  population <- terra::values(template, FALSE)
  stages <- purrr::map_int(1:slots_total, \(slot) bso_stage(.params, slot))
  r_j <- list(terra::values(r_rep, FALSE),
              terra::values(r_dev, FALSE),
              terra::values(r_mat, FALSE),
              terra::values(r_fly, FALSE))
  slot_preflight <- bso_slots_total(.params, 'preflight')

  # sister broods initialization
  slots_total_sb <- sum(.params$num_slots[c('maturation', 'preflight', 'reproduction')])
  slot_before_dev <- bso_slots_total(.params, 'reproduction')
  cells_before_dev <- ((slot_before_dev - 1) * ncells + 1):(slot_before_dev * ncells)
  template_sb <- r_dev[[1]] * 0 * 1:(slots_total_sb + 1)
  population_sb <- terra::values(template_sb, FALSE)
  cells_start_sb <- ((.params$slot_sis - 1) * ncells + 1):(.params$slot_sis * ncells)
  cells_finished_sb <- ((slots_total_sb - 1) * ncells + 1):((slots_total_sb) * ncells)
  slot_preflight_sb <- sum(.params$num_slots[c('maturation', 'preflight')])

  slot_permutation_sb <- unlist(purrr::map(c(3, 4, 1), \(s) {
    purrr::map(which(stages == s), \(slot) ((slot - 1) * ncells + 1):(slot * ncells) )
  }))

  # onset initialization
  onset_keys <- ((onset_slot - 1) * ncells + 1):(onset_slot * ncells)
  onset_vec <- terra::values(onset_hourly, FALSE)
  onset_lyrs <- terra::nlyr(onset_hourly)
  last_onset <- max(terra::values(onset_lyrs - terra::which.lyr(onset_hourly[[onset_lyrs:1]]) + 1), na.rm = TRUE)

  # diapause initialization
  if(!is.null(diapause_hourly)) not_diapaused <- !terra::values(diapause_hourly, FALSE)

  # initialization for daily output
  if(daily) {
    steps_daily <- which(format(terra::time(r_dev), '%H') == '00')[-1]
    agg <- population
    out_daily <- list()
    agg_sb <- population_sb
    out_daily_sb <- list()
    count <- 0

    # initialization of flight output
    current_flight <- terra::values(template[[1]], FALSE)
    agg_flight <- current_flight
    flight_keys_offset <- (slot_preflight - 1) * ncells
    flight_keys <- (flight_keys_offset + 1):(flight_keys_offset + ncells)
    out_daily_flight <- list()

    current_flight_sb <- terra::values(template[[1]], FALSE)
    agg_flight_sb <- current_flight_sb
    flight_keys_offset_sb <- (slot_preflight_sb - 1) * ncells
    flight_keys_sb <- (flight_keys_offset_sb + 1):(flight_keys_offset_sb + ncells)
    out_daily_flight_sb <- list()
  }


  vec_finished <- purrr::map(1:terra::nlyr(r_dev), .progress = .get_pb(.quiet), \(step) {

    # calculate mean of last day if daily output is requested
    if(daily) if(step %in% steps_daily & count > 0) {

      rst_daily <- round(terra::setValues(template, agg / count))
      terra::time(rst_daily) <- rep(as.Date(terra::time(r_dev[[step]])) - 1, terra::nlyr(rst_daily))
      out_daily[[length(out_daily) + 1]] <<- rst_daily
      agg <<- population * 0

      rst_daily_sb <- round(terra::setValues(template_sb, agg_sb / count))
      terra::time(rst_daily_sb) <- rep(as.Date(terra::time(r_dev[[step]])) - 1, terra::nlyr(rst_daily_sb))
      out_daily_sb[[length(out_daily_sb) + 1]] <<- rst_daily_sb
      agg_sb <<- population_sb * 0

      count <<- 0

      rst_daily_flight <- round(terra::setValues(template[[1]], agg_flight))
      terra::time(rst_daily_flight) <- rep(as.Date(terra::time(r_dev[[step]])), terra::nlyr(rst_daily_flight))
      out_daily_flight[[length(out_daily_flight) + 1]] <<- rst_daily_flight
      agg_flight <<- current_flight

      rst_daily_flight_sb <- round(terra::setValues(template[[1]], agg_flight_sb))
      terra::time(rst_daily_flight_sb) <- rep(as.Date(terra::time(r_dev[[step]])), terra::nlyr(rst_daily_flight_sb))
      out_daily_flight_sb[[length(out_daily_flight_sb) + 1]] <<- rst_daily_flight_sb
      agg_flight_sb <<- current_flight_sb
    }


    # which cells of vector data represent the current step
    cell_keys <- 1:ncells + (step - 1) * ncells

    # in which cells and slots are beetles present and should be processed
    to_process <- which(!is.na(population) & population != 0)
    to_process <- to_process[to_process <= slots_total * ncells]

    to_process_sb <- which(!is.na(population_sb) & population_sb != 0)
    to_process_sb <- to_process_sb[to_process_sb <= slots_total_sb * ncells]

    # shortcut if no beetles can develop and onset is over
    if(step > last_onset & length(to_process) == 0 & length(to_process_sb) == 0) {

      if(daily) {
        agg <<- agg + population
        agg_sb <<- agg_sb + population_sb

        count <<- count + 1
      }

      return(population[cells_finished])
    }

    # calculate transition probabilities and avoid beetles to enter preflight
    # stage if diapause already began
    transition_probs <- purrr::map(stages, \(stage) r_j[[stage]][cell_keys])
    transition_probs_sb <- transition_probs
    if(!is.null(diapause_hourly)) transition_probs[[slot_preflight - 1]] <- transition_probs[[slot_preflight - 1]] * not_diapaused[cell_keys]
    transition_probs <- unlist(transition_probs)
    transition_probs_sb <- unlist(transition_probs_sb)
    transition_probs_sb <- transition_probs_sb[slot_permutation_sb]

    # calculate the number of beetles that are changing the slot
    div <- stats::rbinom(length(to_process), population[to_process], transition_probs[to_process])

    # update population
    population[to_process] <<- population[to_process] - div
    population[to_process + ncells] <<- population[to_process + ncells] + div


    # sister breeding

    if(!is.null(diapause_hourly)) x <- which(to_process %in% (cells_before_dev * not_diapaused[cell_keys]))
    else x <- which(to_process %in% cells_before_dev)

    cells_before_dev_process <- to_process[x]


    n_sister_breeders <- stats::rbinom(length(x), div[x], .params$psis / (1 + .params$psis))

    population[cells_before_dev_process + ncells] <<- population[cells_before_dev_process + ncells] - n_sister_breeders

    div_sb <- stats::rbinom(length(to_process_sb), population_sb[to_process_sb], transition_probs_sb[to_process_sb]) # ?

    population_sb[to_process_sb] <<- population_sb[to_process_sb] - div_sb
    population_sb[to_process_sb + ncells] <<- population_sb[to_process_sb + ncells] + div_sb

    cells_x <- which(cells_finished_sb %in% to_process_sb)
    cells_y <- which(to_process_sb %in% cells_finished_sb)

    population[cells_before_dev[cells_x] + ncells] <<- population[cells_before_dev[cells_x] + ncells] + div_sb[cells_y]

    y <- which(cells_before_dev %in% cells_before_dev_process) + (.params$slot_sis - 1) * ncells
    population_sb[y] <<- population_sb[y] + n_sister_breeders


    # add beetles which emerge on the current step
    if(step <= last_onset)  population[onset_keys] <<- population[onset_keys] + onset_vec[cell_keys]

    # aggregate hourly population to calculate daily mean afterwards
    if(daily) {
      agg <<- agg + population
      agg_sb <<- agg_sb + population_sb
      count <<- count + 1

      # number of beetles flying
      flight_keys_to_process <- to_process[to_process %in% flight_keys]
      if(length(flight_keys_to_process) > 0) {
        current_flight[flight_keys_to_process - flight_keys_offset] <- div[which(to_process %in% flight_keys)]
      }
      agg_flight <<- agg_flight + current_flight

      # second flight
      flight_keys_to_process_sb <- to_process_sb[to_process_sb %in% flight_keys_sb]
      if(length(flight_keys_to_process_sb) > 0) {
        current_flight_sb[flight_keys_to_process_sb - flight_keys_offset_sb] <- div_sb[which(to_process_sb %in% flight_keys_sb)]
      }
      agg_flight_sb <<- agg_flight_sb + current_flight_sb
    }

    return(population[cells_finished])
  })



  if(daily) if(count > 0) {

    rst_daily <- round(terra::setValues(template, agg / count))
    terra::time(rst_daily) <- rep(as.Date(terra::time(r_dev[[terra::nlyr(r_dev)]])), terra::nlyr(rst_daily))
    out_daily[[length(out_daily) + 1]] <- rst_daily
    terra::time(rst_daily) <- rep(as.Date(terra::time(r_dev[[1]])), terra::nlyr(rst_daily))
    out_daily <- c(list(rst_daily * 0), out_daily)

    rst_daily_sb <- round(terra::setValues(template_sb, agg_sb / count))
    terra::time(rst_daily_sb) <- rep(as.Date(terra::time(r_dev[[terra::nlyr(r_dev)]])), terra::nlyr(rst_daily_sb))
    out_daily_sb[[length(out_daily_sb) + 1]] <- rst_daily_sb
    terra::time(rst_daily_sb) <- rep(as.Date(terra::time(r_dev[[1]])), terra::nlyr(rst_daily_sb))
    out_daily_sb <- c(list(rst_daily_sb * 0), out_daily_sb)


    rst_daily_flight <- round(terra::setValues(template[[1]], agg_flight))
    out_daily_flight[[length(out_daily_flight) + 1]] <- rst_daily_flight
    terra::time(rst_daily_flight) <- rep(as.Date(terra::time(r_dev[[1]])), terra::nlyr(rst_daily_flight))
    out_daily_flight <- c(list(rst_daily_flight * 0), out_daily_flight)[-(length(out_daily_flight) + 1)]

    rst_daily_flight_sb <- round(terra::setValues(template[[1]], agg_flight_sb))
    out_daily_flight_sb[[length(out_daily_flight_sb) + 1]] <- rst_daily_flight_sb
    terra::time(rst_daily_flight_sb) <- rep(as.Date(terra::time(r_dev[[1]])), terra::nlyr(rst_daily_flight_sb))
    out_daily_flight_sb <- c(list(rst_daily_flight_sb * 0), out_daily_flight_sb)[-(length(out_daily_flight_sb) + 1)]
  }


  # prepare output

  out <- list(finished = terra::setValues(r_dev, unlist(vec_finished)))

  if(daily) {
    out_daily_flight <- terra::rast(out_daily_flight)

    out_daily_flight_sb <- terra::rast(out_daily_flight_sb)

    out$daily <- out_daily[-1]
    out$daily_sb <- out_daily_sb[-1]
    out$daily_flight <- out_daily_flight
    out$daily_flight_sb <- out_daily_flight_sb
  }

  return(out)
}



bso_calc_development <- function(.params,
                                 .onset,
                                 .diapause,
                                 .mortality,
                                 r_dev,
                                 r_mat,
                                 r_fly,
                                 r_rep,
                                 max_generations = 4,
                                 .storage = NULL,
                                 .quiet = FALSE) {

  slots_total <- bso_slots_total(.params)
  slots_rep <- bso_slots_total(.params, 'reproduction')

  # convert cumulative onset to hourly onset
  onset_hourly <- .onset[[1]] - c(.onset[[1]][[1]] * 0, .onset[[1]][[-terra::nlyr(.onset[[1]])]])

  # convert daily diapause to hourly diapause
  if(is.null(.diapause)) diapause_hourly <- as.logical(onset_hourly * 0)
  else {
    rst_last <- .diapause[[1]] * 0
    diapause_hourly <- terra::rast(purrr::map(terra::time(.onset[[1]]), \(time) {
      if(format(time, '%H') == '12') rst_last <<- .diapause[[terra::time(.diapause) == as.Date(time)]]
      return(rst_last)
    }))
  }
  terra::time(diapause_hourly) <- terra::time(.onset[[1]])

  # initialization
  development <- list()
  generation <- 1
  generation_onset <- onset_hourly


  .msg(4, .quiet, 'hibernating generation')

  num_lyrs <- terra::nlyr(.onset[[2]][[1]])
  sd <- bso_slots_total(.params, 'pupa') + .params$slot_dia

  development$gen_0$individuals <- purrr::map(.onset[[2]], \(x) {

    x[[sd]] <- .onset[[6]] - sum(x[[(sd + 1):num_lyrs]])
    return(x)
  })

  development$gen_0$flight <- .onset[[4]]

  while(generation <= max_generations) {

    .msg(4, .quiet, 'generation ', generation)

    dev <- bso_develop_generation(.params,
                                  r_dev, r_mat, r_fly, r_rep,
                                  generation_onset, diapause_hourly = diapause_hourly,
                                  .quiet = .quiet)

    development[[paste0('gen_', generation)]]$individuals <- dev$daily
    development[[paste0('gen_', generation)]]$flight <- dev$daily_flight
    development[[paste0('gen_', generation - 1)]]$sister_breeders <- dev$daily_sb
    development[[paste0('gen_', generation - 1)]]$flight_2 <- dev$daily_flight_sb


    purrr::walk(1:length(dev$daily), \(i) {
      development[[paste0('gen_', generation - 1)]]$individuals[[i]] <<- c(
        development[[paste0('gen_', generation - 1)]]$individuals[[i]],
        development[[paste0('gen_', generation)]]$individuals[[i]][[1:slots_rep]]
      )
    })

    # when does the next generation emerges?

    if(generation < max_generations) {

      generation_onset <- dev$finished
      generation_onset <- generation_onset - c(generation_onset[[1]] * 0, generation_onset[[-terra::nlyr(generation_onset)]])

      terra::time(generation_onset) <- terra::time(onset_hourly)

      if(sum(terra::values(generation_onset), na.rm = TRUE) == 0) break
    }

    generation <- generation + 1
  }


  return(development)
}


.create_model('bso',
             list(
               params = list(

                 dd_onset_start_date = '01-01',
                 dd_onset_base = 5.124198e+00,
                 dd_onset_threshold = 100,

                 slot_dia = 6,

                 k = 2.853738e-02,

                 alpha = c(2.549060e-05, 0.0000789, 1.009450e-05),
                 tlo = c(-1.297644e+01, 4.760089e+00, -4.424628e+00),
                 tup = c(3.600070e+01, 4.002483e+01, 3.999390e+01),

                 tfly_min = 1.610640e+01,
                 tfly_max = 3.129010e+01,
                 pfly_max = 9.863263e-03,
                 beta = 1.363763e+00,

                 num_slots = c(
                   'reproduction' = 11,
                   'egg' = 18,
                   'larva' = 45,
                   'pupa' = 8,
                   'maturation' = 8,
                   'preflight' = 1
                 ),

                 model_end_date = '12-30',

                 psis = 2.994450e-01,
                 slot_sis = 4,

                 diapause_first = 210,
                 diapause_last = 232,
                 tdia_min = 1.645209e+01
               ),

               onset = list(
                 setup = list(thourly = bso_calc_thourly,
                              tphloem = bso_calc_tphloem,
                              r_dev = bso_calc_r_dev,
                              r_mat = bso_calc_r_mat,
                              r_fly = bso_calc_r_fly,
                              r_rep = bso_calc_r_rep,
                              dd = bso_calc_dd),
                 compute = bso_calc_onset,
                 type = 'bso'
               ),

               development = list(
                 setup = list(thourly = bso_calc_thourly,
                              tphloem = bso_calc_tphloem,
                              r_dev = bso_calc_r_dev,
                              r_mat = bso_calc_r_mat,
                              r_fly = bso_calc_r_fly,
                              r_rep = bso_calc_r_rep),
                 compute = bso_calc_development,
                 type = 'bso'
               ),

               diapause = list(
                 compute = bso_calc_diapause
               )
             )
)


