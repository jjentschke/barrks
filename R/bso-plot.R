
#' @include utils.R
NULL





#' Plot a stage diagram (BSO only)
#'
#' A stage diagram illustrates the share of individuals are in a specific
#' developmental stage over time.
#'
#' @param .pheno `r .doc_pheno('BSO', 'bso_phenology')`
#' @param .station `r .doc_station('.pheno')`
#' @param .stages List of stages to plot. Elements will be passed to
#' [bso_get_individuals_df()]. Look there for more information.
#' @param .lty,.lwd Vectors of line types or line widths that are used to plot
#' the different stages. Should have the same length as `.stages` or 1.
#' @param .colors,.labels Vectors of colors/labels starting from the hibernating
#' generation followed consecutively by elements for the filial generations
#' (not including sisterbroods).
#' @param .legend_col,.legend_lty Manipulate the appearance of the legends for
#' colors and line types. Pass `TRUE`/`FALSE` to enable/disable the respective legend.
#' For the customization of the respective legend, a list of parameters for
#' [graphics::legend] can be passed.
#' @param ... arguments passed to [base::plot()].
#'
#' @returns None
#'
#' @examples
#' \donttest{
#' # This may take a few minutes...
#'
#' # calculate phenology
#' p <- bso_phenology('bso', barrks_data('stations'), .quiet = TRUE)
#'
#' bso_plot_stage_diagram(p)
#' }
#' @export

bso_plot_stage_diagram <- function(.pheno,
                                   .station = prop_stations(.pheno)[1],
                                   .stages = list('white', 'brown'),
                                   .lty = c('dashed', 'solid'),
                                   .lwd = 2,
                                   .colors = barrks_colors('bso_stages'),
                                   .labels = barrks_labels('bso_stages'),
                                   .legend_col = TRUE,
                                   .legend_lty = TRUE,
                                   ...) {

  year <- prop_year(.pheno)
  dates <- prop_dates(.pheno)
  keys_available <- c()

  fun_args <- list(type = 'n', xlab = 'date', ylab = 'dev', xaxs = 'i', yaxs = 'i', xaxt = 'n')
  args <- list(...)

  purrr::walk(names(args), \(key) fun_args[[key]] <<- args[[key]])

  do.call(base::plot, c(list(c(min(dates), max(dates)), c(0, 1)), fun_args))
  .add_date_axis(dates)

  gens <- c(0, prop_filial_generations(.pheno))

  purrr::walk(1:length(gens), \(i) {

    keys_available <<- c(keys_available, i)

    color <- .colors[i]

    purrr::walk(1:length(.stages), \(j) {
      df <- bso_get_individuals_df(.pheno, gens[i], .station, .stages[[j]])

      if(length(.lty) == 1) ltyi <- .lty
      else ltyi <- .lty[[j]]
      if(length(.lwd) == 1) lwdi <- .lwd
      else lwdi <- .lwd[[j]]

      graphics::lines(df$date, df$individuals / .pheno$meta$n, lty = ltyi, lwd = lwdi, col = color)
    })

  })

  # TODO: find another solution to hide '0'-values
  graphics::lines(c(min(dates), max(dates)), c(0,0), col = 'black', lwd = max(.lwd))


  if(isTRUE(.legend_col) | is.list(.legend_col)) {

    args_legend <- list(
      x = 'topleft',
      col = .colors[keys_available],
      lty = 1,
      lwd = 2,
      xjust = 0.05,
      yjust = 0.95
    )

    if(is.list(.legend_col)) {
      for(k in names(.legend_col)) {
        args_legend[[k]] <- .legend_col[[k]]
      }
    }
    do.call(graphics::legend, c(list(.labels[keys_available]), args_legend))
  }

  if(isTRUE(.legend_lty) | is.list(.legend_lty)) {

    args_legend <- list(
      x = 'bottomright',
      lty = .lty,
      lwd = .lwd,
      xjust = 0.05,
      yjust = 0.95
    )

    if(is.list(.legend_lty)) {
      for(k in names(.legend_lty)) {
        args_legend[[k]] <- .legend_lty[[k]]
      }
    }
    do.call(graphics::legend, c(list(.stages), args_legend))
  }

  return(invisible(NULL))
}



#' Plot a flight diagram (BSO only)
#'
#' A flight diagram illustrates the daily share of flying individuals over time.
#'
#' @param .pheno `r .doc_pheno('BSO', 'bso_phenology')`
#' @param .station `r .doc_station('.pheno')`
#' @param .colors,.labels Vectors of colors/labels starting from the first and the
#' second flght of the hibernating generation followed consecutively by elements
#' for the filial generations (first and second flight).
#' @param .xlim Date vector of length to that limits the dates plotted.
#' @param .legend Pass `FALSE` if no legend should be plotted. Otherwise the
#' value will be passed to `legend()` as first argument. Look there for more
#' information.
#' @param ... arguments passed to `graphics::barplot()`.
#'
#' @returns None
#'
#' @examplesIf rlang::is_installed("graphics")
#' \donttest{
#' # This may take a few minutes...
#'
#' # calculate phenology
#' p <- bso_phenology('bso', barrks_data('stations'), .quiet = TRUE)
#'
#' bso_plot_flight_diagram(p)
#' }
#' @export

bso_plot_flight_diagram <- function(.pheno ,
                                    .station = prop_stations(.pheno)[1],
                                    .colors = barrks_colors('bso_flight'),
                                    .labels = barrks_labels('bso_flight'),
                                    .xlim = NULL,
                                    .legend = 'topright',
                                    ...) {

  if(!requireNamespace('graphics', quietly = TRUE)) stop('package graphics required!')

  year <- prop_year(.pheno)
  dates <- prop_dates(.pheno)
  if(!is.null(.xlim)) dates <- dates[dates >= .xlim[1] & dates <= .xlim[2]]


  # set plot parameters
  plot_args <- list(col = .colors,
                    border = NA,
                    space = 0,
                    axes = FALSE,
                    ylab = 'rel. flight activity',
                    tck = 0,
                    main = paste(names(.station), year))
  fun_args <- list(...)
  purrr::walk(names(fun_args), \(arg_name) {
    if(arg_name != '') plot_args[[arg_name]] <<- fun_args[[arg_name]]
  })


  generations <- c(0, prop_filial_generations(.pheno))
  flight <- matrix(ncol = length(dates), nrow = 0)
  keys_available <- c()

  purrr::walk(1:length(generations), \(i) {

    generation <- generations[i]

    df_flight <- bso_get_flight_df(.pheno, generation, .station, 1, dates)
    if(sum(df_flight$flight) > 0) {
      flight <<- rbind(flight, matrix(df_flight$flight / .pheno$meta$n, ncol = length(dates)))
      keys_available <<- c(keys_available, i * 2 - 1)
    }

    df_flight2 <- bso_get_flight_df(.pheno, generation, .station, 2, dates)
    if(sum(df_flight2$flight) > 0) {
      flight <<- rbind(flight, matrix(df_flight2$flight / .pheno$meta$n, ncol = length(dates)))
      keys_available <<- c(keys_available, i * 2)
    }
  })

  months <- unique(lubridate::month(dates))
  first_dates <- as.Date(paste0(year, '-', months, '-01'))
  first_dates <- first_dates[first_dates %in% dates]
  first_doys <- lubridate::yday(first_dates)
  days_in_month <- lubridate::days_in_month(first_dates)
  last_doy <- unname(first_doys[length(months)] - 1 + days_in_month[length(months)])
  mid_doys <- first_doys - 1 + days_in_month / 2
  offset <- lubridate::yday(min(dates)) - 1

  do.call(graphics::barplot, c(list(height = flight), plot_args))

  graphics::axis(1, at = c(0, lubridate::yday(max(dates)) - offset) , labels = FALSE, lwd.ticks = 0, pos = 0)
  graphics::axis(1, at = c(first_doys, last_doy) - offset - 1, labels = FALSE, pos = 0)
  graphics::axis(1, at = mid_doys - offset - 1, labels = names(mid_doys), pos = 0, tick = FALSE)
  graphics::axis(2, pos = 0)

  if(.legend != FALSE & length(keys_available)) {
    graphics::legend(.legend,
           inset = 0.01,
           .labels[keys_available],
           pt.bg = .colors[keys_available],
           col = .colors[keys_available],
           pch = 22,
           pt.cex = 2,
           box.lty = 0,
           xjust = 0.05,
           yjust = 0.95)
  }

  return(invisible(NULL))
}
