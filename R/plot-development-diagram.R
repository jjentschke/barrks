


#' Plot a development diagram
#'
#' A development diagram illustrates the beetles' development of all appearing
#' generations within a year.
#'
#' @param .phenos List of (named) phenology objects or a single phenology that will be
#' plotted (see [phenology()]).
#' @param .station `r .doc_station()`
#' @param .generations Generations that will be shown.
#' @param .colors,.fill,.labels Character vectors. Change the line colors, fill
#' or labels of the generations starting from the first generation followed
#' consecutively by elements for the other generations (including sister broods).
#' @param .legend_col,.legend_lty Manipulate the appearance of the legends for
#' colors and line types. Pass `TRUE`/`FALSE` to enable/disable the respective legend.
#' For the customization of the respective legend, a list of parameters for
#' [graphics::legend] can be passed.
#' @param .group Select the phenology objects that will be used to draw the filling. It can be
#' a character vector of the phenology names, an integer vector of the phenology
#' numbers, or `TRUE` if all phenology objects should be used.
#' @param .minmax_only If `TRUE`, only the minimum and the maximum development
#' line will be plotted.
#' @param .fun_bg Function to draw a background.
#' @param .lty,.lwd Use specific line types and line widths.
#' Vectors of the same length as `.phenos` will assign the values to the
#' respective phenology.
#' @param .date_split,.lty2,.lwd2,.fill2 When `.date_split` is reached, the
#' appearance of the plot will change according to the respective values.
#' @param .date_stop If specified, no data will be plotted after the respective
#' date.
#' @param ... Parameters passed to [base::plot()].
#'
#' @returns None
#'
#' @examples
#' \donttest{
#' # calculate phenology
#' p <- phenology('phenips-clim', barrks_data('stations'), .quiet = TRUE)
#'
#' # plot development diagram of the station 'Mannheim'
#' plot_development_diagram(p, 'Mannheim', .lwd = 4, .legend_lty = FALSE)
#' }
#' @seealso [stations]
#' @export

# TODO: restructure function, currently confusing to read

plot_development_diagram <- function(.phenos,
                                     .station = prop_stations(.phenos[[1]])[1],
                                     .generations = NULL,
                                     .colors = barrks_colors('diagram_lines'),
                                     .fill = barrks_colors('diagram_fill'),
                                     .labels = barrks_labels('diagram'),
                                     .legend_col = TRUE,
                                     .legend_lty = TRUE,
                                     .group = TRUE,
                                     .minmax_only = FALSE,
                                     .fun_bg = NULL,
                                     .lty = 'solid',
                                     .lwd = 2,
                                     .date_split = NULL,
                                     .date_stop = NULL,
                                     .lty2 = 'dotted',
                                     .lwd2 = 2,
                                     .fill2 = NA,
                                     ...) {

  n_vals <- min(length(.labels), length(.colors), length(.fill))

  keys <- c(paste0('gen_', 1 + (0:(n_vals - 1)) * 0.5))[1:n_vals]

  .labels <- .labels[1:n_vals]
  .colors <- .colors[1:n_vals]
  .fill <- .fill[1:n_vals]

  names(.labels) <- keys
  names(.colors) <- keys
  names(.fill) <- keys


  if(.is_phenology(.phenos)) .phenos <- list(.phenos)
  if(is.null(names(.phenos))) names(.phenos) <- 1:length(.phenos)
  if(is.list(.station) & is.null(names(.station))) names(.station) <- names(.phenos)
  if(is.numeric(.group)) .group <- names(.phenos)[.group]
  if(isTRUE(.group)) .group <- names(.phenos)

  if(is.character(.station)) .station <- prop_stations(.phenos[[1]])[.station]


  if(length(.lty) > 1 & is.null(names(.lty))) names(.lty) <- names(.phenos)
  if(length(.lwd) > 1 & is.null(names(.lwd))) names(.lwd) <- names(.phenos)
  if(length(.lty2) > 1 & is.null(names(.lty2))) names(.lty2) <- names(.phenos)


  # which dates should be plotted?

  dates <- c()
  purrr::walk(.phenos, \(p) dates <<- append(dates, prop_dates(p)))
  dates <- unique(dates)
  year <- format(dates[[1]], '%Y')


  # set plot parameters

  if(is.list(.station)) main <- names(.station[[1]][1])
  else main <- names(.station[1])

  args <- list(xlab = NA, ylab = 'development', xaxs = 'i', yaxs = 'i', xaxt = 'n', main = paste(main, year))
  fun_args <- list(...)

  xaxt <- 's'
  if('xaxt' %in% fun_args) xaxt <- fun_args['xaxt']

  purrr::walk(names(fun_args), \(arg_name) {
    if(arg_name != '') args[[arg_name]] <<- fun_args[[arg_name]]
  })

  if(!is.null(args$xlim)) {
    dates <- dates[dates >= args$xlim[1] & dates <= args$xlim[2]]
    dates_full <- seq(as.Date(args$xlim[1]), as.Date(args$xlim[2]), by = 'day')
  } else dates_full <- dates

  args$x <- c(min(dates_full), max(dates_full))
  args$y <- c(0, 1)
  args$type <- 'n'

  do.call(base::plot, args)
  if(xaxt != 'n') .add_date_axis(dates_full)

  if(is.function(.fun_bg)) .fun_bg()


  if(!is.null(.date_stop)) dates <- dates[dates <= .date_stop]



  # fetch development

  generations <- numeric()

  devs <- purrr::map(names(.phenos), \(key) {

    pheno <- .phenos[[key]]

    # save all occuring generations
    purrr::walk(prop_hatched_generations(pheno), \(g) generations <<- append(generations, g))

    if(is.list(.station)) stat <- .station[key]
    else stat <- .station

    df <- get_development_df(pheno, stat, dates = dates)

    # df_mortality <- get_mortality_df(pheno, stat, FALSE, dates)
    # if(!is.null(df_mortality)) df <- dplyr::left_join(df, df_mortality,
    #                                                   by = c('station', 'doy'))
    # else df$mortality <- 0

    cols <- colnames(df)
    cols <- cols[stringr::str_detect(cols, '^gen_')]

    purrr::walk(cols, \(col) {

      #df[[col]] <<- ifelse(df[[col]] == -2, NA, df[[col]])

      vec <- df[[col]]

      x <- purrr::map_lgl(1:(length(vec) - 1), \(i) {
        #if(is.na(vec[i]) & vec[i + 1] > 0) return(TRUE)
        if(is.na(vec[i]) | is.na(vec[i + 1])) return(FALSE)
        if(vec[i] < 0 & vec[i + 1] >= 0) return(TRUE)
        return(FALSE)
      })

      y <- purrr::map_lgl(2:length(vec), \(i) {
        if(is.na(vec[i - 1]) | is.na(vec[i])) return(FALSE)
        if(vec[i - 1] > 0 & vec[i] < 0) return(TRUE)
        return(FALSE)
      })

      df[x, col] <<- 0
      #df[y, col] <<- NA
    })

    return(df)
  })
  names(devs) <- names(.phenos)

  generations <- sort(unique(generations))
  if(!is.null(.generations)) generations <- generations[generations %in% .generations]

  generations <- generations[generations >= 1]


  split_lines <- function(x) {

    breaks <- which(purrr::map_lgl(1:(length(x) - 1), \(i) {

      if(x[i] > x[i + 1]) return(TRUE)
      return(FALSE)
    }))

    purrr::map2(c(1, breaks + 1), c(breaks, length(x)), \(from, to) {
      c(rep(NA, from - 1), x[from:to], rep(NA, length(x) - to))
    })
  }


  plot_content <- function(generation,
                           gen_devs,
                           dates,
                           .lwd,
                           .lty,
                           .fill) {

    gen_key <- paste0('gen_', generation)

    if(length(.fill) > 1) .fill <- .fill[[gen_key]]


    has_gen_date <- purrr::map(devs, \(dev) {
      return(!is.na(dev[[gen_key]]))
      # dev[[gen_key]][is.na(dev[[gen_key]])] <- -1
      # return(dev[[gen_key]] >= 0)
    })
    has_gen <- purrr::map_lgl(has_gen_date, \(x) any(x))

    gen_devs <- gen_devs[has_gen]

    if(!isFALSE(.group)) {

      has_gen_date_group <- purrr::reduce(has_gen_date[names(has_gen_date) %in% .group], \(a, b) a & b)

      dev_min_group <- purrr::map_dbl(1:length(gen_devs[[1]]), \(i) {
        min(purrr::map_dbl(gen_devs[names(gen_devs) %in% .group], \(gen_dev) gen_dev[i]), na.rm = FALSE)
      })
      dev_min_group1 <- ifelse(has_gen_date_group, dev_min_group, NA)
      dev_min_group1 <- ifelse(dev_min_group1 == -2, NA, dev_min_group1)

      dev_max_group <- purrr::map_dbl(1:length(gen_devs[[1]]), \(i) {

        x <- purrr::map_dbl(gen_devs[names(gen_devs) %in% .group], \(gen_dev) gen_dev[i])
        if(all(is.na(x))) return(NA)

        max(x, na.rm = TRUE)
      })
      dev_max_group1 <- ifelse(has_gen_date_group, dev_max_group, NA)
      dev_max_group1 <- ifelse(dev_max_group1 == -2, NA, dev_max_group1)

      if(all(has_gen) & !is.na(.fill)) {

        purrr::walk(2:length(dev_min_group), \(i) {
          graphics::polygon(c(dates[i - 1], dates[i], dates[i], dates[i - 1]),
                            c(dev_min_group1[i - 1], dev_min_group1[i], dev_max_group1[i], dev_max_group1[i - 1]),
                            col = .fill,
                            border = NA)
        })
      }
    }


    if(.minmax_only) {

      purrr::walk(split_lines(dev_min_group), \(l) {
        l <- ifelse(l == -2, NA, l)
        graphics::lines(dates,
                        ifelse(l < 0, NA, l),
                        col = .colors[[paste0('gen_', generation)]],
                        lwd = .lwd,
                        lty = .lty)
      })

      purrr::walk(split_lines(dev_max_group), \(l) {
        l <- ifelse(l == -2, NA, l)
        graphics::lines(dates,
                        ifelse(l < 0, NA, l),
                        col = .colors[[paste0('gen_', generation)]],
                        lwd = .lwd,
                        lty = .lty)
      })

    } else  {

      purrr::walk(names(gen_devs), \(name_pheno) {
        gen_dev <- gen_devs[[name_pheno]]


        if(length(.lty) == 1) lty_i <- .lty
        else lty_i <- .lty[[name_pheno]]

        if(length(.lwd) == 1) lwd_i <- .lwd
        else lwd_i <- .lwd[[name_pheno]]

        purrr::walk(split_lines(gen_dev), \(l) {
          l <- ifelse(l == -2, NA, l)
          graphics::lines(dates,
                          l,
                          col = .colors[[gen_key]],
                          lwd = lwd_i,
                          lty = lty_i)
        })
      })
    }
  }


  purrr::walk(generations, \(generation) {

    gen_key <- paste0('gen_', generation)

    gen_devs <- purrr::map(devs, \(x) x[[gen_key]])

    has_gen_date <- purrr::map(devs, \(dev) {
      ifelse(is.na(dev[[gen_key]]), FALSE, dev[[gen_key]] >= 0)
      # return(!is.na(dev[[gen_key]]))
      # dev[[gen_key]][is.na(dev[[gen_key]])] <- -1
      # return(dev[[gen_key]] >= 0)
    })
    has_gen <- purrr::map_lgl(has_gen_date, \(x) any(x))
    #has_gen_date_group <- purrr::reduce(has_gen_date[names(has_gen_date) %in% .group], \(a, b) a & b)

    if(!any(has_gen)) {
      generations <<- generations[generations != generation]
      return()
    }

    has_gen <- purrr::map_lgl(has_gen_date, \(x) any(x))


    if(!is.null(.date_split)) {
      gen_devs1 <- purrr::map(gen_devs, \(x) x[dates <= .date_split])
      plot_content(generation, gen_devs1, dates[dates <= .date_split], .lwd, .lty, .fill)

      has_gen2 <- purrr::map_lgl(has_gen_date, \(x) any(x[dates >= .date_split]))
      if(any(has_gen2)) {
        gen_devs2 <- purrr::map(gen_devs, \(x) x[dates >= .date_split])
        plot_content(generation, gen_devs2, dates[dates >= .date_split], .lwd2, .lty2, .fill2)
      }
    } else {
      plot_content(generation, gen_devs, dates, .lwd, .lty, .fill)
    }
  })



  if(isTRUE(.legend_col) | is.list(.legend_col)) {

    if(length(generations) > 0) {

      keys <- paste0('gen_', generations)

      args_legend <- list(
        x = 'topleft',
        pt.bg = .fill[keys],
        col = .colors[keys],
        pch = 22,
        pt.lwd = 2,
        pt.cex = 2
      )

      if(is.list(.legend_col)) {
        for(k in names(.legend_col)) {
          args_legend[[k]] <- .legend_col[[k]]
        }
      }
      do.call(graphics::legend, c(list(.labels[keys]), args_legend))
    }
  }

  if(isTRUE(.legend_lty) | is.list(.legend_lty)) {

    args_legend <- list(
      x = 'bottomright',
      lty = .lty,
      lwd = .lwd
    )

    if(is.list(.legend_lty)) {
      for(k in names(.legend_lty)) {
        args_legend[[k]] <- .legend_lty[[k]]
      }
    }
    do.call(graphics::legend, c(list(names(.phenos)), args_legend))
  }

  return(invisible())
}


.add_date_axis <- function(dates) {

  year <- lubridate::year(dates[1])
  months <- unique(lubridate::month(dates))
  month_names <- unique(lubridate::month(dates, label= TRUE))

  first_dates <- as.Date(paste0(year, '-', months, '-01'))
  days_in_month <- lubridate::days_in_month(first_dates)
  mid_dates <- first_dates + days_in_month / 2

  graphics::axis(1, at = c(0, max(dates)) , labels = FALSE, lwd.ticks = 0, pos = 0)
  graphics::axis(1, at = first_dates, labels = FALSE, pos = 0)
  graphics::axis(1, at = mid_dates, labels = month_names, pos = 0, tick = FALSE)
}



