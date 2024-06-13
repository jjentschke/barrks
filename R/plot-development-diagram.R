


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
#' @param .legend_col,legend_lty Manipulate the appearance of the legends for
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
#' @param .split_date,lty2 When `.split_date` is reached, the line type will
#' change to `lty2` and the area between the lines will not be filled anymore.
#' @param ... Parameters passed to [base::plot()].
#'
#' @export

# TODO: restructure function, currently a bit confusing to read

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
                                     .split_date = NULL,
                                     .lty2 = 'dotted',
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

  args <- list(xlab = 'date', ylab = 'dev', xaxs = 'i', yaxs = 'i', xaxt = 'n', main = paste(main, year))
  fun_args <- list(...)

  xaxt <- 's'
  if('xaxt' %in% fun_args) xaxt <- fun_args['xaxt']

  purrr::walk(names(fun_args), \(arg_name) {
    if(arg_name != '') args[[arg_name]] <<- fun_args[[arg_name]]
  })

  if(!is.null(args$xlim)) {
    dates <- dates[dates >= args$xlim[1] & dates <= args$xlim[2]]
    dates_full <- seq(args$xlim[1], args$xlim[2], by = 'days')
  }
  else dates_full <- dates

  args$x <- c(min(dates), max(dates))
  args$y <- c(0, 1)
  args$type <- 'n'

  do.call(base::plot, args)
  if(xaxt != 'n') .add_date_axis(dates_full)

  if(is.function(.fun_bg)) .fun_bg()


  # fetch development

  generations <- numeric()

  devs <- purrr::map(names(.phenos), \(key) {

    pheno <- .phenos[[key]]

    # save all occuring generations
    purrr::walk(prop_hatched_generations(pheno), \(g) generations <<- append(generations, g))

    if(is.list(.station)) stat <- .station[key]
    else stat <- .station

    df <- get_development_df(pheno, stat, dates = dates)

    cols <- colnames(df)
    cols <- cols[stringr::str_detect(cols, '^gen_')]

    purrr::walk(cols, \(col) {

      vec <- df[[col]]

      x <- purrr::map_lgl(2:length(vec), \(i) {
        if(is.na(vec[i - 1]) | is.na(vec[i])) return(FALSE)
        if(vec[i - 1] < 0 & vec[i] > 0) return(TRUE)
        return(FALSE)
      })
      y <- purrr::map_lgl(2:length(vec), \(i) {
        if(is.na(vec[i - 1]) | is.na(vec[i])) return(FALSE)
        if(vec[i] < 0 & vec[i - 1] > 0) return(TRUE)
        return(FALSE)
      })

      df[x, col] <<- 0
      df[y, col] <<- NA
    })

    return(df)
  })
  names(devs) <- names(.phenos)

  generations <- sort(unique(generations))
  if(!is.null(.generations)) generations <- generations[generations %in% .generations]

  generations <- generations[generations >= 1]


  purrr::walk(generations, \(generation) {

    has_gen <- purrr::map_lgl(devs, \(dev) {
      key <- paste0('gen_', generation)
      return(key %in% names(dev) & any(dev[[key]] >= 0, na.rm = TRUE))
    })


    if(!any(has_gen)) {
      generations <<- generations[generations != generation]
      return()
    }

    gen_devs <- purrr::map(devs[has_gen], \(dev) dev[[paste0('gen_', generation)]])


    if(length(gen_devs) == 0) return()

    if(!is.null(.split_date)) {
      dates1 <- dates[dates <= .split_date]
      dates2 <- dates[dates >= .split_date]
    }
    else {
      dates1 <- dates
      dates2 <- c()
    }

    if(any(names(gen_devs) %in% .group)) {
      g <- .group[.group %in% names(gen_devs)]
      gen_dev_min <- purrr::map(1:length(gen_devs[[1]]), \(i) {
        min(purrr::map_dbl(gen_devs[g], \(gen_dev) gen_dev[i]))
      })
      gen_dev_max <- purrr::map(1:length(gen_devs[[1]]), \(i) {
        x <- purrr::map_dbl(gen_devs[g], \(gen_dev) gen_dev[i])
        if(all(is.na(x))) return(NA)
        max(x, na.rm = TRUE)
      })

      dates_group_min <- dates[!is.na(gen_dev_min)]
      dates_group_max <- dates[!is.na(gen_dev_max)]
      gen_dev_min <- gen_dev_min[!is.na(gen_dev_min)]
      gen_dev_max <- gen_dev_max[!is.na(gen_dev_max)]
      keys_min1 <- dates_group_min %in% dates1
      keys_min2 <- dates_group_min %in% dates2
      keys_max1 <- dates_group_max %in% dates1
      keys_max2 <- dates_group_max %in% dates2

      keys_max3 <- which(purrr::map_lgl(gen_dev_min[keys_min1], \(x) !is.null(x)))

      if(all(has_gen)) {
        polygon(c(dates_group_max[keys_max3], rev(dates_group_min[keys_max3])),
                unlist(c(gen_dev_max[keys_max3], rev(gen_dev_min[keys_max3]))),
                col = .fill[[paste0('gen_', generation)]],
                border = NA)
      }


      if(.minmax_only) {

        if(all(has_gen)) {
          lines(dates_group_min[keys_min1],
                gen_dev_min[keys_min1],
                col = .colors[[paste0('gen_', generation)]],
                lwd = .lwd,
                lty = .lty)

          lines(dates_group_min[keys_min2],
                gen_dev_min[keys_min2],
                col = .colors[[paste0('gen_', generation)]],
                lwd = .lwd,
                lty = .lty2)

          k <- which(gen_dev_min >= 0)[1]
          if(k < length(gen_dev_min)) k <- (k-1):(k+1)
          else k <- (k-1):k

          lines(c(dates_group_min[k - 1], dates_group_min[k]),
                c(gen_dev_min[k - 1], gen_dev_min[k]),
                col = .colors[[paste0('gen_', generation)]],
                lwd = .lwd)
        }

        lines(dates_group_max[keys_max1],
              gen_dev_max[keys_max1],
              col = .colors[[paste0('gen_', generation)]],
              lwd = .lwd,
              lty = .lty)
        lines(dates_group_max[keys_max2],
              gen_dev_max[keys_max2],
              col = .colors[[paste0('gen_', generation)]],
              lwd = .lwd,
              lty = .lty2)

        k <- which(gen_dev_max >= 0)[1]
        if(k < length(gen_dev_max)) k <- (k-1):(k+1)
        else k <- (k-1):k

        lines(c(dates_group_max[k - 1], dates_group_max[k]),
              c(gen_dev_max[k - 1], gen_dev_max[k]),
              col = .colors[[paste0('gen_', generation)]],
              lwd = .lwd)
      }
    }


    if(!.minmax_only) {
      purrr::walk(names(gen_devs), \(name_pheno) {
        gen_dev <- gen_devs[[name_pheno]]


        if(length(.lty) == 1) lty_i <- .lty
        else lty_i <- .lty[[name_pheno]]

        if(length(.lwd) == 1) lwd_i <- .lwd
        else lwd_i <- .lwd[[name_pheno]]

        lines(dates,
              gen_dev,
              col = .colors[[paste0('gen_', generation)]],
              lwd = lwd_i,
              lty = lty_i)
      })
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
      do.call(legend, c(list(.labels[keys]), args_legend))
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
    do.call(legend, c(list(names(.phenos)), args_legend))
  }

}


.add_date_axis <- function(dates) {

  year <- lubridate::year(dates[1])
  months <- unique(lubridate::month(dates))
  month_names <- unique(lubridate::month(dates, label= TRUE))

  first_dates <- as.Date(paste0(year, '-', months, '-01'))
  days_in_month <- lubridate::days_in_month(first_dates)
  mid_dates <- first_dates + days_in_month / 2

  axis(1, at = c(0, max(dates)) , labels = FALSE, lwd.ticks = 0, pos = 0)
  axis(1, at = first_dates, labels = FALSE, pos = 0)
  axis(1, at = mid_dates, labels = month_names, pos = 0, tick = FALSE)
}



