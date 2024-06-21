

if(at_home()) {

  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(terra)))

# prepare input data -----------------------------------------------------------

  load('data/test-data-bso.rda')

  data <- barrks_data()
  dates <- terra::time(data$sunrise)

  # use same sunrise/sunset as original BSO
  dlen <- 12 - 4 * cos(pi * (1:365+10)/183)
  data$sunrise <- (data$sunrise[[1]] * 0 + 11 - dlen/2) * 60
  data$sunset <- (data$sunset[[1]] * 0 + 11 + dlen/2) * 60

  terra::time(data$sunrise) <- dates
  terra::time(data$sunset) <- dates



# calculate phenology ----------------------------------------------------------

  # the phenology will be calculated in 3 steps to save time for debugging

  pheno_setup <- bso_phenology('bso', data, .setup_only = TRUE, .quiet = TRUE)
  pheno_subs <- bso_phenology('bso', get_input_data(pheno_setup), .quiet = TRUE,
                              .submodels = c('onset', 'diapause', 'mortality'))
  pheno <- bso_phenology('bso', get_input_data(pheno_subs), .quiet = TRUE,
                         .onset = pheno_subs$onset,
                         .diapause = pheno_subs$diapause,
                         .mortality = pheno_subs$mortality)



# build result data frame ------------------------------------------------------

  cell <- cellFromRowCol(get_input_data(pheno)$tmin, 10, 10)
  stages <- c('maturation', 'preflight', 'reproduction', 'egg', 'larva', 'pupa')


  result <- map_dfr(0:max(prop_filial_generations(pheno)), \(g) {

    map(stages, \(stage) {

      a <- bso_get_individuals_df(pheno, g, cell, stage) %>%
        transmute(generation = g, doy, individuals)

      colnames(a) <- c('generation', 'doy', stage)

      a
    }) %>%
      reduce(\(a, b) full_join(a, b, by = c('generation', 'doy')))
  })




# compare results --------------------------------------------------------------

  df <- full_join(result, test_output_bso, by = c('generation', 'doy'))
  keys <- c('maturation', 'preflight', 'reproduction', 'egg', 'larva', 'pupa')

  walk(keys, \(key) {

    diff <- (df[[paste0(key, '.x')]] - df[[paste0(key, '.y')]]) / pheno$meta$n
    expect_true(all(diff < 0.0005, na.rm = TRUE))
  })


# TODO: test against original plot-functions


# plot results for debugging ---------------------------------------------------

  if(FALSE) {

    inds <- function(x, keys) Reduce(\(a,b) a+b, x[keys])

    keys_plot <- keys[2]
    plot(result$doy, inds(result, keys))
    points(test_output_bso$doy, inds(test_output_bso, keys), col = 'red')


    # TODO: By plotting the differences of original and barrks BSO a tiny but
    # systematic deviation can by found
    key <- keys[[1]]
    plot(df$doy, df[[paste0(key, '.x')]] - df[[paste0(key, '.y')]], col = df$generation + 1)

    p <- bso_translate_phenology(pheno)

    bso_plot_flight_diagram(pheno, cell)
    bso_plot_stage_diagram(pheno, cell)

    get_generations_rst(p) %>% plot()
    plot_development_diagram(p, cell, prop_filial_generations(p), .lwd = 15)
  }
}
