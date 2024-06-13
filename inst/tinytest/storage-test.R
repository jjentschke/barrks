

storage_test <- function(m,
                         storage,
                         test_dates = c('2015-01-31',
                                        '2015-05-01',
                                        '2015-05-02',
                                        '2015-07-31',
                                        '2015-12-31'),
                         pheno_func = phenology) {

  if(dir.exists(storage)) unlink(storage, recursive = T)


  data <- barrks_data()
  dates <- terra::time(data$tmin)


  pheno <- pheno_func(m, data, .quiet = TRUE)

  purrr::walk(test_dates, .progress = paste0(model(m)$name, ' storage test'), \(test_date) {

    dates <- dates[dates <= test_date]

    phenology(m,
              data,
              .dates = dates,
              .storage = storage,
              .quiet = TRUE)
  })

  pheno_stored <- phenology(m, .storage = storage, .quiet = TRUE)

  return(list(pheno, pheno_stored))
}

