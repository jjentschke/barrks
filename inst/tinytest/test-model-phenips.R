

# duration: ~ 22 sec



#load('inst/tinytest/data/test-data-phenips.rda')
load('data/test-data-phenips.rda')



pheno <- phenology('phenips', test_input_phenips, exposure = 'sunny', .quiet = TRUE)

df_dev <- get_development_df(pheno)

df_test <- dplyr::full_join(test_output_phenips,
                     df_dev,
                     by = c('date', 'station'),
                     suffix = c('.x', '.y'))

purrr::walk(seq(1, 4.5, 0.5), function(generation) {
  if(paste0('gen_', generation) %in% names(df_test)) {
    val.x <- df_test[[paste0('gen_', generation)]]
    val.x <- ifelse(val.x < 0, 0, val.x)
    val.y <- df_test[[paste0('dev_', generation)]]
    expect_true(all(abs(val.x - val.y) < 0.00001 | val.y > 0.99999))
  }
})



developed_generations <- get_generations_df(pheno, threshold = 0.6)
df_test <- dplyr::full_join(test_output_phenips, developed_generations, by = c('date', 'station'))

purrr::walk(seq(1, 4.5, 0.5), function(generation) {
  col <- paste0('dev_', generation)
  if(col %in% names(df_test))
    expect_true(all((df_test[[col]] > 0.6) == (df_test$generation >= generation)))
})


# ---------------------------------------------------------------------------- #
#                                                                              #
# TODO: Results in shaded areas not equal the test data                        #
#                                                                              #
# No correction of the linear development was performed in the original data   #
# for shaded areas.                                                            #
#                                                                              #
# The test is disabled...                                                      #
#                                                                              #
# ---------------------------------------------------------------------------- #

if(FALSE) {

  pheno_shaded <- phenology('phenips', test_input_phenips, exposure = 'shaded', .quiet = TRUE)

  df_dev <- get_development_df(pheno_shaded)

  df_test <- dplyr::full_join(test_output_phenips,
                              df_dev,
                              by = c('date', 'station'))


  purrr::walk(seq(1, 5, 0.5), function(generation) {
    if(paste0('dev_shade_', generation) %in% names(df_test)) {
      val.x <- df_test[[paste0('dev_shade_', generation)]]
      val.y <- df_test[[paste0('gen_', generation)]]
      val.y <- ifelse(val.y < 0, 0, val.y)

      expect_true(all(abs(val.x - val.y) < 0.00001 | val.x > 0.99999))
    }
  })
}



