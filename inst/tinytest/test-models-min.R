
# test if models work with onset and development submodels only

data <- barrks_data()


# test a few models only when running on CRAN
if(at_home()) {
  models <- list_models()
}  else models <- c('phenips', 'phenips-clim', 'joensson')


for(m in models) {

  if(m == 'bso') {
    p <- bso_phenology(m, data, .submodels = c('onset', 'development'), .quiet = TRUE)
    p <- bso_translate_phenology(p, .quiet = TRUE)
  } else p <- phenology(m, data, .submodels = c('onset', 'development'), .quiet = TRUE)

  rst <- get_generations_rst(p, categorical = FALSE)

  expect_true(sum(terra::values(rst), na.rm = TRUE) > 0)
}
