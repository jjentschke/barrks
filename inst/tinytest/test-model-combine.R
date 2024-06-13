

path <- system.file('extdata/sample-data', package = 'barrks')

m <- model_combine('phenips',
                   list(model = 'rity', submodels = 'onset'),
                   list(model = 'joensson', submodels = 'diapause'),
                   list(model = 'phenips-clim', submodels = 'mortality'))


pheno <- phenology(m, path, .quiet = TRUE)


onset <- get_onset_rst(phenology('rity', path, .submodels = 'onset', .quiet = TRUE), FALSE)
diapause <- get_diapause_rst(phenology('joensson', path, .submodels = 'diapause', .quiet = TRUE), FALSE)
mortality <- get_mortality_rst(phenology('phenips-clim', path, .submodels = 'mortality', .quiet = TRUE), FALSE)
pheno2 <- phenology('phenips', .onset = onset, .diapause = diapause, .mortality = mortality, path, .quiet = TRUE)


expect_true(all(terra::values(get_onset_rst(pheno, FALSE) == onset), na.rm = TRUE))
expect_true(all(terra::values(get_diapause_rst(pheno, FALSE) == diapause), na.rm = TRUE))
expect_true(all(terra::values(get_mortality_rst(pheno, FALSE) == mortality), na.rm = TRUE))



purrr::walk(prop_hatched_generations(pheno), \(g) {
  expect_true(all(terra::values(get_development_rst(pheno, g) == get_development_rst(pheno2, g)), na.rm = TRUE))
})




