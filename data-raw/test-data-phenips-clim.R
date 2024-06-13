

library(barrks)


pheno <- phenology('phenips-clim', barrks_data('stations'),
                   exposure = 'sunny', onset_mode = 0.1, oviposition_mode = 0.1, diapause_mode = 'thermal')

test_output_phenips_clim <- get_development_df(pheno)


save(test_output_phenips_clim, file = 'inst/tinytest/data/test-data-phenips-clim.rda')
