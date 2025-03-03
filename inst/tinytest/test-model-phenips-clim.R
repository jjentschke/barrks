


#load('inst/tinytest/data/test-data-phenips-clim.rda')
load('data/test-data-phenips-clim.rda')

pheno <- phenology('phenips-clim', barrks_data('stations'),
                   exposure = 'sunny', onset_mode = 0.1, oviposition_mode = 0.1, diapause_mode = 'thermal',
                   .quiet = TRUE)

expect_true(all(get_development_df(pheno) == test_output_phenips_clim, na.rm = TRUE))
expect_true(all(is.na(get_development_df(pheno)) == is.na(test_output_phenips_clim)))

