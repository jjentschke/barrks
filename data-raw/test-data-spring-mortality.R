

library(barrks)


data <- barrks_data('stations')

data[data$date == '2011-04-06', 'tmin'] <- -6


pheno <- phenology('phenips-clim', data,
                   exposure = 'sunny', onset_mode = 0.1, oviposition_mode = 0.1, diapause_mode = 'thermal')

test_output_spring_mortality <- get_development_df(pheno)


save(test_output_spring_mortality, file = 'inst/tinytest/data/test-data-spring-mortality.rda')
