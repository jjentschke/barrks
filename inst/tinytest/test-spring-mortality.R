

load('data/test-data-spring-mortality.rda')

data <- barrks_data('stations')

data[data$date == '2011-04-06', 'tmin'] <- -6


pheno <- phenology('phenips-clim', data,
                   exposure = 'sunny',
                   onset_mode = 0.1,
                   oviposition_mode = 0.1,
                   diapause_mode = 'thermal',
                   .quiet = TRUE)

df <- get_development_df(pheno)

expect_true(all(df == test_output_spring_mortality |
                (is.na(df) & is.na(test_output_spring_mortality))))
