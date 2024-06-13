
library(tidyverse)
library(barrks)

data <- read.csv('data-raw/test-data/PHENIPS_BW_31.10.2022.csv', TRUE, sep = ';', dec = ',')

dates <- data$Datum %>% unique() %>% as.Date()

orte <- (data$Ort %>% unique())[1:2]
alias <- orte
names(alias) <- orte


data_filtered <- data %>% filter(Ort %in% orte)



dl <- create_daylengths_df(list(Mannheim = list(lat = 49.5, lon = 8.55),
                                Rheinstetten = list(lat = 48.97, lon = 8.33)),
                           dates)


test_input_phenips <- data_filtered %>%
  transmute(date = as.Date(Datum),
            station = alias[Ort],
            tmean = T_mean,
            tmax = T_max,
            rad = Rad_sum) %>%
  full_join(dl)

# usethis::use_data(station_data, overwrite = TRUE)


test_output_phenips <- data_filtered %>%
  transmute(date = as.Date(Datum),
            station = alias[Ort],
            teff = T_max_eff,
            dev_1 = F1,
            dev_1.5 = GB,
            dev_2 = F2,
            dev_2.5 = GB_F2,
            dev_3 = F3,
            dev_shade_1 = F1_min,
            dev_shade_1.5 = GB_min,
            dev_shade_2 = F2_min,
            dev_shade_2.5 = GB_F2_min,
            dev_shade_3 = F3_min)


save(test_input_phenips, test_output_phenips, file = 'inst/tinytest/data/test-data-phenips.rda')



