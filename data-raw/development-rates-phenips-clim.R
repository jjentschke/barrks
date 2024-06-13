
library(tidyverse)


data <- read_csv2('data-raw/phenips-clim/development-rates-phenips-clim.csv')

dev_rates_phenips_clim <- (data[, -1]  / 200)# %>%
rownames(dev_rates_phenips_clim) <- as.character(data[[1]])
colnames(dev_rates_phenips_clim) <- as.character(as.numeric(colnames(dev_rates_phenips_clim)) * 10)

dev_rates_phenips_clim <- dev_rates_phenips_clim * 6 / 5

write_csv(dev_rates_phenips_clim, 'inst/extdata/dev-rates-phenips-clim.csv')



#
#
# data <- read_csv2('data-raw/phenips-clim/development-rates-phenips-clim.csv')
#
# dev_rates_phenips_clim <- (data[, -1] / 200)# %>%
# rownames(dev_rates_phenips_clim) <- as.character(data[[1]])
#  # mutate(amplitude = data[[1]] / 10, .before = '0')
# colnames(dev_rates_phenips_clim) <- as.character(as.numeric(colnames(dev_rates_phenips_clim)) * 10)
#
# dev_rates_phenips_clim <- as.matrix(dev_rates_phenips_clim)
#
# usethis::use_data(dev_rates_phenips_clim, internal = TRUE, overwrite = TRUE)
