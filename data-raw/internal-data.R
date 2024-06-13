

library(tidyverse)
library(barrks)


data <- read_csv2('data-raw/phenips-clim/development-rates-phenips-clim.csv')

dev_rates_phenips_clim <- (data[, -1]  / 200)# %>%
rownames(dev_rates_phenips_clim) <- as.character(data[[1]])
colnames(dev_rates_phenips_clim) <- as.character(as.numeric(colnames(dev_rates_phenips_clim)) * 10)

dev_rates_phenips_clim <- as.matrix(dev_rates_phenips_clim) * 6 / 5


default_colors <- c('#BBBBBB',
                    '#5FC0C0', '#188A8A',
                    '#FFF37F', '#E6D528',
                    '#FF7F7F', '#E62828',
                    '#986ECC', '#59269C')


default_labels <- c('-', '1', '1s', '2', '2s', '3', '3s', '4', '4s')
default_labels_diagram <- default_labels[-1]

default_colors_dark <- purrr::map_chr(default_colors, function(c) {
  col <- col2rgb(c)
  fac <-  2 / 3
  return(rgb(col[1] * fac, col[2] * fac, col[3] * fac, maxColorValue = 255))
})

default_colors_diagram_lines <- default_colors_dark[-1]

default_colors_diagram_fill <- purrr::map_chr(default_colors, function(c) {
  col <- col2rgb(c)
  return(rgb(col[1], col[2], col[3], 255 * 0.7,  maxColorValue = 255))
})[-1]


# BSO

bso_default_colors_flight <- c('#888888', '#555555',
                               '#5FC0C0', '#188A8A',
                               '#FFF37F', '#E6D528',
                               '#FF7F7F', '#E62828',
                               '#986ECC', '#59269C')



bso_default_labels_flight <- c('0 -> 1', '0 -> 1s',
                               '1 -> 2', '1 -> 2s',
                               '2 -> 3', '2 -> 3s',
                               '3 -> 4', '3 -> 4s',
                               '4 -> 5', '4 -> 5s')


bso_default_colors_stages <- default_colors_dark[c(1, 2, 4, 6, 8)]

bso_default_labels_stages <- 0:4




######## Stations ############


stations <- c('Freiburg', 'Mannheim', 'Konstanz', 'Stuttgart')
year <- 2011

df1 <- purrr::map_dfr(stations, function(station) {

  file_climate <- list.files(paste0('data-raw/dwd/', stringr::str_to_lower(station), '-klima/'),
                             'produkt_klima_tag',
                             full.names = TRUE)[1]
  read_csv2(file_climate, locale = locale(decimal_mark = '.')) %>%
    mutate(date = lubridate::ymd(MESS_DATUM)) %>%
    filter(lubridate::year(date) == year) %>%
    mutate(station = station, tmean = as.double(TMK), tmin = as.double(TNK), tmax = as.double(TXK)) %>%
    select(date, station, tmean, tmin, tmax)
})


df2 <- purrr::map_dfr(stations, function(station) {
  file_radiation <- list.files(paste0('data-raw/dwd/', stringr::str_to_lower(station), '-strahlung/'),
                               'produkt_st_tag',
                               full.names = TRUE)[1]
  read_delim(file_radiation, ';') %>%
    mutate(date = lubridate::ymd(MESS_DATUM)) %>%
    filter(lubridate::year(date) == year) %>%
    mutate(station = station, rad = as.double(FG_STRAHL) / 0.36) %>%
    select(date, station, rad)
})

dates <- unique(df1$date)

coords <- purrr::map_dfr(stations, function(station) {
  file <- list.files(paste0('data-raw/dwd/', stringr::str_to_lower(station), '-klima/'),
                     'Metadaten_Geographie',
                     full.names = TRUE)[1]
  x <- read_delim(file, ';', locale = locale(decimal_mark = '.')) %>%
    mutate(lat = as.double(Geogr.Breite), lon = as.double(Geogr.Laenge))

  tibble(station = station, lon = x$lon[[length(x$lon)]], lat = x$lat[[length(x$lat)]])
})

df3 <- create_suntimes_df(coords, dates)


station_data <- full_join(df1, df2) %>%
  full_join(df3)

station_data <- station_data %>%
  mutate(daylength = (sunset - sunrise) / 60)


# replace missing radiation values with mean values of other stations

walk(which(station_data$rad < 0), \(row) {
  d <- station_data$date[row]
  station_data$rad[row] <<- mean(filter(station_data, date == d, rad >= 0)$rad)
})







usethis::use_data(dev_rates_phenips_clim,
                  default_colors,
                  default_colors_diagram_lines,
                  default_colors_diagram_fill,
                  default_labels,
                  default_labels_diagram,
                  bso_default_colors_flight,
                  bso_default_labels_flight,
                  bso_default_colors_stages,
                  bso_default_labels_stages,
                  station_data,
                  internal = TRUE, overwrite = TRUE)






