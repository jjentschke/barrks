
dates <- unique(station_data$date)


# test spatially homogenneous events

x <- .create_events(station_data, lubridate::yday(dates[1]), trigger = TRUE, .quiet = TRUE)

expect_true(all(terra::time(x) %in% dates))
expect_true(all(dates %in% terra::time(x)))
expect_true(all(terra::values(x)))



x <- .create_events(station_data, lubridate::yday('2011-04-01'), .quiet = TRUE)
expect_true( all(terra::values(x[[terra::time(x) == '2011-04-01']])) )
expect_true( !any(terra::values(x[[terra::time(x) != '2011-04-01']])) )



test_dates <- as.Date(c('2011-04-01', '2011-05-01', '2011-06-01'))
x <- .create_events(station_data, lubridate::yday(test_dates), .quiet = TRUE)
expect_true( all(terra::values(x[[terra::time(x) %in% test_dates]])) )
expect_true( !any(terra::values(x[[!terra::time(x) %in% test_dates]])) )



x <- .create_events(station_data, lubridate::yday('2011-04-01'), trigger = TRUE, .quiet = TRUE)
expect_true( all(terra::values(x[[terra::time(x) >= '2011-04-01']])) )
expect_true( !any(terra::values(x[[terra::time(x) < '2011-04-01']])) )


# test events created with station data


doys <- data.frame(station = c('Stuttgart', 'Stuttgart', 'Konstanz', 'Konstanz'),
                   doy = lubridate::yday(c('2011-04-10', '2011-04-25', '2011-04-15', '2011-04-25')))


x <- .create_events(station_data, doys, trigger = TRUE, .quiet = TRUE)
df <- .rsts2df(list(x = x), .extract_stations(station_data))
expect_true(all(df[df$station == 'Stuttgart' & df$date < '2011-04-10',]$x == 0))
expect_true(all(df[df$station == 'Stuttgart' & df$date >= '2011-04-10',]$x == 1))
expect_true(all(df[df$station == 'Konstanz' & df$date < '2011-04-15',]$x == 0))
expect_true(all(df[df$station == 'Konstanz' & df$date >= '2011-04-15',]$x == 1))



x <- .create_events(station_data, doys, .quiet = TRUE)
df <- .rsts2df(list(x = x), .extract_stations(station_data))
expect_true(all(df[df$station == 'Stuttgart' & df$date %in% as.Date(c('2011-04-10', '2011-04-25')),]$x == 1))
expect_true(all(df[df$station == 'Stuttgart' & (!df$date %in% as.Date(c('2011-04-10', '2011-04-25'))),]$x == 0))
expect_true(all(df[df$station == 'Konstanz' & df$date %in% as.Date(c('2011-04-15', '2011-04-25')),]$x == 1))
expect_true(all(df[df$station == 'Konstanz' & (!df$date %in% as.Date(c('2011-04-15', '2011-04-25'))),]$x == 0))



# test events with raster data

template <- .template_rst(station_data)
doy <- c(100, 110, 120, 130)
rst_doy <- terra::setValues(template[[1]], doy)
x <- .create_events(station_data, rst_doy, .quiet = TRUE)
expect_true(all(terra::values(terra::which.lyr(x)) == doy))


doy2 <- c(105, 115, 125, 135)
rst_doy2 <- terra::setValues(template[[1]], doy2)
x <- .create_events(template, c(rst_doy, rst_doy2), .quiet = TRUE)
vals <- as.logical(x[1])
expect_true(all(vals[c(100, 105)]) & !any(vals[c(-100, -105)]))
vals <- as.logical(x[2])
expect_true(all(vals[c(110, 115)]) & !any(vals[c(-110, -115)]))
vals <- as.logical(x[3])
expect_true(all(vals[c(120, 125)]) & !any(vals[c(-120, -125)]))
vals <- as.logical(x[4])
expect_true(all(vals[c(130, 135)]) & !any(vals[c(-130, -135)]))






