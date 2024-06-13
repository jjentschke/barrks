

station_data <- barrks_data('stations')

rsts <- .df2rsts(station_data)

expect_true(all(names(station_data) == c('date', 'station', names(rsts))))

#crds <- get_station_coords(rsts)
stations <- .extract_stations(station_data)

expect_true(all(stations_names(stations) %in% unique(station_data$station)))


purrr::walk(length(stations), \(i) {

  cell <- stations_cells(stations)[[i]]
  name <- stations_names(stations)[[i]]

  expect_true(is.numeric(cell))
  expect_true(length(cell) == 1)

  purrr::walk(names(rsts), \(key) {
    expect_true(all(rsts[[key]][cell] == station_data[[key]][station_data$station == name]))
  })
})
