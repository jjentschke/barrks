


library(barrks)


# use data that is publicly available

# https://www.lgl-bw.de/Produkte/Open-Data/
borders_fr <- terra::vect('data-raw/spatial/AX_Gebiet_Regierungsbezirk.shp')[1,]

# https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/
rsts_tmin <- terra::rast('data-raw/dwd/tasmin_hyras_5_2015_v5-0_de.nc')
rsts_tmean <- terra::rast('data-raw/dwd/tas_hyras_5_2015_v5-0_de.nc')
rsts_tmax <- terra::rast('data-raw/dwd/tasmax_hyras_5_2015_v5-0_de.nc')
rsts_rad <- terra::rast('data-raw/dwd/rsds_hyras_5_2015_v3-0_de.nc')


borders_fr_prj <- terra::project(borders_fr, terra::crs(rsts_tmean))


rsts_tmin_fr <- terra::crop(rsts_tmin, borders_fr_prj, mask = TRUE)
rsts_tmean_fr <- terra::crop(rsts_tmean, borders_fr_prj, mask = TRUE)
rsts_tmax_fr <- terra::crop(rsts_tmax, borders_fr_prj, mask = TRUE)
rsts_rad_fr <- terra::crop(rsts_rad, borders_fr_prj, mask = TRUE)

dates <- as.Date(terra::time(rsts_tmin_fr))

terra::time(rsts_tmin_fr) <- dates
terra::time(rsts_tmean_fr) <- dates
terra::time(rsts_tmax_fr) <- dates
terra::time(rsts_rad_fr) <- dates

keys <- (dates >= as.Date('2015-01-01') & dates <= as.Date('2015-12-31'))


if(!dir.exists('inst/extdata/sample-data')) dir.create('inst/extdata/sample-data')


terra::writeCDF(round(rsts_tmin_fr[[keys]] * 10),
                'inst/extdata/sample-data/tmin.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)
terra::writeCDF(round(rsts_tmean_fr[[keys]] * 10),
                'inst/extdata/sample-data/tmean.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)
terra::writeCDF(round(rsts_tmax_fr[[keys]] * 10),
                'inst/extdata/sample-data/tmax.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)
terra::writeCDF(round(rsts_rad_fr[[keys]] * 24),
                'inst/extdata/sample-data/rad.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)




suntimes <- create_suntimes_rsts(rsts_tmean_fr)


terra::writeCDF(suntimes$sunrise[[keys]],
                'inst/extdata/sample-data/sunrise.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)

terra::writeCDF(suntimes$sunset[[keys]],
                'inst/extdata/sample-data/sunset.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)


rsts_daylength <- create_daylength_rst(rsts_tmean_fr)

terra::writeCDF(round(rsts_daylength * 100),
                'inst/extdata/sample-data/daylength.nc',
                prec = 'short',
                compression = 9,
                overwrite = TRUE)


