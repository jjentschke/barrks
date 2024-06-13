


library(barrks)


# use data that is publicly available

# https://www.lgl-bw.de/Produkte/Open-Data/
borders_fr <- terra::vect('data-raw/spatial/AX_Gebiet_Regierungsbezirk.shp')[1,]

# https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/
rsts_tmin <- terra::rast('data-raw/dwd/tasmin_hyras_5_2015_v5-0_de.nc')
rsts_tmean <- terra::rast('data-raw/dwd/tas_hyras_5_2015_v5-0_de.nc')
rsts_tmax <- terra::rast('data-raw/dwd/tasmax_hyras_5_2015_v5-0_de.nc')
rsts_rad <- terra::rast('data-raw/dwd/rsds_hyras_5_2015_v2-0_de.nc')


borders_fr_prj <- terra::project(borders_fr, terra::crs(rsts_tmean))


rsts_tmin_fr <- terra::crop(rsts_tmin, borders_fr_prj, mask = TRUE)
rsts_tmean_fr <- terra::crop(rsts_tmean, borders_fr_prj, mask = TRUE)
rsts_tmax_fr <- terra::crop(rsts_tmax, borders_fr_prj, mask = TRUE)
rsts_rad_fr <- terra::crop(rsts_rad, borders_fr_prj, mask = TRUE)
# radiation has another projection...
rsts_rad_fr <- terra::project(rsts_rad_fr, terra::crs(rsts_tmean_fr))

dates <- as.Date(terra::time(rsts_tmin_fr))

terra::time(rsts_tmin_fr) <- dates
terra::time(rsts_tmean_fr) <- dates
terra::time(rsts_tmax_fr) <- dates
terra::time(rsts_rad_fr) <- dates

keys <- (dates >= as.Date('2015-01-01') & dates <= as.Date('2015-12-31'))

terra::writeRaster(rsts_tmin_fr[[keys]],
                   'inst/extdata/sample-data/tmin.tif',
                   overwrite = TRUE)
terra::writeRaster(rsts_tmean_fr[[keys]],
                   'inst/extdata/sample-data/tmean.tif',
                   overwrite = TRUE)
terra::writeRaster(rsts_tmax_fr[[keys]],
                   'inst/extdata/sample-data/tmax.tif',
                   overwrite = TRUE)
terra::writeRaster(rsts_rad_fr[[keys]] * 24,
                   'inst/extdata/sample-data/rad.tif',
                   overwrite = TRUE)




suntimes <- create_suntimes_rsts(rsts_tmean_fr)

terra::writeRaster(suntimes$sunrise[[keys]],
                'inst/extdata/sample-data/sunrise.tif',
                overwrite = TRUE)

terra::writeRaster(suntimes$sunset[[keys]],
                'inst/extdata/sample-data/sunset.tif',
                overwrite = TRUE)


rst_epsg4258 <- terra::project(rsts_tmean_fr, 'EPSG:4258')
rsts_daylength <- create_daylength_rst(rst_epsg4258)

terra::writeRaster(rsts_daylength,
                   'inst/extdata/sample-data/daylength.tif',
                   overwrite = TRUE)



rst_aoi <- rsts_daylength[[1]] * 0 + 1
terra::writeRaster(rst_aoi,
                   'inst/extdata/aoi.tif',
                   overwrite = TRUE)


# R:/rasterdaten/dgm/DGM25UTM
rst_dgm <- terra::rast('data-raw/DGM25UTM/DGM25M_UTM.tif')
rst_dgm <- terra::project(rst_dgm, terra::crs(rsts_tmean))

coords <- terra::crds(rsts_tmin_fr[[1]], na.rm = FALSE)
cells <- terra::cellFromXY(rst_dgm, coords)
rst_dgm_fr <- terra::setValues(rsts_tmin_fr[[1]], rst_dgm[cells])
rst_dgm_fr <- terra::crop(rst_dgm_agg, borders_fr_prj, mask = TRUE)

rst_dgm_agg <- terra::aggregate(rst_dgm, 207)
rst_dgm_fr <- terra::crop(rst_dgm_agg, borders_fr_prj, mask = TRUE)

terra::writeRaster(rst_dgm_fr,
                   'inst/extdata/sample-data/dgm.tif',
                   overwrite = TRUE)


