

rst <- terra::rast(nrows = 1, ncols = 1, nlyrs = 10,
                   vals = c(0, 0, NA, 0, 1, 0, NA, 1, NA, 0))

vals <- terra::values(barrks:::.trigger_rst(rst))

expect_true(all(vals == c(rep(FALSE, 4), rep(TRUE, 6))))
