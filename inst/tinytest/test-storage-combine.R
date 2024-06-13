

if(at_home()) {

  source('storage-test.R')


  m <- model_combine('phenips-clim',
                     list(model = 'rity', submodels = 'onset'),
                     list(model = 'lange', submodels = 'diapause'),
                     list(model = 'phenips', 'submodels = mortality'))


  phenos <- storage_test(m, 'storage-combine')

  purrr::walk(names(phenos[[1]]), \(key) {
    if(!class(phenos[[1]][[key]]) == 'SpatRaster') return()
    expect_true(all(terra::values(phenos[[1]][[key]] - phenos[[2]][[key]]) < 0.00001, na.rm = TRUE))
  })

  expect_true(all(phenos[[1]]$dates == phenos[[2]]$dates))

  unlink('storage-combine', recursive = TRUE)
}
