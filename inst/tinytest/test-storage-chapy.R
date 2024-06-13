

if(at_home()) {

  source('storage-test.R')


  phenos <- storage_test('chapy', 'storage-chapy')

  purrr::walk(names(phenos[[1]]), \(key) {
    if(!class(phenos[[1]][[key]]) == 'SpatRaster') return()
    expect_true(all(terra::values(phenos[[1]][[key]] - phenos[[2]][[key]]) < 0.00001, na.rm = TRUE))
  })

  expect_true(all(phenos[[1]]$dates == phenos[[2]]$dates))

  unlink('storage-chapy', recursive = TRUE)
}
