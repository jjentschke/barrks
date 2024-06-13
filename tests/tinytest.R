
if ( requireNamespace("tinytest", quietly=TRUE) ){
  home <- length(unclass(packageVersion("barrks"))[[1]]) == 4
  tinytest::test_package("barrks", at_home = home)
}

