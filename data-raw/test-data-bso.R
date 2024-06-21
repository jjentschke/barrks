
library(ncdf4)
library(tidyverse)
library(barrks)



source("data-raw/bso-original/borki_calc4.R") # source functions of the original model

# Load Temperature data
tmin <- barrks_data()$tmin
tmax <- barrks_data()$tmax


xx <- 10
yy <- 10

#Temp.mm <- data.frame(Tmin[xx,yy,],Tmax[xx,yy,])
Temp.mm <- data.frame(unname(unlist(tmin[xx,yy])), unname(unlist(tmax[xx,yy])))
names(Temp.mm) <- c("tmin","tmax")
re <- calc_bari(NULL, parm, Temp.mm, 365 - 1) # calculation phenology

result_matrix <- re$res


test_output_bso <- map_dfr(1:4, \(i) {

  offset <- (i - 1) * 9

  df <- as.data.frame(result_matrix[, 1:9 + offset])
  colnames(df) <- c('maturation', 'preflight', 'reproduction', 'egg', 'larva', 'pupa', 'regeneration', 'SB_preflight', 'SB_reproduction')
#df
  cbind(generation = i, doy = 1:nrow(df), df)
})




test_output_bso <- rbind(
  filter(test_output_bso, generation == 1) %>%
    mutate(generation = 0, maturation =
             0, preflight = 0, reproduction = 0, egg = 0, larva = 0, pupa = 0,
           regeneration = 0, SB_preflight = 0, SB_reproduction = 0),
  test_output_bso
)


keys <- c('maturation', 'preflight')

walk(1:4, \(g) {

  df <- filter(test_output_bso, generation == g)

  walk(df$doy, \(d) {

    choose_last <- test_output_bso$doy == d & test_output_bso$generation == g - 1
    choose <- test_output_bso$doy == d & test_output_bso$generation == g

    test_output_bso[choose_last, keys] <<- test_output_bso[choose, keys]
  })
})

test_output_bso$maturation[1:84] <- 1e+09
test_output_bso$preflight[1:84] <- 0


save(test_output_bso, file = 'inst/tinytest/data/test-data-bso.rda')


