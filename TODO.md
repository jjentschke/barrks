
# TODO

## General

- [ ] Implement argument checks
- [ ] Add examples to function documentations (e.g. for `model_combine()`)
- [ ] `create_daylength_...()`: switch from suncalc to geosphere (less dependencies...)
- [ ] Make output of `create_suntimes_...()` and `create_daylength_...()` consistent (minutes OR hours)
- [ ] Define a `BarrksPheno` (or similar) class
- [ ] Make order of input raster layers irrelevant (use the date)
- [ ] `phenology()`: All rasters from `.storage` are used even though `.dates` are not `NULL`
- [ ] Further customization options for `plot_development_diagram()`:
  - [ ] fill incomplete generations: yes/no
  - [x] customize fill after .split_date
- [ ] Unify status messages (print temporary only?)
- [ ] Make storage messages optional 


## Models

- [ ] BSO
  - [ ] Implement original daylength function?
  - [ ] Define functions `bso_save_phenology()` and `bso_load_phenology()`
  - [ ] Enable application of mortality
  - [ ] `bso_plot_flight_diagram()`/`bso_plot_stage_diagram()`: make x-axis optional
  - [ ] Improve performance of `bso_plot_stage_diagram()`
  - [ ] Define functions to get the share of individuals flying / in a specific
        stage / ...
  - [ ] Improve performace of `bso_calc_tphloem()`
- [ ] Lange: Improve performance
- [ ] PHENIPS-Clim
  - [ ] Improve performance of the calculation of development rates
  - [ ] Allow custom intervals or function for `dev_rates`


## Formal testing

- [ ] BSO: storage, sister breeders
- [ ] `load_phenology()`/`save_phenology`


## Design

- [ ] Package logo
- [ ] Use DTM in vignette 'model-comparison'
- [ ] `barrks_colors('bso_flight')` appropriate?
