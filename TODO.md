
# TODO

## Additional features

- [ ] Provide functions to fetch start and end of each generation
- [ ] Further customization options for `plot_development_diagram()`:
  - [ ] Fill incomplete generations: yes/no
- [ ] `save_phenology`, `load_phenology()`
  - [ ] Include input data
  - [ ] Use metadata file to allow custom storage structures


## General

- [ ] `create_daylength_...()`: switch from suncalc to geosphere (less dependencies...)
- [ ] Make output of `create_suntimes_...()` and `create_daylength_...()` consistent (minutes OR hours)
- [ ] Define a `BarrksPheno` (or similar) class
- [ ] Make order of input raster layers irrelevant (use the date)
- [ ] `phenology()`: All rasters from `.storage` are used even though `.dates` are not `NULL`
- [ ] `plot_development_diagram()`: Adjust legend if `.minmax_only == TRUE`
- [ ] Improve performance of `create_suntimes_df()`
- [ ] Improve performance of `get_mortality_rst()`


## Models

- [ ] BSO
  - [ ] `get_onset_rst()` is not working for BSO
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
  - [ ] Determine `first_diapause_date` dependent on the day length


## Documentation and notifications

- [ ] Implement argument checks
- [ ] `get_hibernating_generations_...()`: warn if NAs are produced
- [ ] Add examples to function documentations (e.g. for `model_combine()`)
- [ ] Unify status messages (print temporary only?)
- [ ] Make storage messages optional 


## Formal testing

- [ ] BSO: storage, sister breeders
- [ ] Test `load_phenology()`, `save_phenology`
- [ ] Test NAs for stepwise calculation
- [ ] Test if all model parameters are documented
- [ ] Implement optical tests for `plot_development_diagram()`


## Appearance

- [ ] Package logo
- [ ] Use DTM in vignette 'model-comparison'
- [ ] `barrks_colors('bso_flight')` appropriate?
