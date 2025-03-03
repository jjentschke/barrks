
# barrks 1.1.0

## Bug fixes

* Calculation of thermal diapause in PHENIPS-Clim was not correct (difference of
  one day in comparison to the previous version)
* The thermal diapause in PHENIPS-Clim was not triggered if last `TRUE` was
  returned before `first_diapause_date`
* The hibernating generations were determined when a mortality event occured
  before the diapause has started
* Stepwise calculations that use a storage could result in additional NAs before
* `load_phenology()` was not working when hibernating generations were not
  present
* Changing the value of `onset_mode` in PHENIPS-Clim resulted in an error for
  station data (#3)
* `plot_development_diagram()` generated incorrect plots if a generation was not
  present in all passed phenology objects on a day when a mortality event
  occured

## New features

* PHENIPS-Clim provides the facility to specify an alternative onset calculation.
  By default, it is used to trigger the onset depending on the cumulative
  maximum temperature if the standard onset calculation fails.


# barrks 1.0.1

* Minor changes in the documentation (typos, english correction, wrong links)


# barrks 1.0.0

* Initial CRAN submission.
