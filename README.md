
# barrks

<!-- badges: start -->
<!-- badges: end -->

The goal of `barrks` (**bar**k beetle **r**aster **k**it for
**s**easonal development) is to calculate the phenological development
of bark beetles. Rather than implementing one specific model, the
package provides a collection of different models that can be chosen.
Additionally, the models can be customized and combined to create an
individual model. The calculations can be done spatially explicit by
using raster inputs, or based on station inputs that are available as
data frames. Even though most of the implemented models describe the
phenology of *Ips typographus*, the package is not limited to particular
bark beetle species. For instance, CHAPY models the phenology of
*Pityogenes chalcographus* and the package may be extended by models for
additional bark beetle species. The full documentation of `barrks` can
be found [here](https://jjentschke.github.io/barrks).

The following table lists the models that are implemented in the
package.

| Model        | Publication                             | Species            | Help                                                              |
|--------------|-----------------------------------------|--------------------|-------------------------------------------------------------------|
| BSO          | Jakoby, Lischke, and Wermelinger (2019) | *I. typographus*   | `?model.bso.apply` <br/> `?model.bso.customize`                   |
| Lange        | Lange, Oekland, and Krokene (2006)      | *I. typographus*   | `?model.lange.apply` <br/> `?model.lange.customize`               |
| Jönsson      | Jönsson et al. (2011)                   | *I. typographus*   | `?model.joensson.apply` <br/> `?model.joensson.customize`         |
| PHENIPS      | Baier, Pennerstorfer, and Schopf (2007) | *I. typographus*   | `?model.phenips.apply` <br/> `?model.phenips.customize`           |
| PHENIPS‑Clim | \-                                      | *I. typographus*   | `?model.phenips_clim.apply` <br/> `?model.phenips_clim.customize` |
| RITY         | Ogris et al. (2019)                     | *I. typographus*   | `?model.rity.apply` <br/> `?model.rity.customize`                 |
| CHAPY        | Ogris et al. (2020)                     | *P. chalcographus* | `?model.chapy.apply` <br/> `?model.chapy.customize`               |

## Installation

<!--You can install the latest released version of `barrks` from CRAN from within R:
&#10;
``` r
install.packages('barrks')
```
-->

The development version of `barrks` can be installed from
[GitHub](https://github.com/):

``` r
devtools::install_github("jjentschke/barrks")
```

`barrks` will be published on CRAN soon.

## Basic Example

`barrks` comes with sample data that will be used below. The phenology
is calculated with `phenology()` which takes all necessary inputs as
arguments. Subsequently, the rasters of emerged generations by date can
be retrieved with `get_generations_rst()`. `terra::plot()` can be used
to visualize these rasters.

``` r

library(barrks)
library(tidyverse)
library(terra)


# calculate phenology
pheno <- phenology('phenips-clim', barrks_data())

# plot number of prevailing generations on 4 different dates
dates <- c('2015-04-15', '2015-06-15', '2015-08-15', '2015-10-15')
get_generations_rst(pheno, dates) %>% plot(mar = c(0.2, 0.1, 2, 5),
                                           axes = FALSE, box = TRUE, nr = 1,
                                           cex.main = 1.9, plg = list(cex = 1.8))
```

<div class="figure">

<img src="man/figures/README-generations-plot-1.png" alt="Generations plot" width="100%" />
<p class="caption">
Generations plot
</p>

</div>

`barrks` makes it easy to plot the development of the individual
generations. To illustrate that, a “shaded” variant of the phenology
above is calculated and the development diagram for a specific cell
(called “station” in `barrks`) is plotted for bith phenology variants.

``` r

pheno_shaded <- phenology('phenips-clim', barrks_data(), exposure = 'shaded')

plot_development_diagram(list(pheno, pheno_shaded),
                         stations_create('Example', 234),
                         .legend_lty = FALSE,
                         xlim = as.Date(c('2015-04-01', '2015-12-31')))
```

<div class="figure">

<img src="man/figures/README-development-diagram-1.png" alt="Development diagram" width="100%" />
<p class="caption">
Development diagram
</p>

</div>

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Baier2007" class="csl-entry">

Baier, Peter, Josef Pennerstorfer, and Axel Schopf. 2007.
“<span class="nocase">PHENIPS—A comprehensive phenology model of *Ips
typographus* (L.)(Col., Scolytinae) as a tool for hazard rating of bark
beetle infestation</span>.” *Forest Ecology and Management* 249 (3):
171–86. https://doi.org/<https://doi.org/10.1016/j.foreco.2007.05.020>.

</div>

<div id="ref-Jakoby2019" class="csl-entry">

Jakoby, Oliver, Heike Lischke, and Beat Wermelinger. 2019.
“<span class="nocase">Climate change alters elevational phenology
patterns of the European spruce bark beetle (*Ips typographus*)</span>.”
*Global Change Biology* 25 (12): 4048–63.
https://doi.org/<https://doi.org/10.1111/gcb.14766>.

</div>

<div id="ref-Jonsson2011" class="csl-entry">

Jönsson, Anna Maria, Susanne Harding, Paal Krokene, Holger Lange, Ake
Lindeloew, Bjoern Oekland, Hans Peter Ravn, and Leif Martin Schroeder.
2011. “<span class="nocase">Modelling the potential impact of global
warming on *Ips typographus* voltinism and reproductive
diapause</span>.” *Climatic Change* 109: 695–718.
https://doi.org/<https://doi.org/10.1007/s10584-011-0038-4>.

</div>

<div id="ref-Lange2006" class="csl-entry">

Lange, Holger, Bjoern Oekland, and Paal Krokene. 2006.
“<span class="nocase">Thresholds in the life cycle of the spruce bark
beetle under climate change</span>.” *Interjournal Complex Syst.* 1648
(January).

</div>

<div id="ref-Ogris2019" class="csl-entry">

Ogris, Nikica, Mitja Ferlan, Tine Hauptman, Roman Pavlin, Andreja
Kavčič, Maja Jurc, and Maarten De Groot. 2019.
“<span class="nocase">RITY–A phenology model of Ips typographus as a
tool for optimization of its monitoring</span>.” *Ecological Modelling*
410: 108775.
https://doi.org/<https://doi.org/10.1016/j.ecolmodel.2019.108775>.

</div>

<div id="ref-Ogris2020" class="csl-entry">

Ogris, Nikica, Mitja Ferlan, Tine Hauptman, Roman Pavlin, Andreja
Kavčič, Maja Jurc, and Maarten de Groot. 2020.
“<span class="nocase">Sensitivity analysis, calibration and validation
of a phenology model for *Pityogenes chalcographus* (CHAPY)</span>.”
*Ecological Modelling* 430: 109137.
https://doi.org/<https://doi.org/10.1016/j.ecolmodel.2020.109137>.

</div>

</div>
