% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bso-plot.R
\name{bso_plot_flight_diagram}
\alias{bso_plot_flight_diagram}
\title{Plot a flight diagram (BSO only)}
\usage{
bso_plot_flight_diagram(
  .pheno,
  .station = prop_stations(.pheno)[1],
  .colors = barrks_colors("bso_flight"),
  .labels = barrks_labels("bso_flight"),
  .xlim = NULL,
  .legend = "topright",
  ...
)
}
\arguments{
\item{.pheno}{A BSO phenology (see \code{\link[=bso_phenology]{bso_phenology()}})}

\item{.station}{Pass a character vector to choose a station assigned to \code{.pheno} by its name, or pass a different station. See \code{\link[=stations_create]{stations_create()}} for details.}

\item{.colors, .labels}{Vectors of colors/labels starting from the first and the
second flght of the hibernating generation followed consecutively by elements
for the filial generations (first and second flight).}

\item{.xlim}{Date vector of length to that limits the dates plotted.}

\item{.legend}{Pass \code{FALSE} if no legend should be plotted. Otherwise the
value will be passed to \code{legend()} as first argument. Look there for more
information.}

\item{...}{arguments passed to \code{graphics::barplot()}.}
}
\value{
None
}
\description{
A flight diagram illustrates the daily share of flying individuals over time.
}
\examples{
\dontshow{if (rlang::is_installed("graphics")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
# This may take a few minutes...

# calculate phenology
p <- bso_phenology('bso', barrks_data('stations'), .quiet = TRUE)

bso_plot_flight_diagram(p)
}
\dontshow{\}) # examplesIf}
}
