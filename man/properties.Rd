% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenology-properties.R
\name{properties}
\alias{properties}
\alias{prop_year}
\alias{prop_dates}
\alias{prop_first_date}
\alias{prop_last_date}
\alias{prop_hatched_generations}
\alias{prop_filial_generations}
\alias{prop_sister_broods}
\alias{prop_stations}
\title{Get phenology properties}
\usage{
## get the year the phenology was calculated for
prop_year(pheno)

## get all dates that are covered by the phenology
prop_dates(pheno)

## get the first date that is covered by the phenology
prop_first_date(pheno)

## get the last date that is covered by the phenology
prop_last_date(pheno)

## get all hatched generations as numeric vector
prop_hatched_generations(pheno)

## get all hatched filial generations as numeric vector
prop_filial_generations(pheno)

## get all hatched sister broods as numeric vector
prop_sister_broods(pheno)

## get the stations assigned to the phenology
prop_stations(pheno)
}
\arguments{
\item{pheno}{A  phenology (see \code{\link[=phenology]{phenology()}})}
}
\value{
The requested property.
}
\description{
To examine a phenology, there are different functions to query its properties.
}
\examples{
\donttest{
# calculate phenology
p <- phenology('phenips-clim', barrks_data(), .quiet = TRUE)

# print all generations that were hatched
prop_hatched_generations(p)
}
}
