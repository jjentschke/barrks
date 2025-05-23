% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenology.R
\name{analyse.phenology}
\alias{analyse.phenology}
\title{Analyse a phenology}
\description{
Here, all functions are listed that are available to analyse the results of
a \code{\link[=phenology]{phenology()}}-call.
}
\details{
Get phenology properties:
\itemize{
\item \code{\link[=prop_dates]{prop_dates()}}
\item \code{\link[=prop_filial_generations]{prop_filial_generations()}}
\item \code{\link[=prop_first_date]{prop_first_date()}}
\item \code{\link[=prop_hatched_generations]{prop_hatched_generations()}}
\item \code{\link[=prop_last_date]{prop_last_date()}}
\item \code{\link[=prop_sister_broods]{prop_sister_broods()}}
\item \code{\link[=prop_stations]{prop_stations()}}
\item \code{\link[=prop_year]{prop_year()}}
}

Get phenology results (raster-based):
\itemize{
\item \code{\link[=get_development_rst]{get_development_rst()}}
\item \code{\link[=get_diapause_rst]{get_diapause_rst()}}
\item \code{\link[=get_generations_rst]{get_generations_rst()}}
\item \code{\link[=get_hibernating_generations_rst]{get_hibernating_generations_rst()}}
\item \code{\link[=get_mortality_rst]{get_mortality_rst()}}
\item \code{\link[=get_onset_rst]{get_onset_rst()}}
}

Get phenology results (station-based):
\itemize{
\item \code{\link[=get_development_df]{get_development_df()}}
\item \code{\link[=get_diapause_df]{get_diapause_df()}}
\item \code{\link[=get_generations_df]{get_generations_df()}}
\item \code{\link[=get_hibernating_generations_df]{get_hibernating_generations_df()}}
\item \code{\link[=get_mortality_df]{get_mortality_df()}}
\item \code{\link[=get_onset_df]{get_onset_df()}}
}

Plot phenology results (station-based):
\itemize{
\item \code{\link[=plot_development_diagram]{plot_development_diagram()}}
}
}
\seealso{
\link{analyse.phenology.bso}
}
