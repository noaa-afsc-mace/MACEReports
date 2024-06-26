% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_historical_age_comps_bubbleplot.R
\name{plot_historical_age_comps_bubbleplot}
\alias{plot_historical_age_comps_bubbleplot}
\title{Bubbleplot of the historical numbers- and biomass- at age for the survey timeseries.}
\usage{
plot_historical_age_comps_bubbleplot(
  survey_year_vector,
  age_vector,
  numbers_vector,
  biomass_vector,
  max_age = 12
)
}
\arguments{
\item{survey_year_vector}{A vector identifying the year a given numbers or abundance value is from. This vector must be equal in length to \code{age_vector}, \code{biomass_vector}, and \code{numbers_vector}.}

\item{age_vector}{A vector of fish ages (units are assumed to be years). This vector must be equal in length to \code{survey_year_vector}, \code{biomass_vector}, and \code{numbers_vector}.}

\item{numbers_vector}{A vector of fish numbers (units are expected as individual fish.) This vector must be equal in length to \code{survey_year_vector}, \code{age_vector}, and \code{biomass_vector}.}

\item{biomass_vector}{A vector of fish weights (units are expected as KG.) This vector must be equal in length to \code{survey_year_vector}, \code{age_vector}, and \code{numbers_vector}.}

\item{max_age}{Any years >= than this age will be grouped for plotting, and presented as a 'plus' category. Defaults to 12, as this is consistent with how we generally present pollock age values.}
}
\value{
A base r plot.
}
\description{
Compare a survey timeseries numbers- and biomass- at age to visualize cohorts and compare trends over time. Because abundances can vary greatly over time, abundances are presented as the square-root transform of numbers and biomass values. These plots are taken from work done by Cole Monnahan (cole.monnahan@noaa.gov) and Dave McGowan (david.mcgowan@noaa.gov). You should input biomass values in KG and numbers as numbers of individuals! They will be summarized and plotted as biomass (1000s t) and numbers (millions) as these units work well for our typical pollock surveys.
}
\examples{
\dontrun{
# simulate some fish populations
years <- rep(c(2013,2015, 2017, 2019, 2021), 100000/5)
ages <- round(rpois(n = 100000, lambda = seq(1,6,1)), digits = 0) + 1
numbers <- sample(seq(0, 3e6, 100), size = 100000, replace = TRUE)
weights <- (ages^3 * .005) * numbers

#plot it
plot_historical_age_comps_bubbleplot(survey_year_vector = years,
age_vector = ages,
numbers_vector = numbers,
biomass_vector = weights)

#plot with a higher maximum age grouping
plot_historical_age_comps_bubbleplot(survey_year_vector = years,
 age_vector = ages,
 numbers_vector = numbers,
 biomass_vector = weights,
 max_age = 14)
}
}
