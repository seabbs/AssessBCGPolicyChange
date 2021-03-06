% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_annual_decrease_inc_rates.R
\name{sample_annual_decrease_inc_rates}
\alias{sample_annual_decrease_inc_rates}
\title{Samples the Percentage Annual Decrease Based On Incidence Rates}
\usage{
sample_annual_decrease_inc_rates(samples = 10,
  TB_cohorts = c("15-19 years", "20-24 years", "25-29 years"),
  max_year = 2023, min_year = 1969, years_to_avg = 3, cores = 1,
  verbose = TRUE)
}
\arguments{
\item{samples}{Numeric, the number of samples to take. Defaults to 10.}

\item{TB_cohorts}{A character vector indicating the names of the cohorts modelled. Defaults to 15-19 years, 20-24 years, and 25-29 years.}

\item{max_year}{Numeric, set as greater than the maximum year in the dataframe if interested in projection.
Defaults to \code{NULL}. If specified, then an average of the percentage decrease for a set number
of years is taken. This is specified with \code{years_to_avg}.}

\item{years_to_avg}{Numeric, the number of years over which to average the percentage decrease
when estimating the ongoing percentage decrease.}

\item{verbose}{Logical, defaults to \code{TRUE}. Should summary measures and plots be returned.}
}
\value{
A nested tibble, containing the sample number, and both a nested dataframe and nested matrix of
annual percentage changes.
}
\description{
This function samples the percentage annual decrease, using Sutherland et al's estimates for
White UK born incidence rates and estimates of UK born incidence rates  in England calculated using the Enhanced Tuberculosis
surveilliance system and the Labour Force Survey. Incidence rates are first sampled from the estimated UK born incidence rates,
by assuming that they are normally distributed. Incidence rates are then sampled from the Sutherland et al. estimates from 1973, 1978, and 1983
using \code{\link[AssessBCGPolicyChange]{sample_incidence_rates}}. Average incidence rates for all cases are then estimated using Sutherland
et al.'s population weights for those BCG vaccinated (75%), unvaccinated (11%), tuberculin positive (8%) and ineligible (6%). Missing
incidence rates are then estimated using a loess fit for each sample, with samples predicted to have an incidence rate below 0.1 per 100,000
having this set to be 0.1 per 100,000. The percentage annual difference is the calculated for each sample
using  \code{\link[AssessBCGPolicyChange]{ sample_per_decrease}}. Projected annual percentage changes are estimated using an average of
the previous 3 years. Similarly for years prior to Sutherland et als incidence rate estimates an average of the previous 3 years are used.
Samples of the percentage annual decrease are returned as a tibble, containing the nested dataframe and a nested matrix ready to be used by
\code{\link[AssessBCGPolicyChange]{sutherland_model}}.
}
\examples{

sample_annual_decrease_inc_rates(samples = 10, cores = 1)

}
\seealso{
sample_per_decrease sample_annual_decrease_nots
}
