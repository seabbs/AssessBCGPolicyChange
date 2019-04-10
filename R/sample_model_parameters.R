#' Function to Sample Model Parameters
#'
#' @description The serial interval is estimated as 1.44 (95% CI 1.29 to 1.63) years based on
#' [this](https://doi.org/10.1093/ije/dyr058) study. Samples were taken by assuming a normal
#' distribution with a standard deviation estimated from the parameters confidence intervals.
#' The above study includes data on the percentage of cases in each year since infection in the
#' form of a figure. This appears to roughly match the original model estimate. As no other estimate
#' is available the point estimate from the original model is used.
#' @inheritParams sample_incidence_rates
#' @param samples Numeric, the number of samples to run. Defaults to 100
#' @param as_sutherland Logical, defaults to \code{FALSE}. If \code{TRUE} then the
#' original estimates used by sutherland et al. are returned. Note these are point estimates.
#' @return A tibble of parameter samples
#' @export
#' @seealso sample_incidence_rates
#' @importFrom tibble tibble
#' @examples
#'
#' sample_model_parameters()
#'
sample_model_parameters <- function(incidence_rates = sutherland_incidence_rates,
                                    population_data = sutherland_data,
                                    samples = 100, as_sutherland = FALSE){

  if (as_sutherland) {

    per_year_one <- 0.764
    sym_lag <- 2
    inc_rates <- list(sutherland_incidence_rates)

  }else{
    ## Generate parameter estimates
    per_year_one <- 0.764

    point_sym_lag <- 1.44
    sd_sym_lag <-  (1.63 - 1.29)/(2*qnorm(0.975))

    sym_lag <- rnorm(samples,  point_sym_lag, sd_sym_lag)

    inc_rates <- sample_incidence_rates(incidence_rates = sutherland_incidence_rates,
                                        population_data = sutherland_data,
                                        samples = samples)
  }

  sample_parameters <- tibble(sample = 1:samples,
                              incidence_rates = inc_rates,
                              per_year_one = per_year_one,
                              sym_lag = sym_lag)

  return(sample_parameters)
}
