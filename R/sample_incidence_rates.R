#' Function to sample Sutherland Incidence rates to Include Uncertainty
#'
#' @param incidence_rates Incidence rates as formated by \code{\link[AssessBCGPolicyChange]{sutherland_incidence_rates}}
#' @param population_data Population data as formated by  \code{\link[AssessBCGPolicyChange]{sutherland_data}}
#' @param samples Numeric, the number of samples to take. Defaults to 100.
#'
#' @return A list of lists. Each list contains matrices of sampled incidence rates for unvaccinated, vaccinated, tuberculin postive
#' and inelgible populations.
#' @export
#' @importFrom purrr map2 transpose
#' @import magrittr
#' @examples
#'
#' sample_incidence_rates(sutherland_incidence_rates, sutherland_data, samples = 5)
#'
sample_incidence_rates <- function(incidence_rates, population_data, samples = 100) {
  ## Sample incidence rates to correctly include uncertainty
  unvac_inc_sample <- sample_inc_rate(incidence_rates$Unvaccinated,
                                      population_data$BCG.Unvaccinated[1:3], samples = samples)
  vac_inc_sample <- sample_inc_rate(incidence_rates$Vaccinated,
                                    population_data$BCG.vaccinations[1:3], samples = samples)

  not_prevented <- map2(unvac_inc_sample, vac_inc_sample, function(x, y) x-y)

  tub_inc_sample <- sample_inc_rate(incidence_rates$Tuberculin,
                                    population_data$BCG.Tuberculin[1:3], samples = samples)

  ## Assumption that those inelgible have the same incidence rates as the unvaccinated. No published data avaialble
  inelg_inc_sample <- unvac_inc_sample

  incidence_rates <- list(unvac_inc_sample, vac_inc_sample, not_prevented, tub_inc_sample, inelg_inc_sample)
  names(incidence_rates) <- c("Unvaccinated", "Vaccinated", "Notifications.Prevented", "Tuberculin", "Inelgible")

  incidence_rates <- incidence_rates %>%
    transpose

  return(incidence_rates)

}
