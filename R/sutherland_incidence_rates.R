#' Incidence rates used in Sutherland et al
#'
#' A dataset containing the incidence rates used by Sutherland et al to estimate the impact of ending the BCG school's
#' scheme. The data has been aggregated for surveys. Details of its collection can be found in the original paper,
#' in \code{sutherland_incidence_rates.R} (see \code{data-raw}) and in the vignette discussing recreating Sutherland et al.'s model.
#' @format A list with 14 elements.
#' \describe{
#'   \item{Unvaccinated}{A matrix of incidence rates for the unvaccinated by age and year of entry}
#'   \item{Vaccinated}{A matrix of incidence rates for the vaccinated by age and year of entry}
#'   \item{Notifications.Prevented}{A matrix of notifications prevented (as rates) by age and year of entry}
#'   \item{Tuberculin}{Incidence rates (per 100,000) in the Tuberculin positive population}
#'   \item{Inelgible}{Incidence rates (per 100,000) in those inelgible for BCG vaccination}
#'}
#'
"sutherland_incidence_rates"
