#' Data used in Sutherland et al
#'
#' A dataset containing the data used by Sutherland et al to estimate the impact of ending the BCG school's
#' scheme. The data has been aggregated for surveys. Details of its collection can be found in the original paper,
#' in \code{sutherland_data.R} (see \code{data-raw}) and in the vignette discussing recreating Sutherland et al.'s model.
#' @format A list with 14 elements.
#' \describe{
#'   \item{Cohort}{A character string indicating the year of entry to study}
#'   \item{Cohort.Ranges}{Age ranges included in the study; 15-19, 20-24 and '25-29 years}
#'   \item{Projected.Cohort}{The number of years for which to project forward (scaled by the length of cohort)}
#'   \item{Cohort.Pop}{The population at entry in each cohort}
#'   \item{Unvaccinated}{Incidence rates (per 100,000) in those unvaccinated by cohort and age group}
#'   \item{Vaccinated}{Incidence rates (per 100,000) in those vaccinated by cohort and age group}
#'   \item{Notifications.Prevented}{Notifications prevented by 100,000 people by cohor and age groups}
#'   \item{BCG.efficacy}{Estimated efficacy of the BCG vaccine}
#'   \item{BCG.vaccinations}{Number of BCG vaccines given by 5 year period}
#'   \item{BCG.Unvaccinated}{Number of BCG unvaccinated by 5 year period}
#'   \item{BCG.Tuberculin}{Number Tuberculin positive by 5 year period}
#'   \item{BCG.Ineglible}{Number inelgible for the BCG schools scheme by 5 year period}
#' }
#'
#'
"sutherland_data"
