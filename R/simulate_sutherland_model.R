#' Function to run multiple simulations of the Sutherland et al. Estimation Model
#' @param list_annual_TB_decrease A list of annual TB decreases with each element as specified for
#' the \code{Annual.TB.Decrease.Yearly} arguement of \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @param list_percentage_year_one A list of percentage secondary notifications in the first year
#'  with each element as specified for the \code{Percentage.Year.One} arguement of
#'  \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @param list_incidence_rates A list of incidence rates with each element as specified for the \code{incidence_rates}
#'  arguement of \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @param cores The number of cores to use, defaults to one
#' @param ... Additional parameters to pass to \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#'
#' @return A list of simulations for each of the outputs of \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @export
#' @importFrom purrr transpose
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @import foreach
#'
#' @examples
#'
#' simulate_sutherland_model(cores = 1)
#'
simulate_sutherland_model <- function(list_annual_TB_decrease = list(TB_decrease_as_matrix(sutherland_TB_decrease),
                                                                     TB_decrease_as_matrix(sutherland_TB_decrease)),
                                      list_incidence_rates = list(sutherland_incidence_rates, sutherland_incidence_rates),
                                      list_percentage_year_one = rep(0.764, 2), list_sym_lag = list(2, 2),
                                      cores = 1, ...) {
  if (cores > 1) {
    doParallel::registerDoParallel(cores = cores)
    }


  samples <- length(list_annual_TB_decrease)

  simulation_estimates <- foreach(i = 1:samples) %dopar%
  {
    sutherland_model(incidence_rates = list_incidence_rates[[i]],
                     Annual.TB.Decrease.Yearly = list_annual_TB_decrease[[i]],
                     Percentage.Year.One = list_percentage_year_one[[i]],
                     Sym.Lag = list_sym_lag[[i]], ...)
  }

  if (cores > 1) {
  doParallel::stopImplicitCluster()
  }

  simulation_estimates <- purrr::transpose(simulation_estimates)

  return(simulation_estimates)
}
