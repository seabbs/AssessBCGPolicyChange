#' Fit Percentage of First Year Cases
#'
#' @description Fit the percentage of cases in the first year using the estimation model and
#' original results of sutherland et al. using sum of least squares.
#'
#' @param model Model function to fit, by default uses \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @param results Orginal results, by default uses \code{\link[AssessBCGPolicyChange]{sutherland_results}}
#' @param ... Pass additional to \code{model}. See \code{\link[AssessBCGPolicyChange]{sutherland_model}} for details if
#' using this as the \code{model} function.
#'
#' @return The best fitting value for the percentage of cases in the first year using the sum of least squares.
#' @export
#'
#' @examples
#'
#'fit_model_to_data(model = sutherland_model, results = sutherland_results)
#'
fit_model_to_data <- function(model = sutherland_model, results = sutherland_results, ...){


   model_fit <- optimize(function(x) {

    model_results <- model(Percentage.Year.One = x, ...)
    model_total_nots <- model_results[["Table 5 - Total Effects of ending the schools BCG scheme"]][c('1986','1991','1996'), c('1988', '1993', '1998', '2003', '2008', '2013')]

    sum_of_squares <- sum((results - model_total_nots)^2)

    return(sum_of_squares)
     },
    interval = c(0,1)
   )

return(model_fit$minimum)
  }
