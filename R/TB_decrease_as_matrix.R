#' Annual Decreae in TB from Single Estimate for Each Group
#'
#' @description From a vetor of assumed annual decreases in TB, this function constructs a
#' matrix with yearly estimates of TB decrease
#' @param TB_decrease A named vector of percentage decreases (i.e 1\%) in TB incidence. Names should refer to age group.
#' @param TB_cohorts A character vector indicating the names of the cohorts modelled. Defaults to 15-19 years, 20-24 years, and 25-29 years.
#' @param data_start Numeric, the year the first data point originates, defaults to 1969 as in Sutherland et al.
#' @param projected_cohort Numeric, the number of cohort for which to simulate, defaults to 5. Combines with \code{cohort_length}
#' to give the length of time to simulate.
#' @param cohort_length Numeric, the lengh of time spent in each cohort, defaults to 5 years as in Sutherland et al.
#' @inheritParams sutherland_model
#' @return A matrix of annual TB incidence decreases as a percentage (0.01) for a given number of years and age groups
#' @export
#'
#' @examples
#'
#' TB_decrease <- c(10, 8, 11)
#'
#'
#' TB_decrease_as_matrix(TB_decrease)
#'
TB_decrease_as_matrix <- function(TB_decrease, TB_cohorts = c('15-19 years', '20-24 years', '25-29 years'), data_start = 1969, projected_cohort = 9, cohort_length = 5) {
  TB_decrease_matrix <- t(matrix(rep(TB_decrease/100,
                                     (projected_cohort)*cohort_length),3,(projected_cohort)*cohort_length))
  rownames(TB_decrease_matrix) <- as.character(sapply(1:nrow(TB_decrease_matrix), function(i) { M <- data_start + i - 1 }) )
  colnames(TB_decrease_matrix) <- names(TB_decrease)

  return(TB_decrease_matrix)
}
