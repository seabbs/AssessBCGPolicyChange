#' Function to Generate Annual Change Scenarios Based on Sutherland Assumptions
#'
#' @param TB_decrease A numeric vector of percentage decreases in TB notifications rates (formated as 0.09 etc.).
#' @param cohorts_to_sim The number of cohorts to simulate. Defaults to 11
#' @param samples Numeric, the number of samples to generate for each scenario. Defaults to 10
#'
#' @return A dataframe with each row representing a sample, with annual decrease stored as a matrix
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom purrr map transpose flatten
#' @seealso TB_decrease_as_matrix
#' @examples
#'
#' gen_annual_change_scenarios()
gen_annual_change_scenarios <- function(TB_decrease = c(0.09, 0.05, 0.01),
                                        cohorts_to_sim = 11, samples = 10){


  TB_decrease_list <- map(1:samples, function(i) {
    map(TB_decrease, ~TB_decrease_as_matrix(rep(. * 100, 3),
                        TB_cohorts = c("15-19 years", "20-24 years", "25-29 years"),
                        data_start = 1969,
                        projected_cohort = cohorts_to_sim,
                        cohort_length = 5)
        )
  }) %>% transpose

 TB_decrease_name <- paste0(round(TB_decrease * 100, digits = 1), "% decrease")

 scenarios <- tibble(scenario = map(TB_decrease_name, ~rep(., samples)) %>% unlist,
                     sample = rep(1:samples, length(TB_decrease_name)),
                     annual_TB_decrease = flatten(TB_decrease_list))

 return(scenarios)
}
