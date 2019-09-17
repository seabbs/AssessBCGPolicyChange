#' A Function to Generate Scenario Results from the Sutherland Model
#'
#' @param scenarios A dataframe of scenarios, see the examples for a structural example.
#' @param parameter_samples A dataframe of parameter samples, see the examples for an example.
#' @param cores The number of cores to use, defaults to one
#' @param ... Additional arguments passed to \code{sutherland_model}.
#' @return A dataframe combining the scenarios, parameter samples, and model results
#' @export
#' @import magrittr
#' @importFrom dplyr full_join bind_cols select
#' @importFrom purrr flatten map
#' @importFrom tibble tibble
#' @importFrom tidyr spread
#' @examples
#'
#' scenarios <- gen_annual_change_scenarios(TB_decrease = c(0.09, 0.05, 0.01),
#' cohorts_to_sim = 11, samples = 10)
#'
#' parameter_samples <- sample_model_parameters(incidence_rates = sutherland_incidence_rates,
#' population_data = sutherland_data, samples = 10)
#'
#' gen_scenario_results(scenarios, parameter_samples, cores = 1)
gen_scenario_results <- function(scenarios, parameter_samples, cores = 1, ...) {

  if (max(scenarios$sample) != max(parameter_samples$sample)) {
    stop("The number of samples in scenarios and parameter_samples must match")
  }

  ## Bind scenarios and parameter samples
  scenarios <- scenarios %>%
    full_join(parameter_samples, by = "sample")

  ## Simulate model - map results into data frame
  model_sims <- simulate_sutherland_model(list_annual_TB_decrease = scenarios$annual_TB_decrease,
                                                          list_incidence_rates = scenarios$incidence_rates,
                                                          list_percentage_year_one = scenarios$per_year_one,
                                                          list_sym_lag = scenarios$sym_lag,
                                                          cores = cores, ...)

  model_sims <- tibble(output =  map(names(model_sims),
                                     ~rep(., length(flatten(model_sims))/length(names(model_sims)))) %>%
                         unlist,
                       simulation = model_sims %>% flatten,
                       sample = rep(1:as.integer(length(flatten(model_sims))/length(names(model_sims))),
                                    length(names(model_sims)))
  ) %>%
    spread(output, simulation)

  ## Join scenarios and samples
  scenario_results <- scenarios %>%
    bind_cols(model_sims %>%
                select(-sample))

  return(scenario_results)
}
