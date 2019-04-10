
#' Samples the Percentage Annual Decrease Based On Incidence Rates
#'
#' @description This function samples the percentage annual decrease, using Sutherland et al's estimates for
#' White UK born incidence rates and estimates of UK born incidence rates  in England calculated using the Enhanced Tuberculosis
#' surveilliance system and the Labour Force Survey. Incidence rates are first sampled from the estimated UK born incidence rates,
#' by assuming that they are normally distributed. Incidence rates are then sampled from the Sutherland et al. estimates from 1973, 1978, and 1983
#' using \code{\link[AssessBCGPolicyChange]{sample_incidence_rates}}. Average incidence rates for all cases are then estimated using Sutherland
#' et al.'s population weights for those BCG vaccinated (75%), unvaccinated (11%), tuberculin positive (8%) and ineligible (6%). Missing
#' incidence rates are then estimated using a loess fit for each sample, with samples predicted to have an incidence rate below 0.1 per 100,000
#' having this set to be 0.1 per 100,000. The percentage annual difference is the calculated for each sample
#'  using  \code{\link[AssessBCGPolicyChange]{ sample_per_decrease}}. Projected annual percentage changes are estimated using an average of
#'  the previous 3 years. Similarly for years prior to Sutherland et als incidence rate estimates an average of the previous 3 years are used.
#'  Samples of the percentage annual decrease are returned as a tibble, containing the nested dataframe and a nested matrix ready to be used by
#'  \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @param samples Numeric, the number of samples to take. Defaults to 10.
#' @param verbose Logical, defaults to \code{TRUE}. Should summary measures and plots be returned.
#' @return A nested tibble, containing the sample number, and both a nested dataframe and nested matrix of
#' annual percentage changes.
#' @seealso sample_per_decrease sample_annual_decrease_nots
#' @inheritParams sample_annual_decrease_nots
#' @export
#' @import magrittr
#' @import ggplot2
#' @import multidplyr
#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr spread nest unnest gather
#' @importFrom purrr map map2
#' @examples
#'
#' sample_annual_decrease_inc_rates(samples = 10, cores = 1)
#'
sample_annual_decrease_inc_rates <- function(samples = 10,  TB_cohorts = c("15-19 years", "20-24 years", "25-29 years"),
                                             max_year = 2023, min_year = 1969, years_to_avg = 3, cores = 1, verbose = TRUE) {

  ## sample incidence rates from ETS and LFS estimation
  sample_inc_rates_ets <- inc_rates_uk_e %>%
    mutate(sample = map2(incidence, sd, function(inc, sd_sample) {
      tibble(sample = 1:samples,
             inc_rate_sample = rnorm(samples, mean = inc, sd = sd_sample)
      )})
    ) %>%
    unnest() %>%
    mutate(inc_rate_sample = ifelse(inc_rate_sample < 0, 0, inc_rate_sample))

  ## sample incidence rates from sutherland notification surverys
  ## Estimate overall incidence rates using a weighted average based on quoted population estimates
  suth_sample_inc <- tibble(sample = 1:samples,
                            inc_rates = sample_incidence_rates(sutherland_incidence_rates,
                                                               sutherland_data,
                                                               samples = samples)
  ) %>%
    mutate(inc_rates = inc_rates %>%
             map(function(inc, wts) {
               inc <- inc$Unvaccinated * wts["unvac"] + inc$Vaccinated * wts["vac"] + inc$Tuberculin * wts["tub"] + inc$Inelgible * wts["inelg"]
             }, wts = c(unvac = 0.11, vac = 0.75, tub = 0.08, inelg = 0.06))
    ) %>%
    mutate(inc_rates = inc_rates %>%
             map(~as_tibble(.))
    ) %>%
    mutate(inc_rates = inc_rates %>%
             map(~mutate(., year = c(1973, 1978, 1983)))
    ) %>%
    unnest %>%
    mutate_at(.vars = TB_cohorts,
              .funs = funs(ifelse(. == 0, NA, .))) %>%
    gather(key = "age_group", value = "inc_rate_sample", -sample, -year) %>%
    mutate(age_group = factor(age_group)) %>%
    rowwise %>%
    mutate(year = case_when(age_group %in% "20-24 years" && year == 1973 ~ 1978,
                            age_group %in% "20-24 years" && year == 1983 ~ 1973,
                            age_group %in% "20-24 years" && year == 1978 ~ 1983,
                            age_group %in% "25-29 years" && year == 1973 ~ 1983,
                            age_group %in% "25-29 years" && year == 1983 ~ 1973,
                            TRUE ~ year))


  ## bind two datasets together
  sample_inc_rates <- suth_sample_inc %>%
    bind_rows(sample_inc_rates_ets %>%
                select(year, sample, age_group, inc_rate_sample))

  ##Add missing years for each sample and age group
  sample_inc_rates <- tibble(year = seq(min(sample_inc_rates$year),
                                        max(sample_inc_rates$year),
                                        by = 1), id = 1) %>%
    left_join(tibble(sample = 1:samples, id = 1), by = "id") %>%
    left_join(tibble(age_group = factor(TB_cohorts), id = 1),
              by = "id") %>%
    select(-id) %>%
    left_join(sample_inc_rates, by = c("year", "sample", "age_group"))

 if (verbose) {
   ## Set up internal plot function
   plot_annual_sample_hex <- function(df, var, loess = FALSE) {

     plot <- df %>%
       ggplot(aes_string(x = "year", y = var, col = "age_group", group = "age_group", fill = "age_group")) +
       geom_hex(aes(alpha = ..count..))

     if (loess) {
       plot <- plot +
         geom_smooth(method = "loess", data = df[sample(1:nrow(df), 1000),])
     }
      plot <- plot +
       facet_wrap(~age_group) +
       scale_fill_viridis_d(end = 0.9) +
       scale_colour_viridis_d(end = 0.9) +
       theme_minimal()

      print(plot)
   }
   ## Plot sampled data
   message("Loess fit uses 1000 sampled data points")
   sample_inc_rates %>%
     plot_annual_sample_hex("inc_rate_sample", loess = TRUE)

 }

  ## Estimate incidence rates in missing years for each sample and age group
  ## If a prediction is less than or equal to be zero then assume it is actually 0.1
  ## Zero incidence rates are extremley unlikely
  sample_inc_rates <- sample_inc_rates %>%
    group_by(sample, age_group) %>%
    nest() %>%
    mutate(pred_inc_rate_sample = map(data, ~predict(loess(inc_rate_sample ~ year, data = .), .))) %>%
    unnest() %>%
    ungroup %>%
    mutate(pred_inc_rate_sample = ifelse(pred_inc_rate_sample <= 0.01, 0.01, pred_inc_rate_sample)) %>%
    mutate(inc_rate_sample = ifelse(is.na(inc_rate_sample), pred_inc_rate_sample, inc_rate_sample)) %>%
    select(year, sample, age_group, inc_rate_sample, pred_inc_rate_sample)

  if (verbose) {
    ## Plot sampled data along with data estimated using a loess fit for each sample
    sample_inc_rates %>%
      ggplot(aes(x = year, y = inc_rate_sample, col = age_group, group = age_group, fill = age_group)) +
      geom_hex(aes(alpha = ..count..)) +
      facet_wrap(~age_group) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      theme_minimal()

  }


  ## Need to take
  cluster <- create_cluster(cores = cores) %>%
    cluster_library(packages = c("AssessBCGPolicyChange", "dplyr")) %>%
    cluster_copy(sample) %>%
    cluster_copy(max_year) %>%
    cluster_copy(min_year) %>%
    cluster_copy(years_to_avg)

  ## sample percentage decrease - using a simple moving average for future years
  sample_per_decrease <- sample_inc_rates %>%
    partition(age_group, sample, cluster = cluster) %>%
    group_by(age_group, sample) %>%
    do(
      per_sample = select(., year, inc_rate_sample) %>%
        sample_per_decrease(., max_year = max_year, min_year = min_year, samples = 1, years_to_avg = years_to_avg)
    ) %>%
    collect %>%
    unnest %>%
    select(-sample1, -inc_rate_sample)

if (verbose) {
  ## plot annual percentage decrease
  plot_annual_sample_hex(sample_per_decrease, "prop_decrease_sample")

  ## number of samples with very large percentage changes
  message("\n", sample_per_decrease %>%
    filter(abs(prop_decrease_sample) > 2) %>%
    nrow,
    " samples have very large percentage changes")

  ## plot annual percentage decrease, dropping extremely large values
  sample_per_decrease %>%
    filter(abs(prop_decrease_sample) < 2) %>%
    plot_annual_sample_hex("prop_decrease_sample")
}

  ## spread annual percentage decrease for age groups
  sample_per_decrease <- sample_per_decrease %>%
    spread(key = "age_group", value = "prop_decrease_sample")

  per_decrease_age <- sample_per_decrease  %>%
    group_by(sample) %>%
    nest(.key = "per_decrease_df") %>%
    ungroup %>%
    mutate(per_decrease_mat = map(per_decrease_df, function(x) {
      dm_x <- data.matrix(x[,-1])
      row.names(dm_x) <-  unlist(x[,1])
      return(dm_x)
    }))

  return(per_decrease_age)
}
