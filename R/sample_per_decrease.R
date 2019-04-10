#' A Function to Sample the Annual Percentage Decrease with Uncertainty
#'
#' @param df A dataframe containing a year variable and with the second variable being the
#' one to sample from.
#' @param samples Numeric, the number of samples to take.
#' @param max_year Numeric, set as greater than the maximum year in the dataframe if interested in projection.
#' Defaults to \code{NULL}. If specified, then an average of the percentage decrease for a set number
#' of years is taken. This is specified with \code{years_to_avg}.
#' @param min_year Numeric, set as smaller than the minimum year in the dataframe if interested in backwards projection.
#' Defaults to \code{NULL}. If specified, then an average of the percentage decrease for a set number
#' of years is taken. This is specified with \code{years_to_avg}.
#' @param years_to_avg Numeric, the number of years over which to average the percentage decrease
#' when estimating the ongoing percentage decrease.
#' @return A dataframe containing annual percentage decrease samples.
#' @export
#' @import magrittr
#' @importFrom dplyr mutate select full_join everything summarise_all ungroup
#' @importFrom tidyr unnest
#' @importFrom purrr map2_dbl map2
#' @importFrom tibble tibble
#' @examples
#'
#'  df <- data.frame(year = c(2000:2010), measure = seq(10, 110, 10))
#'
#' sample_per_decrease(df, samples = 2)
#'
sample_per_decrease <- function(df, max_year = NULL, min_year = NULL, samples = 100, years_to_avg = 3) {
  df$measure <- df[[2]]

  ## Internal function to estimate the standard devation using prop.test
  prop_sd <- function(p, n) {
    if (p > n) {
      num <- n
      denom <- p
    }else{
      num <- p
      denom <- n
    }
    prop <- suppressWarnings(prop.test(num, denom))

    ## Estimate ci and back calc sd
    lci <- prop$conf.int[1]
    uci <- prop$conf.int[2]
    sd <- (uci-lci)/(2*qnorm(0.975))

    return(sd)
  }

  df_sample <- df %>%
    mutate(prev_year_m = lag(measure)) %>%
    mutate(prop_decrease =  1 - measure / prev_year_m) %>%
    na.omit()

  if(!is.null(max_year) && max(df$year) < max_year) {
    projected_years <- df_sample %>%
      ungroup %>%
      filter(year >= max(year) - (years_to_avg - 1)) %>%
      select(measure, prev_year_m, prop_decrease) %>%
      summarise_all(.funs = mean) %>%
      mutate(key = 1) %>%
      full_join(tibble(key = 1, year = (max(df_sample$year) + 1):max_year), by = "key") %>%
      select(year, everything()) %>%
      select(-key)

    df_sample <- df_sample %>%
      bind_rows(projected_years)
  }

  if(!is.null(min_year) && min(df$year) > min_year) {
    projected_years <- df_sample %>%
      ungroup %>%
      filter(year >= min(year) + (years_to_avg - 1)) %>%
      select(measure, prev_year_m, prop_decrease) %>%
      summarise_all(.funs = mean) %>%
      mutate(key = 1) %>%
      full_join(tibble(key = 1, year = min_year:(min(df_sample$year) - 1)), by = "key") %>%
      select(year, everything()) %>%
      select(-key)

    df_sample <- df_sample %>%
      bind_rows(projected_years)
  }

  df_sample <- df_sample %>%
    mutate(prop_decrease_sd = map2_dbl(measure, prev_year_m, prop_sd)) %>%
    mutate(prop_decrease_sample =  map2(prop_decrease, prop_decrease_sd, function(x, y) {
      tibble(sample = 1:samples, prop_decrease_sample = rnorm(samples, mean = x, sd = y))})) %>%
    select(-measure, -prop_decrease, -prev_year_m, -prop_decrease_sd) %>%
    unnest
  return(df_sample)
}



