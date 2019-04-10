
#' Samples the Percentage Annual Decrease Based On Notifications
#'
#' @description This function samples the percentage annual decrease, using TB notifications.
#' The percentage annual change is first estimated using \code{prop.test}, along with the 95% confidence
#' intervals. Assuming that the percentage annual decrease is normally distributed multiple samples are then
#' taken. This is returned as a tibble, containing the nested dataframe and a nested matrix ready to be
#' used by \code{\link[AssessBCGPolicyChange]{sutherland_model}}.
#' @param notifications A dataframe of notifications, it must contain a year variable, and the second variable
#' must be the number of notifications. If not specified defaults to \code{\link[AssessBCGPolicyChange]{nots_ew}}.
#' @param samples Numeric, the number of samples to take. Defaults to 100.
#' @return A nested tibble, containing the sample number, and both a nested dataframe and nested matrix of
#' annual percentage changes.
#' @seealso sample_per_decrease
#' @inheritParams TB_decrease_as_matrix
#' @inheritParams sample_per_decrease
#' @export
#' @import magrittr
#' @importFrom dplyr filter mutate left_join select group_by ungroup
#' @importFrom tibble tibble
#' @importFrom tidyr spread nest
#' @importFrom purrr map
#' @examples
#'
#' sample_annual_decrease_nots()
sample_annual_decrease_nots <- function(notifications = NULL,
                                        TB_cohorts = c("15-19 years", "20-24 years", "25-29 years"),
                                        samples = 100, max_year = NULL, years_to_avg = 3) {
  if(is.null(notifications)) {
    nots <- nots_ew
  }else{
    nots <- notifications
  }

  ## Sample percentage decrease of TB
  nots <- nots_ew %>%
    filter(year >= 1968) %>%
    sample_per_decrease(samples = samples, max_year = max_year,
                        years_to_avg = years_to_avg)

  ## Bind percentage decrease to age groups
  age_groups <- tibble(age_groups = TB_cohorts, key = 1)

  nots_by_age <- nots %>%
    mutate(key = 1) %>%
    left_join(age_groups, by = "key") %>%
    select(year, sample, age_groups, prop_decrease_sample) %>%
    spread(key = "age_groups", value = "prop_decrease_sample")


    nots_by_age <- nots_by_age %>%
      group_by(sample) %>%
      nest(.key = "per_decrease_df") %>%
      ungroup %>%
      mutate(per_decrease_mat = map(per_decrease_df, function(x) {
        dm_x <- data.matrix(x[,-1])
        row.names(dm_x) <-  unlist(x[,1])
        return(dm_x)
      }))

    return(nots_by_age)
}

