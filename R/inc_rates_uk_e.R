#' Incidence Rates per 100,000 in the UK born in England
#'
#' A dataset containing the estimate incidence rates and associated variables for the UK born in England. Estimated using the Labour Force Survey and the Enhanced Tubercuolosis
#' Surveillance system. See [here](https://www.samabbott.co.uk/tbinenglanddataclean)  for details.
#' @format A dataframe with 51 rows and 6 variables
#'
#' \describe{
#'   \item{year}{The calendar year}
#'   \item{age_group}{Age groups used in the model, these are 15-19 years old, 20-24 years old and 25-29 years old.}
#'   \item{incidence}{The estimated incidence per 100,000 individuals}
#'   \item{lci}{The lower confidence interval \(95% CI\)}
#'   \item{uci}{The lower confidence interval \(95% CI\)}
#'   \item{sd}{The standard deviation of the estimated incidence rates}
#'   }
#'
#'
"inc_rates_uk_e"
