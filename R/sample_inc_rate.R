
#' Function to Sample from the Underlying Distribution of an Incidence Rate
#'
#' @description The number of cases are back calculated using the incidence rate and
#' the size of the population. Samples are then taken of notifications, assuming a poisson
#' distribution. Incidence rates are then calculated based on these samples. Multiple samples
#' of a matrix input may be taken with the output formated as a list.
#' @param inc_rate Incidence rates formated as \code{\link[AssessBCGPolicyChange]{sutherland_incidence_rates}}.
#' @param pop Population estimates for each cohort as a vector.
#' @param inc_per The scaling factor for the incidence rate, defaults to 100,000.
#' @param samples Numeric, the number of samples to take. Defaults to 100.
#'
#' @return Incidence rate samples as a list, formated as a 3 by 3 matrix.
#' @export
#'
#' @importFrom purrr map_dbl map
#' @examples
#'
#' sample_inc_rate(Unvaccinated, sutherland_data$BCG.Unvaccinated[1:3], samples = 5)
#'
sample_inc_rate <- function(inc_rate, pop, inc_per = 100000, samples = 100) {

    nots <- inc_rate * pop / inc_per
    nots_flat <- as.vector(nots)
    pop <- as.vector(pop)

    sample_nots <- map(1:samples, function(i){
      sample_nots <- purrr::map_dbl(nots_flat, ~rpois(n = 1, lambda = .))
      sample_inc_rate <- sample_nots / pop * inc_per

      sample_inc_rate <- matrix(sample_inc_rate, 3, 3)
      colnames(sample_inc_rate) <- colnames(inc_rate)
      rownames(sample_inc_rate) <- rownames(inc_rate)


      return(sample_inc_rate)
    })

    return(sample_nots)
}

