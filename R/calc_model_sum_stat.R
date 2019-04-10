
#' Function to Calculate Summary Measures from Multiple Runs of the Sutherland et al.'s Model
#'
#' @param x A list of multiple simulation runs of a singl table as produced by \code{\link{AssessBCGPolicyChange}}.
#' @param ci_level Numeric, specifies the confidence level to use. Defaults to 0.95.
#' @param ci_space Character, string to use to seperate confidence intervals, defaults to "-"
#' @param round_level Integer, the number of digits to which to round the summarised results. Defaults to 0
#'
#' @return A list of tables for a set of summary measures. These are; mean with CI, median, 2.5% quantile,
#' 97.5% quantile, 25%  quantile, 75% quantile, mean, lower CI, and upper CI.
#' @export
#'
#' @import magrittr
#' @importFrom purrr map
#' @examples
#'
#'## Run multiple model simulations
#'model_sims <- simulate_sutherland_model(cores = 1)
#'
#'## Summarise the Total effects of ending the scheme table
#'calc_model_sum_stat(model_sims[["Table 5 - Total Effects of ending the schools BCG scheme"]])

calc_model_sum_stat <- function(x, ci_level = 0.95, ci_space = "-", round_level = 0) {

  CI <- function(x,ci = .95) {
    a<-mean(x)
    s<-sd(x)
    n<-length(x)
    error<-qt(ci+(1-ci)/2,df=n-1)*s/sqrt(n)
      return(c(upper = a + error,mean = a,lower = a - error))
    }

  ## convert objects to matrices
  if(!is.matrix(x[[1]]))
  {
    x <- x %>% map(as.matrix)
  }

  ##Preallocate
  sim_results_mean_CI <- sim_results_median <- sim_results_mean <- sim_results_lower <- sim_results_upper <- matrix(NA, nrow=nrow(x[[1]]), ncol=ncol(x[[1]]))
  sim_results_med_IQR <- sim_results_2.5 <- sim_results_97.5 <- sim_results_25 <- sim_results_75 <- sim_results_mean_CI
  samples <- length(x)

  for(i in 1:nrow(x[[1]]))
  {
    for(j in 1:ncol(x[[1]]))
    {
      tmp <- vector(length=samples)
      for(k in 1:samples)
      {
        tmp[k] <- x[[k]][i,j]

      }
      #Drop NAN and NA values
      tmp <- tmp[complete.cases(tmp)]
      tmp2 <- CI(tmp, ci=ci_level) %>% round(digits=round_level)
      sim_results_mean_CI[i,j] <- ifelse(tmp2[2]==tmp2[3] && tmp2[2]==tmp2[1], paste0(tmp2[2]), paste0(tmp2[2], ' (', tmp2[3], ci_space, tmp2[1], ')'))
      sim_results_median[i,j] <- median(tmp)
      sim_results_2.5[i,j] <- quantile(tmp, probs = c(0.025))
      sim_results_97.5[i,j] <- quantile(tmp, probs = c(0.975))
      sim_results_25[i,j] <- quantile(tmp, probs = c(0.25))
      sim_results_75[i,j] <- quantile(tmp, probs = c(0.75))
      sim_results_mean[i,j] <- mean(tmp) %>% round(digits=round_level)
      sim_results_lower[i,j] <-  tmp2[3]
      sim_results_upper[i,j] <-  tmp2[1]
    }
  }
  out <- list(sim_results_mean_CI,sim_results_median, sim_results_2.5, sim_results_97.5, sim_results_25, sim_results_75, sim_results_mean, sim_results_lower, sim_results_upper)
  out <- lapply(out, function(y){
    colnames(y) <- colnames(x[[1]])
    rownames(y) <- rownames(x[[1]])
    return(y)
  })
  names(out) <- c('main', 'median', '2.5', '97.5', '25', '75', 'mean', 'lower', 'upper')
  return(out)
}
