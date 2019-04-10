#' Estimate Transmission chain parameters using Sutherland et al.s Approach
#'
#' @param Sutherland Logical, defaults to \code{TRUE}. Whether or not to use Sutherland et al.'s
#' assumption for estimating the basic reproduction number.
#' @param Repro.No If \code{Sutherland} is \code{FALSE} sets the basic reproduction number used to estimate
#' the transmission chain parameters.
#' @return A dlist containing the expected total secondary notifications (Expected.Total.Sec.Note),
#' the size of the first generation (Size.First.Gen) and the average interval to all secondary cases (Avg.Int.All.Sec).
#' @export
#' @inheritParams sutherland_model
#' @examples
#'
#'sutherland_trans_params()
#'
sutherland_trans_params <- function(Annual.TB.Decrease.Yearly = TB_decrease_as_matrix(sutherland_TB_decrease), Sym.Lag = sutherland_gen_time,
                                    Sutherland = TRUE, Repro.No = NULL,
                                    Cohort.Length = 5)
{

  ################################## Calculate parameters for secondary notifcations model
  ################ Expected Total no. notifications
  if (Sutherland) {
    Expected.Total.Sec.Note <- (1 - rowMeans(Annual.TB.Decrease.Yearly))^Sym.Lag
  }else {
    Expected.Total.Sec.Note <- rep(Repro.No, nrow(Annual.TB.Decrease.Yearly))
    names(Expected.Total.Sec.Note) <- rownames(Annual.TB.Decrease.Yearly)
  }


  ############### Size of first Gen
  Size.First.Gen <- Expected.Total.Sec.Note / (Expected.Total.Sec.Note + 1)
  names(Size.First.Gen) <- rownames(Annual.TB.Decrease.Yearly)
  ############### average interval to all secondary notifications

  Avg.Int.All.Sec <- Sym.Lag/(1 - Size.First.Gen)
  names(Avg.Int.All.Sec) <- rownames(Annual.TB.Decrease.Yearly)

  out <- list(Expected.Total.Sec.Note, Size.First.Gen, Avg.Int.All.Sec)
  names(out) <- c("Expected.Total.Sec.Note", "Size.First.Gen", "Avg.Int.All.Sec")
  return(out)
}
