#' REGRANS broken-stick regression method
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 20th percentile of
#'   the x-variable.
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param verbose Return all breakpoints tested and their sum of squares, or
#'   only the estimated SM50?
#' @param n_tries Number of breakpoints to test within the unknown range.
#'
#' @return If verbose = TRUE, a data frame with the breakpoints tested and
#'   their sum of squares. Otherwise, a single value for the breakpoint with the
#'   lowest sum of squares.
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n=100, L50=100, allo_params=c(1, 0.2, 1.1, 0.2))
#' regrans_fun(fc, "x", "y", verbose = FALSE)
#' head(regrans_fun(fc, "x", "y", verbose = TRUE), n=30)
regrans_fun <- function(dat,
                        xvar,
                        yvar,
                        lower = NULL,
                        upper = NULL,
                        verbose = FALSE,
                        n_tries = 100) {

  x <- dat[[xvar]]
  y <- dat[[yvar]]

  if (is.null(lower)) {
    lower <- stats::quantile(x, 0.2)
  }

  if (is.null(upper)) {
    upper <- stats::quantile(x, 0.8)
  }

  changept_choices <- seq(lower, upper, length.out = n_tries)

  help_fun <- function(i)
  {
    x2star <- (x - i) * as.numeric(x > i)
    fit <- stats::lm(y ~ x + x2star)
    sum_sq <- stats::anova(fit)["Residuals", "Sum Sq"]
    return(sum_sq)
  }

  breakpt <- sapply(changept_choices, help_fun)

  breakpt <- data.frame(changept = changept_choices, sum_sq = breakpt)

  if (verbose == TRUE) {
    return(breakpt)
  }
  else {
    out <- breakpt[which.min(breakpt$sum_sq), "changept"]
    return(out)
  }

}
