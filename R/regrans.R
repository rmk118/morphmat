#' REGRANS broken-stick regression method
#'
#' @param x Integer or double vector of measurements for the x-axis variable
#'   (e.g., carapace width).
#' @param y Integer or double vector of measurements for the y-axis variable
#'   (e.g., claw height).
#' @param min_changept Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the minimum value of the x-variable.
#' @param max_changept Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the maximum value of
#'   the x-variable.
#' @param n_tries Number of breakpoints to test within the unknown range.
#'
#' @return Data frame with breakpoints tested and their sum of squares
#' @export
#'
#' @examples
#' fc <- fake_crabs(n=100, allo_params=c(1, 0.2, 1.1, 0.2))
#' regrans_fun(fc$x, fc$y)
regrans_fun <- function(x,
                        y,
                        min_changept = min(x, na.rm = TRUE),
                        max_changept = max(x, na.rm = TRUE),
                        n_tries = 100) {

  changept_choices <- seq(min_changept, max_changept, l = n_tries)

  help_fun <- function(i)
  {
    x2star <- (x - i) * as.numeric(x > i)
    fit <- stats::lm(y ~ x + x2star)
    sum_sq <- stats::anova(fit)["Residuals", "Sum Sq"]
    return(sum_sq)
  }

  breakpt <- sapply(changept_choices,help_fun)

  breakpt <- data.frame(changept = changept_choices, sum_sq = breakpt)

  return(breakpt)
}
