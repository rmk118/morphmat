#' Broken-stick method from Bradley Stevens
#'
#' @description Fits a broken-stick model to estimate size at maturity. Code
#'   adapted from Dr. Bradley Stevens at the University of Maryland Eastern
#'   Shore. Differs from the broken-stick methods implemented in `regrans()`,
#'   `chngpt::chngpt()`, `segmented::segmented()`, `SiZer::piecewise.linear()`,
#'   etc. in that only values of the x-axis variable present in the data are
#'   tested as possible SM50 values.
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 20th percentile of
#'   the x-variable.
#' @param log Boolean; should both variables be log-transformed before performing the
#'   regression? Defaults to FALSE.
#' @param verbose Should additional output be returned besides the SM50
#'   estimate?
#'
#' @returns If verbose is FALSE (the default), an estimate of SM50. Otherwise,
#'   output is a list that also includes the original data with a column
#'   representing which line (immature or mature) the point was assigned to, the
#'   immature amd mature slope and intercept parameters, and the F and p-values
#'   for the final piecewise model.
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' broken_stick_stevens(fc, xvar = "x", yvar = "y", verbose = FALSE)
broken_stick_stevens <- function(dat,
                                 xvar,
                                 yvar,
                                 lower = NULL,
                                 upper = NULL,
                                 log = FALSE,
                                 verbose = FALSE) {

  new_dat <- dat %>% dplyr::arrange(.data[[xvar]])

  if (isTRUE(log)) {
    xraw <- log(new_dat[[xvar]])
    yraw <- log(new_dat[[yvar]])
  }
  else {
    xraw <- new_dat[[xvar]]
    yraw <- new_dat[[yvar]]
  }

  if (is.null(lower)) {
    lower <- stats::quantile(xraw, 0.2, names = FALSE)
  }

  if (is.null(upper)) {
    upper <- stats::quantile(xraw, 0.8, names = FALSE)
  }

  left_x <- (xraw <= lower) # T/F vector
  low_ndx <- sum(left_x) # largest group 1 point
  right_x <- (xraw >= upper) # T/F vector
  high_ndx <- (length(xraw) - sum(right_x)) + 1 # smallest group 2 point
  min_x <- xraw[low_ndx] # lowest T value
  min_y <- yraw[low_ndx] # lowest T value

  new_dat$xvar <- xraw
  new_dat$yvar <- yraw

  # null model - single line to describe both maturity stages
  lm0 <- stats::lm(yvar ~ xvar, data = new_dat)
  rss0 <- stats::anova(lm0)[[2, 2]] # residual sum of squares
  ms0 <- stats::anova(lm0)[[3]] # mean squared error
  F0 <- ms0[1] / ms0[2] # F value
  n0 <- dim(new_dat)[1]
  rss_min <- rss0
  mse0 <- mean(lm0$residuals ^ 2)

  # assign group membership
  # 1 = left line, 2 = right line
  memb <- rep(1, nrow(new_dat))
  memb_low <- (xraw <= min_x) # T/F list if less than low range
  memb_high <- (yraw > min_y) # T/F list if GT than high range
  memb[memb_low] <- 1 # assign 1 to those < low
  memb[memb_high] <- 2 # assign 2 to those > high
  memb_sum1 <- summary(as.factor(memb))
  new_dat$prior <- memb
  new_dat$group <- memb

  run <- 0

  while (min_x < upper) {
    run <- run + 1

    # Left regression
    lm1 <- stats::lm(
      I(yvar[memb == 1] - min_y) ~ 0 + I(xvar[memb == 1] - min_x),
      data = new_dat
      )
    b1 <- stats::coef(lm1)[[1]]
    a1 <- min_y - (b1 * min_x)
    df1 <- stats::anova(lm1)[[1]]
    rss1 <- stats::anova(lm1)[[2, 2]]
    ms1 <- stats::anova(lm1)[[3]]

    # Right regression
    lm2 <- stats::lm(
      I(yvar[memb == 2] - min_y) ~ 0 + I(xvar[memb == 2] - min_x),
      data = new_dat
      )
    b2 <- stats::coef(lm2)[[1]]
    a2 <- min_y - (b2 * min_x)
    df2 <- stats::anova(lm2)[[1]]
    rss2 <- stats::anova(lm2)[[2, 2]]
    ms2 <- stats::anova(lm2)[[3]]

    # calculate combined RSS and F
    rss_pool <- rss1 + rss2 # add residual sum of squares
    ms_diff <- (rss0 - rss_pool) / 2
    ms_pool <- rss_pool / (n0 - 4)
    F2 <- ms_diff / ms_pool
    F2_p <- 1 - stats::pf(F2,
                          df1 = 2,
                          df = n0 - 4,
                          lower.tail = F)

    if (run == 1 |
        (rss_pool < rss_min)) {
      # Run 1 OR pooled RSS < minimum RSS so far
      rss_min <- rss_pool
      joint_x <- min_x
      joint_y <- min_y
      a1_1 <- a1 # reset old values
      a2_1 <- a2
      b1_1 <- b1
      b2_1 <- b2
    }

    # next point
    low_ndx <- low_ndx + 1
    min_x <- new_dat$xvar[low_ndx]
    min_y <- new_dat$yvar[low_ndx]
    memb_low <- new_dat$xvar <= min_x # T/F list if less than low range
    memb_high <- new_dat$xvar > min_x # T/F list if GT than high range
    memb[memb_low] <- 1 # assign 1 to those < low
    memb[memb_high] <- 2 # assign 2 to those > high
  } # end loop

  SM50 <- joint_x

  memb_low <- new_dat$xvar <= joint_x # T/F list if less than low range
  memb_high <- new_dat$xvar > joint_x # T/F list if GT than high range
  memb[memb_low] <- 1 # assign 1 to those < low
  memb[memb_high] <- 2 # assign 2 to those > high
  new_dat$group <- memb

  output <- list(
    data = new_dat %>% dplyr::select(-c("xvar", "yvar", "prior")),
    SM50 = SM50,
    imm_slope = b1_1,
    imm_int = a1_1,
    mat_slope = b2_1,
    mat_int = a2_1,
    F_val = F2,
    p_val = F2_p
  )
  if (verbose == TRUE) {
    return(output)
  }
  else
    return(SM50)


}
