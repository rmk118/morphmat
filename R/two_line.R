#' Two-line regression method for estimating size at maturity
#'
#' @description Fits separate linear models for the allometric growth of
#'   immature and mature individuals. Code adapted from Dr. Bradley Stevens at
#'   the University of Maryland Eastern Shore.
#'
#' @details The optimal breakpoint between lines is found by minimizing the
#'   residual sum of squares when iterating over (1) num_bps evenly-spaced
#'   values within the possible range or (2) all values of the x-axis variable
#'   present in the unknown range.
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
#' @param verbose Should additional output be returned besides the SM50
#'   estimate?
#' @param bps Should the values tested as possible breakpoints be restricted to
#'   only observed values of the x-variable ("obs"), or should it be a specified
#'   number of evenly-spaced values between the lower and upper limits of the
#'   unknown region ("even", the default)
#' @param log Boolean; should both variables be log-transformed before performing the
#'   regression? Defaults to FALSE.
#' @param num_bps When `bps = "even"`, how many values should be tested as
#'   possible endpoints? Defaults to 100, but should be increased.
#'
#' @returns If verbose is FALSE (the default), two possible estimates of SM50:
#'   the breakpoint x-value marking the transition between immature and mature
#'   points/lines, and the intersection point where the two lines cross. The
#'   intersection value will typically be extremely unrealistic unless the
#'   slopes of the lines are drastically different. If verbose is TRUE, output
#'   is a list that also includes the original data with a column representing
#'   which line (immature or mature) the point was assigned to, the immature and
#'   mature slope and intercept parameters, and the intersection point of the
#'   two lines.
#' @export
#'
#' @examples
#' #' set.seed(12)
#' fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' two_line(fc, xvar = "x", yvar = "y", verbose = FALSE)
#'
#' @seealso [two_line_logistic()] for an alternative two-line model with a
#'   logistic transition between the left and right segments and
#'   [piecewise_mods()] for segmented/piecewise regression methods.
#'
two_line <- function(dat,
                     xvar,
                     yvar,
                     lower = NULL,
                     upper = NULL,
                     verbose = FALSE,
                     bps = "even",
                     log = FALSE,
                     num_bps = 100) {


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

  lm0 <- stats::lm(yvar ~ xvar, data = new_dat)
  rss0 <- stats::anova(lm0)[[2, 2]] # residual sum of squares
  ms0 <- stats::anova(lm0)[[3]] # mean squared error
  F0 <- ms0[1] / ms0[2] # F value
  n0 <- dim(new_dat)[1]
  rss_min <- rss0
  mse0 <- mean(lm0$residuals ^ 2)

  # assign group membership
  # 1 = left line, 2= right line
  memb <- rep(1, nrow(new_dat))
  memb_low <- (xraw <= min_x) # T/F list if less than low range
  memb_high <- (yraw > min_y) # T/F list if GT than high range
  memb[memb_low] <- 1 # assign 1 to those < low
  memb[memb_high] <- 2 # assign 2 to those > high
  memb_sum1 <- summary(as.factor(memb))
  new_dat$group <- memb

  #### Loop

  if (bps == "obs") {
    mse <- rep(0, n0)

    for (i in 1:n0) {
      piecewise1 <- stats::lm(
        yvar ~ xvar * (xvar < xvar[i]) + xvar * (xvar >= xvar[i]),
        data = new_dat)
      mse[i] <- mean(piecewise1$residuals ^ 2)
    }

    ### find breakpoint (bp) that gives lowest MSE
    bp_ind <- which(mse == min(mse))
    bp <- new_dat$xvar[bp_ind] # this is not necessarily where the lines cross

  }

  if (bps == "even") {
    ## determine increment for loop
    steps <- seq(lower, upper, l = num_bps)

    #### Loop
    mse <- rep(0, num_bps)
    for (i in 1:num_bps) {
      piecewise1 <- stats::lm(yvar ~ xvar * (xvar < steps[i]) +
                         xvar * (xvar >= steps[i]), data = new_dat)
      mse[i] <- mean(piecewise1$residuals ^ 2)
    }

    ### find breakpoint (bp) that gives lowest MSE
    bp_ind <- which(mse == min(mse))
    bp <- steps[bp_ind] # this is not necessarily where the lines cross
  }

  if (length(bp) > 1) {
    bp <- stats::median(bp)
  }

  ## rerun piecewise regression at best bp
  piecewise2 <- stats::lm(yvar ~ xvar * (xvar < bp) + xvar * (xvar > bp),
                          data = new_dat)

  pw_vals <- stats::coef(piecewise2)
  pw_vals[which(is.na(pw_vals))] <- 0
  a_lo <- pw_vals[1] + pw_vals[3]
  b_lo <- pw_vals[2] + pw_vals[5]
  a_hi <- pw_vals[1] + pw_vals[4]
  b_hi <- pw_vals[2]

  jx <- as.numeric((a_lo - a_hi) / (b_hi - b_lo)) #the point where 2 lines meet

  ####  Reassign group membership
  memb_pw <- rep(1, n0)
  memb_pw[new_dat$xvar >= bp] <- 2
  new_dat$group <- memb_pw

  output <- list(
    data = new_dat,
    breakpoint = bp,
    intersection = jx,
    imm_slope = b_lo,
    imm_int = a_lo,
    mat_slope = b_hi,
    mat_int = a_hi
  )

  if (verbose == TRUE) {
    return(output)
  }
  else
    return(c(breakpoint = bp, intersection = jx))

}
