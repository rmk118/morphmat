#' Piecewise regression approaches to estimating SM50
#'
#' @description A wrapper function allowing for multiple methods of piecewise regression to
#' be applied using a standard format for inputs. See `vignette("broken-stick")`
#' and `vignette("two-line")` for more information.
#'
#' @details The `two_line_logistic()` function is closely related but not included in this wrapper function because you will generally want more control over the initial values of the parameters and the nonlinear least-squares algorithm may not always converge.
#'
#' This function is primarily intended for easy comparison between the SM50 estimates produced by a variety of different piecewise regression models. For follow-up analyses, we recommend calling the specific function(s) of interest (`regrans()`, `broken_stick_stevens()`, `two_line()`, `segmented::segmented()`, or `chngpt::chngpt()`) and exploring how changing the upper and lower bounds for possible SM50 values and the number of breakpoints to be tested may influence the resulting estimates.
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param method Method to use for the regression. A single string or vector
#'   containing one or more of c("segmented", "chngpt", "regrans", "stevens",
#'   "TL"), or "all" to return the results of all methods for comparison.
#' @param log Boolean; should both variables be log-transformed before performing the
#'   regression? Defaults to FALSE.
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same scale as the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 20th percentile of
#'   the x-variable.
#'
#' @returns An estimate of SM50 from the specified method(s).
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' piecewise_mods(fc, xvar = "x", yvar = "y", method = c("segmented", "chngpt"))
piecewise_mods <- function(dat,
                           xvar,
                           yvar,
                           lower = NULL,
                           upper = NULL,
                           log = FALSE,
                           method = c("segmented", "chngpt", "regrans", "stevens", "TL", "all")) {
  dat <- dat %>% dplyr::arrange(.data[[xvar]])
  out <- c()

  if (isTRUE(log)) {
    dat$xvar <- log(dat[[xvar]])
    dat$yvar <- log(dat[[yvar]])
  }
  else {
    dat$xvar <- dat[[xvar]]
    dat$yvar <- dat[[yvar]]
  }

  if (is.null(lower)) {
    lower <- stats::quantile(dat$xvar, 0.2, names = FALSE)
  }

  if (is.null(upper)) {
    upper <- stats::quantile(dat$xvar, 0.8, names = FALSE)
  }

  if ("chngpt" %in% method | "all" %in% method) {

   temp <- chngpt::chngptm(
     formula.1 = yvar~1,
     formula.2 = ~xvar,
     family = "gaussian",
     data = dat,
     type = "segmented",
     var.type = "default",
     weights = NULL,
     lb.quantile = stats::ecdf(dat$xvar)(lower),
     ub.quantile = stats::ecdf(dat$xvar)(upper),
   )$chngpt

   if (!("all" %in% method) & length(method) == 1) {
     out <- temp
   }
   else {
     out <- append(out, c(chngpt = temp))
   }
 }

  if ("segmented" %in% method | "all" %in% method) {
    temp_lm <- stats::lm(yvar ~ xvar, data = dat)
    seg_lm <- segmented::segmented(temp_lm)$psi[2]

    if (!("all" %in% method) & length(method) == 1) {
      out <- seg_lm
    }
    else {
      out <- append(out, c(segmented = seg_lm))
    }
  }

  if ("regrans" %in% method | "all" %in% method) {
    temp <- regrans(
      dat,
      xvar = "xvar",
      yvar = "yvar",
      lower = lower,
      upper = upper,
      verbose = FALSE)

    if (!("all" %in% method) & length(method) == 1) {
      out <- temp
    }
    else {
      out <- append(out, c(REGRANS = temp))
    }
  }

  if ("stevens" %in% method | "all" %in% method) {

    temp <- broken_stick_stevens(
      dat,
      xvar = "xvar",
      yvar = "yvar",
      lower = lower,
      upper = upper,
      verbose = FALSE
    )
    if (!("all" %in% method) & length(method) == 1) {
      out <- temp
    }
    else {
      out <- append(out, c(Stevens = temp))
    }
  }

  if ("TL" %in% method | "all" %in% method) {
    temp <- two_line(
      dat,
      xvar = "xvar",
      yvar = "yvar",
      lower = lower,
      upper = upper,
      verbose = FALSE
    )
    if (!("all" %in% method) & length(method) == 1) {
      out <- temp
    }
    else {
      out <- append(out, c(Two_line = temp))
    }
  }

  return(out)
}
