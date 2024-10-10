#' Broken-stick (segmented) approaches to estimating SM50
#'
#' A wrapper function allowing for multiple methods of broken-stick regression to be applied using a standard format for inputs. See `vignette("broken-stick")` for more information.
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param method Method to use for the regression. A single string or string
#'   vector containing one or more of c("segmented", "chngpt", "regrans",
#'   "stevens"), or "all" to return the results of all methods for comparison.
#' @param verbose Boolean; Should the standard error, confidence intervals, etc.
#'   be returned, or just the estimate of SM50?
#' @param ci Integer; type of confidence intervals to return for SM50, defaults
#'   to 95%.
#' @param trans Transformation to be applied to the data before performing the
#'   regression: "none", "log" (both variables are log-transformed), or "std"
#'   (both variables are standardized = scaled and centered). If no string is
#'   provided, no transformation is performed (i.e., the default is "none").
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same trans of the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same trans of the data. Defaults to the 20th percentile of
#'   the x-variable.
#'
#' @returns If verbose is FALSE (the default), an estimate of SM50 from the
#'   specified method(s). Otherwise, output is a list that also includes the
#'   standard error and confidence intervals for the SM50 estimate as well as
#'   the model object(s) and/or regression parameters for the lines in the
#'   broken-stick models.
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n=100, L50=100, allo_params=c(1, 0.2, 1.1, 0.2))
#' broken_stick(fc, xvar="x", yvar="y", method=c("segmented", "chngpt"))
broken_stick <- function(dat,
                         xvar,
                         yvar,
                         verbose = FALSE,
                         ci = 95,
                         lower = NULL,
                         upper = NULL,
                         trans = "none",
                         method = c("segmented", "chngpt", "regrans", "stevens", "all")) {
  out <- c()

  if (trans == "log") {
    dat$xvar <- log(dat[[xvar]])
    dat$yvar <- log(dat[[yvar]])
  }
  else if (trans == "std") {
    dat$xvar <- scale(dat[[xvar]])
    dat$xvar <- scale(dat[[yvar]])
  }
  else {
    dat$xvar <- dat[[xvar]]
    dat$yvar <- dat[[yvar]]
  }

  if (is.null(lower)) {
    lower <- stats::quantile(dat$xvar, 0.2)
  }

  if (is.null(upper)) {
    upper <- stats::quantile(dat$xvar, 0.8)
  }

  if ("chngpt" %in% method | "all" %in% method) {

    temp <- chngpt::chngptm(
      formula.1 = yvar ~ 1,
      formula.2 = ~ xvar,
      family = "gaussian",
      data = dat,
      type = "segmented",
      var.type = "default",
      weights = NULL,
      lb.quantile = 0.2,
      ub.quantile = 0.8,
    )$chngpt

    if (!("all" %in% method) & length(method)==1) {
      out <- temp
    }
    else {
      out <- append(out, c(chngpt=temp))
    }
  }

  if ("segmented" %in% method | "all" %in% method) {
    temp_lm <- stats::lm(yvar~xvar, data = dat)
    seg_lm <- segmented::segmented(temp_lm)$psi[2]
    out <- append(out, c(segmented=seg_lm))
  }
  if ("regrans" %in% method | "all" %in% method) {
    temp  <- regrans_fun(dat, xvar, yvar, verbose = FALSE)
    if (!("all" %in% method) & length(method)==1) {
      out <- temp
    }
    else {
      out <- append(out, c(REGRANS = temp))
    }

  }
  if ("stevens" %in% method | "all" %in% method) {
    temp <- broken_stick_stevens(dat,
                                 xvar = xvar,
                                 yvar = yvar,
                                 verbose = FALSE)
    if (!("all" %in% method) & length(method)==1) {
      out <- temp
    }
    else {
      out <- append(out, c(Stevens = temp))
    }
  }

  return(out)
}
