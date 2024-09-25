#' Broken-stick (segmented) approaches to estimating SM50
#'
#' A wrapper function allowing for multiple methods of broken-stick regression to be applied using a standard format for inputs. See `vignette("broken-stick")` for more information.
#'
#' @param dat data frame or matrix containing the data
#' @param x Integer or double vector of measurements for the x-axis variable
#'   (e.g., carapace width).
#' @param y Integer or double vector of measurements for the y-axis variable
#'   (e.g., claw height).
#' @param method Method to use for the regression. A single string or string
#'   vector containing one or more of c("segmented", "chngpt", "regrans",
#'   "stevens"), or "all" to return the results of all methods for comparison.
#' @param verbose Boolean; Should the standard error, confidence intervals,
#' @param ci Integer; type of confidence intervals to return for SM50, defaults
#'   to 95%.
#' @param scale Transformation to be applied to the data before performing the
#'   regression: "none", "log" (both variables are log-transformed), or "std"
#'   (both variables are standardized = scaled and centered). If no string is
#'   provided, no transformation is performed (i.e., the default is "none").
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 20th percentile of
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
#' broken_stick(iris, x="x", y="y", method=c("segmented", "chngpt"))
broken_stick <- function(dat,
                         x,
                         y,
                         verbose = FALSE,
                         ci = 95,
                         lower = stats::quantile(x, 0.2),
                         upper = stats::quantile(x, 0.8),
                         scale = c("log", "none", "std"),
                         method = c("segmented", "chngpt", "regrans", "stevens", "all")) {
  a <- c()

  if (missing(scale)) {
    scale <- "none"
  }

  if ("chngpt" %in% method) {
    a <- append(a, 1)
  }
  if ("segmented" %in% method) {
    a <- append(a, 2)
  }
  if ("regrans" %in% method) {
    a <- append(a, 3)
  }
  if ("stevens" %in% method) {
    a <- append(a, 4)
  }
  else if ("all" %in% method) {
    a <- append(a, 1:4)
  }
  return(a)
}
