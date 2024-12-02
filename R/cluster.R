#' Classification approaches to estimating SM50
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param log Boolean; should both variables be log-transformed before performing the
#'   regression? Defaults to FALSE.
#' @param method Classification method to use. A single string or vector
#'   containing one or more of c("mclust", "Somerton", "kmeans", "hclust", "infl_pt"), or "all" to return the results of all methods for comparison.
#' @returns An estimate of SM50 from the specified method(s).
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' cluster_mods(fc, xvar = "x", yvar = "y", method = c("kmeans"))
cluster_mods <- function(dat,
                         xvar,
                         yvar,
                         log = FALSE,
                         method = c("mclust", "Somerton", "kmeans", "hclust", "infl_pt", "all")) {

  new_dat <- data.frame(xvar = dat[[xvar]], yvar = dat[[yvar]])

  if (isTRUE(log)) {
    new_dat$xvar <- log(dat[[xvar]])
    new_dat$yvar <- log(dat[[yvar]])
  }

  if ("kmeans" %in% method | "all" %in% method) {

    out <- new_dat

}

  return(out)

}
