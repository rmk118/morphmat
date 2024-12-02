#' Two-line logistic model
#'
#' @description Fits a nonlinear model made up of two distinct line segments
#'   connected by a logistic curve.
#'
#' @details This relies on `minpack.lm::nlsLM()`, which is often able to
#'   converge when the default `stats::nls()` function cannot find a solution.
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param imm_int Starting value for the immature intercept parameter when
#'   fitting the NLS model
#' @param imm_slope tarting value for the immature slope parameter when fitting
#'   the NLS model
#' @param mat_int Starting value for the mature intercept parameter when fitting
#'   the NLS model
#' @param mat_slope Starting value for the mature slope parameter when fitting
#'   the NLS model
#' @param SM50_start Starting value for SM50 parameter when fitting the NLS
#'   model. If not provided, taken to be the median of the x-variable
#' @param alpha_start Starting value for the logistic slope parameter when
#'   fitting the NLS model
#' @param log Boolean; should both variables be log-transformed before performing the
#'   regression? Defaults to FALSE.
#' @param verbose Should additional output be returned besides the SM50
#'   estimate?
#'
#' @returns If verbose is FALSE (the default), an estimate of SM50. Otherwise,
#'   output is the NLS model object.
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(
#'   error_scale = 17,
#'   slope = 9,
#'   L50 = 75,
#'   n = 800,
#'   allo_params = c(0.9, 0.25, 1.05, 0.2),
#'   x_mean = 85
#' )
#' two_line_logistic(fc, "x", "y", verbose = FALSE)
two_line_logistic <- function(dat,
                              xvar,
                              yvar,
                              imm_int = 1,
                              imm_slope = 0.2,
                              mat_int = 4,
                              mat_slope = 0.3,
                              SM50_start = NULL,
                              alpha_start = 9,
                              log = FALSE,
                              verbose = FALSE) {

  if (isTRUE(log)) {
    tll_dat <- data.frame(xvar = log(dat[[xvar]]), yvar = log(dat[[yvar]]))
  }
  else {
    tll_dat <- data.frame(xvar = dat[[xvar]], yvar = dat[[yvar]])
  }

  if (is.null(SM50_start)) {
    SM50_start <- stats::median(tll_dat$xvar)
  }

  TLL_fun <- function(xvar,
                      int1,
                      slope1,
                      int2,
                      slope2,
                      SM50,
                      alpha) {
    (int1 + slope1 * xvar) * (1 - (1 / (1 + exp(
      -(xvar - SM50) / alpha
    )))) + (int2 + slope2 * xvar) * (1 / (1 + exp(-(xvar - SM50) / alpha)))
  }

  nls_out <- minpack.lm::nlsLM(
    formula =  yvar ~ TLL_fun(xvar, int1, slope1, int2, slope2, SM50, alpha),
    data = tll_dat,
    start = list(
      int1 = imm_int,
      slope1 = imm_slope,
      int2 = mat_int,
      slope2 = mat_slope,
      SM50 = SM50_start,
      alpha = alpha_start
    ),
    lower = c(-Inf, 0, -Inf, 0, 0, 0),
    control = minpack.lm::nls.lm.control(maxiter = 500, maxfev = 10000)
  )

  if (verbose == TRUE) {
    return(nls_out)
  }
  else
    return(stats::coef(nls_out)["SM50"])
}

