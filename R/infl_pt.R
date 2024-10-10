#' Maturity classification based on the minimum density of CH/CW ratios
#'
#' @description
#' `infl_pt()` can be used to classify individuals as immature or mature
#' when there is a clear ratio of the y-axis variable to the x-axis variable
#' that separates the immature and mature clusters.
#'
#' For example, this would be an effective classification method if the
#' transition to maturity of a population of Tanner crabs
#' (*Chionoecetes bairdi*) was evident by an increase in the
#' log(claw height)/log(carapace width) ratio from below 0.2 to above
#' 0.2. `infl_pt_fun()` finds this discriminating line by creating a kernel
#' density estimate (visually similar to a smoothed histogram) of the
#' y-var/x-var ratio for all points, then finding the local minimum separating
#' the two peaks representing the maturity clusters.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @param dat optional data frame or matrix containing the data
#' @param x Name of column (or integer or double vector) containing measurements
#'   for the x-axis variable (e.g., carapace width).
#' @param y Name of column (or integer or double vector) containing measurements
#'   for the y-axis variable (e.g., claw height).
#' @param plot Boolean; should a plot of the density curve with the identified
#'   minimum be created?
#'
#' @returns An integer corresponding to the ratio (y/x) at which the local
#'   minimum was found. This ratio serves as the discriminant line when
#'   separating observations by maturity status.
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(12)
#' x <- rnorm(100, mean = 2, sd = 3)
#' y <- rnorm(100, mean = 15, sd = 3)
#' z <- c(x, y)
#' hist(z)
#' dat1 <- data.frame(xvar=rep(1, 200),yvar=z)
#' infl_pt(dat1, "xvar", "yvar", TRUE)
#' fc <- fake_crustaceans(n=100, allo_params=c(1, 0.2, 1.1, 0.2))
#' infl_pt(fc, "x", "y", TRUE)
#'
infl_pt <- function(dat, x, y, plot = FALSE) {
  # find the ratio between the two morphometric variables
  ratio <- dat[[y]]/dat[[x]]

  # compute a kernel density estimate (essentially a smoothed histogram)
  density_test <- stats::density(ratio)

  # convert into a data frame
  density_test <- data.frame(x = density_test$x, y = density_test$y)

  # find the local minimum between the two peaks
  density_test$is_min <- splus2R::peaks(
    x = -density_test$y, span = 3, strict = FALSE)

  min <- density_test %>%
    dplyr::filter(.data$is_min == TRUE) %>%
    dplyr::pull(x)

  min <- stats::median(min)

  # optionally visualize the density plot with minimum
  if (plot == TRUE) {
    print(ggplot2::ggplot() +
            ggplot2::geom_line(aes(x = density_test$x, y = density_test$y)) +
            ggplot2::geom_vline(xintercept = min, lty = "dashed") +
            labs(x = "Ratio", y = NULL) +
            ggplot2::theme_light())
  }

  # return the minimum ratio, equivalent to the slope of a line
  # separating the two clouds of points
  if (is.na(min)) {
    warning("No local minimum detected.")
  }

  return(min)
}
