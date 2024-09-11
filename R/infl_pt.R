#' Inflection point ratio-based method
#'
#' @param x Integer or double vector of measurements for the x-axis
#'   variable (e.g., carapace width).
#' @param y Integer or double vector of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param plot Boolean; should a plot of the density curve with the identified
#'   minimum be created?
#'
#' @returns An integer corresponding to the ratio (y/x) at which the local
#'   minimum was found. This ratio serves as the discriminant line when
#'   separating observations by maturity status.
#' @export
#'
#' @examples
infl_pt_fun <- function(x, y, plot = FALSE) {
  ratio <- y / x # find the ratio between the two morphometric variables

  # compute a kernel density estimate (essentially a smoothed histogram)
  densityTest <- density(ratio)

  # convert into a data frame
  densityTest <- data.frame(x = densityTest$x, y = densityTest$y)

  # find the local minimum between the two peaks
  densityTest$is_min <- ggpmisc:::find_peaks(-densityTest$y, ignore_threshold = -0.01)
  min <- densityTest %>%
    dplyr::filter(is_min == TRUE) %>%
    dplyr::pull(x)
  min <- stats::median(min)

  if (is.na(min)) {
    densityTest$is_min <- ggpmisc:::find_peaks(-densityTest$y)
    min <- densityTest %>%
      dplyr::filter(is_min == TRUE) %>%
      dplyr::pull(x)
    min <- stats::median(min)
  }

  # optionally visualize the density plot with minimum
  if (plot == TRUE) {
    print(ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = densityTest$x, y = densityTest$y)) +
      ggplot2::geom_vline(xintercept = min, lty = "dashed") +
      labs(x = "Ratio", y = NULL) +
      ggplot2::theme_light())
  }

  # return the minimum ratio, equivalent to the slope of a line
  # separating the two clouds of points
  return(min)
}
