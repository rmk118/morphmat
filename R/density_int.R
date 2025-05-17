#' Distribution interval derived (DID) cutline method
#'
#' @description Implementation of the method described by Richar & Foy (2022) (DOI:
#'   10.1139/facets-2021-0061).
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (or integer or double vector) containing
#'   measurements for the x-axis variable (e.g., carapace width).
#' @param yvar Name of column (or integer or double vector) containing
#'   measurements for the y-axis variable (e.g., claw height).
#' @param log Boolean; should both variables be log-transformed before
#'   performing the regression? Defaults to FALSE.
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same scale as the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same scale of the data. Defaults to the 20th percentile of
#'   the x-variable.
#' @param int_num Integer; how many intervals between the lower and upper bound
#'   should be used? Defaults to 25. With fewer intervals, each interval will
#'   contain more points, increasing the accuracy of the estimated density
#'   minimum for a given interval. However, the linear regression of the minima
#'   distributions (the divisions between immature and mature individuals within
#'   an interval) against the midpoints of those intervals may be more reliable
#'   with more intervals.
#' @param plot Boolean; should a plot of the data with the calculated minima and
#'   discriminating line be displayed?
#' @param adjust the bandwidth used for the kernel density estimate is actually
#'   adjust*bw. This makes it easy to specify values like ‘half the default’
#'   bandwidth.
#'
#' @returns Something
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n = 1000, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' density_int(dat = fc, xvar = "x", yvar = "y", upper = 120)
#' density_int(dat = fc, xvar = "x", yvar = "y", upper = log(120), log = TRUE)
density_int <- density_int <- function(dat,
                                       xvar,
                                       yvar,
                                       lower = NULL,
                                       upper = NULL,
                                       int_num = 25,
                                       log = FALSE,
                                       plot = FALSE,
                                       adjust = 1) {
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

  int_width <- (upper - lower) / int_num

  i <- 1
  int_bottom <- lower
  df_ints <- data.frame(
    int_bottom = rep(NA, int_num),
    int_top = rep(NA, int_num),
    min = rep(NA, int_num),
    n_obs = rep(NA, int_num)
  )

  ##### BEGIN LOOP
  while (i < int_num + 1) {
    int_top_temp <- int_bottom + int_width
    temp_df <- dat %>% filter(xvar >= int_bottom, xvar <= int_top_temp)
    n_obs <- nrow(temp_df)

    if (n_obs < 5) {
      abort(
        paste(
          "Each interval must contain at least 5 data points. The interval from",
          round(int_bottom, 3),
          "to",
          round(int_top_temp, 3),
          "only contains",
          n_obs,
          "points.",
          sep = " "
        )
      )
    }

    df_ints$int_bottom[i] <- int_bottom
    df_ints$int_top[i] <- int_top_temp
    df_ints$n_obs[i] <- n_obs

    # compute a kernel density estimate
    density_test <- stats::density(temp_df$yvar, adjust = adjust)

    # convert into a data frame
    density_test <- data.frame(x = density_test$x, density = density_test$y)

    span <- 5

    # find the local maxima - should be two modes
    density_test$is_max <- splus2R::peaks(x = density_test$density,
                                          span = span,
                                          strict = TRUE)
    modes <- density_test %>%
      dplyr::filter(.data$is_max == TRUE) %>%
      dplyr::pull(x)


    while(length(modes) > 2) {
      span <- span + 2

      density_test$is_max <- splus2R::peaks(x = density_test$density,
                                            span = span,
                                            strict = TRUE)
      modes <- density_test %>%
        dplyr::filter(.data$is_max == TRUE) %>%
        dplyr::pull(x)

    }

    if(length(modes) < 2) {
      #   int_num <- int_num - 1
      #   int_width <- (upper - lower) / int_num
      #   i <- 1
      #   int_bottom <- lower
      #   df_ints <- data.frame(
      #     int_bottom = rep(NA, int_num),
      #     int_top = rep(NA, int_num),
      #     min = rep(NA, int_num),
      #     n_obs = rep(NA, int_num)
      #   )
      abort(
        paste(
          "Each interval should contain two peaks in the density of points along the y-axis. The interval from",
          round(int_bottom, 3),
          "to",
          round(int_top_temp, 3),
          "only contains",
          length(modes),
          "peaks. Try decreasing the int_num argument or changing the adjust argument, which is a multiplier for the smoothing bandwidth.",
          sep = " "
        )
      )
    }
    # else {
    between_modes <- density_test %>% filter(x > modes[1], x < modes[2])

    interval_min <- between_modes[which.min(between_modes$density), "x"]

    df_ints$min[i] <- interval_min

    int_bottom <- int_top_temp
    i <- i + 1
    # }
  } # end loop

  df_ints$midpt <- (df_ints$int_bottom + df_ints$int_top) / 2

  # optionally visualize the data with the discriminant line
  if (plot == TRUE) {
    if (log == TRUE) {
      xlab <- paste0("ln(", xvar, ")")
      ylab <- paste0("ln(", yvar, ")")
    }
    else {
      xlab <- xvar
      ylab <- yvar
    }

    lm_density <- stats::lm(min ~ midpt, data = df_ints)
    pred_line <- data.frame(x = dat$xvar,
                            y = stats::predict(lm_density, data.frame(midpt = dat$xvar)))
    print(
      ggplot2::ggplot() +
        ggplot2::geom_point(data = dat, aes(x = xvar, y = yvar)) +
        ggplot2::geom_point(
          data = na.omit(df_ints),
          aes(x = midpt, y = min),
          color = "red"
        ) +
        ggplot2::geom_line(data = pred_line, aes(x, y)) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::theme_light()
    )
  }

  return(df_ints)
}
