#' Somerton method
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x -
#'   axis variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#'   variable (e.g., claw height).
#' @param upper Integer or double; the upper bound for possible SM50 values.
#'   Must be on the same trans of the data. Defaults to the 80th percentile of
#'   the x-variable.
#' @param lower Integer or double; the lower bound for possible SM50 values.
#'   Must be on the same trans of the data. Defaults to the 20th percentile of
#'   the x-variable.
#' @param trans Transformation to be applied to the data before performing the
#'   regression: "none", "log" (both variables are log-transformed), or "std"
#'   (both variables are standardized = scaled and centered). If no string is
#'   provided, no transformation is performed (i.e., the default is "none").
#' @param max_iter Maximum number of iterations
#'
#' @returns Output is a list that also includes the input data frame with a
#'   column specifying which maturity group each point was assigned to, vectors
#'   of the R-squared and residual sum of squares for each iteration, and the
#'   linear model objects corresponding to each maturity group.
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' out_df <- somerton_fun(fc, xvar = "x", yvar = "y")[[1]]
#' mod <- glm(data = out_df, pred_mat_num ~ x, family = binomial(link = "logit"))
#' unname(-coef(mod)[1] / coef(mod)[2])
somerton_fun <- function(
    dat, # data.frame with columns corresponding to xvar, yvar
    xvar, # X variable
    yvar, # Y variable
    trans = "none", # transformation to apply
    lower = NULL, # lower bound of unknown range
    upper = NULL, # upper bound of unknown range
    max_iter = 50 # maximum number of iterations
) {

  if (is.null(lower)) {
    lower <- stats::quantile(dat[[xvar]], 0.2)
  }

  if (is.null(upper)) {
    upper <- stats::quantile(dat[[xvar]], 0.8)
  }

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


  df <- dat %>%
    dplyr::mutate(group = dplyr::case_when(xvar < lower ~ "juv",
                                           xvar > upper ~ "adult",
                                           .default = NA))

  df$temp_group <- df$group


  rsq_vec <- rep(NA, max_iter)
  RSS_vec <- rep(NA, max_iter)

  for (i in 1:max_iter) {
    # fit known juveniles
    juv_df <- df[df$temp_group == "juv", ]
    juv_fit <- stats::lm(yvar ~ xvar, juv_df) # fit linear model
    rss_juv <- stats::anova(juv_fit)[[2]][2] # juvenile model RSS

    # fit known adults
    ad_df <- df[df$temp_group == "adult", ]
    ad_fit <- stats::lm(yvar ~ xvar, ad_df) # fit linear model
    rss_ad <- stats::anova(ad_fit)[[2]][2] # adult model RSS

    RSS <- rss_juv + rss_ad # add residual sum of squares
    TSS <- sum((df$yvar - mean(df$yvar)) ^ 2) # total sum of squares
    rsq <- 1 - (RSS / TSS)

    df <- df %>%
      dplyr::mutate(
        pred_juv = stats::predict(juv_fit, newdata = data.frame(xvar)),
        # predict yvar based on juvenile model
        pred_ad = stats::predict(ad_fit, newdata = data.frame(xvar)),
        # predict yvar based on adult model
        resid_juv = abs(.data$yvar - .data$pred_juv),
        # juvenile residuals
        resid_ad = abs(.data$yvar - .data$pred_ad),
        # adult residuals
        # temp_group = if_else(resid_juv < resid_ad, "juv", "adult"))
        # Option 1: all points can be reclassified to either maturity stage
        temp_group = dplyr::case_when(
          is.na(.data$group) & resid_juv < resid_ad ~ "juv",
          is.na(.data$group) & resid_juv >= resid_ad ~ "adult",
          .default = .data$group
        )
      )

    rsq_vec[i] = rsq
    RSS_vec[i] = RSS
  }


  df <- df %>% dplyr::rename(init_group = .data$group,
                             pred_mat =  .data$temp_group) %>%
    dplyr::mutate(pred_mat_num = dplyr::if_else(.data$pred_mat == "adult",
                                                1, 0))

  output <- list(
    data = df,
    rsq = rsq_vec,
    RSS = RSS_vec,
    juv_mod = juv_fit,
    adult_mod = ad_fit
  )
  return(output)
}
