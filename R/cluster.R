#' Classification approaches to estimating SM50
#'
#' @param dat data frame or matrix containing the data
#' @param xvar Name of column (integer or double) of measurements for the x-axis
#' variable (e.g., carapace width).
#' @param yvar Name of column (integer or double) of measurements for the y-axis
#' variable (e.g., claw height).
#' @param log Boolean; should both variables be log-transformed before
#' performing the regression? Defaults to FALSE.
#' @param method Classification method to use. A single string or vector
#' containing one or more of c("mclust", "Somerton", "kmeans", "hclust",
#' "infl_pt"), or "all" to return the results of all methods for comparison.
#' @param plot Boolean; optionally display a plot of the input data shaded
#' according to the maturity classifications from the specified method(s).
#' Defaults to FALSE.
#' @returns A data frame with each point classified according to the specified
#' method(s).
#' @export
#'
#' @examples
#' set.seed(12)
#' fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
#' cluster_mods(fc, xvar = "x", yvar = "y", method = c("kmeans"))
#' cluster_mods(fc, xvar = "x", yvar = "y", method = c("all"), plot = TRUE)
cluster_mods <- function(dat,
                         xvar,
                         yvar,
                         log = FALSE,
                         method = c("mclust", "Somerton", "kmeans", "hclust", "infl_pt", "all"),
                         plot = FALSE) {
  method <- tolower(method)
  new_dat <- data.frame(xvar = dat[[xvar]], yvar = dat[[yvar]])

  if (isTRUE(log)) {
    new_dat$xvar <- log(dat[[xvar]])
    new_dat$yvar <- log(dat[[yvar]])
  }

  out_df <- data.frame()

  if ("infl_pt" %in% method | "all" %in% method) {

    disc <- infl_pt(new_dat, "xvar", "yvar", plot = FALSE)
    temp_df <- new_dat %>%
      dplyr::mutate(pred_mat = (dplyr::if_else(.data$yvar / .data$xvar > disc, 1, 0))) %>%
      dplyr::mutate(method = "infl_pt")
    out_df <- out_df %>% dplyr::bind_rows(temp_df)

  }

  if ("kmeans" %in% method | "all" %in% method) {

    temp_vec <- kmeans(new_dat, centers = 2, iter.max = 15)$cluster - 1
    temp_df <- new_dat %>%
      dplyr::mutate(pred_mat = temp_vec, method = "kmeans")
    mature_label <- dplyr::slice_max(temp_df, xvar) %>% dplyr::pull(pred_mat)
    temp_df <- temp_df %>%
      dplyr::mutate(pred_mat = dplyr::if_else(pred_mat == mature_label,
                                as.numeric(1), as.numeric(0)))
    out_df <- out_df %>% dplyr::bind_rows(temp_df)

  }

  if ("hclust" %in% method | "all" %in% method) {

    temp_vec <- (cutree(hclust(
      dist(new_dat, method = "euclidean"), method = "ward.D"), k = 2) - 1)
    temp_df <- new_dat %>%
      dplyr::mutate(pred_mat = temp_vec, method = "hclust")
    mature_label <- dplyr::slice_max(temp_df, xvar) %>% dplyr::pull(pred_mat)
    temp_df <- temp_df %>%
      dplyr::mutate(pred_mat = dplyr::if_else(pred_mat == mature_label,
                                as.numeric(1), as.numeric(0)))
    out_df <- out_df %>% dplyr::bind_rows(temp_df)
  }

  if ("somerton" %in% method | "all" %in% method) {

    temp_vec <- somerton(new_dat, "xvar", "yvar")[[1]]$pred_mat_num
    temp_df <- new_dat %>%
      dplyr::mutate(pred_mat = temp_vec, method = "Somerton")
    out_df <- out_df %>% dplyr::bind_rows(temp_df)
  }

  if ("mclust" %in% method | "all" %in% method) {

    temp_vec <- mclust::Mclust(
      data = new_dat,
      G = 2,
      verbose = FALSE,
      modelNames = "EVV"
    )$classification
    temp_df <- new_dat %>%
      dplyr::mutate(pred_mat = temp_vec, method = "mclust")
    mature_label <- dplyr::slice_max(temp_df, xvar) %>% dplyr::pull(pred_mat)
    temp_df <- temp_df %>%
      dplyr::mutate(pred_mat = dplyr::if_else(pred_mat == mature_label,
                                as.numeric(1), as.numeric(0)))
    out_df <- out_df %>% dplyr::bind_rows(temp_df)
  }

  if (isTRUE(plot)) {

    print(ggplot() +
      geom_point(data = out_df, aes(xvar, yvar, color = as.factor(pred_mat)),
                 alpha = 0.4) +
      labs(x = xvar, y = yvar, color = "Predicted group") +
      theme_bw() +
      facet_wrap(~method) +
      scale_color_manual(
        values = c("0" = "lightblue", "1" = "black"),
        breaks = c(0, 1),
        labels = c("0" = "Immature", "1" = "Mature")) +
      theme(
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 13)) +
      guides(color = guide_legend(override.aes = list(alpha = 1))))
  }

  return(out_df)

}
