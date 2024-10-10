#' Generate artificial data for simulation testing
#'
#' @description Generates an artificial sample of morphometric data with
#'   specified characteristics. Recommended to use set.seed() before running
#'   to ensure reproducibility.
#'
#' @param L50 Integer or double; the desired true length at 50% maturity on the
#'   scale of the x-axis/reference variable. Defaults to 100 mm.
#' @param slope Integer or double; the desired slope parameter for the logistic
#'   equation describing the probability of maturity at a given value of the
#'   x-axis/reference variable. Default is 5.
#' @param n Sample size of the simulated data set. Default is 1000 individuals.
#' @param x_mean Mean of the reference variable (e.g., carapace width). Default
#'   is 105 mm.
#' @param x_sd Standard deviation of the reference variable (e.g., carapace
#'   width). Default is 20 mm.
#' @param allo_params A numeric vector of length 4 containing the parameters
#'   controlling how the allometric relationship between the x and y variables
#'   changes at maturity. Should contain the immature slope parameter, immature
#'   intercept parameter, mature slope parameter, and mature intercept
#'   parameter, in that order.
#' @param error_scale Scaling for the error added to the simulated data
#'
#' @returns A data frame with n rows. Columns are: (1) the x variable on the
#'   original scale, (2) the probability of maturity for the individual, (3) the
#'   assigned maturity status, 1 or 0, (4) the y variable on the original scale,
#'   (5) the log-transformed x variable, and (6) the log-transformed y variable
#' @export
#'
#' @examples
#' set.seed(123)
#' fake_crustaceans(n=25)
fake_crustaceans <- function(L50 = 100, # length at 50% maturity on ref var scale
                       slope = 5, # slope parameter for logistic maturity
                       n = 1000, # number of crustaceans sampled
                       # mean of reference variable, e.g., carapace width in mm
                       x_mean = 105,
                       # standard deviation of reference variable
                       x_sd = 20,
                       allo_params = c(1.2, # immature slope parameter
                                       0.1, # immature intercept parameter
                                       1.2, # mature slope parameter
                                       0.1),# mature intercept parameter
                       error_scale = 20) # SD of errors
{


  # Create normal distribution of carapace widths for a given n, mean, and SD
  fake_crustaceans <- data.frame(x = stats::rnorm(n = n, mean = x_mean, sd = x_sd))

  # Add probability of maturity for each individual crab
  # based on a logistic distribution with given location (L50) and
  # shape (slope of the logistic curve) parameters
  fake_crustaceans$prob_mat <- stats::plogis(fake_crustaceans$x, L50, slope)

  # Based on the probabilities of maturity,
  # use a binomial distribution to assign each crab a maturity status
  # (0 = immature, 1 = mature)
  mature_vec <- stats::rbinom(n, 1, fake_crustaceans$prob_mat)

  # Add vector of maturities to data frame of x-vars and maturity probabilities
  fake_crustaceans$mature <- as.factor(mature_vec)

  err_sd <- fake_crustaceans %>%
    dplyr::summarise(
      range = max(.data$x, na.rm = TRUE) - min(.data$x, na.rm = TRUE)
    ) %>%
    dplyr::mutate(err_sd = .data$range * 0.01 / error_scale) %>%
    dplyr::pull(err_sd)

  err <- stats::rnorm(n = n, sd = err_sd)
  fake_crustaceans$errs <- exp(err)

  a0 <- allo_params[1] # Immature slope parameter
  b0 <- allo_params[2] # Immature intercept parameter
  a1 <- allo_params[3] # Mature slope parameter
  b1 <- allo_params[4] # Immature intercept parameter

  fake_crustaceans <- fake_crustaceans %>%
    #if crab is immature, use immature parameters
    dplyr::mutate(y = dplyr::case_when(
      .data$mature == 0 ~ b0 * (.data$x ^ (a0)) * .data$errs,
      #if crab is mature, use mature parameters
      .data$mature == 1 ~ b1 * (.data$x ^ (a1)) * .data$errs),
      log_x = log(.data$x),  #find log of x
      log_y = log(.data$y)  #find log of x
    )

  fake_crustaceans <- fake_crustaceans %>% dplyr::select(-"errs")

  return(fake_crustaceans)

}
