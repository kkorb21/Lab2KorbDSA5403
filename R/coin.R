#' Bayesian Analysis for a Discrete-Theta Coin Model
#'
#' Computes the posterior distribution for a binomial coin-flip experiment
#' using a discrete grid of possible values for theta. The function
#' returns posterior probabilities, the equal–tail 1-alpha credible
#' interval, the posterior mean, and the prior mean. A plot is also produced
#' overlaying the prior, likelihood, and posterior.
#'
#' @name coin
#'
#' @param theta A numeric vector of possible values for the probability of
#'   success theta, typically a grid on the interval 0 to 1.
#' @param prior Either:
#'   \itemize{
#'     \item a numeric vector of prior probabilities (same length as `theta` and summing to 1), or
#'     \item a character string specifying a built‑in prior:
#'       "uniform" or "triangular".
#'   }
#' @param n The number of binomial trials.
#' @param z The number of observed successes.
#' @param alpha The significance level used to compute the equal–tail
#'   1-alpha credible interval. Defaults to 0.05.
#'
#' @returns A named list with components:
#' \describe{
#'   \item{posterior}{Posterior probabilities over the grid `theta`.}
#'   \item{interval}{A numeric vector giving the equal–tail credible interval
#'     (L, U).}
#'   \item{posterior_mean}{The posterior mean of theta.}
#'   \item{prior_mean}{The prior mean of theta.}
#' }
#'
#' @importFrom stats dbinom
#' @importFrom graphics layout legend lines plot
#' @export
#'
#' @examples
#' # Uniform prior (numeric)
#' theta <- seq(0, 1, length = 101)
#' prior <- rep(1/101, 101)
#' coin(theta, prior, n = 20, z = 12, alpha = 0.05)
#'
#' # Uniform prior (keyword)
#' coin(theta, prior = "uniform", n = 10, z = 4, alpha = 0.05)
#'
#' # Triangular prior
#' coin(theta, prior = "triangular", n = 10, z = 4, alpha = 0.05)

coin <- function(theta, prior, n, z, alpha = 0.05) {

  # --- Convert prior if character keyword -----------------------------------
  if (is.character(prior)) {
    prior <- tolower(prior)

    if (prior == "uniform") {
      prior <- rep(1 / length(theta), length(theta))

    } else if (prior == "triangular") {
      p <- pmin(theta, 1 - theta)
      prior <- p / sum(p)

    } else {
      stop("prior must be a numeric vector or one of: 'uniform', 'triangular'")
    }
  }

  # --- Input checks ---------------------------------------------------------
  if (length(theta) != length(prior)) stop("theta and prior must be same length.")
  if (abs(sum(prior) - 1) > 1e-8) stop("prior must sum to 1.")
  if (z < 0 || z > n) stop("z must be between 0 and n.")

  # --- Likelihood on discrete theta ----------------------------------------
  likelihood <- dbinom(z, size = n, prob = theta)

  # --- Unnormalized posterior ----------------------------------------------
  unnorm_post <- likelihood * prior

  # --- Normalize to get posterior probabilities ----------------------------
  posterior <- unnorm_post / sum(unnorm_post)

  # --- Posterior mean -------------------------------------------------------
  post_mean <- sum(theta * posterior)

  # --- Prior mean -----------------------------------------------------------
  prior_mean <- sum(theta * prior)

  # --- Equal-tail credible interval -----------------------------------------
  cdf_post <- cumsum(posterior)
  L_index <- which(cdf_post >= alpha/2)[1]
  U_index <- which(cdf_post >= 1 - alpha/2)[1]

  L <- theta[L_index]
  U <- theta[U_index]

  # --- Plot: prior, likelihood, posterior -----------------------------------

  layout(matrix(1:1, nrow = 1))

  # Scale likelihood to match prior height
  lik_scaled <- likelihood / max(likelihood) * max(prior)

  # Compute dynamic y-axis limit
  ymax <- max(c(prior, posterior, lik_scaled))

  plot(theta, prior, type = "l", lwd = 2, col = "blue",
       ylim = c(0, ymax),
       ylab = "Density / Probability", xlab = expression(theta),
       main = "Prior, Likelihood, and Posterior")

  lines(theta, lik_scaled,
        col = "darkgreen", lwd = 2, lty = 2)

  lines(theta, posterior, col = "red", lwd = 2)

  legend("topright",
         legend = c("Prior", "Likelihood (scaled)", "Posterior"),
         col = c("blue", "darkgreen", "red"),
         lwd = c(2,2,2), lty = c(1,2,1))

  # --- Return named list ----------------------------------------------------
  return(list(
    posterior = posterior,
    interval = c(L = L, U = U),
    posterior_mean = post_mean,
    prior_mean = prior_mean
  ))
}
