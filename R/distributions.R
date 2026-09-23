# R/distributions.R ---------------------------------------------------------
# Small helpers for the coin / binomial work in the pre-reading and the
# workshop warm-up. Definitions only, no side effects.

#' Simulate a run of coin flips and return the running proportion of heads
#'
#' @param n_flips Integer. How many flips. Default 1000.
#' @param prob Numeric in (0, 1). Probability of heads on one flip. Default 0.5.
#' @return A tibble with columns `flip` and `proportion`, where `proportion` is
#'   the cumulative proportion of heads seen up to and including that flip.
#' @examples
#' running_proportion(50)
running_proportion <- function(n_flips = 1000, prob = 0.5) {

  if (prob < 0 || prob > 1) {
    stop("prob must be between 0 and 1. You gave: ", prob, ".")
  }

  outcomes <- rbinom(n_flips, size = 1, prob = prob)
  tibble::tibble(
    flip       = seq_len(n_flips),
    proportion = cumsum(outcomes) / seq_len(n_flips)
  )
}
