# R/beans.R -----------------------------------------------------------------
# Session 1: the jar of beans, and the helpers that use it.
#
# Definitions only. No library() calls, no side effects beyond creating the
# objects below. Sourced by every chapter of Session 1.
#
# If you prefer the R/functions/ convention used later in the module, this file
# splits cleanly into R/functions/sampling_functions.R (draw_one_mean,
# one_interval) and R/functions/edna_functions.R (the Block 5 pair). It is kept
# as one file here because students are not writing functions in week 1.

# ---- The population -------------------------------------------------------
# The seed is FIXED AT 1 for everybody, deliberately. The population is a fact
# about the world: it does not vary between students and it does not vary
# between weeks. Only your samples from it vary.
#
# That distinction is the whole of Block 2. Do not change this seed.

set.seed(1)

population <- tibble::tibble(
  bean_id = 1:2000,
  mass_g  = round(rnorm(2000, mean = 0.52, sd = 0.08), 3)  # balance reads to 1 mg
)

# ---- The true values ------------------------------------------------------
# Available only because this is a teaching jar and it has been weighed in full.
# In real research you never have these, and that is the entire problem.

mu    <- mean(population$mass_g)
sigma <- sd(population$mass_g)

# ---- The demonstration seed -----------------------------------------------
# The worked examples printed in this book run on this seed. Your own numbers
# will differ from the ones you see here. That is not an error; it is Block 2.

demo_seed <- 20260317


#' Set the random seed from a student ID and a session code
#'
#' @param student_id Numeric of length 1. Your ID number.
#' @param session_code Numeric of length 1. The four-digit code from the board.
#' @return Invisibly, the seed used. Called for its side effect.
#' @examples
#' set_my_seed(12345678, 4271)
set_my_seed <- function(student_id, session_code) {

  if (!is.numeric(student_id) || !is.numeric(session_code)) {
    stop(
      "Both student_id and session_code must be numbers. ",
      "You gave: ", class(student_id), " and ", class(session_code), "."
    )
  }

  # The modulo keeps the result inside R's integer range; 7919 is prime, which
  # makes accidental collisions between students vanishingly unlikely.
  my_seed <- (student_id + session_code * 7919) %% 2147483647
  set.seed(my_seed)
  invisible(my_seed)
}


#' Draw one sample of beans from the jar and return its mean
#'
#' @param n Integer. Beans per sample. Default 10.
#' @return Numeric of length 1. The mean mass, in grams.
#' @examples
#' draw_one_mean(10)
draw_one_mean <- function(n = 10) {
  mean(sample(population$mass_g, size = n))
}


#' Draw one sample and build a confidence interval on its mean
#'
#' @param n Integer. Beans per sample. Default 10.
#' @param multiplier Numeric. How many standard errors either side of the mean.
#'   Default 1.96.
#' @return A one-row tibble: mean_g, se_g, lower, upper.
#' @examples
#' one_interval(n = 10)
one_interval <- function(n = 10, multiplier = 1.96) {
  s  <- sample(population$mass_g, size = n)
  m  <- mean(s)
  se <- sd(s) / sqrt(n)
  tibble::tibble(
    mean_g = m,
    se_g   = se,
    lower  = m - multiplier * se,
    upper  = m + multiplier * se
  )
}


#' Natural frequency table for an eDNA assay
#'
#' @param ponds Numeric. How many ponds to imagine. Default 1000.
#' @param prevalence Numeric in (0, 1). Proportion of ponds holding the species.
#' @param sensitivity Numeric in (0, 1). P(positive | species present).
#' @param false_positive_rate Numeric in (0, 1). P(positive | species absent).
#' @return A tibble of counts, never percentages.
#' @examples
#' edna_table(prevalence = 0.02)
edna_table <- function(ponds               = 1000,
                       prevalence          = 0.02,
                       sensitivity         = 0.90,
                       false_positive_rate = 0.05) {
  present <- ponds * prevalence
  absent  <- ponds - present

  tibble::tibble(
    truth         = c("Species present", "Species absent"),
    n_ponds       = c(present, absent),
    test_positive = c(present * sensitivity,       absent * false_positive_rate),
    test_negative = c(present * (1 - sensitivity), absent * (1 - false_positive_rate))
  )
}


#' Posterior probability the species is present, given one positive test
#'
#' @param prevalence Numeric in (0, 1). The prior.
#' @param sensitivity Numeric in (0, 1). Default 0.90.
#' @param false_positive_rate Numeric in (0, 1). Default 0.05.
#' @return Numeric. P(species present | positive test).
#' @examples
#' prob_present_given_positive(0.02)   # about 0.27
prob_present_given_positive <- function(prevalence,
                                        sensitivity         = 0.90,
                                        false_positive_rate = 0.05) {
  true_positive  <- prevalence * sensitivity
  false_positive <- (1 - prevalence) * false_positive_rate
  true_positive / (true_positive + false_positive)
}
