# Session 1 populations -------------------------------------------------------
#
# PLACEHOLDER. Both populations below are parametric stand-ins with plausible
# parameters. They exist so the book compiles and so an absent student has
# something to sample from.
#
# bean_population() must be replaced with the real measured tub once it has
# been weighed: read data/beans.csv and return the same two columns. Nothing
# downstream should need to change. Until then the book's numbers will not
# match the numbers generated in the room.
#
# Requires withr, which ships as a dependency of the tidyverse but is not
# attached by it.

# Beetle body mass, in milligrams. Used in the pre-class reading only, so that
# the bean population is not previewed before the workshop.
beetle_population <- function(n = 5000, seed = 4071) {
  withr::with_seed(seed, {
    tibble::tibble(
      beetle_id = seq_len(n),
      mass_mg   = round(rgamma(n, shape = 6, scale = 15), 1)
    )
  })
}

# The tub of beans, in grams, recorded to the precision of a bench balance.
# Right-skewed with an interior mode, which is what dried beans actually look
# like and which keeps the population mean visible on a histogram.
bean_population <- function(n = 800, seed = 5119) {
  withr::with_seed(seed, {
    tibble::tibble(
      bean_id = seq_len(n),
      mass_g  = round(rgamma(n, shape = 6, scale = 0.09), 2)
    )
  })
}
