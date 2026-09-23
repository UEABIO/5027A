# Session 3 dataset pilot
# ------------------------------------------------------------------
# Freezes the sixteen values used in Session 3. Run once, record the
# five numbers listed at the foot of this file in COURSE-STATUS.md,
# then write data/diversity.csv and do not regenerate it.
#
# Nothing downstream is buildable until this has run: the board axis,
# the observed difference, the four misconception statements and every
# code block in 03-computing.qmd are written against these numbers.

library(tidyverse)

# ------------------------------------------------------------------
# 1. Input
# ------------------------------------------------------------------
# `candidates` must be one row per biological sample:
#
#   sample_id  character
#   arm        exactly two levels, randomly allocated
#   shannon    numeric, Shannon diversity computed at a common depth
#
# Conditions that must hold before this script is worth running:
#   - one sample per subject, no repeated timepoints
#   - no co-housed animals, no shared cages or households
#   - allocation to arm was random, not observational
#   - at least 12 samples per arm, so the screen has room to search

candidates <- read_csv("data-raw/candidate_diversity.csv")

stopifnot(
  n_distinct(candidates$sample_id) == nrow(candidates),
  n_distinct(candidates$arm) == 2,
  all(!is.na(candidates$shannon))
)

# ------------------------------------------------------------------
# 2. Exact permutation distribution for one 8 + 8 subset
# ------------------------------------------------------------------
# All choose(16, 8) = 12,870 label assignments, enumerated rather than
# sampled. This is the quantity the class estimates with sixty dots.

exact_perm <- function(values, group, n_per_group = 8) {
  stopifnot(length(values) == 2 * n_per_group)

  group <- as.character(group)
  arms <- sort(unique(group))
  stopifnot(length(arms) == 2)

  observed <- mean(values[group == arms[1]]) - mean(values[group == arms[2]])

  splits <- combn(seq_along(values), n_per_group)
  diffs <- apply(splits, 2, \(i) mean(values[i]) - mean(values[-i]))

  eps <- 1e-9

  list(
    arms = arms,
    observed = observed,
    n_splits = ncol(splits),
    p_two = mean(abs(diffs) >= abs(observed) - eps),
    p_one = mean(diffs >= observed - eps),
    rand_sd = sd(diffs),
    diffs = diffs
  )
}

# Unpooled standard error, for the Chance et al. (2024) agreement check.

unpooled_se <- function(values, group) {
  tibble(values = values, group = as.character(group)) |>
    summarise(v = var(values), n = n(), .by = group) |>
    summarise(se = sqrt(sum(v / n))) |>
    pull(se)
}

# ------------------------------------------------------------------
# 3. Screen candidate subsets
# ------------------------------------------------------------------
# se_ratio compares the standard deviation of the randomisation
# distribution with the textbook unpooled standard error. Shuffling
# derives its spread from the overall variability rather than the
# within-group variability, so these diverge when the effect is
# strong. Hour 2 compares the two p-values in front of the class, so
# the ratio must be close to one.

screen_subsets <- function(data, n_draws = 500, n_per_group = 8) {
  draw_one <- function(i) {
    sub <- slice_sample(data, n = n_per_group, by = arm)
    ep <- exact_perm(sub$shannon, sub$arm, n_per_group)

    tibble(
      draw = i,
      observed = ep$observed,
      p_two = ep$p_two,
      rand_sd = ep$rand_sd,
      se_ratio = ep$rand_sd / unpooled_se(sub$shannon, sub$arm),
      ids = list(sub$sample_id)
    )
  }

  withr::with_seed(3, map(seq_len(n_draws), draw_one)) |>
    list_rbind()
}

screened <- screen_subsets(candidates)

# ------------------------------------------------------------------
# 4. Selection
# ------------------------------------------------------------------
# p between 0.02 and 0.10: the class count is small but never zero,
#   and the alpha discussion in hour 2 has something to bite on.
# se_ratio near 1: the shuffle and the analytic shortcut agree, so
#   hour 2 does not have to explain away a discrepancy.

shortlist <- screened |>
  filter(
    between(p_two, 0.02, 0.10),
    between(se_ratio, 0.90, 1.15)
  ) |>
  arrange(abs(p_two - 0.05))

shortlist |>
  select(draw, observed, p_two, rand_sd, se_ratio) |>
  print(n = 20)

# Inspect the leading candidate before accepting it. Reject any subset
# whose values cannot be averaged by hand: check the range, and check
# that two decimal places are enough to separate them.

chosen_draw <- shortlist$draw[1]
chosen_ids <- shortlist |> filter(draw == chosen_draw) |> pull(ids) |> pluck(1)

chosen <- candidates |>
  filter(sample_id %in% chosen_ids) |>
  mutate(shannon = round(shannon, 2)) |>
  arrange(arm, sample_id)

# Rounding to two decimals changes the values, so recompute on the
# rounded numbers. These, not the unrounded ones, are what the cards
# carry and what the class will average.

final <- exact_perm(chosen$shannon, chosen$arm)

# ------------------------------------------------------------------
# 5. Freeze
# ------------------------------------------------------------------

write_csv(chosen, "data/diversity.csv")

# ------------------------------------------------------------------
# 6. Record these five numbers in COURSE-STATUS.md
# ------------------------------------------------------------------
# They fix the board axis, the handout, the misconception statements
# and every worked number in 03-workshop.qmd and 03-computing.qmd.

cat(
  "Arms (subtraction is first minus second): ",
  paste(final$arms, collapse = " - "), "\n",
  "Observed difference:                      ", round(final$observed, 3), "\n",
  "Exact two-sided p over 12,870 splits:     ", round(final$p_two, 4), "\n",
  "Exact one-sided p:                        ", round(final$p_one, 4), "\n",
  "SD of randomisation distribution:         ", round(final$rand_sd, 3), "\n",
  "Unpooled SE:                              ",
  round(unpooled_se(chosen$shannon, chosen$arm), 3), "\n",
  "Suggested board axis limits:              ",
  round(-4 * final$rand_sd, 1), "to", round(4 * final$rand_sd, 1), "\n",
  sep = " "
)

# The board axis should be pre-drawn to these limits with a bin width
# giving roughly fifteen bins, so it does not need rescaling mid-activity.

tibble(diff = final$diffs) |>
  ggplot(aes(diff)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = c(-1, 1) * final$observed, linetype = "dashed") +
  labs(
    x = paste(final$arms, collapse = " minus "),
    y = "Number of the 12,870 possible label assignments"
  )
