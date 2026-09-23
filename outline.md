## Session 1: Probability and the sampling idea

By the end, students should be able to:

Define probability in frequentist terms (long-run relative frequency) and distinguish a random variable from a realised observation.
Describe the common distributions they will meet (at minimum the normal, and one discrete case such as the binomial) in terms of what generates them, not just their shape.
Articulate the sampling idea: that a statistic computed from a sample is itself a random quantity that would vary across repeated samples. This is the conceptual foundation the rest of the course rests on, so it is the load-bearing objective here.

## Session 2: Estimation and uncertainty

By the end, students should be able to:

Compute and interpret the sample mean and standard deviation as descriptions of a sample.
Distinguish the standard deviation from the standard error, stating in plain language that the SE is the standard deviation of the sampling distribution of the mean. This is the distinction most learners fail, so protect time for it.
State the central limit theorem and explain what it does and does not promise (it concerns the sampling distribution of the mean, not the distribution of the data).
Construct a confidence interval and give the correct frequentist interpretation in terms of long-run coverage. At this point, deliver the single Bayesian contrast: the interpretation students instinctively reach for is the credible interval, and explain why the frequentist object does not license it. Frame this as two questions, not as a deficiency.

## Session 3: Testing, power, and permutation

By the end, students should be able to:

Explain a hypothesis test as a comparison of an observed statistic against a null distribution, and state correctly that a p-value is P(data at least this extreme | null), not P(null | data).
Distinguish Type I and Type II error, and describe power as the same machinery run forwards to ask how often a real effect would be detected.
Interpret non-significance correctly as an absence of evidence rather than evidence of absence.
Generate a null distribution by permutation and show that it closely matches the analytic result, thereby making the parametric assumption concrete. This is where the permutation idea earns its place, as the device that demystifies what a p-value under an assumed distribution actually means.

## Session 4: The linear model

By the end, students should be able to:

Express a two-group comparison as a linear model, and recognise that the t-test they already met is that model with a single binary predictor.
Interpret intercept and slope coefficients, their standard errors, and the associated tests, connecting each back to the estimation and inference ideas from sessions 2 and 3.
Fit a simple linear model in R and read the output critically, identifying which numbers correspond to which concepts already taught.
State the core assumptions of the model and why they matter for the inference.
