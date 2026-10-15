# ============================================================
# WEEK 6 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, modelsummary, infer)
set.seed(606)


# ============================================================
# 1. DATA: GERMAN ELECTION RESULTS (2025) ----
# ============================================================
# Berlin (state "11") left out; east: 1 for the former GDR, 0 for the West

gerda <- read_csv("data/federal_cty_harm.csv")

east_codes <- c("12", "13", "14", "15", "16")

counties <- gerda |>
  filter(
    election_year == 2025,
    state != "11"
  ) |>
  mutate(
    density = population / area,
    east    = if_else(state %in% east_codes, 1, 0),
    region  = if_else(east == 1, "East", "West")
  ) |>
  select(
    county_code, state, east, region,
    cdu_csu, spd, greens = gruene, fdp, linke = linke_pds, afd, bsw,
    turnout, density
  ) |>
  mutate(across(cdu_csu:turnout, ~ .x * 100))
counties

counties_east <- counties |>
  filter(east == 1)
counties_west <- counties |>
  filter(east == 0)

counties |>
  count(region)

region_colors <- c(
  "West" = "#2f7f93",
  "East" = "darkorange"
)


# ============================================================
# 2. INTERACTIONS WITH A DUMMY ----
# ============================================================
# dummy only: AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * East_i + epsilon_i

control_model <- lm(afd ~ turnout + east, data = counties)
control_model |>
  tidy()

# interaction: AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * East_i
#                      + beta_3 * Turnout_i * East_i + epsilon_i

interaction_model <- lm(afd ~ turnout * east, data = counties)
interaction_model |>
  tidy()

interaction_beta_0 <- interaction_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
interaction_beta_1 <- interaction_model |>
  tidy() |>
  filter(term == "turnout") |>
  pull(estimate)
interaction_beta_2 <- interaction_model |>
  tidy() |>
  filter(term == "east") |>
  pull(estimate)
interaction_beta_3 <- interaction_model |>
  tidy() |>
  filter(term == "turnout:east") |>
  pull(estimate)

# East: modifying variable; Turnout and East: constitutive terms
# (Brambor, Clark and Golder 2006)

# one line per region
# West (East = 0): beta_0 + beta_1 * Turnout
# East (East = 1): (beta_0 + beta_2) + (beta_1 + beta_3) * Turnout
# marginal effect of turnout: dAfD/dTurnout = beta_1 + beta_3 * East
region_lines <- tibble(
  region    = c("West", "East"),
  intercept = c(interaction_beta_0, interaction_beta_0 + interaction_beta_2),
  slope     = c(interaction_beta_1, interaction_beta_1 + interaction_beta_3)
)
region_lines

# same lines as two separate regressions, one per region
west_model <- lm(afd ~ turnout, data = counties_west)
west_model |>
  tidy()

east_model <- lm(afd ~ turnout, data = counties_east)
east_model |>
  tidy()

# equal and separate slopes
# equal slopes: intercept beta_0 (West), beta_0 + beta_2 (East), slope beta_1
control_beta_0 <- control_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
control_beta_1 <- control_model |>
  tidy() |>
  filter(term == "turnout") |>
  pull(estimate)
control_beta_2 <- control_model |>
  tidy() |>
  filter(term == "east") |>
  pull(estimate)

control_lines <- tibble(
  model     = "Equal slopes: turnout + east",
  region    = c("West", "East"),
  intercept = c(control_beta_0, control_beta_0 + control_beta_2),
  slope     = control_beta_1
)
interaction_lines <- region_lines |>
  mutate(model = "Separate slopes: turnout * east")

slope_lines <- bind_rows(control_lines, interaction_lines)
slope_lines

# geom_abline() draws a line from intercept and slope
ggplot() +
  geom_point(
    data    = counties,
    mapping = aes(x = turnout, y = afd, color = region),
    alpha   = 0.3
  ) +
  geom_abline(
    data      = slope_lines,
    mapping   = aes(intercept = intercept, slope = slope, color = region),
    linewidth = 1
  ) +
  facet_wrap(~model) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "AfD and turnout, without and with interaction",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)",
    color = NULL
  )


# ============================================================
# 3. INFERENCE FOR A REGRESSION SLOPE ----
# ============================================================
# model: AfD_i = beta_0 + beta_1 * Turnout_i + epsilon_i
# 1.a All counties (399), 1.b East only (75, east_model from above)

all_model <- lm(afd ~ turnout, data = counties)
all_model |>
  tidy()

# fitted values of both models, stacked
all_fit <- all_model |>
  augment() |>
  mutate(sample = "1.a All counties")
east_fit <- east_model |>
  augment() |>
  mutate(sample = "1.b East only")

test_fits <- bind_rows(all_fit, east_fit)

test_fits |>
  ggplot(aes(x = turnout)) +
  geom_point(aes(y = afd), alpha = 0.4) +
  geom_line(aes(y = .fitted), color = "#2f7f93", linewidth = 1) +
  facet_wrap(~sample) +
  labs(
    title = "AfD vote share and turnout in both samples",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)"
  )

# glance(): the model as a whole, tidy(): each coefficient
# model-level numbers of both models, stacked
all_model_stats <- all_model |>
  glance() |>
  mutate(sample = "1.a All counties")
east_model_stats <- east_model |>
  glance() |>
  mutate(sample = "1.b East only")

slope_model_stats <- bind_rows(all_model_stats, east_model_stats) |>
  select(sample, nobs, r.squared, deviance, df.residual, sigma)
slope_model_stats

# turnout coefficient of both models, stacked
all_coefs <- all_model |>
  tidy(conf.int = TRUE) |>
  mutate(sample = "1.a All counties")
east_coefs <- east_model |>
  tidy(conf.int = TRUE) |>
  mutate(sample = "1.b East only")

slope_coefs <- bind_rows(all_coefs, east_coefs) |>
  filter(term == "turnout") |>
  select(sample, estimate, std.error, statistic, p.value, conf.low, conf.high)
slope_coefs

slope_tests <- slope_coefs |>
  left_join(slope_model_stats, by = "sample")
slope_tests

# slope = Cov(x, y) / Var(x): estimate
slope_tests |>
  select(sample, estimate)

# n: nobs, sum of squared residuals: deviance
slope_tests |>
  select(sample, nobs, deviance)

# sum of squares of x
slope_tests <- slope_tests |>
  # shortcut for the sum of squares via R^2 (one predictor only), step by step in Week 4, "OLS by hand"
  mutate(ssx = r.squared / (1 - r.squared) * deviance / estimate^2)
slope_tests |>
  select(sample, ssx)

# degrees of freedom: observations left after estimating k slopes
# and the intercept, df = n - k - 1, with k = 1
slope_tests <- slope_tests |>
  mutate(df = nobs - 1 - 1)

# residual variance: estimate of the unknown error variance sigma^2,
# built from the residuals: sigma2_hat = sum(e_hat_i^2) / (n - k - 1)
slope_tests <- slope_tests |>
  mutate(sigma2_hat = deviance / df)

# standard error: SE(beta_1_hat) = sqrt(sigma2_hat / sum((x_i - x_bar)^2))
slope_tests <- slope_tests |>
  mutate(se = sqrt(sigma2_hat / ssx))
slope_tests |>
  select(sample, df, sigma2_hat, ssx, se)


# ============================================================
# 4. HYPOTHESIS TEST FOR A SLOPE ----
# ============================================================
# H_0: beta_1 = 0    H_A: beta_1 != 0
# (0 is the usual null value, any other value can be tested too)

# t-value: t = (beta_1_hat - 0) / SE(beta_1_hat)
slope_tests <- slope_tests |>
  mutate(t_value = estimate / se)
slope_tests |>
  select(sample, estimate, se, t_value)

# under H_0, t follows a t distribution with n - k - 1 degrees of freedom
# 97.5% quantiles: t vs. Normal
tibble(df = c(1, 5, 30, 73, 397)) |>
  mutate(
    t_quantile = qt(0.975, df),
    z_quantile = qnorm(0.975)
  )

# critical value at significance level alpha: t_(1 - alpha/2, n - k - 1)
alpha <- 0.05

slope_tests <- slope_tests |>
  mutate(t_critical = qt(1 - alpha / 2, df))

# reject H_0 if |t| > critical value: if H_0 were true, a t-value this
# extreme would occur in fewer than 5% of samples
slope_tests <- slope_tests |>
  mutate(reject_h0 = abs(t_value) > t_critical)
slope_tests |>
  select(sample, t_value, t_critical, reject_h0)

# p-value: p = 2 * Pr(t_(n - k - 1) < -|t|)
# probability of a result at least as extreme as ours if H_0 were true
slope_tests <- slope_tests |>
  mutate(p_value = 2 * pt(-abs(t_value), df))
slope_tests |>
  select(sample, t_value, df, p_value)

east_test <- slope_tests |>
  filter(sample == "1.b East only")
east_p <- east_test |>
  pull(p_value)
east_p

# confidence interval: beta_1_hat +/- t_(1 - alpha/2, n - k - 1) * SE(beta_1_hat)
slope_tests <- slope_tests |>
  mutate(
    conf_low  = estimate - t_critical * se,
    conf_high = estimate + t_critical * se
  )
slope_tests |>
  select(sample, estimate, conf_low, conf_high)

east_ci <- slope_tests |>
  filter(sample == "1.b East only")

east_conf_low <- east_ci |>
  pull(conf_low)
east_conf_high <- east_ci |>
  pull(conf_high)

# coefficient plot: reject H_0 exactly when the 95% CI excludes 0
slope_tests |>
  ggplot(aes(x = estimate, y = fct_rev(sample))) +
  geom_vline(aes(xintercept = 0, linetype = "H0: slope = 0")) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  scale_linetype_manual(values = c("H0: slope = 0" = "dashed")) +
  labs(
    title    = "Turnout slopes with 95% confidence intervals",
    x        = "Change in AfD vote share per point of turnout",
    y        = NULL,
    linetype = NULL
  )

# comparison with glance(): df.residual, sigma (= sqrt(sigma2_hat))
slope_tests |>
  mutate(sigma_hat = sqrt(sigma2_hat)) |>
  select(sample, df, df.residual, sigma_hat, sigma)

# comparison with tidy(): std.error, statistic, p.value, conf.low, conf.high
# (tidy() always tests against 0)
slope_tests |>
  select(sample, se, std.error, t_value, statistic, p_value, p.value)

slope_tests |>
  select(sample, conf_low, conf.low, conf_high, conf.high)


# ============================================================
# 5. SIMULATED NULL WORLDS ----
# ============================================================
# steps after https://nullworlds.andrewheiss.com

# Step 1: the sample statistic
observed_all <- counties |>
  specify(afd ~ turnout) |>
  calculate(stat = "slope")
observed_all

observed_east <- counties_east |>
  specify(afd ~ turnout) |>
  calculate(stat = "slope")
observed_east

# Step 2: a world without a relationship, each shuffle is one sample from it
shuffle_demo <- counties_east |>
  specify(afd ~ turnout) |>
  hypothesize(null = "independence") |>
  generate(reps = 4, type = "permute")

shuffle_demo |>
  ggplot(aes(x = turnout, y = afd)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#2f7f93") +
  facet_wrap(~replicate) +
  labs(
    title = "Four shuffled versions of the Eastern counties",
    x     = "Turnout (%), shuffled",
    y     = "AfD vote share (%)"
  )

reps <- 5000

shuffled_all <- counties |>
  specify(afd ~ turnout) |>
  hypothesize(null = "independence") |>
  generate(reps = reps, type = "permute")

shuffled_east <- counties_east |>
  specify(afd ~ turnout) |>
  hypothesize(null = "independence") |>
  generate(reps = reps, type = "permute")

# Step 3: one slope per shuffle
null_all <- shuffled_all |>
  calculate(stat = "slope")

null_east <- shuffled_east |>
  calculate(stat = "slope")
null_east |>
  slice_head(n = 5)

observed_all_slope <- observed_all |>
  pull(stat)
observed_east_slope <- observed_east |>
  pull(stat)

null_all_labelled <- null_all |>
  as_tibble() |>
  mutate(
    sample   = "1.a All counties",
    observed = observed_all_slope
  )
null_east_labelled <- null_east |>
  as_tibble() |>
  mutate(
    sample   = "1.b East only",
    observed = observed_east_slope
  )

null_worlds <- bind_rows(null_all_labelled, null_east_labelled)

null_worlds |>
  ggplot(aes(x = stat)) +
  geom_histogram(aes(fill = "Slopes in the null world"), bins = 40, alpha = 0.5) +
  geom_vline(
    mapping   = aes(xintercept = observed, color = "Observed slope"),
    linewidth = 1,
    key_glyph = "path"
  ) +
  facet_wrap(~sample) +
  scale_fill_manual(values = c("Slopes in the null world" = "#2f7f93")) +
  scale_color_manual(values = c("Observed slope" = "darkorange")) +
  labs(
    title = "Slopes in the null world and the observed slope",
    x     = "Slope",
    y     = "Number of shuffles",
    fill  = NULL,
    color = NULL
  )

# Step 4: the p-value, share of null-world slopes at least as extreme
null_p <- null_worlds |>
  group_by(sample) |>
  summarize(p_simulated = mean(abs(stat) >= abs(observed)))
null_p

# shuffles that count toward the p-value
null_extreme <- null_worlds |>
  mutate(shuffle = if_else(
    abs(stat) >= abs(observed),
    "At least as extreme as observed (p-value)",
    "Other shuffles"
  ))

null_extreme |>
  ggplot(aes(x = stat)) +
  geom_histogram(aes(fill = shuffle), bins = 40) +
  geom_vline(
    mapping   = aes(xintercept = observed, color = "Observed slope"),
    linewidth = 1,
    key_glyph = "path"
  ) +
  facet_wrap(~sample) +
  scale_fill_manual(values = c(
    "At least as extreme as observed (p-value)" = "firebrick",
    "Other shuffles"                            = "grey75"
  )) +
  scale_color_manual(values = c("Observed slope" = "darkorange")) +
  labs(
    title = "The p-value in the null world",
    x     = "Slope",
    y     = "Number of shuffles",
    fill  = NULL,
    color = NULL
  )

# infer's built-in version
null_east |>
  get_p_value(obs_stat = observed_east, direction = "two-sided")

# Step 5: the decision, simulated vs. analytical p-values
slope_tests |>
  select(sample, p_value) |>
  left_join(null_p, by = "sample")

# all steps at once: step 1, then steps 2 to 4 in one pipe, per sample
infer_all <- counties |>
  specify(afd ~ turnout)
infer_all

test_statistic_all <- infer_all |>
  calculate(stat = "slope")
test_statistic_all

infer_all |>
  hypothesize(null = "independence") |>
  generate(reps = reps, type = "permute") |>
  calculate(stat = "slope") |>
  get_p_value(obs_stat = test_statistic_all, direction = "two-sided")

infer_east <- counties_east |>
  specify(afd ~ turnout)
infer_east

test_statistic_east <- infer_east |>
  calculate(stat = "slope")
test_statistic_east

infer_east |>
  hypothesize(null = "independence") |>
  generate(reps = reps, type = "permute") |>
  calculate(stat = "slope") |>
  get_p_value(obs_stat = test_statistic_east, direction = "two-sided")


# ============================================================
# 6. INFERENCE IN MULTIPLE REGRESSION ----
# ============================================================
# model: AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * East_i + epsilon_i

# df = n - k - 1 = 399 - 2 - 1
control_model |>
  glance() |>
  select(nobs, df.residual)

# t_j = beta_j_hat / SE(beta_j_hat), p_j = 2 * Pr(t_(n - k - 1) < -|t_j|)
control_model |>
  tidy(conf.int = TRUE)

# testing the interaction: H_0: beta_3 = 0
interaction_model |>
  tidy(conf.int = TRUE) |>
  filter(term == "turnout:east")

# SE of the marginal effect beta_1 + beta_3 * Z (Week 7):
# sqrt(Var(beta_1) + Z^2 * Var(beta_3) + 2 * Z * Cov(beta_1, beta_3))

# regression table with standard errors and stars
turnout_models <- list(
  "Turnout only"   = all_model,
  "Turnout + East" = control_model,
  "Turnout x East" = interaction_model
)

coef_labels <- c(
  "(Intercept)"  = "Intercept",
  "turnout"      = "Turnout (%)",
  "east"         = "East",
  "turnout:east" = "Turnout (%) x East"
)

modelsummary(
  turnout_models,
  output   = "tinytable",
  coef_map = coef_labels,
  stars    = TRUE,
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  fmt      = 3
)

# coefficient plot: modelplot(draw = FALSE) returns the data, one row per model and coefficient
turnout_model_coefs <- modelplot(turnout_models, coef_map = coef_labels, draw = FALSE) |>
  as_tibble() |>
  filter(term != "Intercept")
turnout_model_coefs

turnout_model_coefs |>
  ggplot(aes(x = estimate, y = fct_rev(model))) +
  geom_vline(aes(xintercept = 0, linetype = "H0: coefficient = 0")) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  facet_wrap(~term, ncol = 1, scales = "free") +
  scale_linetype_manual(values = c("H0: coefficient = 0" = "dashed")) +
  labs(
    title    = "Coefficients of the three models",
    x        = "Estimate with 95% confidence interval",
    y        = NULL,
    linetype = NULL
  )


# ============================================================
# 7. TRY IT: PREDICTIONS FROM THE INTERACTION MODEL ----
# ============================================================
# Your task: with the interaction model, predict the AfD vote share of
# a Western and an Eastern county at a turnout of 80% and at 81%. Report,
# per region, both predictions and the difference between them. Which
# coefficients of the model do the two differences correspond to?
