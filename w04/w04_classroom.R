# ============================================================
# WEEK 4 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, modelsummary)
set.seed(404)


# ============================================================
# 1. THE LINEAR REGRESSION MODEL ----
# ============================================================
# model:         Y_i = beta_0 + beta_1 * X_i + epsilon_i
# fitted line:   y_hat_i = beta_0_hat + beta_1_hat * x_i
# residual:      e_i = y_i - y_hat_i
# OLS minimizes: sum of (y_i - beta_0_hat - beta_1_hat * x_i)^2
# https://setosa.io/ev/ordinary-least-squares-regression/
# https://www.econometrics-with-r.org/4.2-estimating-the-coefficients-of-the-linear-regression-model.html


# ============================================================
# 2. DATA: GERMAN ELECTION RESULTS (2025) ----
# ============================================================

gerda <- read_csv("data/federal_cty_harm.csv")

counties <- gerda |>
  filter(election_year == 2025) |>
  mutate(density = population / area) |>
  select(
    county_code, state,
    cdu_csu, spd, greens = gruene, fdp, linke = linke_pds, afd, bsw,
    turnout, population, area, density
  ) |>
  mutate(across(cdu_csu:turnout, ~ .x * 100))
counties


# ============================================================
# 3. OLS BY HAND: GREENS AND FDP ----
# ============================================================
# model: Greens_i = beta_0 + beta_1 * FDP_i + epsilon_i

counties |>
  ggplot(aes(x = fdp, y = greens)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Greens vs. FDP vote share by county, 2025",
    x     = "FDP vote share (%)",
    y     = "Greens vote share (%)"
  )

# slope: beta_1_hat = Cov(x, y) / Var(x)
counties |>
  summarize(beta_1 = cov(fdp, greens) / var(fdp))

# written out: sum((x_i - x_bar) * (y_i - y_bar)) / sum((x_i - x_bar)^2)
mean_x <- counties |>
  summarize(mean_x = mean(fdp)) |>
  pull(mean_x)
mean_x

mean_y <- counties |>
  summarize(mean_y = mean(greens)) |>
  pull(mean_y)
mean_y

ols_hand <- counties |>
  select(county_code, fdp, greens) |>
  mutate(
    diff_x         = fdp - mean_x,
    diff_y         = greens - mean_y,
    diff_xy        = diff_x * diff_y,
    diff_x_squared = diff_x^2
  )
ols_hand

ols_sums <- ols_hand |>
  summarize(
    numerator   = sum(diff_xy),
    denominator = sum(diff_x_squared)
  )
ols_sums

numerator <- ols_sums |>
  pull(numerator)
denominator <- ols_sums |>
  pull(denominator)

beta_1 <- numerator / denominator
beta_1

# intercept: beta_0_hat = y_bar - beta_1_hat * x_bar
beta_0 <- mean_y - beta_1 * mean_x
beta_0

# fitted line: Greens_hat_i = -1.34 + 2.75 * FDP_i
counties |>
  ggplot(aes(x = fdp, y = greens)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = beta_0, slope = beta_1, color = "#2f7f93") +
  labs(
    title = "Greens vs. FDP, OLS line by hand",
    x     = "FDP vote share (%)",
    y     = "Greens vote share (%)"
  )


# ============================================================
# 4. FITTED VALUES AND RESIDUALS ----
# ============================================================
# y_hat_i = beta_0_hat + beta_1_hat * x_i
# e_i     = y_i - y_hat_i

ols_hand <- ols_hand |>
  mutate(
    greens_hat = beta_0 + beta_1 * fdp,
    residual   = greens - greens_hat
  )
ols_hand

# OLS residuals sum to zero (up to floating-point noise)
ols_hand |>
  summarize(sum_residuals = sum(residual))


# ============================================================
# 5. GOODNESS OF FIT ----
# ============================================================
# TSS = sum((y_i - y_bar)^2)      total variation of y
# ESS = sum((y_hat_i - y_bar)^2)  variation the line reproduces
# RSS = sum(e_i^2)                variation left over, minimized by OLS

ols_hand <- ols_hand |>
  mutate(
    diff_y_squared   = diff_y^2,
    diff_hat         = greens_hat - mean_y,
    diff_hat_squared = diff_hat^2,
    residual_squared = residual^2
  )
ols_hand

ss_sums <- ols_hand |>
  summarize(
    tss = sum(diff_y_squared),
    ess = sum(diff_hat_squared),
    rss = sum(residual_squared)
  )
ss_sums

tss <- ss_sums |>
  pull(tss)
ess <- ss_sums |>
  pull(ess)
rss <- ss_sums |>
  pull(rss)

# TSS = ESS + RSS
c(tss = tss, ess_plus_rss = ess + rss)

# R^2 = ESS / TSS = 1 - RSS / TSS
r_squared <- ess / tss
r_squared

1 - rss / tss

# correlation: r_xy = sqrt(R^2), with the sign of beta_1_hat
r <- sqrt(r_squared)
r

counties |>
  summarize(r = cor(fdp, greens))


# ============================================================
# 6. LINEAR MODELS WITH lm() ----
# ============================================================
# model: Greens_i = beta_0 + beta_1 * FDP_i + epsilon_i

fdp_model <- lm(greens ~ fdp, data = counties)
fdp_model

# base R's classic model printout
summary(fdp_model)

# broom: one row per coefficient
fdp_model |>
  tidy()

lm_beta_0 <- fdp_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
lm_beta_1 <- fdp_model |>
  tidy() |>
  filter(term == "fdp") |>
  pull(estimate)
lm_beta_0
lm_beta_1

# fitted line: Greens_hat_i = -1.34 + 2.75 * FDP_i
coef_comparison <- tibble(
  term    = c("(Intercept)", "fdp"),
  by_hand = c(beta_0, beta_1)
) |>
  left_join(tidy(fdp_model), by = "term") |>
  select(term, by_hand, estimate)
coef_comparison

# broom: one row of model-level statistics
# adjusted R^2 = 1 - (1 - R^2) * (n - 1) / (n - k - 1)
fdp_model |>
  glance()

n_counties <- nrow(counties)
1 - (1 - r_squared) * (n_counties - 1) / (n_counties - 1 - 1)

# broom: fitted values and residuals next to the data
fdp_fit <- fdp_model |>
  augment()
fdp_fit

fdp_fit |>
  summarize(rss = sum(.resid^2))

# newdata: apply the fitted line to other rows, here 15 random counties
fdp_model |>
  augment(newdata = counties |> slice_sample(n = 15)) |>
  select(county_code, fdp, greens, .fitted, .resid)

# regression table (tinytable adapts to the render format)
# https://modelsummary.com/vignettes/get_started.html
modelsummary(
  fdp_model,
  output    = "tinytable",
  coef_map  = c(
    "(Intercept)" = "Intercept",
    "fdp"         = "FDP vote share (%)"
  ),
  statistic = NULL,
  stars     = FALSE,
  gof_map   = c("nobs", "r.squared"),
  fmt       = 3
)

# residual plot: an OLS line through the residuals lies on zero
fdp_fit |>
  ggplot(aes(x = fdp, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "#2f7f93", linewidth = 0.8) +
  geom_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = FALSE,
    color    = "firebrick",
    linetype = "dashed"
  ) +
  labs(
    title = "Residuals of the Greens-FDP model",
    x     = "FDP vote share (%)",
    y     = "Residual (percentage points)"
  )


# ============================================================
# 7. LOG TRANSFORMATIONS: GREENS AND POPULATION DENSITY ----
# ============================================================

counties |>
  ggplot(aes(x = density, y = greens)) +
  geom_point(alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 4000, 1000)) +
  labs(
    title = "Greens vote share vs. density by county, 2025",
    x     = "Residents per square km",
    y     = "Greens vote share (%)"
  )

counties |>
  ggplot(aes(x = density, y = greens)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(
    title = "Greens vote share vs. density by county, 2025",
    x     = "Residents per square km (log scale)",
    y     = "Greens vote share (%)"
  )

# distribution of density, raw and logged
density_scales <- counties |>
  mutate(log_density = log(density)) |>
  select(county_code, density, log_density) |>
  pivot_longer(c(density, log_density), names_to = "scale", values_to = "value")
density_scales

density_scales |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#2f7f93", alpha = 0.6, color = "white") +
  facet_wrap(~ scale, scales = "free") +
  labs(
    title = "Population density, raw and logged",
    x     = NULL,
    y     = "Counties"
  )

# linear model: Greens_i = beta_0 + beta_1 * Density_i + epsilon_i
density_model <- lm(greens ~ density, data = counties)
density_model |>
  tidy()

density_beta_0 <- density_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
density_beta_1 <- density_model |>
  tidy() |>
  filter(term == "density") |>
  pull(estimate)
# fitted line: Greens_hat_i = 8.03 + 0.00377 * Density_i

# log model: Greens_i = beta_0 + beta_1 * log(Density_i) + epsilon_i
log_density_model <- lm(greens ~ log(density), data = counties)
log_density_model |>
  tidy()

log_beta_0 <- log_density_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
log_beta_1 <- log_density_model |>
  tidy() |>
  filter(term == "log(density)") |>
  pull(estimate)
# fitted line: Greens_hat_i = -5.29 + 2.73 * log(Density_i)

# augment(data = ) keeps all original columns next to the fitted values
density_fit <- density_model |>
  augment(data = counties) |>
  mutate(model = "Linear: density")
log_density_fit <- log_density_model |>
  augment(data = counties) |>
  mutate(model = "Log: log(density)")

density_fits <- bind_rows(density_fit, log_density_fit)

density_fits |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "#2f7f93", linewidth = 0.8) +
  geom_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = FALSE,
    color    = "firebrick",
    linetype = "dashed"
  ) +
  facet_wrap(~ model, scales = "free_x") +
  labs(
    title = "Residuals of the two density models",
    x     = "Fitted Greens vote share (%)",
    y     = "Residual (percentage points)"
  )

density_glance <- density_model |>
  glance() |>
  mutate(model = "Linear: density")
log_density_glance <- log_density_model |>
  glance() |>
  mutate(model = "Log: log(density)")

bind_rows(density_glance, log_density_glance) |>
  select(model, r.squared, adj.r.squared)

# a named list of models: one column per model
density_models <- list(
  "Linear" = density_model,
  "Log"    = log_density_model
)

modelsummary(
  density_models,
  output    = "tinytable",
  coef_map  = c(
    "(Intercept)"  = "Intercept",
    "density"      = "Density (residents per square km)",
    "log(density)" = "log(Density)"
  ),
  statistic = NULL,
  stars     = FALSE,
  gof_map   = c("nobs", "r.squared"),
  fmt       = 3
)

# raw density axis: the log model is a curve
density_fits |>
  ggplot(aes(x = density)) +
  geom_point(aes(y = greens), data = counties, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 4000, 1000)) +
  scale_color_manual(
    values = c("Linear: density" = "firebrick", "Log: log(density)" = "#2f7f93")
  ) +
  labs(
    title = "Greens vote share and population density",
    x     = "Residents per square km",
    y     = "Greens vote share (%)",
    color = NULL
  )

# log density axis: the log model is a straight line
density_fits |>
  ggplot(aes(x = density)) +
  geom_point(aes(y = greens), data = counties, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), linewidth = 1) +
  scale_x_log10() +
  scale_color_manual(
    values = c("Linear: density" = "firebrick", "Log: log(density)" = "#2f7f93")
  ) +
  labs(
    title = "Greens vote share and population density",
    x     = "Residents per square km (log scale)",
    y     = "Greens vote share (%)",
    color = NULL
  )

# interpreting a log coefficient: densities differing by a factor c
# delta_y_hat = beta_1_hat * log(c * x) - beta_1_hat * log(x) = beta_1_hat * log(c)

# doubling (c = 2)
log_beta_1 * log(2)

# tenfold (c = 10)
log_beta_1 * log(10)

# 10% denser (c = 1.1): roughly beta_1_hat / 10
log_beta_1 * log(1.1)


# ============================================================
# 8. TRY IT: PREDICTING A COUNTY'S GREENS SHARE ----
# ============================================================
# Your task: predict the Greens' vote share in three hypothetical
# counties with 100, 1,000 and 3,000 residents per km², once with the
# linear and once with the log density model. Where do the two models
# disagree most, and why? (augment() takes new data through its
# newdata argument.)


# ============================================================
# 9. TRY IT: AFD AND SPD ----
# ============================================================
# Your task: regress the AfD's vote share on the SPD's,
#   AfD_i = beta_0 + beta_1 * SPD_i + epsilon_i
# Compute beta_0_hat, beta_1_hat and R^2 by hand, confirm them with
# lm(), and draw the residual plot. How well does SPD support describe
# AfD support, and what does the residual plot show?
