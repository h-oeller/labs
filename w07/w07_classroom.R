# ============================================================
# WEEK 7 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, marginaleffects, infer, ggdag, dagitty, patchwork)
set.seed(707)


# ============================================================
# 1. DATA: ELECTION RESULTS AND COUNTY STRUCTURE ----
# ============================================================
# GERDA 2021 without Berlin, plus the INKAR median age of the same year
gerda <- read_csv("data/federal_cty_harm.csv")

# p_load(gerda); gerda_covariates() |> write_csv("data/cty_covariates.csv")
covariates <- read_csv("data/cty_covariates.csv")

covariates_2021 <- covariates |>
  filter(year == 2021) |>
  select(county_code, median_age)
covariates_2021

east_codes <- c("12", "13", "14", "15", "16")

counties <- gerda |>
  filter(
    election_year == 2021,
    state != "11"
  ) |>
  mutate(
    east   = if_else(state %in% east_codes, 1, 0),
    region = if_else(east == 1, "East", "West")
  ) |>
  select(county_code, state, east, region, afd, turnout) |>
  mutate(across(c(afd, turnout), ~ .x * 100)) |>
  left_join(covariates_2021, by = "county_code")
counties

counties |>
  ggplot(aes(x = median_age)) +
  geom_histogram(binwidth = 0.5, fill = "#2f7f93", color = "white") +
  labs(
    title = "Median age of the 399 counties",
    x     = "Median age (years)",
    y     = "Counties"
  )

region_colors <- c(
  "West" = "#2f7f93",
  "East" = "darkorange"
)


# ============================================================
# 2. THE STANDARD ERROR OF A MARGINAL EFFECT ----
# ============================================================
# Week 6's model, 2021: AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * East_i + beta_3 * Turnout_i * East_i + epsilon_i
interaction_model <- lm(afd ~ turnout * east, data = counties)
interaction_model |>
  tidy()

interaction_beta_1 <- interaction_model |>
  tidy() |>
  filter(term == "turnout") |>
  pull(estimate)
interaction_beta_3 <- interaction_model |>
  tidy() |>
  filter(term == "turnout:east") |>
  pull(estimate)
interaction_p_3 <- interaction_model |>
  tidy() |>
  filter(term == "turnout:east") |>
  pull(p.value)

# moderator Z: dY/dX = beta_1 + beta_3 * Z (here Z = East: 0 West, 1 East)
# SE(dY/dX) = sigma_hat_(dY/dX)
#           = sqrt(Var_hat(beta_1_hat) + Z^2 * Var_hat(beta_3_hat) + 2 * Z * Cov_hat(beta_1_hat, beta_3_hat))
# variance-covariance matrix V_hat: variances on the diagonal, covariances off it
interaction_vcov <- interaction_model |>
  vcov() |>
  as_tibble(rownames = "term")
interaction_vcov

# the three ingredients of SE(dY/dX); SE(beta_j_hat) = sqrt(Var_hat(beta_j_hat)) is the diagonal
var_beta_1 <- interaction_vcov |>
  filter(term == "turnout") |>
  pull(turnout)
var_beta_3 <- interaction_vcov |>
  filter(term == "turnout:east") |>
  pull(`turnout:east`)
cov_beta_1_3 <- interaction_vcov |>
  filter(term == "turnout") |>
  pull(`turnout:east`)
cov_beta_1_3

# dAfD/dTurnout = beta_1_hat + beta_3_hat * East
me_regions <- tibble(
  region = c("West", "East"),
  east   = c(0, 1)
) |>
  mutate(me = interaction_beta_1 + interaction_beta_3 * east)

# SE = sqrt(Var(beta_1) + East^2 * Var(beta_3) + 2 * East * Cov(beta_1, beta_3))
me_regions <- me_regions |>
  mutate(se = sqrt(var_beta_1 + east^2 * var_beta_3 + 2 * east * cov_beta_1_3))
me_regions

# CI_95 = ME +/- t_(n-k-1, 0.975) * SE
interaction_df <- interaction_model |>
  glance() |>
  pull(df.residual)

me_regions <- me_regions |>
  mutate(
    lower = me - qt(0.975, interaction_df) * se,
    upper = me + qt(0.975, interaction_df) * se
  )
me_regions

me_regions |>
  ggplot(aes(x = region, y = me, color = region)) +
  geom_hline(aes(yintercept = 0, linetype = "No relationship")) +
  geom_pointrange(aes(ymin = lower, ymax = upper), linewidth = 1) +
  scale_color_manual(values = region_colors, guide = "none") +
  scale_linetype_manual(values = c("No relationship" = "dashed")) +
  labs(
    title    = "Marginal effect of turnout by region",
    x        = "Region",
    y        = "Change in AfD share per point of turnout",
    color    = NULL,
    linetype = NULL
  )

# slopes(): marginal effects; datagrid() builds the rows (unnamed variables at their mean)
me_regions_slopes <- slopes(
  interaction_model,
  variables = "turnout",
  newdata   = datagrid(east = c(0, 1)),
  df        = "residual" # without it: Normal instead of t with the model's df
)
me_regions_slopes

me_regions_slopes_values <- me_regions_slopes |>
  as_tibble() |>
  select(east, me_slopes = estimate, se_slopes = std.error)

me_regions |>
  select(region, east, me, se) |>
  left_join(me_regions_slopes_values, by = "east")


# ============================================================
# 3. INTERACTIONS WITH A CONTINUOUS MODERATOR ----
# ============================================================
counties |>
  ggplot(aes(x = turnout, y = afd, color = median_age)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_c(breaks = seq(40, 50, 2)) +
  labs(
    title = "AfD support, turnout and median age",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)",
    color = "Median age (years)"
  ) +
  theme(legend.key.width = unit(1.5, "cm")) +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5))

# AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * Age_i + beta_3 * Turnout_i * Age_i + epsilon_i
age_model <- lm(afd ~ turnout * median_age, data = counties)
age_model |>
  tidy()

age_beta_0 <- age_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
age_beta_1 <- age_model |>
  tidy() |>
  filter(term == "turnout") |>
  pull(estimate)
age_beta_2 <- age_model |>
  tidy() |>
  filter(term == "median_age") |>
  pull(estimate)
age_beta_3 <- age_model |>
  tidy() |>
  filter(term == "turnout:median_age") |>
  pull(estimate)

# dAfD/dTurnout = beta_1_hat + beta_3_hat * Age
# lines at the 10th, 50th and 90th percentile of median age
age_levels <- counties |>
  summarize(
    p10 = quantile(median_age, 0.1),
    p50 = quantile(median_age, 0.5),
    p90 = quantile(median_age, 0.9)
  ) |>
  pivot_longer(
    everything(),
    names_to        = "percentile",
    names_prefix    = "p",
    names_transform = as.numeric,
    values_to       = "median_age"
  )
age_levels

age_levels_values <- age_levels |>
  pull(median_age)

turnout_min <- counties |>
  summarize(min(turnout)) |>
  pull()
turnout_max <- counties |>
  summarize(max(turnout)) |>
  pull()
turnout_grid <- seq(turnout_min, turnout_max, length.out = 50)

# AfD_hat = beta_0 + beta_1 * Turnout + beta_2 * Age + beta_3 * Turnout * Age
age_lines <- expand_grid(age_levels, turnout = turnout_grid) |>
  mutate(
    afd_hat   = age_beta_0 + age_beta_1 * turnout + age_beta_2 * median_age + age_beta_3 * turnout * median_age,
    age_label = str_glue("{percentile}th percentile: {round(median_age, 1)} years")
  )

age_min <- counties |>
  summarize(min(median_age)) |>
  pull()
age_max <- counties |>
  summarize(max(median_age)) |>
  pull()
other_age_step <- 0.5

# the same for every half year
other_age_lines <- expand_grid(
  median_age = seq(ceiling(age_min), floor(age_max), by = other_age_step),
  turnout    = turnout_grid
) |>
  mutate(afd_hat = age_beta_0 + age_beta_1 * turnout + age_beta_2 * median_age + age_beta_3 * turnout * median_age)

age_lines_plot <- ggplot(mapping = aes(x = turnout, y = afd_hat)) +
  geom_point(data = counties, aes(y = afd), color = "grey60", alpha = 0.5) +
  geom_line(data = other_age_lines, aes(group = median_age, linetype = "Every half year"), color = "grey40", linewidth = 0.3) +
  geom_line(data = age_lines, aes(color = age_label), linewidth = 1) +
  scale_color_viridis_d(end = 0.85) +
  scale_linetype_manual(values = c("Every half year" = "solid")) +
  guides(color = guide_legend(ncol = 1, order = 1)) +
  labs(
    title    = "Predicted AfD support at different median ages",
    x        = "Turnout (%)",
    y        = "AfD vote share (%)",
    color    = "Median age",
    linetype = NULL
  )
age_lines_plot

# the same lines with marginaleffects
predictions(
  age_model,
  newdata = datagrid(turnout = turnout_grid, median_age = age_levels_values)
)

age_vcov <- age_model |>
  vcov() |>
  as_tibble(rownames = "term")

age_var_beta_1 <- age_vcov |>
  filter(term == "turnout") |>
  pull(turnout)
age_var_beta_3 <- age_vcov |>
  filter(term == "turnout:median_age") |>
  pull(`turnout:median_age`)
age_cov_beta_1_3 <- age_vcov |>
  filter(term == "turnout") |>
  pull(`turnout:median_age`)

# marginal effect is zero at Age_0 = -beta_1_hat / beta_3_hat
age_me_zero <- -age_beta_1 / age_beta_3
age_me_zero

age_grid <- seq(min(age_me_zero, age_min), age_max, length.out = 50)

# SE = sqrt(Var(beta_1) + Age^2 * Var(beta_3) + 2 * Age * Cov(beta_1, beta_3)); CI with t(n-k-1)
age_df <- age_model |>
  glance() |>
  pull(df.residual)

age_me_curve <- tibble(median_age = age_grid) |>
  mutate(
    me    = age_beta_1 + age_beta_3 * median_age,
    se    = sqrt(age_var_beta_1 + median_age^2 * age_var_beta_3 + 2 * median_age * age_cov_beta_1_3),
    lower = me - qt(0.975, age_df) * se,
    upper = me + qt(0.975, age_df) * se
  )
age_me_curve

# band width at its narrowest point and at the oldest county
narrow_band <- age_me_curve |>
  slice_min(se)
wide_band <- age_me_curve |>
  slice_max(median_age)

band_widths <- bind_rows(narrow_band, wide_band) |>
  mutate(
    label       = str_glue("width {round(upper - lower, 2)}"),
    label_hjust = if_else(median_age == age_max, 1, 0.5)
  )

age_me_curve |>
  ggplot(aes(x = median_age)) +
  geom_hline(aes(yintercept = 0, linetype = "No relationship")) +
  geom_vline(aes(xintercept = age_me_zero, linetype = "Marginal effect = 0")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#2f7f93", alpha = 0.25) +
  geom_line(aes(y = me), color = "#2f7f93", linewidth = 1) +
  geom_segment(
    data  = band_widths,
    aes(xend = median_age, y = lower, yend = upper),
    arrow = arrow(ends = "both", length = unit(0.15, "cm"))
  ) +
  geom_label(data = band_widths, aes(y = upper, label = label, hjust = label_hjust), size = 3, vjust = -0.3) +
  geom_rug(data = counties, aes(x = median_age), alpha = 0.4) +
  scale_linetype_manual(values = c("No relationship" = "dashed", "Marginal effect = 0" = "dotted")) +
  labs(
    title    = "Marginal effect of turnout by median age",
    x        = "Median age (years)",
    y        = "Change in AfD share per point of turnout",
    linetype = NULL
  )

slopes(
  age_model,
  variables = "turnout",
  newdata   = datagrid(median_age = age_levels_values),
  df        = "residual"
)

# the whole curve with marginaleffects
slopes(
  age_model,
  variables = "turnout",
  newdata   = datagrid(median_age = age_grid),
  df        = "residual"
)


# ============================================================
# 4. EXPECTED VS. PREDICTED VALUES ----
# ============================================================
# AfD_i = beta_0 + beta_1 * Age_i + epsilon_i,  epsilon_i ~ N(0, sigma^2)
simple_model <- lm(afd ~ median_age, data = counties)
simple_sigma <- simple_model |>
  glance() |>
  pull(sigma)
simple_sigma

# augment(): expected value E(AfD | Age); predicted value adds an error draw from N(0, sigma_hat^2)
simple_values <- simple_model |>
  augment() |>
  mutate(predicted = .fitted + rnorm(n(), 0, simple_sigma))
simple_values

simple_values |>
  ggplot(aes(x = median_age)) +
  geom_point(aes(y = afd, color = "Observed"), alpha = 0.5) +
  geom_point(aes(y = predicted, color = "Predicted: + error draw"), alpha = 0.5) +
  geom_line(aes(y = .fitted, color = "Expected: augment()"), linewidth = 1) +
  scale_color_manual(values = c(
    "Observed"                = "grey60",
    "Predicted: + error draw" = "darkorange",
    "Expected: augment()"     = "#2f7f93"
  )) +
  labs(
    title = "Expected and predicted values",
    x     = "Median age (years)",
    y     = "AfD vote share (%)",
    color = NULL
  )

# the same expected values with marginaleffects
predictions(simple_model)


# ============================================================
# 5. QUANTITIES OF INTEREST: BOOTSTRAPPING THE COEFFICIENTS ----
# ============================================================
# 1. bootstrap samples  2. refit per sample  3. scenario  4. QoI per sample  5. summarize
n_boot <- 1000

boot_samples <- expand_grid(
  sample_id = 1:n_boot,
  counties
) |>
  group_by(sample_id) |>
  slice_sample(prop = 1, replace = TRUE) |>
  ungroup()
boot_samples

# beta_tilde^(r) = OLS estimate on bootstrap sample r; one row per replicate inside rowwise()
boot_coefs <- tibble(replicate = 1:n_boot) |>
  rowwise() |>
  mutate(
    boot_samples |>
      filter(sample_id == replicate) |>
      lm(afd ~ turnout * median_age, data = _) |>
      tidy() |>
      select(term, estimate) |>
      pivot_wider(names_from = term, values_from = estimate)
  ) |>
  ungroup() |>
  rename(intercept = `(Intercept)`) |>
  pivot_longer(-replicate, names_to = "term", values_to = "estimate")
boot_coefs

# the same with infer
boot_coefs_infer <- counties |>
  specify(afd ~ turnout * median_age) |>
  generate(reps = n_boot, type = "bootstrap") |>
  fit()
boot_coefs_infer

# the same with marginaleffects
boot_coefs_marginaleffects <- hypotheses(age_model) |>
  inferences(method = "boot", R = n_boot) |>
  get_draws() |>
  as_tibble() |>
  select(replicate = drawid, term, estimate = draw) |>
  mutate(term = if_else(term == "(Intercept)", "intercept", term))
boot_coefs_marginaleffects

# SE_boot(beta_j_hat) = sd(beta_tilde_j^(1), ..., beta_tilde_j^(R))
boot_se <- boot_coefs |>
  group_by(term) |>
  summarize(se_boot = sd(estimate))

infer_se <- boot_coefs_infer |>
  group_by(term) |>
  summarize(se_infer = sd(estimate))

marginaleffects_se <- boot_coefs_marginaleffects |>
  group_by(term) |>
  summarize(se_marginaleffects = sd(estimate))

age_se <- age_model |>
  tidy() |>
  select(term, se_ols = std.error) |>
  mutate(term = if_else(term == "(Intercept)", "intercept", term))

age_se |>
  left_join(boot_se, by = "term") |>
  left_join(infer_se, by = "term") |>
  left_join(marginaleffects_se, by = "term")

boot_betas <- boot_coefs |>
  pivot_wider(names_from = term, values_from = estimate) |>
  rename(
    beta_0 = intercept,
    beta_1 = turnout,
    beta_2 = median_age,
    beta_3 = `turnout:median_age`
  )
boot_betas

lm_estimate <- tibble(
  beta_1 = age_beta_1,
  beta_3 = age_beta_3
)

boot_betas_plot <- boot_betas |>
  ggplot(aes(x = beta_1, y = beta_3)) +
  geom_point(alpha = 0.3, color = "#2f7f93") +
  geom_point(data = lm_estimate, aes(shape = "Estimate from lm()"), size = 4) +
  scale_shape_manual(values = c("Estimate from lm()" = 4)) +
  labs(
    title = "Bootstrap samples of two coefficients",
    x     = "Turnout coefficient",
    y     = "Interaction coefficient",
    shape = NULL
  )
boot_betas_plot

# Step 3: scenario: mean turnout, young (10th percentile) and old (90th percentile) county
turnout_mean <- counties |>
  summarize(mean(turnout)) |>
  pull()
age_young <- age_levels |>
  filter(percentile == 10) |>
  pull(median_age)
age_old <- age_levels |>
  filter(percentile == 90) |>
  pull(median_age)


# ============================================================
# 6. EXPECTED VALUES AND FIRST DIFFERENCES ----
# ============================================================
# Step 4: E(AfD | Turnout_bar, Age)^(r) = beta_0^(r) + beta_1^(r) * Turnout_bar + beta_2^(r) * Age + beta_3^(r) * Turnout_bar * Age
# FD^(r) = E(AfD | Age_old)^(r) - E(AfD | Age_young)^(r)
boot_qoi <- boot_betas |>
  mutate(
    ev_young = beta_0 + beta_1 * turnout_mean + beta_2 * age_young + beta_3 * turnout_mean * age_young,
    ev_old   = beta_0 + beta_1 * turnout_mean + beta_2 * age_old + beta_3 * turnout_mean * age_old,
    fd       = ev_old - ev_young
  )
boot_qoi

# Step 5: mean and 2.5% / 97.5% quantiles
boot_qoi_long <- boot_qoi |>
  pivot_longer(c(ev_young, ev_old, fd), names_to = "quantity", values_to = "value") |>
  mutate(quantity = fct_inorder(quantity))
boot_qoi_long

qoi_summary <- boot_qoi_long |>
  group_by(quantity) |>
  summarize(
    estimate = mean(value),
    lower    = quantile(value, 0.025),
    upper    = quantile(value, 0.975)
  )
qoi_summary

fd_summary <- qoi_summary |>
  filter(quantity == "fd")
fd_estimate <- fd_summary |>
  pull(estimate)
fd_lower <- fd_summary |>
  pull(lower)
fd_upper <- fd_summary |>
  pull(upper)

qoi_colors <- c(
  ev_young = "#2f7f93",
  ev_old   = "darkorange",
  fd       = "grey40"
)
qoi_labels <- c(
  ev_young = "EV, young county",
  ev_old   = "EV, old county",
  fd       = expression(atop("First difference", E(AfD ~ "|" ~ Age[old]) - E(AfD ~ "|" ~ Age[young])))
)

qoi_plot <- boot_qoi_long |>
  ggplot(aes(x = value, fill = quantity)) +
  geom_histogram(binwidth = 0.05, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = qoi_colors, labels = qoi_labels) +
  labs(
    title = "Bootstrap distributions of three quantities",
    x     = "AfD vote share (%) or difference (points)",
    y     = "Bootstrap samples",
    fill  = NULL
  )
qoi_plot


# ============================================================
# 7. COMPARISON WITH MARGINALEFFECTS ----
# ============================================================
# delta method by default; inferences() for bootstrap or simulation
evs_delta <- predictions(
  age_model,
  newdata = datagrid(turnout = turnout_mean, median_age = c(age_young, age_old)),
  df      = "residual"
)
evs_boot <- evs_delta |>
  inferences(method = "boot", R = n_boot)
evs_simulation <- evs_delta |>
  inferences(method = "simulation", R = n_boot)
evs_boot
evs_simulation

fds_delta <- comparisons(
  age_model,
  variables = list(median_age = c(age_young, age_old)),
  newdata   = datagrid(turnout = turnout_mean),
  df        = "residual"
)
fds_boot <- fds_delta |>
  inferences(method = "boot", R = n_boot)
fds_simulation <- fds_delta |>
  inferences(method = "simulation", R = n_boot)
fds_boot
fds_simulation


# ============================================================
# 8. EXPECTED VALUES OVER A RANGE ----
# ============================================================
range_scenarios <- expand_grid(
  turnout    = turnout_grid,
  median_age = c(age_young, age_old)
)

range_evs <- expand_grid(boot_betas, range_scenarios) |>
  mutate(ev = beta_0 + beta_1 * turnout + beta_2 * median_age + beta_3 * turnout * median_age) |>
  group_by(turnout, median_age) |>
  summarize(
    estimate = mean(ev),
    lower    = quantile(ev, 0.025),
    upper    = quantile(ev, 0.975),
    .groups  = "drop"
  ) |>
  mutate(scenario = if_else(median_age == age_young, "ev_young", "ev_old"))
range_evs

range_labels <- c(
  ev_young = str_glue("Young county: {round(age_young, 1)} years"),
  ev_old   = str_glue("Old county: {round(age_old, 1)} years")
)

range_evs_plot <- range_evs |>
  ggplot(aes(x = turnout, color = scenario, fill = scenario)) +
  geom_point(data = counties, aes(y = afd), color = "grey60", fill = NA, alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
  geom_line(aes(y = estimate), linewidth = 1) +
  scale_color_manual(values = qoi_colors, labels = range_labels, aesthetics = c("color", "fill")) +
  labs(
    title = "Expected AfD support with 95% intervals",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)",
    color = NULL,
    fill  = NULL
  )
range_evs_plot

# the same bands with marginaleffects (estimates from the original fit)
predictions(
  age_model,
  newdata = datagrid(turnout = turnout_grid, median_age = c(age_young, age_old))
) |>
  inferences(method = "boot", R = n_boot)

# marginal effect per bootstrap sample: beta_1^(r) + beta_3^(r) * Age
range_mes <- expand_grid(boot_betas, median_age = age_grid) |>
  mutate(me = beta_1 + beta_3 * median_age) |>
  group_by(median_age) |>
  summarize(
    lower = quantile(me, 0.025),
    upper = quantile(me, 0.975)
  )

range_mes_bands <- range_mes |>
  mutate(method = "Bootstrap")
formula_bands <- age_me_curve |>
  select(median_age, lower, upper) |>
  mutate(method = "SE formula")

bind_rows(range_mes_bands, formula_bands) |>
  ggplot(aes(x = median_age)) +
  geom_hline(aes(yintercept = 0, linetype = "No relationship")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = method), alpha = 0.3) +
  geom_line(data = age_me_curve, aes(y = me), color = "#2f7f93", linewidth = 1) +
  scale_fill_manual(values = c("Bootstrap" = "#2f7f93", "SE formula" = "darkorange")) +
  scale_linetype_manual(values = c("No relationship" = "dashed")) +
  labs(
    title    = "Marginal effect of turnout: two intervals",
    x        = "Median age (years)",
    y        = "Change in AfD share per point of turnout",
    fill     = "95% interval",
    linetype = NULL
  )

# the bootstrap band with marginaleffects
slopes(
  age_model,
  variables = "turnout",
  newdata   = datagrid(median_age = age_grid)
) |>
  inferences(method = "boot", R = n_boot)


# ============================================================
# 9. PREDICTED VALUES ----
# ============================================================
sigma_hat <- age_model |>
  glance() |>
  pull(sigma)
sigma_hat

# PV^(r) = E(AfD | Turnout, Age)^(r) + e^(r),  e^(r) ~ N(0, sigma_hat^2)
boot_pvs <- boot_qoi |>
  mutate(
    pv_young = ev_young + rnorm(n(), 0, sigma_hat),
    pv_old   = ev_old + rnorm(n(), 0, sigma_hat)
  )

pv_summary <- boot_pvs |>
  pivot_longer(c(pv_young, pv_old), names_to = "quantity", values_to = "value") |>
  group_by(quantity) |>
  summarize(
    estimate = mean(value),
    lower    = quantile(value, 0.025),
    upper    = quantile(value, 0.975)
  )
pv_summary

# analytical prediction interval
scenarios <- tibble(
  scenario   = c("young", "old"),
  turnout    = turnout_mean,
  median_age = c(age_young, age_old)
)

age_model |>
  augment(newdata = scenarios, interval = "prediction") |>
  select(scenario, .fitted, .lower, .upper)

ev_pv_draws <- boot_pvs |>
  pivot_longer(c(ev_young, ev_old, pv_young, pv_old), names_to = c("type", "scenario"), names_sep = "_", values_to = "value") |>
  mutate(
    type = if_else(
      type == "ev",
      "Expected value: estimation uncertainty",
      "Predicted value: estimation + fundamental uncertainty"
    ),
    scenario = str_glue("{scenario} county")
  )

ev_pv_draws |>
  ggplot(aes(x = value, fill = scenario)) +
  geom_histogram(binwidth = 0.25, alpha = 0.6, position = "identity") +
  facet_wrap(~type, ncol = 1) +
  scale_fill_manual(values = c("young county" = "#2f7f93", "old county" = "darkorange")) +
  labs(
    title = "Expected and predicted AfD shares",
    x     = "AfD vote share (%)",
    y     = "Bootstrap samples",
    fill  = NULL
  )

# prediction bands over the turnout range
range_pvs <- expand_grid(boot_betas, range_scenarios) |>
  mutate(
    ev = beta_0 + beta_1 * turnout + beta_2 * median_age + beta_3 * turnout * median_age,
    pv = ev + rnorm(n(), 0, sigma_hat)
  ) |>
  group_by(turnout, median_age) |>
  summarize(
    lower   = quantile(pv, 0.025),
    upper   = quantile(pv, 0.975),
    .groups = "drop"
  ) |>
  mutate(scenario = if_else(median_age == age_young, "ev_young", "ev_old"))

range_evs_plot +
  geom_ribbon(data = range_pvs, aes(ymin = lower, ymax = upper), alpha = 0.08, linetype = "dashed", linewidth = 0.4) +
  labs(
    title    = "Expected and predicted AfD support with 95% intervals",
    subtitle = "Narrow bands: expected values. Wide dashed bands: predicted values."
  )


# ============================================================
# 10. TRY IT: THE MARGINAL EFFECT OF AGE ----
# ============================================================
# Your task: report the marginal effect of median age on the AfD share
# in a county with 70% and in one with 80% turnout, each with a 95%
# bootstrap interval from boot_betas. Check both against slopes().
# dAfD/dAge = beta_2_hat + beta_3_hat * Turnout


# ============================================================
# 11. TRY IT: A PREDICTION FOR MANNHEIM ----
# ============================================================
# Mannheim has the county code 08222.
# Your task: give the AfD share the age model predicts for Mannheim with
# a 95% interval that includes the fundamental uncertainty, and say
# whether Mannheim's actual result lies inside it.


# ============================================================
# 12. TRY IT: FIRST DIFFERENCES IN TURNOUT ----
# ============================================================
# Your task: in the young and in the old county of our scenario, how
# much does the expected AfD share change when turnout rises from 72% to
# 80%? Give both first differences with 95% bootstrap intervals, and the
# difference between the two first differences with its interval.
