# ============================================================
# WEEK 5 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, modelsummary, ggdag, dagitty)


# ============================================================
# 1. THE MULTIPLE REGRESSION MODEL ----
# ============================================================
# model: Y_i = beta_0 + beta_1 * X_i1 + beta_2 * X_i2 + ... + beta_k * X_ik + epsilon_i
# OLS minimizes: sum of (y_i - beta_0_hat - beta_1_hat * x_i1 - ... - beta_k_hat * x_ik)^2
# beta_j_hat: average change in Y for a one-unit increase in X_j,
#             holding all other predictors constant (statistical control)


# ============================================================
# 2. DATA: GERMAN ELECTION RESULTS (2025) ----
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

counties |>
  count(region)

region_colors <- c(
  "West" = "#2f7f93",
  "East" = "darkorange"
)


# ============================================================
# 3. TURNOUT AND AFD SUPPORT ----
# ============================================================
# model: AfD_i = beta_0 + beta_1 * Turnout_i + epsilon_i

counties |>
  ggplot(aes(x = turnout, y = afd)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "AfD vote share vs. turnout by county, 2025",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)"
  )

bivariate_model <- lm(afd ~ turnout, data = counties)
bivariate_model |>
  tidy()

bivariate_beta_0 <- bivariate_model |>
  tidy() |>
  filter(term == "(Intercept)") |>
  pull(estimate)
bivariate_beta_1 <- bivariate_model |>
  tidy() |>
  filter(term == "turnout") |>
  pull(estimate)
bivariate_beta_0
bivariate_beta_1
# fitted line: AfD_hat_i = 131 - 1.32 * Turnout_i

counties |>
  ggplot(aes(x = turnout, y = afd, color = region)) +
  geom_point(alpha = 0.6) +
  geom_abline(
    intercept = bivariate_beta_0,
    slope     = bivariate_beta_1,
    linetype  = "dashed"
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "AfD vote share vs. turnout by region, 2025",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)",
    color = NULL
  )

region_means <- counties |>
  group_by(region) |>
  summarize(
    mean_afd     = mean(afd),
    mean_turnout = mean(turnout)
  )
region_means


# ============================================================
# 4. STATISTICAL CONTROL BY HAND ----
# ============================================================
# purge east out of both variables, then regress the residuals on each other

# step 1: AfD_i = gamma_0 + gamma_1 * East_i + u_i
# fitted values are the two region means; residuals: AfD_tilde_i = AfD_i - AfD_hat_i
afd_east_model <- lm(afd ~ east, data = counties)
afd_east_model |>
  tidy()

partialled <- afd_east_model |>
  augment(data = counties) |>
  select(county_code, region, afd, turnout, afd_resid = .resid)
partialled

# step 2: Turnout_i = delta_0 + delta_1 * East_i + v_i
# residuals: Turnout_tilde_i = Turnout_i - Turnout_hat_i
turnout_east_model <- lm(turnout ~ east, data = counties)
turnout_east_model |>
  tidy()

partialled <- turnout_east_model |>
  augment(data = partialled) |>
  select(county_code, region, afd, turnout, afd_resid, turnout_resid = .resid)
partialled

# step 3: AfD_tilde_i = alpha_0 + beta_1 * Turnout_tilde_i + w_i
original_panel <- partialled |>
  mutate(
    panel = "Original",
    x     = turnout,
    y     = afd
  )
residual_panel <- partialled |>
  mutate(
    panel = "East-West difference removed",
    x     = turnout_resid,
    y     = afd_resid
  )

partialled_panels <- bind_rows(original_panel, residual_panel) |>
  mutate(panel = factor(panel, levels = c("Original", "East-West difference removed")))

partialled_panels |>
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = region), alpha = 0.6) +
  geom_smooth(
    method   = "lm",
    formula  = y ~ x,
    se       = FALSE,
    color    = "black",
    linetype = "dashed"
  ) +
  facet_wrap(~ panel, scales = "free") +
  scale_color_manual(values = region_colors) +
  labs(
    title = "AfD vs. turnout, before and after removing East",
    x     = "Turnout (percentage points)",
    y     = "AfD share (percentage points)",
    color = NULL
  )

partialled_model <- lm(afd_resid ~ turnout_resid, data = partialled)
partialled_model |>
  tidy()

partialled_beta_1 <- partialled_model |>
  tidy() |>
  filter(term == "turnout_resid") |>
  pull(estimate)
partialled_beta_1


# ============================================================
# 5. MULTIPLE REGRESSION WITH lm() ----
# ============================================================
# model: AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * East_i + epsilon_i

control_model <- lm(afd ~ turnout + east, data = counties)
control_model |>
  tidy()

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
control_beta_0
control_beta_1
control_beta_2
# fitted line: AfD_hat_i = 62.2 - 0.514 * Turnout_i + 16 * East_i

# same turnout slope as the three steps by hand
tibble(
  three_steps = partialled_beta_1,
  lm          = control_beta_1
)

# a dummy shifts the intercept: one line per region, same slope
#   West: AfD_hat_i = beta_0_hat + beta_1_hat * Turnout_i
#   East: AfD_hat_i = (beta_0_hat + beta_2_hat) + beta_1_hat * Turnout_i
control_fit <- control_model |>
  augment(data = counties)

control_fit |>
  ggplot(aes(x = turnout, color = region)) +
  geom_point(aes(y = afd), alpha = 0.5) +
  geom_line(aes(y = .fitted), linewidth = 1) +
  geom_abline(
    intercept = bivariate_beta_0,
    slope     = bivariate_beta_1,
    linetype  = "dashed"
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "AfD vote share and turnout, controlling for East",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)",
    color = NULL
  )

# adjusted R^2 penalizes each additional predictor
bivariate_glance <- bivariate_model |>
  glance() |>
  mutate(model = "Turnout only")
control_glance <- control_model |>
  glance() |>
  mutate(model = "Turnout + East")

bind_rows(bivariate_glance, control_glance) |>
  select(model, r.squared, adj.r.squared)

turnout_models <- list(
  "Turnout only"   = bivariate_model,
  "Turnout + East" = control_model
)

modelsummary(
  turnout_models,
  output    = "tinytable",
  coef_map  = c(
    "(Intercept)" = "Intercept",
    "turnout"     = "Turnout (%)",
    "east"        = "East"
  ),
  statistic = NULL,
  stars     = FALSE,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  fmt       = 3
)


# ============================================================
# 6. DATA: POLLING STATIONS ON ELECTION DAY ----
# ============================================================
# which controls belong in a model is a causal question the data can't answer
# 300 polling districts: district, eligible, last_turnout, booths, wait, observer
# booths are allocated by eligible voters; observers go to every station with
# a queue over 12 minutes and to every 6-booth station

stations <- read_csv("w05/polling_stations.csv")
stations

stations |>
  count(booths)


# ============================================================
# 7. CAUSAL GRAPHS ----
# ============================================================
# DAG: nodes are variables, arrows direct causal effects, no cycles
# backdoor path: a path between X and Y that starts with an arrow into X
# confounder Z: X <- Z -> Y, open; controlling for Z closes it
# collider K:   X -> K <- Y, closed; controlling for K opens it
# adjustment set: controls that close every backdoor path without opening a new one
# DAG model code also works at https://www.dagitty.net/dags.html ("Model code" box)

source("include/plot_dag.R")

node_labels <- tribble(
  ~name,           ~label,
  "booths",        "Booths",
  "wait",          "Waiting time",
  "eligible",      "Eligible voters",
  "observer",      "Observer visit",
  "usual_turnout", "Usual turnout",
  "last_turnout",  "Last turnout"
)


# ============================================================
# 8. TWO COMPETING DAGS ----
# ============================================================
# usual turnout: the district's voting habit, known to the office but never recorded
# disputed arrow: usual_turnout -> booths

# DAG A: strict booths-per-voter rule, no arrow
dag_a <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [pos="1.5,-1.7"]
  observer      [pos="1.5,-0.75"]
  usual_turnout [latent, pos="1.5,1.2"]
  last_turnout  [pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
}')

plot_dag(dag_a, labels = node_labels)

# DAG B: officials add booths where turnout is usually high
dag_b <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [pos="1.5,-1.7"]
  observer      [pos="1.5,-0.75"]
  usual_turnout [latent, pos="1.5,1.2"]
  last_turnout  [pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
  usual_turnout -> booths
}')

plot_dag(dag_b, labels = node_labels)

# minimal adjustment sets among the observed variables
adjustmentSets(dag_a)

# none under DAG B: only usual turnout itself closes its backdoor path
dag_b |>
  adjustmentSets() |>
  length()


# ============================================================
# 9. 5 MODEL VARIANTS ----
# ============================================================
# under DAG B; controls marked [adjusted] in each DAG's model code
# Elig = eligible voters, T-Usual = usual turnout, T-Last = last turnout, Obs = observer visit
# * usual turnout is unobserved in practice: variant 3 is impossible with real data

# predicted waiting time for 1 to 6 booths, every control at its average
prediction_grid <- stations |>
  summarize(
    eligible     = mean(eligible),
    last_turnout = mean(last_turnout)
  ) |>
  expand_grid(booths = 1:6)
prediction_grid

variant_colors <- c(
  "1: none"                         = "grey50",
  "2: Elig"                         = "#3a9ad9",
  "3: Elig, T-Usual*"               = "forestgreen",
  "4: Elig, T-Last"                 = "darkorchid",
  "5: Elig, T-Last, Obs (no visit)" = "goldenrod",
  "5: Elig, T-Last, Obs (visit)"    = "darkorange"
)

# true effect of one booth (red line in the coefficient plots), revealed at the end of the lab
true_effect <- -3

# --- variant 1: no controls ---
# model: Wait_i = beta_0 + beta_1 * Booths_i + epsilon_i

dag_v1 <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [pos="1.5,-1.7"]
  observer      [pos="1.5,-0.75"]
  usual_turnout [latent, pos="1.5,1.2"]
  last_turnout  [pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
  usual_turnout -> booths
}')

plot_dag(dag_v1, labels = node_labels)

# every path between booths and wait, open or closed given the controls
paths(dag_v1, Z = adjustedNodes(dag_v1)) |>
  as_tibble()

isAdjustmentSet(dag_v1, adjustedNodes(dag_v1))

model_v1 <- lm(wait ~ booths, data = stations)
model_v1 |>
  tidy()

beta_v1 <- model_v1 |>
  tidy() |>
  filter(term == "booths") |>
  pull(estimate)
beta_v1

lines_v1 <- model_v1 |>
  augment(newdata = prediction_grid) |>
  mutate(model = "1: none")

stations |>
  ggplot(aes(x = booths)) +
  geom_jitter(aes(y = wait), width = 0.15, height = 0, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), data = lines_v1, linewidth = 1) +
  scale_color_manual(values = variant_colors) +
  labs(
    title = "Waiting time vs. booths, variant 1",
    x     = "Voting booths",
    y     = "Waiting time (minutes)",
    color = NULL
  )

# modelplot(draw = FALSE): estimates and 95% CIs as a tibble
variant_models <- list("1: none" = model_v1)

modelplot(variant_models, coef_map = c(booths = "Booths"), draw = FALSE) |>
  ggplot(aes(x = estimate, y = fct_rev(model))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = true_effect, color = "firebrick", linewidth = 1) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  labs(
    title = "Estimated vs. true effect of one booth",
    x     = "Minutes of waiting time",
    y     = NULL
  )

# --- variant 2: controlling for eligible voters (confounder) ---
# model: Wait_i = beta_0 + beta_1 * Booths_i + beta_2 * Eligible_i + epsilon_i

dag_v2 <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [adjusted, pos="1.5,-1.7"]
  observer      [pos="1.5,-0.75"]
  usual_turnout [latent, pos="1.5,1.2"]
  last_turnout  [pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
  usual_turnout -> booths
}')

plot_dag(dag_v2, labels = node_labels)

paths(dag_v2, Z = adjustedNodes(dag_v2)) |>
  as_tibble()

isAdjustmentSet(dag_v2, adjustedNodes(dag_v2))

model_v2 <- lm(wait ~ booths + eligible, data = stations)
model_v2 |>
  tidy()

beta_v2 <- model_v2 |>
  tidy() |>
  filter(term == "booths") |>
  pull(estimate)
beta_v2

lines_v2 <- model_v2 |>
  augment(newdata = prediction_grid) |>
  mutate(model = "2: Elig")

lines_v1_v2 <- bind_rows(lines_v1, lines_v2)

stations |>
  ggplot(aes(x = booths)) +
  geom_jitter(aes(y = wait), width = 0.15, height = 0, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), data = lines_v1_v2, linewidth = 1) +
  scale_color_manual(values = variant_colors) +
  labs(
    title = "Waiting time vs. booths, variants 1 and 2",
    x     = "Voting booths",
    y     = "Waiting time (minutes)",
    color = NULL
  )

variant_models <- list(
  "1: none" = model_v1,
  "2: Elig" = model_v2
)

modelplot(variant_models, coef_map = c(booths = "Booths"), draw = FALSE) |>
  ggplot(aes(x = estimate, y = fct_rev(model))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = true_effect, color = "firebrick", linewidth = 1) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  labs(
    title = "Estimated vs. true effect of one booth",
    x     = "Minutes of waiting time",
    y     = NULL
  )

# --- variant 3: usual turnout (unobserved confounder), imagined observed ---
# model: Wait_i = beta_0 + beta_1 * Booths_i + beta_2 * Eligible_i + beta_3 * UsualTurnout_i + epsilon_i

dag_v3 <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [adjusted, pos="1.5,-1.7"]
  observer      [pos="1.5,-0.75"]
  usual_turnout [adjusted, pos="1.5,1.2"]
  last_turnout  [pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
  usual_turnout -> booths
}')

plot_dag(dag_v3, labels = node_labels)

paths(dag_v3, Z = adjustedNodes(dag_v3)) |>
  as_tibble()

isAdjustmentSet(dag_v3, adjustedNodes(dag_v3))

# the office's usual turnout records, joined by district
stations_usual <- read_csv("w05/usual_turnout.csv") |>
  right_join(stations, by = "district")
stations_usual

model_v3 <- lm(wait ~ booths + eligible + usual_turnout, data = stations_usual)
model_v3 |>
  tidy()

beta_v3 <- model_v3 |>
  tidy() |>
  filter(term == "booths") |>
  pull(estimate)
beta_v3

usual_grid <- stations_usual |>
  summarize(usual_turnout = mean(usual_turnout)) |>
  expand_grid(prediction_grid)

lines_v3 <- model_v3 |>
  augment(newdata = usual_grid) |>
  mutate(model = "3: Elig, T-Usual*")

lines_v2_v3 <- bind_rows(lines_v2, lines_v3)

stations |>
  ggplot(aes(x = booths)) +
  geom_jitter(aes(y = wait), width = 0.15, height = 0, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), data = lines_v2_v3, linewidth = 1) +
  scale_color_manual(values = variant_colors) +
  labs(
    title   = "Waiting time vs. booths, variants 2 and 3",
    x       = "Voting booths",
    y       = "Waiting time (minutes)",
    color   = NULL,
    caption = "* Usual turnout is unobserved in practice: impossible with real data"
  )

variant_models <- list(
  "1: none"           = model_v1,
  "2: Elig"           = model_v2,
  "3: Elig, T-Usual*" = model_v3
)

modelplot(variant_models, coef_map = c(booths = "Booths"), draw = FALSE) |>
  ggplot(aes(x = estimate, y = fct_rev(model))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = true_effect, color = "firebrick", linewidth = 1) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  labs(
    title   = "Estimated vs. true effect of one booth",
    x       = "Minutes of waiting time",
    y       = NULL,
    caption = "* Usual turnout is unobserved in practice: impossible with real data"
  )

# --- variant 4: last turnout as a proxy for usual turnout ---
# model: Wait_i = beta_0 + beta_1 * Booths_i + beta_2 * Eligible_i + beta_3 * LastTurnout_i + epsilon_i

dag_v4 <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [adjusted, pos="1.5,-1.7"]
  observer      [pos="1.5,-0.75"]
  usual_turnout [latent, pos="1.5,1.2"]
  last_turnout  [adjusted, pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
  usual_turnout -> booths
}')

plot_dag(dag_v4, labels = node_labels)

paths(dag_v4, Z = adjustedNodes(dag_v4)) |>
  as_tibble()

isAdjustmentSet(dag_v4, adjustedNodes(dag_v4))

model_v4 <- lm(wait ~ booths + eligible + last_turnout, data = stations)
model_v4 |>
  tidy()

beta_v4 <- model_v4 |>
  tidy() |>
  filter(term == "booths") |>
  pull(estimate)
beta_v4

lines_v4 <- model_v4 |>
  augment(newdata = prediction_grid) |>
  mutate(model = "4: Elig, T-Last")

lines_v2_v4 <- bind_rows(lines_v2, lines_v4)

stations |>
  ggplot(aes(x = booths)) +
  geom_jitter(aes(y = wait), width = 0.15, height = 0, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), data = lines_v2_v4, linewidth = 1) +
  scale_color_manual(values = variant_colors) +
  labs(
    title = "Waiting time vs. booths, variants 2 and 4",
    x     = "Voting booths",
    y     = "Waiting time (minutes)",
    color = NULL
  )

variant_models <- list(
  "1: none"           = model_v1,
  "2: Elig"           = model_v2,
  "3: Elig, T-Usual*" = model_v3,
  "4: Elig, T-Last"   = model_v4
)

modelplot(variant_models, coef_map = c(booths = "Booths"), draw = FALSE) |>
  ggplot(aes(x = estimate, y = fct_rev(model))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = true_effect, color = "firebrick", linewidth = 1) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  labs(
    title   = "Estimated vs. true effect of one booth",
    x       = "Minutes of waiting time",
    y       = NULL,
    caption = "* Usual turnout is unobserved in practice: impossible with real data"
  )

# --- variant 5: adding the observer visit (collider) ---
# model: Wait_i = beta_0 + beta_1 * Booths_i + beta_2 * Eligible_i + beta_3 * LastTurnout_i
#                 + beta_4 * Observer_i + epsilon_i

dag_v5 <- dagitty('dag {
  booths        [exposure, pos="0,0"]
  wait          [outcome, pos="3,0"]
  eligible      [adjusted, pos="1.5,-1.7"]
  observer      [adjusted, pos="1.5,-0.75"]
  usual_turnout [latent, pos="1.5,1.2"]
  last_turnout  [adjusted, pos="3,1.6"]
  eligible      -> booths
  eligible      -> wait
  booths        -> wait
  booths        -> observer
  wait          -> observer
  usual_turnout -> wait
  usual_turnout -> last_turnout
  usual_turnout -> booths
}')

plot_dag(dag_v5, labels = node_labels)

# controlling for the collider opens booths -> observer <- wait
paths(dag_v5, Z = adjustedNodes(dag_v5)) |>
  as_tibble()

isAdjustmentSet(dag_v5, adjustedNodes(dag_v5))

model_v5 <- lm(wait ~ booths + eligible + last_turnout + observer, data = stations)
model_v5 |>
  tidy()

beta_v5 <- model_v5 |>
  tidy() |>
  filter(term == "booths") |>
  pull(estimate)
beta_v5

# one line for visited, one for unvisited stations
lines_v5 <- model_v5 |>
  augment(newdata = expand_grid(prediction_grid, observer = 0:1)) |>
  mutate(model = if_else(observer == 1, "5: Elig, T-Last, Obs (visit)", "5: Elig, T-Last, Obs (no visit)"))

lines_v4_v5 <- bind_rows(lines_v4, lines_v5)

stations |>
  ggplot(aes(x = booths)) +
  geom_jitter(aes(y = wait), width = 0.15, height = 0, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = model), data = lines_v4_v5, linewidth = 1) +
  scale_color_manual(values = variant_colors) +
  labs(
    title = "Waiting time vs. booths, variants 4 and 5",
    x     = "Voting booths",
    y     = "Waiting time (minutes)",
    color = NULL
  )

variant_models <- list(
  "1: none"              = model_v1,
  "2: Elig"              = model_v2,
  "3: Elig, T-Usual*"    = model_v3,
  "4: Elig, T-Last"      = model_v4,
  "5: Elig, T-Last, Obs" = model_v5
)

modelplot(variant_models, coef_map = c(booths = "Booths"), draw = FALSE) |>
  ggplot(aes(x = estimate, y = fct_rev(model))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = true_effect, color = "firebrick", linewidth = 1) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2f7f93") +
  geom_point(size = 3, color = "#2f7f93") +
  labs(
    title   = "Estimated vs. true effect of one booth",
    x       = "Minutes of waiting time",
    y       = NULL,
    caption = "* Usual turnout is unobserved in practice: impossible with real data"
  )


# ============================================================
# 10. OVERVIEW ----
# ============================================================

modelsummary(
  variant_models,
  output    = "tinytable",
  coef_map  = c(
    "booths"        = "Booths",
    "eligible"      = "Eligible voters",
    "usual_turnout" = "Usual turnout (%)",
    "last_turnout"  = "Last turnout (%)",
    "observer"      = "Observer visit"
  ),
  statistic = NULL,
  stars     = FALSE,
  gof_map   = c("nobs", "r.squared"),
  fmt       = 3,
  notes     = "* Usual turnout is unobserved in practice: variant 3 is impossible with real data."
)
# R^2 rises with every control: a better fit says nothing about whether a control belongs

# isAdjustmentSet(): is the set of controls we chose valid? all five variants in one run
variant_dags <- tibble(
  variant = names(variant_models),
  dag     = list(dag_v1, dag_v2, dag_v3, dag_v4, dag_v5)
)

variant_dags |>
  rowwise() |>
  mutate(
    controls = str_flatten_comma(adjustedNodes(dag)),
    valid    = isAdjustmentSet(dag, adjustedNodes(dag))
  ) |>
  ungroup() |>
  select(variant, controls, valid)

# adjustmentSets(): finds the valid sets from the DAG alone, ignores [adjusted] marks
dag_b |>
  adjustmentSets() |>
  length()

# usual turnout observed: one minimal set
adjustmentSets(dag_v3)

# every valid set, not just the smallest ones
adjustmentSets(dag_v3, type = "all")


# ============================================================
# 11. THE TRUE EFFECT ----
# ============================================================
# each booth estimate with its 95% CI, as a share of the true effect,
# and whether the CI covers the true effect

variant_estimates <- variant_models |>
  modelplot(
    coef_map = c(booths = "Booths"),
    draw     = FALSE
  ) |>
  rowwise() |>
  mutate(
    share_of_true  = estimate / true_effect,
    true_within_ci = between(true_effect, conf.low, conf.high)
  ) |>
  ungroup() |>
  select(model, estimate, conf.low, conf.high, share_of_true, true_within_ci)
variant_estimates


# ============================================================
# 12. TRY IT: AFD SUPPORT, TURNOUT AND DENSITY ----
# ============================================================
# Your task: estimate the turnout slope for AfD support while holding
# population density constant (on a log scale, as in Week 4) instead of
# East. Purge log(density) from both variables by hand, regress the
# residuals on each other, and confirm the slope with lm(). How does it
# compare to the slope with east held constant?


# ============================================================
# 13. TRY IT: THE MODEL VARIANTS UNDER DAG A ----
# ============================================================
# Your task: suppose DAG A is the true DAG instead. For the control
# sets of model variants 1, 2, 4 and 5 (the ones that don't need usual
# turnout), check with isAdjustmentSet() whether each would be a valid
# adjustment set under DAG A and under DAG B. Which variants would then
# have estimated the effect without bias?


# ============================================================
# 14. TRY IT: A CARE HOME IN THE DISTRICT ----
# ============================================================
# Your task: some districts include a care home whose residents often
# need assistance in the booth, which slows down the queue. Add a node
# care_home to DAG B (with an arrow into waiting time only) and plot it.
# Would you need to control for it? Would controlling for it hurt? Use
# paths() and adjustmentSets() to argue.
