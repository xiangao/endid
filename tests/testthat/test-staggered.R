test_that("endid_staggered works on synthetic staggered panel", {
  set.seed(42)

  # 60 units, 10 periods, 3 cohorts (g=4,6,8), 20 never-treated
  N <- 40
  T_total <- 6
  n_per <- N / 4  # 10 per group
  gvar_vec <- c(rep(4, n_per), rep(6, n_per), rep(8, n_per), rep(NA, n_per))

  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  gvar <- rep(gvar_vec, each = T_total)
  alpha_i <- rep(rnorm(N, sd = 0.5), each = T_total)

  # Treatment effect = 0.8 for all cohorts
  treat_effect <- 0.8
  post_treat <- !is.na(gvar) & time >= gvar
  y <- alpha_i + treat_effect * post_treat + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, gvar = gvar)

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", gvar = "gvar",
                  rolling = "demean",
                  noise_dim = 5, hidden_dim = 20, num_layer = 2,
                  num_epochs = 20, lr = 1e-3, nboot = 2, silent = TRUE)

  expect_s3_class(result, "endid")
  expect_equal(result$design, "staggered")
  expect_true(is.numeric(result$att_overall$att))
  expect_true(is.numeric(result$att_overall$se))
  expect_true(is.data.frame(result$qte))
  expect_true(length(result$cohort_results) >= 1)
})

test_that("endid_staggered ATT is reasonable on simple DGP", {
  set.seed(123)

  N <- 40
  T_total <- 6
  n_per <- N / 4
  gvar_vec <- c(rep(4, n_per), rep(6, n_per), rep(8, n_per), rep(NA, n_per))

  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  gvar <- rep(gvar_vec, each = T_total)
  alpha_i <- rep(rnorm(N, sd = 0.3), each = T_total)

  treat_effect <- 1.0
  post_treat <- !is.na(gvar) & time >= gvar
  y <- alpha_i + treat_effect * post_treat + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, gvar = gvar)

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", gvar = "gvar",
                  rolling = "demean",
                  noise_dim = 5, hidden_dim = 20, num_layer = 2,
                  num_epochs = 50, lr = 1e-3, nboot = 2, silent = TRUE)

  # ATT should be within 1.0 of 1.0 (very few epochs)
  expect_true(abs(result$att_overall$att - 1.0) < 1.0)
})

test_that("endid_staggered works with not_yet_treated control group", {
  set.seed(99)

  N <- 30
  T_total <- 6
  n_per <- N / 3  # No never-treated group
  gvar_vec <- c(rep(4, n_per), rep(6, n_per), rep(8, n_per))

  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  gvar <- rep(gvar_vec, each = T_total)
  alpha_i <- rep(rnorm(N, sd = 0.5), each = T_total)

  treat_effect <- 0.5
  post_treat <- time >= gvar
  y <- alpha_i + treat_effect * post_treat + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, gvar = gvar)

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", gvar = "gvar",
                  rolling = "demean", control_group = "not_yet_treated",
                  noise_dim = 5, hidden_dim = 20, num_layer = 2,
                  num_epochs = 20, lr = 1e-3, nboot = 2, silent = TRUE)

  expect_s3_class(result, "endid")
  expect_equal(result$design, "staggered")
  expect_equal(result$control_group, "not_yet_treated")
})

test_that("endid_staggered errors with no never-treated and never_treated control", {
  N <- 30
  T_total <- 6
  gvar_vec <- c(rep(3, 15), rep(5, 15))

  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  gvar <- rep(gvar_vec, each = T_total)
  y <- rnorm(N * T_total)

  df <- data.frame(unit = unit, time = time, y = y, gvar = gvar)

  expect_error(endid(df, y = "y", ivar = "unit", tvar = "time", gvar = "gvar",
                     rolling = "demean", control_group = "never_treated",
                     num_epochs = 1, nboot = 1))
})
