test_that("endid works on common-timing panel data", {
  set.seed(42)

  # Simple panel: 40 units, 6 periods, treatment at t=4
  N <- 40; T_total <- 6; tpost1 <- 4
  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  D <- rep(c(0, 1), each = N / 2 * T_total)
  post <- as.integer(time >= tpost1)
  alpha_i <- rep(rnorm(N), each = T_total)
  treat_effect <- 0.5
  y <- alpha_i + treat_effect * (post & D == 1) + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, post = post,
                   d = rep(rep(c(0, 1), each = N / 2), each = T_total))

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", post = "post",
                  dvar = "d",
                  rolling = "demean",
                  noise_dim = 5, hidden_dim = 50, num_layer = 2,
                  num_epochs = 50, lr = 1e-3, nboot = 2, silent = TRUE)

  expect_s3_class(result, "endid")
  expect_equal(result$design, "common_timing")
  expect_true(is.numeric(result$att_overall$att))
  expect_true(is.data.frame(result$qte))
  expect_true(!is.null(result$engression_model))
  expect_true(!is.null(result$cross_section))
})

test_that("endid ATT is reasonable on simple DGP", {
  set.seed(123)
  N <- 60; T_total <- 6; tpost1 <- 4
  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  D <- rep(c(0, 1), each = N / 2 * T_total)
  post <- as.integer(time >= tpost1)
  alpha_i <- rep(rnorm(N, sd = 0.5), each = T_total)
  treat_effect <- 1.0
  y <- alpha_i + treat_effect * (post & D == 1) + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, post = post,
                   d = rep(rep(c(0, 1), each = N / 2), each = T_total))

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", post = "post",
                  dvar = "d",
                  rolling = "demean",
                  noise_dim = 5, hidden_dim = 50, num_layer = 2,
                  num_epochs = 100, lr = 1e-3, nboot = 2, silent = TRUE)

  # Should be within 0.8 of 1.0 (very few epochs tolerance)
  expect_true(abs(result$att_overall$att - 1.0) < 0.8)
})
