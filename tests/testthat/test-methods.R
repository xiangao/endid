test_that("print.endid works for common_timing", {
  set.seed(42)
  N <- 40; T_total <- 6; tpost1 <- 4
  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  D <- rep(c(0, 1), each = N / 2 * T_total)
  post <- as.integer(time >= tpost1)
  alpha_i <- rep(rnorm(N), each = T_total)
  y <- alpha_i + 0.5 * (post & D == 1) + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, post = post,
                   d = rep(rep(c(0, 1), each = N / 2), each = T_total))

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", post = "post",
                  dvar = "d", rolling = "demean",
                  noise_dim = 5, hidden_dim = 50, num_layer = 2,
                  num_epochs = 200, lr = 1e-3, nboot = 10, silent = TRUE)

  out <- capture.output(print(result))
  expect_true(any(grepl("ATT", out)))
  expect_true(any(grepl("common_timing", out)))
})

test_that("summary.endid returns correct structure", {
  set.seed(42)
  N <- 40; T_total <- 6; tpost1 <- 4
  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  D <- rep(c(0, 1), each = N / 2 * T_total)
  post <- as.integer(time >= tpost1)
  alpha_i <- rep(rnorm(N), each = T_total)
  y <- alpha_i + 0.5 * (post & D == 1) + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, post = post,
                   d = rep(rep(c(0, 1), each = N / 2), each = T_total))

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", post = "post",
                  dvar = "d", rolling = "demean",
                  noise_dim = 5, hidden_dim = 50, num_layer = 2,
                  num_epochs = 200, lr = 1e-3, nboot = 10, silent = TRUE)

  s <- summary(result)
  expect_s3_class(s, "summary.endid")
  expect_true(is.data.frame(s$att_table))
  expect_true(is.data.frame(s$qte_table))
  expect_null(s$cohort_table)  # common timing has no cohort table

  # Print summary
  out <- capture.output(print(s))
  expect_true(any(grepl("ATT", out)))
  expect_true(any(grepl("QTE", out)))
})

test_that("plot.endid produces a ggplot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  N <- 40; T_total <- 6; tpost1 <- 4
  unit <- rep(1:N, each = T_total)
  time <- rep(1:T_total, times = N)
  D <- rep(c(0, 1), each = N / 2 * T_total)
  post <- as.integer(time >= tpost1)
  alpha_i <- rep(rnorm(N), each = T_total)
  y <- alpha_i + 0.5 * (post & D == 1) + rnorm(N * T_total, sd = 0.3)

  df <- data.frame(unit = unit, time = time, y = y, post = post,
                   d = rep(rep(c(0, 1), each = N / 2), each = T_total))

  result <- endid(df, y = "y", ivar = "unit", tvar = "time", post = "post",
                  dvar = "d", rolling = "demean",
                  noise_dim = 5, hidden_dim = 50, num_layer = 2,
                  num_epochs = 200, lr = 1e-3, nboot = 10, silent = TRUE)

  p <- plot(result)
  expect_s3_class(p, "ggplot")
})
