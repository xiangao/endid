test_that("bootstrap_endid returns correct structure", {
  set.seed(42)
  n <- 200
  D <- rep(c(0, 1), each = n / 2)
  Y <- D * 0.5 + rnorm(n, sd = 0.3)

  result <- bootstrap_endid(
    Y = Y, D = D, controls = NULL,
    nboot = 20,
    noise_dim = 5, hidden_dim = 50, num_layer = 2,
    num_epochs = 100, lr = 1e-3, silent = TRUE
  )

  expect_true(is.numeric(result$se))
  expect_length(result$se, 1)
  expect_true(result$se > 0)
  expect_true(is.numeric(result$ci_lower))
  expect_true(is.numeric(result$ci_upper))
  expect_true(result$ci_lower < result$ci_upper)
  expect_true(is.numeric(result$att_mean))
  expect_true(is.numeric(result$qte_se))
  expect_true(is.numeric(result$qte_mean))
  expect_true(is.matrix(result$qte_boot_mat))
})
