test_that("fit_engression_cs returns correct structure", {
  set.seed(42)
  n <- 200
  D <- rep(c(0, 1), each = n / 2)
  Y <- D * 0.5 + rnorm(n, sd = 0.3)

  result <- fit_engression_cs(
    Y = Y, D = D, controls = NULL,
    noise_dim = 5, hidden_dim = 50, num_layer = 2,
    num_epochs = 200, lr = 1e-3, silent = TRUE
  )

  expect_s3_class(result$model, "engression")
  expect_true(is.numeric(result$att))
  expect_length(result$att, 1)
  expect_true(is.data.frame(result$qte))
  expect_true(all(c("quantile", "effect") %in% names(result$qte)))
  expect_equal(nrow(result$qte), 9)  # default quantiles 0.1..0.9
})

test_that("fit_engression_cs ATT is close to true effect", {
  set.seed(123)
  n <- 400
  D <- rep(c(0, 1), each = n / 2)
  Y <- D * 1.0 + rnorm(n, sd = 0.5)

  result <- fit_engression_cs(
    Y = Y, D = D, controls = NULL,
    noise_dim = 5, hidden_dim = 50, num_layer = 2,
    num_epochs = 500, lr = 1e-3, silent = TRUE
  )

  # ATT should be within 0.5 of true effect (1.0) -- generous tolerance for NN
  expect_true(abs(result$att - 1.0) < 0.5)
})

test_that("fit_engression_cs works with controls", {
  set.seed(42)
  n <- 200
  D <- rep(c(0, 1), each = n / 2)
  X1 <- rnorm(n)
  Y <- D * 0.5 + 0.3 * X1 + rnorm(n, sd = 0.3)
  controls_mat <- matrix(X1, ncol = 1, dimnames = list(NULL, "X1"))

  result <- fit_engression_cs(
    Y = Y, D = D, controls = controls_mat,
    noise_dim = 5, hidden_dim = 50, num_layer = 2,
    num_epochs = 200, lr = 1e-3, silent = TRUE
  )

  expect_s3_class(result$model, "engression")
  expect_true(is.numeric(result$att))
})
