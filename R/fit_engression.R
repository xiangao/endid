#' Fit engression on a DiD cross-section
#'
#' Core estimation function. Fits engression on Y ~ (D, controls) and computes
#' ATT and QTE by comparing predictions under D=1 vs D=0.
#'
#' @param Y Numeric vector of residualized outcomes (ydot_postavg).
#' @param D Binary treatment indicator (0/1).
#' @param controls Matrix of control variables or NULL.
#' @param quantiles Numeric vector of quantiles for QTE (default: seq(0.1, 0.9, 0.1)).
#' @param nsample Number of Monte Carlo samples for engression predictions (default: 500).
#' @param noise_dim Engression noise dimension (default: 5).
#' @param hidden_dim Engression hidden layer width (default: 100).
#' @param num_layer Engression number of layers (default: 3).
#' @param num_epochs Training epochs (default: 1000).
#' @param lr Learning rate (default: 1e-3).
#' @param silent Suppress training output (default: TRUE).
#' @return List with: model (engression object), att (scalar), qte (data.frame),
#'   samples_treated (numeric vector), samples_control (numeric vector).
#' @keywords internal
fit_engression_cs <- function(Y, D, controls = NULL,
                              quantiles = seq(0.1, 0.9, 0.1),
                              nsample = 500,
                              noise_dim = 5, hidden_dim = 100,
                              num_layer = 3, num_epochs = 1000,
                              lr = 1e-3, silent = TRUE) {

  # Build predictor matrix: [D, controls]
  X <- matrix(D, ncol = 1)
  colnames(X) <- "D"
  if (!is.null(controls)) {
    if (is.vector(controls)) controls <- matrix(controls, ncol = 1)
    X <- cbind(X, controls)
  }

  # Fit engression
  model <- engression::engression(
    X = X, Y = Y,
    noise_dim = noise_dim, hidden_dim = hidden_dim,
    num_layer = num_layer, num_epochs = num_epochs,
    lr = lr, silent = silent
  )

  # Predict for each treated unit individually (not at mean controls)
  # In nonlinear models, E[f(X)] != f(E[X]), so we must average individual effects
  idx_treated <- which(D == 1)
  if (length(idx_treated) == 0) stop("No treated units found in D.")

  if (!is.null(controls)) {
    ctrl_treated <- controls[idx_treated, , drop = FALSE]
    X1_treated <- cbind(D = 1, ctrl_treated)
    X0_treated <- cbind(D = 0, ctrl_treated)
  } else {
    X1_treated <- matrix(1, nrow = length(idx_treated), ncol = 1)
    X0_treated <- matrix(0, nrow = length(idx_treated), ncol = 1)
    colnames(X1_treated) <- colnames(X0_treated) <- "D"
  }

  # ATT: average difference in conditional means for treated units
  yhat1 <- predict(model, X1_treated, type = "mean", nsample = nsample)
  yhat0 <- predict(model, X0_treated, type = "mean", nsample = nsample)
  att <- mean(as.numeric(yhat1) - as.numeric(yhat0))

  # QTE: quantiles of pooled counterfactual distributions for treated units
  s1_raw <- predict(model, X1_treated, type = "sample", nsample = nsample)
  s0_raw <- predict(model, X0_treated, type = "sample", nsample = nsample)
  s1_pool <- as.numeric(s1_raw)
  s0_pool <- as.numeric(s0_raw)

  q1 <- stats::quantile(s1_pool, probs = quantiles, na.rm = TRUE)
  q0 <- stats::quantile(s0_pool, probs = quantiles, na.rm = TRUE)
  qte <- data.frame(
    quantile = quantiles,
    effect = as.numeric(q1) - as.numeric(q0)
  )

  list(
    model = model,
    att = att,
    qte = qte,
    samples_treated = s1_pool,
    samples_control = s0_pool
  )
}
