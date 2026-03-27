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

  # Construct prediction inputs for D=1 and D=0
  # Use mean of controls for prediction (marginal effect)
  if (!is.null(controls)) {
    ctrl_means <- colMeans(controls)
    X1 <- matrix(c(1, ctrl_means), nrow = 1)
    X0 <- matrix(c(0, ctrl_means), nrow = 1)
  } else {
    X1 <- matrix(1, nrow = 1, ncol = 1)
    X0 <- matrix(0, nrow = 1, ncol = 1)
  }

  # ATT: difference in conditional means
  yhat1 <- predict(model, X1, type = "mean", nsample = nsample)
  yhat0 <- predict(model, X0, type = "mean", nsample = nsample)
  att <- as.numeric(yhat1 - yhat0)

  # QTE: difference in conditional quantiles
  q1 <- predict(model, X1, type = "quantile", quantiles = quantiles, nsample = nsample)
  q0 <- predict(model, X0, type = "quantile", quantiles = quantiles, nsample = nsample)
  qte <- data.frame(
    quantile = quantiles,
    effect = as.numeric(q1) - as.numeric(q0)
  )

  # Counterfactual samples
  samples_treated <- as.numeric(predict(model, X1, type = "sample", nsample = nsample))
  samples_control <- as.numeric(predict(model, X0, type = "sample", nsample = nsample))

  list(
    model = model,
    att = att,
    qte = qte,
    samples_treated = samples_treated,
    samples_control = samples_control
  )
}
