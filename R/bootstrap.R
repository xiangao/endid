#' Bootstrap inference for endid
#'
#' Resamples units (with replacement) from the cross-section, refits engression,
#' and computes ATT and QTE on each replicate. Returns SEs and percentile CIs.
#'
#' @param Y Numeric vector of residualized outcomes.
#' @param D Binary treatment indicator.
#' @param controls Matrix of controls or NULL.
#' @param nboot Number of bootstrap replications (default: 200).
#' @param quantiles Quantiles for QTE (default: seq(0.1, 0.9, 0.1)).
#' @param nsample Monte Carlo samples for engression predict (default: 500).
#' @param alpha Significance level for CIs (default: 0.05).
#' @param noise_dim,hidden_dim,num_layer,num_epochs,lr,silent Engression parameters.
#' @return List with: se, ci_lower, ci_upper, att_boot, att_mean,
#'   qte_se, qte_ci_lo, qte_ci_hi, qte_mean, qte_boot_mat.
#' @keywords internal
bootstrap_endid <- function(Y, D, controls = NULL,
                            nboot = 200,
                            quantiles = seq(0.1, 0.9, 0.1),
                            nsample = 500,
                            alpha = 0.05,
                            noise_dim = 5, hidden_dim = 100,
                            num_layer = 3, num_epochs = 1000,
                            lr = 1e-3, silent = TRUE) {

  n <- length(Y)
  att_boot <- numeric(nboot)
  qte_boot_mat <- matrix(NA_real_, nrow = nboot, ncol = length(quantiles))

  for (b in seq_len(nboot)) {
    idx <- sample.int(n, n, replace = TRUE)
    Y_b <- Y[idx]
    D_b <- D[idx]
    ctrl_b <- if (!is.null(controls)) controls[idx, , drop = FALSE] else NULL

    # Skip if resampled data has no treated or no control
    if (sum(D_b == 1) < 2 || sum(D_b == 0) < 2) next

    fit_b <- tryCatch(
      fit_engression_cs(
        Y = Y_b, D = D_b, controls = ctrl_b,
        quantiles = quantiles, nsample = nsample,
        noise_dim = noise_dim, hidden_dim = hidden_dim,
        num_layer = num_layer, num_epochs = num_epochs,
        lr = lr, silent = TRUE
      ),
      error = function(e) NULL
    )

    if (!is.null(fit_b)) {
      att_boot[b] <- fit_b$att
      qte_boot_mat[b, ] <- fit_b$qte$effect
    }
  }

  # Remove failed replicates
  valid <- !is.na(att_boot)
  att_valid <- att_boot[valid]
  qte_valid <- qte_boot_mat[valid, , drop = FALSE]

  se_att <- stats::sd(att_valid, na.rm = TRUE)
  ci_att <- stats::quantile(att_valid, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)

  qte_se <- apply(qte_valid, 2, stats::sd, na.rm = TRUE)
  qte_ci_lo <- apply(qte_valid, 2, stats::quantile, probs = alpha / 2, na.rm = TRUE)
  qte_ci_hi <- apply(qte_valid, 2, stats::quantile, probs = 1 - alpha / 2, na.rm = TRUE)

  list(
    se = se_att,
    ci_lower = unname(ci_att[1]),
    ci_upper = unname(ci_att[2]),
    att_boot = att_valid,
    att_mean = mean(att_valid, na.rm = TRUE),
    qte_se = qte_se,
    qte_ci_lo = qte_ci_lo,
    qte_ci_hi = qte_ci_hi,
    qte_mean = colMeans(qte_valid, na.rm = TRUE),
    qte_boot_mat = qte_valid
  )
}

# Helper to combine point estimate and bootstrap stats into qte data.frame
# Uses bootstrap mean as stable point estimate for stochastic NN estimators
.combine_qte_results <- function(fit, boot, quantiles) {
  data.frame(
    quantile = quantiles,
    effect = boot$qte_mean,
    se = boot$qte_se,
    ci_lower = boot$qte_ci_lo,
    ci_upper = boot$qte_ci_hi,
    row.names = NULL
  )
}
