# Bootstrap inference for endid

Resamples units (with replacement) from the cross-section, refits
engression, and computes ATT and QTE on each replicate. Returns SEs and
percentile CIs. Supports parallel execution via mclapply on Unix-like
systems.

## Usage

``` r
bootstrap_endid(
  Y,
  D,
  controls = NULL,
  nboot = 200,
  quantiles = seq(0.1, 0.9, 0.1),
  nsample = 500,
  alpha = 0.05,
  noise_dim = 5,
  hidden_dim = 100,
  num_layer = 3,
  num_epochs = 1000,
  lr = 0.001,
  silent = TRUE,
  num_cores = 1
)
```

## Arguments

- Y:

  Numeric vector of residualized outcomes.

- D:

  Binary treatment indicator.

- controls:

  Matrix of controls or NULL.

- nboot:

  Number of bootstrap replications (default: 200).

- quantiles:

  Quantiles for QTE (default: seq(0.1, 0.9, 0.1)).

- nsample:

  Monte Carlo samples for engression predict (default: 500).

- alpha:

  Significance level for CIs (default: 0.05).

- noise_dim, hidden_dim, num_layer, num_epochs, lr, silent:

  Engression parameters.

- num_cores:

  Number of cores for parallel execution (default: 1).

## Value

List with: se, ci_lower, ci_upper, att_boot, att_mean, qte_se,
qte_ci_lo, qte_ci_hi, qte_mean, qte_boot_mat.
