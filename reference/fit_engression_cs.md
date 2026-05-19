# Fit engression on a DiD cross-section

Core estimation function. Fits engression on Y ~ (D, controls) and
computes ATT and QTE by comparing predictions under D=1 vs D=0.

## Usage

``` r
fit_engression_cs(
  Y,
  D,
  controls = NULL,
  quantiles = seq(0.1, 0.9, 0.1),
  nsample = 500,
  noise_dim = 5,
  hidden_dim = 100,
  num_layer = 3,
  num_epochs = 1000,
  lr = 0.001,
  silent = TRUE
)
```

## Arguments

- Y:

  Numeric vector of residualized outcomes (ydot_postavg).

- D:

  Binary treatment indicator (0/1).

- controls:

  Matrix of control variables or NULL.

- quantiles:

  Numeric vector of quantiles for QTE (default: seq(0.1, 0.9, 0.1)).

- nsample:

  Number of Monte Carlo samples for engression predictions (default:
  500).

- noise_dim:

  Engression noise dimension (default: 5).

- hidden_dim:

  Engression hidden layer width (default: 100).

- num_layer:

  Engression number of layers (default: 3).

- num_epochs:

  Training epochs (default: 1000).

- lr:

  Learning rate (default: 1e-3).

- silent:

  Suppress training output (default: TRUE).

## Value

List with: model (engression object), att (scalar), qte (data.frame),
samples_treated (numeric vector), samples_control (numeric vector).
