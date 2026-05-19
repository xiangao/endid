# Staggered adoption DiD via engression

Estimates cohort-specific ATT and QTE for staggered treatment designs,
then aggregates across cohorts following Callaway & Sant'Anna (2021).

## Usage

``` r
endid_staggered(
  data,
  y,
  ivar,
  tvar,
  gvar,
  rolling = "demean",
  control_group = "never_treated",
  aggregate = "overall",
  controls = NULL,
  season_var = NULL,
  quantiles = seq(0.1, 0.9, 0.1),
  nsample = 500,
  nboot = 200,
  noise_dim = 5,
  hidden_dim = 100,
  num_layer = 3,
  num_epochs = 1000,
  lr = 0.001,
  num_cores = 1,
  silent = TRUE
)
```

## Arguments

- data:

  Long-format panel data frame.

- y, ivar, tvar, gvar:

  Column names (character).

- rolling, control_group, aggregate, controls, season_var:

  See [`endid`](https://xiangao.github.io/endid/reference/endid.md).

- quantiles, nsample, nboot, noise_dim, hidden_dim, num_layer,
  num_epochs, lr, num_cores, silent:

  Engression / bootstrap parameters.

## Value

An `endid` object with `design = "staggered"`.
