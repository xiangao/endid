# Engression-Based Distributional Difference-in-Differences

Estimates distributional treatment effects by combining Lee & Wooldridge
(2025) panel DiD transformations with engression distributional
regression. Produces ATT, quantile treatment effects (QTE), and
counterfactual distributions.

## Usage

``` r
endid(
  data,
  y,
  ivar,
  tvar,
  gvar = NULL,
  post = NULL,
  dvar = NULL,
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

  A long-format panel data frame.

- y:

  Character. Outcome column name.

- ivar:

  Character. Unit identifier column name.

- tvar:

  Character. Calendar time column name (numeric).

- gvar:

  Character or NULL. First-treatment-year column for staggered designs.
  Units with value 0, NA, or Inf are never-treated.

- post:

  Character or NULL. Binary post-treatment indicator column (0 = pre, 1
  = post). Required when `gvar = NULL`.

- dvar:

  Character or NULL. Binary treatment group indicator column (1 =
  treated unit, 0 = control). Required for common-timing designs when
  `post` is a calendar indicator (all units observed pre and post).

- rolling:

  Character. Transformation method: `"demean"` (default), `"detrend"`,
  `"demeanq"`, `"detrendq"`.

- control_group:

  Character. Control group for staggered designs: `"never_treated"`
  (default) or `"not_yet_treated"`.

- aggregate:

  Character. Aggregation for staggered designs: `"overall"` (default),
  `"cohort"`, or `"none"`.

- controls:

  Character vector or NULL. Time-invariant control column names.

- season_var:

  Character or NULL. Seasonal indicator column.

- quantiles:

  Numeric vector. Quantiles for QTE (default: seq(0.1, 0.9, 0.1)).

- nsample:

  Integer. Monte Carlo samples for engression predictions (default:
  500).

- nboot:

  Integer. Bootstrap replications for inference (default: 200).

- noise_dim:

  Engression noise dimension (default: 5).

- hidden_dim:

  Engression hidden layer width (default: 100).

- num_layer:

  Engression number of layers (default: 3).

- num_epochs:

  Engression training epochs (default: 1000).

- lr:

  Engression learning rate (default: 1e-3).

- num_cores:

  Integer. Number of cores for bootstrap parallelization (default: 1).

- silent:

  Logical. Suppress engression training output (default: TRUE).

## Value

An object of class `"endid"`.

## Examples

``` r
# \donttest{
  castle <- read.csv(system.file("extdata", "castle.csv", package = "endid"))
  castle$gvar <- castle$effyear
  castle$gvar[is.na(castle$gvar) | castle$gvar == 0] <- NA
  res <- endid(castle, "lhomicide", "sid", "year", gvar = "gvar",
               rolling = "demean", num_epochs = 500, nboot = 50)
  print(res)
# }
```
