# endid

[![pkgdown](https://img.shields.io/badge/pkgdown-site-blue.svg)](https://xiangao.github.io/endid/)

Engression-Based Distributional Difference-in-Differences

Implements the method from Lee & Wooldridge (2025), combining panel DiD
transformations with
[engression](https://github.com/xingyu-zhou/engression) distributional
regression to estimate:

- **ATT** — Average Treatment Effect on the Treated
- **QTE** — Quantile Treatment Effects across the outcome distribution
- **Counterfactual distributions** via engression sampling

Supports both **common-timing** and **staggered adoption** designs.

## Installation

``` r

# Install from GitHub
# install.packages("remotes")
remotes::install_github("xiangao/endid")
```

## Usage

### Common-timing design

``` r

library(endid)

result <- endid(
  data = panel_df,
  y = "outcome",
  ivar = "unit_id",
  tvar = "time",
  post = "post_treatment",
  dvar = "treated",
  rolling = "demean"
)

print(result)
summary(result)
plot(result)
```

### Staggered adoption design

``` r

castle <- read.csv(system.file("extdata", "castle.csv", package = "endid"))
castle$gvar <- castle$effyear
castle$gvar[is.na(castle$gvar) | castle$gvar == 0] <- NA

result <- endid(
  data = castle,
  y = "lhomicide",
  ivar = "sid",
  tvar = "year",
  gvar = "gvar",
  rolling = "demean",
  control_group = "never_treated"
)

print(result)
plot(result)
```

## Transformations

| Method     | Description                       | Pre-periods required |
|------------|-----------------------------------|----------------------|
| `demean`   | Subtract pre-treatment mean       | \>= 1                |
| `detrend`  | Remove unit-specific linear trend | \>= 2                |
| `demeanq`  | Seasonal demeaning                | \> n_seasons         |
| `detrendq` | Seasonal detrending               | \> n_seasons + 1     |

## Parameters

Key arguments to
[`endid()`](https://xiangao.github.io/endid/reference/endid.md):

- `rolling` — Transformation method (`"demean"`, `"detrend"`,
  `"demeanq"`, `"detrendq"`)
- `control_group` — For staggered: `"never_treated"` or
  `"not_yet_treated"`
- `aggregate` — For staggered: `"overall"`, `"cohort"`, or `"none"`
- `nboot` — Number of bootstrap replications (default: 200)
- `quantiles` — Quantiles for QTE (default: `seq(0.1, 0.9, 0.1)`)

## Documentation & vignettes

Full documentation: **<https://xiangao.github.io/endid/>**

| Page | Description |
|----|----|
| [Comparison with linear DiD](https://xiangao.github.io/endid/articles/comparison.html) | Synthetic comparison of distributional and linear DiD targets |
| [Castle Doctrine example](https://xiangao.github.io/endid/articles/real_data_example.html) | Replication-style workflow using staggered treatment timing |
| [`endid()`](https://xiangao.github.io/endid/reference/endid.html) | Main estimator |
| [Reference index](https://xiangao.github.io/endid/reference/index.html) | All documented functions on one page |

## References

Lee, S. & Wooldridge, J. M. (2025). Distributional
Difference-in-Differences.
