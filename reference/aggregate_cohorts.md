# Aggregate cohort-level results

Computes weighted average ATT and QTE across cohorts using n_treated
weights. Pools bootstrap vectors for proper inference.

## Usage

``` r
aggregate_cohorts(cohort_results, quantiles, nboot)
```

## Arguments

- cohort_results:

  List of per-cohort result lists.

- quantiles:

  Numeric vector of quantiles.

- nboot:

  Number of bootstrap replications.

## Value

List with att_overall and qte.
