# endid — Project Instructions

## Overview
R package implementing Engression-Based Distributional DiD (Lee & Wooldridge 2025).
Combines panel DiD transformations with engression distributional regression to estimate ATT, QTE, and counterfactual distributions.

## Architecture
- `R/endid.R` — Main entry point. Dispatches to common-timing (inline) or `endid_staggered()`.
- `R/transform.R` — Unit-specific pre-treatment residualization (demean, detrend, demeanq, detrendq).
- `R/fit_engression.R` — Core cross-section estimation via engression.
- `R/bootstrap.R` — Bootstrap inference for ATT and QTE.
- `R/staggered.R` — Staggered adoption: per-cohort estimation + weighted aggregation.
- `R/methods.R` — S3 methods: print, summary, plot.
- `R/endid-package.R` — Package-level documentation and imports.

## Development
```bash
# Run tests
Rscript -e 'devtools::test()'

# Check package
Rscript -e 'devtools::check()'

# Regenerate NAMESPACE / man pages
Rscript -e 'roxygen2::roxygenise()'
```

## Data
- `inst/extdata/castle.csv` — Castle Doctrine dataset (50 states, 2000–2010, staggered adoption).

## Key Design Decisions
- Engression is a neural-network-based distributional regression — results are stochastic. Tests use generous tolerances.
- Bootstrap pools replicate-level draws across cohorts for proper staggered inference.
- `.data` pronoun from rlang used in ggplot2 aes() to pass R CMD check.
- ggplot2 is in Suggests (not Imports); `plot.endid()` checks for availability.
