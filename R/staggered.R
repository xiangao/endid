#' Staggered adoption DiD via engression
#'
#' Estimates cohort-specific ATT and QTE for staggered treatment designs,
#' then aggregates across cohorts following Callaway & Sant'Anna (2021).
#'
#' @param data Long-format panel data frame.
#' @param y,ivar,tvar,gvar Column names (character).
#' @param rolling,control_group,aggregate,controls,season_var See \code{\link{endid}}.
#' @param quantiles,nsample,nboot,noise_dim,hidden_dim,num_layer,num_epochs,lr,silent,ncores
#'   Engression / bootstrap parameters.
#' @return An \code{endid} object with \code{design = "staggered"}.
#' @keywords internal
endid_staggered <- function(data, y, ivar, tvar, gvar,
                            rolling = "demean",
                            control_group = "never_treated",
                            aggregate = "overall",
                            controls = NULL,
                            season_var = NULL,
                            quantiles = seq(0.1, 0.9, 0.1),
                            nsample = 500,
                            nboot = 200,
                            noise_dim = 5, hidden_dim = 100,
                            num_layer = 3, num_epochs = 1000,
                            lr = 1e-3, silent = TRUE,
                            ncores = NULL) {

  if (!gvar %in% names(data)) stop(sprintf("Column '%s' not found in data.", gvar))

  # Identify cohorts and never-treated
  gvals <- data[[gvar]]
  never_treated_mask <- is.na(gvals) | gvals == 0 | is.infinite(gvals)
  never_treated_units <- unique(data[[ivar]][never_treated_mask])
  cohorts <- sort(unique(gvals[!never_treated_mask]))

  if (length(cohorts) == 0) stop("No treatment cohorts found in gvar.")
  if (length(never_treated_units) == 0 && control_group == "never_treated") {
    stop("No never-treated units found. Use control_group = 'not_yet_treated'.")
  }

  cohort_results <- list()

  for (g in cohorts) {
    # Treated units for this cohort
    treated_units <- unique(data[[ivar]][data[[gvar]] == g & !is.na(data[[gvar]])])

    # Control units
    if (control_group == "never_treated") {
      control_units <- never_treated_units
    } else if (control_group == "not_yet_treated") {
      # Not yet treated at time g: gvar > g or never-treated
      nyt_mask <- (!never_treated_mask & data[[gvar]] > g) | never_treated_mask
      control_units <- unique(data[[ivar]][nyt_mask])
    } else {
      stop("control_group must be 'never_treated' or 'not_yet_treated'.")
    }

    # Check minimum sample sizes
    if (length(treated_units) < 2 || length(control_units) < 2) {
      warning(sprintf(
        "Cohort %s: skipping (n_treated=%d, n_control=%d).",
        g, length(treated_units), length(control_units)
      ))
      next
    }

    # Subset data to treated + control
    keep_units <- c(treated_units, control_units)
    df_g <- data[data[[ivar]] %in% keep_units, , drop = FALSE]

    # For not-yet-treated controls, remove their observations from their own
    # treatment date onwards so treated outcomes don't contaminate the control group
    if (control_group == "not_yet_treated") {
      nyt_units <- setdiff(control_units, never_treated_units)
      if (length(nyt_units) > 0) {
        nyt_gvars <- stats::setNames(
          data[[gvar]][match(nyt_units, data[[ivar]])], nyt_units
        )
        df_g <- df_g[!(df_g[[ivar]] %in% nyt_units &
                        df_g[[tvar]] >= nyt_gvars[as.character(df_g[[ivar]])]), ]
      }
    }

    # Create post indicator and treatment indicator
    df_g$.post_cal_ <- as.integer(df_g[[tvar]] >= g)
    tpost1 <- g

    # Apply transformation
    df_trans <- apply_transform(
      df = df_g, y = y, ivar = ivar, tindex = tvar,
      post = ".post_cal_", rolling = rolling,
      tpost1 = tpost1, season_var = season_var
    )

    df_trans$d_ <- as.integer(df_trans[[ivar]] %in% treated_units)

    # Extract first-post cross-section
    cs <- df_trans[df_trans$firstpost == TRUE, , drop = FALSE]

    # Drop units with missing controls (engression cannot handle NAs)
    if (!is.null(controls)) {
      keep <- stats::complete.cases(cs[, controls, drop = FALSE])
      if (sum(keep) < nrow(cs)) {
        cs <- cs[keep, , drop = FALSE]
      }
    }

    Y_cs <- as.numeric(cs$ydot_postavg)
    D_cs <- as.integer(cs$d_)

    # Build controls matrix
    ctrl_mat <- NULL
    if (!is.null(controls)) {
      ctrl_mat <- as.matrix(cs[, controls, drop = FALSE])
    }

    # Skip if degenerate cross-section
    if (sum(D_cs == 1) < 2 || sum(D_cs == 0) < 2) {
      warning(sprintf("Cohort %s: skipping (degenerate cross-section).", g))
      next
    }

    # Fit engression
    fit_g <- tryCatch(
      fit_engression_cs(
        Y = Y_cs, D = D_cs, controls = ctrl_mat,
        quantiles = quantiles, nsample = nsample,
        noise_dim = noise_dim, hidden_dim = hidden_dim,
        num_layer = num_layer, num_epochs = num_epochs,
        lr = lr, silent = silent
      ),
      error = function(e) {
        warning(sprintf("Cohort %s: engression failed (%s).", g, e$message))
        NULL
      }
    )
    if (is.null(fit_g)) next

    # Bootstrap
    boot_g <- tryCatch(
      bootstrap_endid(
        Y = Y_cs, D = D_cs, controls = ctrl_mat,
        nboot = nboot, quantiles = quantiles, nsample = nsample,
        noise_dim = noise_dim, hidden_dim = hidden_dim,
        num_layer = num_layer, num_epochs = num_epochs,
        lr = lr, silent = TRUE, ncores = ncores
      ),
      error = function(e) {
        warning(sprintf("Cohort %s: bootstrap failed (%s).", g, e$message))
        NULL
      }
    )
    if (is.null(boot_g)) next

    cohort_results[[as.character(g)]] <- list(
      cohort = g,
      n_treated = length(treated_units),
      n_control = length(control_units),
      att = boot_g$att_mean,
      se = boot_g$se,
      ci_lower = boot_g$ci_lower,
      ci_upper = boot_g$ci_upper,
      att_boot = boot_g$att_boot,
      qte = .combine_qte_results(fit_g, boot_g, quantiles),
      qte_boot_mat = boot_g$qte_boot_mat
    )

    if (!silent) {
      message(sprintf("Cohort %s: ATT = %.4f (SE = %.4f), n_treated = %d, n_control = %d",
                       g, boot_g$att_mean, boot_g$se, length(treated_units), length(control_units)))
    }
  }

  if (length(cohort_results) == 0) {
    stop("No cohorts could be estimated. Check data and parameters.")
  }

  # --- Aggregation ---
  agg <- aggregate_cohorts(cohort_results, quantiles, nboot)

  structure(
    list(
      design = "staggered",
      att_overall = agg$att_overall,
      qte = agg$qte,
      cohort_results = cohort_results,
      rolling = rolling,
      control_group = control_group,
      aggregate = aggregate,
      controls = controls,
      call = match.call()
    ),
    class = "endid"
  )
}


#' Aggregate cohort-level results
#'
#' Computes weighted average ATT and QTE across cohorts using n_treated weights.
#' Pools bootstrap vectors for proper inference.
#'
#' @param cohort_results List of per-cohort result lists.
#' @param quantiles Numeric vector of quantiles.
#' @param nboot Number of bootstrap replications.
#' @return List with att_overall and qte.
#' @keywords internal
aggregate_cohorts <- function(cohort_results, quantiles, nboot) {
  n_treated <- vapply(cohort_results, function(x) x$n_treated, numeric(1))
  weights <- n_treated / sum(n_treated)

  # Weighted ATT
  atts <- vapply(cohort_results, function(x) x$att, numeric(1))
  att_overall <- sum(weights * atts)

  # Pool bootstrap draws for ATT
  # Pad/truncate bootstrap vectors to common length
  boot_lens <- vapply(cohort_results, function(x) length(x$att_boot), integer(1))
  B <- min(boot_lens)

  att_boot_overall <- numeric(B)
  qte_boot_overall <- matrix(0, nrow = B, ncol = length(quantiles))

  for (i in seq_along(cohort_results)) {
    cr <- cohort_results[[i]]
    att_boot_overall <- att_boot_overall + weights[i] * cr$att_boot[seq_len(B)]
    if (!is.null(cr$qte_boot_mat) && nrow(cr$qte_boot_mat) >= B) {
      qte_boot_overall <- qte_boot_overall + weights[i] * cr$qte_boot_mat[seq_len(B), , drop = FALSE]
    }
  }

  se_overall <- stats::sd(att_boot_overall, na.rm = TRUE)
  ci_overall <- stats::quantile(att_boot_overall, probs = c(0.025, 0.975), na.rm = TRUE)

  # Use the mean of the pooled bootstrap distribution as the point estimate
  # for the aggregate QTE, which is more stable than the weighted average of
  # individual (stochastic) fits.
  qte_agg <- colMeans(qte_boot_overall, na.rm = TRUE)

  qte_se <- apply(qte_boot_overall, 2, stats::sd, na.rm = TRUE)
  qte_ci_lo <- apply(qte_boot_overall, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
  qte_ci_hi <- apply(qte_boot_overall, 2, stats::quantile, probs = 0.975, na.rm = TRUE)

  qte_df <- data.frame(
    quantile = quantiles,
    effect = qte_agg,
    se = qte_se,
    ci_lower = qte_ci_lo,
    ci_upper = qte_ci_hi
  )
  rownames(qte_df) <- NULL

  list(
    att_overall = list(
      att = att_overall,
      se = se_overall,
      ci_lower = unname(ci_overall[1]),
      ci_upper = unname(ci_overall[2]),
      nboot = nboot
    ),
    qte = qte_df
  )
}
