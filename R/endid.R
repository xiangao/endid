#' Engression-Based Distributional Difference-in-Differences
#'
#' Estimates distributional treatment effects by combining Lee & Wooldridge
#' (2025) panel DiD transformations with engression distributional regression.
#' Produces ATT, quantile treatment effects (QTE), and counterfactual
#' distributions.
#'
#' @param data A long-format panel data frame.
#' @param y Character. Outcome column name.
#' @param ivar Character. Unit identifier column name.
#' @param tvar Character. Calendar time column name (numeric).
#' @param gvar Character or NULL. First-treatment-year column for staggered
#'   designs. Units with value 0, NA, or Inf are never-treated.
#' @param post Character or NULL. Binary post-treatment indicator column
#'   (0 = pre, 1 = post). Required when `gvar = NULL`.
#' @param dvar Character or NULL. Binary treatment group indicator column
#'   (1 = treated unit, 0 = control). Required for common-timing designs when
#'   `post` is a calendar indicator (all units observed pre and post).
#' @param rolling Character. Transformation method: `"demean"` (default),
#'   `"detrend"`, `"demeanq"`, `"detrendq"`.
#' @param control_group Character. Control group for staggered designs:
#'   `"never_treated"` (default) or `"not_yet_treated"`.
#' @param aggregate Character. Aggregation for staggered designs:
#'   `"overall"` (default), `"cohort"`, or `"none"`.
#' @param controls Character vector or NULL. Time-invariant control column names.
#' @param season_var Character or NULL. Seasonal indicator column.
#' @param quantiles Numeric vector. Quantiles for QTE (default: seq(0.1, 0.9, 0.1)).
#' @param nsample Integer. Monte Carlo samples for engression predictions (default: 500).
#' @param nboot Integer. Bootstrap replications for inference (default: 200).
#' @param noise_dim Engression noise dimension (default: 5).
#' @param hidden_dim Engression hidden layer width (default: 100).
#' @param num_layer Engression number of layers (default: 3).
#' @param num_epochs Engression training epochs (default: 1000).
#' @param lr Engression learning rate (default: 1e-3).
#' @param silent Logical. Suppress engression training output (default: TRUE).
#'
#' @return An object of class `"endid"`.
#'
#' @examples
#' \donttest{
#'   castle <- read.csv(system.file("extdata", "castle.csv", package = "endid"))
#'   castle$gvar <- castle$effyear
#'   castle$gvar[is.na(castle$gvar) | castle$gvar == 0] <- NA
#'   res <- endid(castle, "lhomicide", "sid", "year", gvar = "gvar",
#'                rolling = "demean", num_epochs = 500, nboot = 50)
#'   print(res)
#' }
#'
#' @export
endid <- function(data, y, ivar, tvar,
                  gvar = NULL, post = NULL, dvar = NULL,
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
                  lr = 1e-3, silent = TRUE) {

  # Input validation
  stopifnot(is.data.frame(data))
  for (v in c(y, ivar, tvar)) {
    if (!v %in% names(data)) stop(sprintf("Column '%s' not found in data.", v))
  }
  if (!rolling %in% c("demean", "detrend", "demeanq", "detrendq")) {
    stop("rolling must be one of: demean, detrend, demeanq, detrendq")
  }

  # Dispatch to staggered if gvar provided
  if (!is.null(gvar)) {
    return(endid_staggered(
      data = data, y = y, ivar = ivar, tvar = tvar, gvar = gvar,
      rolling = rolling, control_group = control_group,
      aggregate = aggregate, controls = controls, season_var = season_var,
      quantiles = quantiles, nsample = nsample, nboot = nboot,
      noise_dim = noise_dim, hidden_dim = hidden_dim,
      num_layer = num_layer, num_epochs = num_epochs,
      lr = lr, silent = silent
    ))
  }

  # --- Common-timing path ---
  if (is.null(post)) stop("For common-timing design, supply 'post' column name.")
  if (!post %in% names(data)) stop(sprintf("Column '%s' not found in data.", post))

  all_periods <- sort(unique(data[[tvar]]))
  post_periods <- sort(unique(data[[tvar]][data[[post]] == 1]))
  tpost1 <- min(post_periods)

  # Treatment indicator: prefer explicit dvar; fall back to units with post==1
  # (only sensible when post is a treatment-receipt indicator, not a calendar indicator)
  if (!is.null(dvar)) {
    if (!dvar %in% names(data)) stop(sprintf("Column '%s' not found in data.", dvar))
    treated_units <- unique(data[[ivar]][data[[dvar]] == 1])
  } else {
    # Infer: treated units are those with post==1 on fewer than all post-period rows
    # i.e., post==1 is used as a treatment-receipt flag (not pure calendar)
    treated_units <- unique(data[[ivar]][data[[post]] == 1])
  }

  # Calendar post indicator for all units
  data$.post_cal_ <- as.integer(data[[tvar]] >= tpost1)

  df_trans <- apply_transform(
    df = data, y = y, ivar = ivar, tindex = tvar,
    post = ".post_cal_", rolling = rolling,
    tpost1 = tpost1, season_var = season_var
  )

  df_trans$d_ <- as.integer(df_trans[[ivar]] %in% treated_units)

  # Extract firstpost cross-section
  cs <- df_trans[df_trans$firstpost == TRUE, , drop = FALSE]

  # Drop units with missing controls (engression cannot handle NAs)
  if (!is.null(controls)) {
    keep <- stats::complete.cases(cs[, controls, drop = FALSE])
    if (sum(keep) < nrow(cs)) {
      warning(sprintf("Dropping %d units with missing controls in cross-section.",
                      nrow(cs) - sum(keep)))
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

  # Fit engression on cross-section
  fit <- fit_engression_cs(
    Y = Y_cs, D = D_cs, controls = ctrl_mat,
    quantiles = quantiles, nsample = nsample,
    noise_dim = noise_dim, hidden_dim = hidden_dim,
    num_layer = num_layer, num_epochs = num_epochs,
    lr = lr, silent = silent
  )

  # Bootstrap inference
  boot <- bootstrap_endid(
    Y = Y_cs, D = D_cs, controls = ctrl_mat,
    nboot = nboot, quantiles = quantiles, nsample = nsample,
    noise_dim = noise_dim, hidden_dim = hidden_dim,
    num_layer = num_layer, num_epochs = num_epochs,
    lr = lr, silent = TRUE
  )

  structure(
    list(
      design = "common_timing",
      att_overall = list(
        att = boot$att_mean,
        se = boot$se,
        ci_lower = boot$ci_lower,
        ci_upper = boot$ci_upper,
        nboot = nboot
      ),
      qte = .combine_qte_results(fit, boot, quantiles),
      engression_model = fit$model,
      cross_section = {
        base_cs <- data.frame(unit = cs[[ivar]], D = D_cs, ydot_postavg = Y_cs)
        if (!is.null(controls)) {
          cbind(base_cs, cs[, controls, drop = FALSE])
        } else {
          base_cs
        }
      },
      samples_treated = fit$samples_treated,
      samples_control = fit$samples_control,
      rolling = rolling,
      controls = controls,
      call = match.call()
    ),
    class = "endid"
  )
}
