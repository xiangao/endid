# Unit-specific pre-treatment transformations for LWDID
# Mirrors Python lwdid/transformations.py logic


#' Apply unit-specific transformation to panel data
#'
#' Computes ydot (residualised outcome), ydot_postavg (post-treatment average
#' of ydot per unit), and marks the firstpost cross-section.
#'
#' @param df Data frame (long format, one row per unit-period)
#' @param y Outcome column name
#' @param ivar Unit identifier column name
#' @param tindex Integer time index column name
#' @param post Binary post-treatment indicator column name (0=pre, 1=post)
#' @param rolling Transformation method: "demean", "detrend", "demeanq", "detrendq"
#' @param tpost1 First post-treatment period index
#' @param season_var Column name of seasonal indicator (required for demeanq/detrendq)
#' @return Data frame with added columns: ydot, ydot_postavg, firstpost
#' @export
apply_transform <- function(df, y, ivar, tindex, post, rolling, tpost1,
                            season_var = NULL) {

  if (!rolling %in% c("demean", "detrend", "demeanq", "detrendq")) {
    stop(sprintf("Invalid rolling method: '%s'. Choose demean, detrend, demeanq, detrendq.", rolling))
  }

  # Split data by unit for faster processing than unit-by-unit loop with indexing
  df_list <- split(df, df[[ivar]])

  res_list <- lapply(df_list, function(unit_df) {
    idx_pre <- !is.na(unit_df[[post]]) & unit_df[[post]] == 0
    y_vals <- unit_df[[y]]

    if (sum(idx_pre, na.rm = TRUE) < 1) {
      # No pre-treatment observations
      unit_df$ydot <- NA_real_
      return(unit_df)
    }

    if (rolling == "demean") {
      y_pre_mean <- mean(y_vals[idx_pre], na.rm = TRUE)
      unit_df$ydot <- y_vals - y_pre_mean

    } else if (rolling == "detrend") {
      if (sum(idx_pre, na.rm = TRUE) < 2) {
        unit_df$ydot <- NA_real_
      } else {
        y_pre  <- y_vals[idx_pre]
        t_pre  <- unit_df[[tindex]][idx_pre]
        t_mean <- mean(t_pre, na.rm = TRUE)
        fit    <- .lm_simple(y_pre, t_pre - t_mean)
        t_all  <- unit_df[[tindex]]
        unit_df$ydot <- y_vals - (fit$alpha + fit$beta * (t_all - t_mean))
      }

    } else if (rolling == "demeanq") {
      if (is.null(season_var)) stop("rolling='demeanq' requires season_var.")
      unit_df$ydot <- .demeanq_unit_fast(unit_df, idx_pre, y, season_var)

    } else if (rolling == "detrendq") {
      if (is.null(season_var)) stop("rolling='detrendq' requires season_var.")
      unit_df$ydot <- .detrendq_unit_fast(unit_df, idx_pre, y, tindex, season_var)
    }

    unit_df
  })

  # Recombine
  df <- do.call(rbind, res_list)

  # Post-treatment average of ydot per unit
  # Spread the single value to all rows of that unit.
  post_mask <- !is.na(df[[post]]) & df[[post]] == 1
  df$ydot_postavg <- NA_real_

  if (any(post_mask)) {
    unit_post_avgs <- tapply(df$ydot[post_mask], df[[ivar]][post_mask], mean, na.rm = TRUE)
    df$ydot_postavg <- unit_post_avgs[as.character(df[[ivar]])]
  }

  # Mark first post-treatment cross-section
  df$firstpost <- !is.na(df[[tindex]]) & df[[tindex]] == tpost1 & !is.na(df$ydot_postavg)

  df
}


# OLS slope + intercept for simple linear regression (y ~ 1 + t_centered)
.lm_simple <- function(y, t_c) {
  valid <- !is.na(y) & !is.na(t_c)
  y <- y[valid]; t_c <- t_c[valid]
  n <- length(y)
  if (n < 2) return(list(alpha = NA_real_, beta = NA_real_))
  tc_m <- mean(t_c)
  t_cc <- t_c - tc_m
  Stt <- sum(t_cc^2)
  if (abs(Stt) < 1e-10) return(list(alpha = mean(y), beta = 0))
  Sty <- sum(t_cc * y)
  beta  <- Sty / Stt
  alpha <- mean(y) - beta * tc_m
  list(alpha = alpha, beta = beta)
}


# Seasonal demeaning for a single unit (demeanq)
.demeanq_unit_fast <- function(unit_df, idx_pre, y, season_var) {
  unit_data_pre <- unit_df[idx_pre, , drop = FALSE]
  y_pre  <- unit_data_pre[[y]]
  s_pre  <- as.factor(unit_data_pre[[season_var]])
  valid  <- !is.na(y_pre) & !is.na(unit_data_pre[[season_var]])
  if (sum(valid) <= nlevels(s_pre)) {
    return(rep(NA_real_, nrow(unit_df)))
  }
  fit <- stats::lm(y_pre ~ s_pre, na.action = stats::na.omit)
  s_all  <- factor(unit_df[[season_var]], levels = levels(s_pre))
  yhat   <- stats::predict(fit, newdata = data.frame(s_pre = s_all))
  unit_df[[y]] - yhat
}


# Seasonal detrending for a single unit (detrendq)
.detrendq_unit_fast <- function(unit_df, idx_pre, y, tindex, season_var) {
  unit_data_pre <- unit_df[idx_pre, , drop = FALSE]
  y_pre  <- unit_data_pre[[y]]
  t_pre  <- unit_data_pre[[tindex]]
  s_pre  <- as.factor(unit_data_pre[[season_var]])
  valid  <- !is.na(y_pre) & !is.na(t_pre) & !is.na(unit_data_pre[[season_var]])
  n_seas <- nlevels(s_pre)
  if (sum(valid) <= (n_seas + 1)) {
    return(rep(NA_real_, nrow(unit_df)))
  }
  t_mean <- mean(t_pre[valid])
  t_c_pre <- t_pre - t_mean
  fit <- stats::lm(y_pre ~ t_c_pre + s_pre, na.action = stats::na.omit)
  t_c_all <- unit_df[[tindex]] - t_mean
  s_all   <- factor(unit_df[[season_var]], levels = levels(s_pre))
  yhat    <- stats::predict(fit, newdata = data.frame(t_c_pre = t_c_all, s_pre = s_all))
  unit_df[[y]] - yhat
}
