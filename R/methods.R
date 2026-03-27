#' Print an endid object
#'
#' @param x An \code{endid} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.endid <- function(x, ...) {
  cat("Engression-Based Distributional DiD\n")
  cat(sprintf("  Design: %s\n", x$design))
  cat(sprintf("  Transformation: %s\n", x$rolling))

  att <- x$att_overall
  cat(sprintf("\n  ATT = %.4f  (SE = %.4f)\n", att$att, att$se))
  cat(sprintf("  95%% CI: [%.4f, %.4f]\n", att$ci_lower, att$ci_upper))
  cat(sprintf("  Bootstrap replications: %d\n", att$nboot))


  if (x$design == "staggered" && !is.null(x$cohort_results)) {
    cat(sprintf("\n  Cohorts estimated: %d\n", length(x$cohort_results)))
    cat(sprintf("  Control group: %s\n", x$control_group))
    cat(sprintf("  Aggregation: %s\n", x$aggregate))

    cat("\n  Cohort-level ATT:\n")
    for (cr in x$cohort_results) {
      cat(sprintf("    g=%s: ATT=%.4f (SE=%.4f), n_treated=%d, n_control=%d\n",
                  cr$cohort, cr$att, cr$se, cr$n_treated, cr$n_control))
    }
  }

  invisible(x)
}


#' Summarize an endid object
#'
#' @param object An \code{endid} object.
#' @param ... Ignored.
#' @return A \code{summary.endid} object (list with att_table and qte_table).
#' @export
summary.endid <- function(object, ...) {
  att <- object$att_overall
  att_table <- data.frame(
    Estimate = att$att,
    SE = att$se,
    CI_Lower = att$ci_lower,
    CI_Upper = att$ci_upper
  )

  qte_table <- object$qte

  cohort_table <- NULL
  if (object$design == "staggered" && !is.null(object$cohort_results)) {
    cohort_table <- do.call(rbind, lapply(object$cohort_results, function(cr) {
      data.frame(
        Cohort = cr$cohort,
        ATT = cr$att,
        SE = cr$se,
        CI_Lower = cr$ci_lower,
        CI_Upper = cr$ci_upper,
        N_Treated = cr$n_treated,
        N_Control = cr$n_control
      )
    }))
    rownames(cohort_table) <- NULL
  }

  out <- list(
    design = object$design,
    rolling = object$rolling,
    att_table = att_table,
    qte_table = qte_table,
    cohort_table = cohort_table
  )
  class(out) <- "summary.endid"
  out
}


#' Print a summary.endid object
#'
#' @param x A \code{summary.endid} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.summary.endid <- function(x, ...) {
  cat("Engression-Based Distributional DiD\n")
  cat(sprintf("Design: %s | Transformation: %s\n\n", x$design, x$rolling))

  cat("--- ATT ---\n")
  print(x$att_table, row.names = FALSE)

  cat("\n--- QTE ---\n")
  print(x$qte_table, row.names = FALSE)

  if (!is.null(x$cohort_table)) {
    cat("\n--- Cohort-level Results ---\n")
    print(x$cohort_table, row.names = FALSE)
  }

  invisible(x)
}


#' Plot QTE from an endid object
#'
#' Produces a quantile treatment effect plot with confidence intervals
#' and an ATT reference line.
#'
#' @param x An \code{endid} object.
#' @param ... Ignored.
#' @return A \code{ggplot} object (invisibly).
#' @export
plot.endid <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot.endid(). Install it with install.packages('ggplot2').")
  }

  qte <- x$qte
  att_val <- x$att_overall$att

  p <- ggplot2::ggplot(qte, ggplot2::aes(x = .data$quantile, y = .data$effect)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
      fill = "steelblue", alpha = 0.25
    ) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue", size = 2) +
    ggplot2::geom_hline(yintercept = att_val, linetype = "dashed", color = "firebrick") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    ggplot2::annotate("text", x = max(qte$quantile), y = att_val,
                      label = sprintf("ATT = %.3f", att_val),
                      hjust = 1, vjust = -0.5, color = "firebrick", size = 3.5) +
    ggplot2::labs(
      title = sprintf("Quantile Treatment Effects (%s)", x$design),
      x = "Quantile",
      y = "Effect"
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}
