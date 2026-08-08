#' Adaptive lasso
#'
#' @description
#' Runs an adaptive lasso: a ridge regression supplies weights
#' (`penalty.factor = 1 / |ridge coefficient|`), and a lasso is then fitted
#' with those weights, so predictors with small ridge coefficients are
#' penalised harder. Cross-validation is repeated `n_cv` times and the median
#' lambda is used, which stabilises selection against the randomness of the CV
#' folds.
#'
#' See Sidhu et al. (2021) for a description of this method.
#'
#' Sidhu, D. M., Westbury, C., Hollis, G., & Pexman, P. M. (2021). Sound
#' symbolism shapes the English language: The maluma/takete effect in English
#' nouns. \emph{Psychonomic Bulletin & Review}, \emph{28}, 1390-1398.
#'
#' Rows missing values on the outcome or any predictor are dropped before
#' fitting. Numeric predictors are standardised by default so the coefficients
#' are comparable.
#'
#' `refit = TRUE` fits an ordinary regression using the selected predictors.
#'
#' @param d A data.frame or data.table.
#' @param dv Character: name of the outcome column.
#' @param predictors Character or numeric: the predictor columns.
#' @param lambda Which lambda to use, `"1se"` (default) or `"min"`.
#' @param n_cv Number of cross-validation runs to take the median lambda over.
#'   Default 11.
#' @param nfolds Folds per cross-validation run. Default 10.
#' @param seed Optional integer seed, for reproducible fold assignment.
#' @param scale Logical. If TRUE (default), standardise numeric predictors.
#' @param r2 Logical. If TRUE, report the R-squared of the lasso predictions.
#'   Default FALSE.
#' @param refit Logical. If TRUE, also fit an ordinary `lm()` using the
#'   selected predictors. Default FALSE. See the note below on inference.
#' @param quiet Logical. If TRUE, suppress printed output. Default FALSE.
#'
#' @return Invisibly, a list with `coefficients`, `selected`, `dropped`,
#'   `lambda`, `data` (the complete-case, scaled data used), `fit` (the
#'   `cv.glmnet` object), and, when requested, `r2` and `lm`.
#'
#' @section Inference after selection:
#' If `refit = TRUE`, the p-values from the `lm()` do not account for the fact
#' that the predictors were chosen by the lasso on the same data. They are
#' anticonservative and should not be reported as ordinary p-values. The
#' coefficients and R-squared are fine.
#'
#' @examples
#' \dontrun{
#' alasso(d, "P_Iconicity", c("Freq_SUBTLEXUS", "AoA_Kuper", "Conc_Brys"))
#' alasso(d, "P_Iconicity", pred_vars, lambda = "min", r2 = TRUE, seed = 999)
#' }
#'
#' @export
alasso <- function(d,
                   dv,
                   predictors,
                   lambda = c("1se", "min"),
                   n_cv = 11,
                   nfolds = 10,
                   seed = NULL,
                   scale = TRUE,
                   r2 = FALSE,
                   refit = FALSE,
                   quiet = FALSE) {

  lambda <- match.arg(lambda)

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("`alasso()` requires the 'glmnet' package.", call. = FALSE)
  }

  d <- as.data.frame(d)

  # --- columns --------------------------------------------------------------
  if (!is.character(dv) || length(dv) != 1L) {
    stop("`dv` must be a single character string.", call. = FALSE)
  }
  if (!dv %in% names(d)) {
    stop("`dv` '", dv, "' not found in `d`.", call. = FALSE)
  }

  pred <- if (is.numeric(predictors)) names(d)[predictors] else predictors
  pred <- setdiff(pred, dv)
  missing_pred <- setdiff(pred, names(d))
  if (length(missing_pred) > 0L) {
    stop("Predictor(s) not found in `d`: ",
         paste(missing_pred, collapse = ", "), call. = FALSE)
  }
  if (length(pred) < 2L) {
    stop("Need at least two predictors.", call. = FALSE)
  }

  if (!is.numeric(d[[dv]])) {
    stop("`dv` must be numeric.", call. = FALSE)
  }

  # --- complete cases -------------------------------------------------------
  n_before <- nrow(d)
  keep <- stats::complete.cases(d[, c(dv, pred), drop = FALSE])
  dd <- d[keep, c(dv, pred), drop = FALSE]
  rownames(dd) <- NULL

  if (nrow(dd) == 0L) {
    stop("No complete cases across `dv` and `predictors`.", call. = FALSE)
  }

  # --- factor predictors ----------------------------------------------------
  cat_pred <- pred[vapply(dd[pred], function(z) {
    is.factor(z) || is.character(z) || is.logical(z)
  }, logical(1))]

  if (length(cat_pred) > 0L) {
    warning("Categorical predictor(s): ", paste(cat_pred, collapse = ", "),
            ". These are dummy-coded, and each dummy is penalised separately, ",
            "so a factor can be partly selected and its levels are not on a ",
            "comparable scale with the standardised numeric predictors. ",
            "Consider removing them or recoding them yourself.",
            call. = FALSE)
  }

  # --- scale numeric predictors ---------------------------------------------
  num_pred <- setdiff(pred, cat_pred)
  if (scale && length(num_pred) > 0L) {
    for (v in num_pred) {
      s <- stats::sd(dd[[v]])
      if (is.na(s) || s == 0) {
        stop("Predictor '", v, "' has no variance among the complete cases.",
             call. = FALSE)
      }
      dd[[v]] <- as.vector(base::scale(dd[[v]]))
    }
  }

  x <- stats::model.matrix(
    stats::as.formula(paste("~", paste(pred, collapse = " + "))), data = dd
  )[, -1, drop = FALSE]
  y <- dd[[dv]]

  if (!is.null(seed)) set.seed(seed)

  # --- ridge step -----------------------------------------------------------
  ridge_fits <- lapply(seq_len(n_cv), function(i) {
    glmnet::cv.glmnet(x, y, alpha = 0, nfolds = nfolds,
                      type.measure = "mse", standardize = TRUE)
  })
  ridge_lam  <- stats::median(vapply(ridge_fits, function(f) f[[paste0("lambda.", lambda)]],
                                     numeric(1)))
  ridge_coef <- as.numeric(stats::coef(ridge_fits[[1]], s = ridge_lam))[-1]

  if (any(ridge_coef == 0)) {
    warning(sum(ridge_coef == 0), " ridge coefficient(s) were exactly zero; ",
            "those predictors are excluded from the lasso entirely.",
            call. = FALSE)
  }
  pen <- 1 / abs(ridge_coef)

  # --- adaptive lasso step --------------------------------------------------
  las_fits <- lapply(seq_len(n_cv), function(i) {
    glmnet::cv.glmnet(x, y, alpha = 1, nfolds = nfolds,
                      type.measure = "mse", penalty.factor = pen)
  })
  las_lam <- stats::median(vapply(las_fits, function(f) f[[paste0("lambda.", lambda)]],
                                  numeric(1)))

  cf  <- stats::coef(las_fits[[1]], s = las_lam)
  cfv <- stats::setNames(as.numeric(cf), rownames(cf))

  slopes   <- cfv[names(cfv) != "(Intercept)"]
  selected <- names(slopes)[slopes != 0]
  dropped  <- names(slopes)[slopes == 0]

  # --- optional R-squared ---------------------------------------------------
  r2_val <- NULL
  if (r2) {
    yhat   <- as.numeric(stats::predict(las_fits[[1]], newx = x, s = las_lam))
    r2_val <- 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
  }

  # --- optional lm refit ----------------------------------------------------
  lm_fit <- NULL
  if (refit) {
    if (length(selected) == 0L) {
      warning("No predictors selected; skipping the lm() refit.", call. = FALSE)
    } else {
      lm_fit <- stats::lm(
        stats::as.formula(paste(dv, "~", paste(selected, collapse = " + "))),
        data = dd
      )
    }
  }

  # --- print ----------------------------------------------------------------
  if (!quiet) {
    cat("\nAdaptive lasso\n")
    cat(sprintf("  %s rows used", formatC(nrow(dd), format = "d", big.mark = ",")))
    if (nrow(dd) < n_before) {
      cat(sprintf(" (%s dropped for missing data)",
                  formatC(n_before - nrow(dd), format = "d", big.mark = ",")))
    }
    cat("\n")
    cat(sprintf("  lambda.%s = %.5f, median of %d CV run%s\n\n",
                lambda, las_lam, n_cv, if (n_cv == 1L) "" else "s"))

    cat(sprintf("Selected (%d)\n", length(selected)))
    if (length(selected) == 0L) {
      cat("  none\n")
    } else {
      ord <- selected[order(abs(slopes[selected]), decreasing = TRUE)]
      w   <- max(nchar(ord))
      cat(sprintf("  %-*s  %8.4f\n", w, "(Intercept)",
                  cfv[["(Intercept)"]]))
      for (v in ord) cat(sprintf("  %-*s  %8.4f\n", w, v, slopes[[v]]))
    }
    cat("\n")

    if (length(dropped) > 0L) {
      cat(sprintf("Dropped (%d)\n  %s\n\n",
                  length(dropped), paste(dropped, collapse = ", ")))
    }

    if (!is.null(r2_val)) {
      cat(sprintf("R-squared %.4f\n\n", r2_val))
    }

    if (!is.null(lm_fit)) {
      cat("lm() refit on the selected predictors:\n")
      print(summary(lm_fit)$coefficients)
      cat("\n  Note: these p-values do not account for the selection step,\n")
      cat("  so they are anticonservative. Do not report them as ordinary\n")
      cat("  p-values.\n\n")
    }
  }

  invisible(list(
    coefficients = cfv,
    selected     = selected,
    dropped      = dropped,
    lambda       = las_lam,
    ridge_lambda = ridge_lam,
    data         = dd,
    fit          = las_fits[[1]],
    r2           = r2_val,
    lm           = lm_fit
  ))
}
