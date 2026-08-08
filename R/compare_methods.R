#' Compare variable selection across several methods
#'
#' @description
#' Fits the same outcome and predictors with a range of approaches and provides
#' the results in one grid: methods as rows, predictors as columns.
#'
#' \describe{
#'   \item{`cor`}{Zero-order correlation with the outcome, marked when
#'     p < `alpha`. A second row applies a Benjamini-Hochberg correction across
#'     the predictors.}
#'   \item{`lm`}{Multiple regression, marked when p < `alpha`.}
#'   \item{`step`}{Stepwise regression in both directions using BIC.}
#'   \item{`alasso`}{Adaptive lasso; see [alasso()].}
#'   \item{`subsets`}{Exhaustive best subsets, model chosen by lowest BIC. With
#'     `subsets_boot = TRUE`, a second row marks predictors selected in at
#'     least `boot_cutoff` of bootstrap resamples.}
#'   \item{`rf`}{VSURF, which returns an interpretation set (all variables
#'     related to the outcome) and a smaller prediction set. With
#'     `rf_lm = TRUE`, each set is refitted with `lm()` and marked when
#'     p < `alpha`.}
#' }
#'
#' Rows are dropped if incomplete on the outcome or any predictor, and numeric
#' predictors are standardised, so all methods see identical data.
#'
#' Marks are `+` for a positive coefficient or correlation, `-` for a negative
#' one, and a tick for the two VSURF rows, which rank rather than sign. A blank
#' means not selected, or not significant.
#'
#' This is an automated version of the approach used in Sidhu et al. (2021).
#'
#' Sidhu, D. M., Westbury, C., Hollis, G., & Pexman, P. M. (2021). Sound
#' symbolism shapes the English language: The maluma/takete effect in English
#' nouns. \emph{Psychonomic Bulletin & Review}, \emph{28}, 1390-1398.
#'
#' `subsets_boot = TRUE` bootstraps the best-subsets procedure and adds a row
#' marking predictors selected in at least `boot_cutoff` of resamples, with the
#' full selection frequencies stored in the result.
#'
#' `rf_lm = TRUE`, on by default, refits an ordinary regression with each of
#' the two VSURF sets and adds a row for each, marked only where a predictor
#' reaches `alpha`.
#'
#' `summary()` on the result prints the full output of any single method, using
#' the same short labels that head the columns of the grid; calling it with no
#' method lists what is available. So `summary(res)` lists the methods,
#' `summary(res, "Reg")` gives the multiple regression, and `summary(res,
#' "aLasso")` gives the adaptive lasso. Every fitted object is also kept, so `m
#' <- summary(res, "Reg")` returns the model itself, ready to pass to `anova()`
#' or `plot()`.
#'
#' @param d A data.frame or data.table.
#' @param dv Character: name of the outcome column.
#' @param predictors Character or numeric: the predictor columns.
#' @param methods Which methods to run. Default all six.
#' @param alpha Significance cutoff, used for the regression row and both
#'   correlation rows, and for the RF refits. Default 0.05.
#' @param scale Logical. If TRUE (default), standardise numeric predictors.
#' @param scale_coded Logical. If TRUE (default), also standardise two-level
#'   predictors that were effects coded, putting every predictor on a common
#'   scale -- which is what the penalised methods assume. Set FALSE to leave
#'   them at -0.5/0.5, so their coefficient is the difference between groups.
#' @param seed Optional integer seed, passed to every method that uses
#'   randomness.
#' @param n_cv Cross-validation runs for the adaptive lasso. Default 11.
#' @param nvmax Largest subset size for best subsets. Defaults to the number of
#'   predictors.
#' @param subsets_boot Logical. If TRUE, also bootstrap the best-subsets
#'   procedure and add a row of predictors selected in at least `boot_cutoff`
#'   of resamples. Default FALSE.
#' @param subsets_B Bootstrap resamples when `subsets_boot = TRUE`. Default 100.
#' @param subsets_method Search method for `leaps::regsubsets()`.
#'   `"exhaustive"` (default) is thorough but slow with many predictors;
#'   `"backward"` or `"forward"` are far faster.
#' @param rf_args Optional named list passed to `VSURF::VSURF()`, e.g.
#'   `list(nfor.thres = 20, nfor.interp = 10, nfor.pred = 10, ntree = 500)`.
#'   Lowering these is the single biggest speed-up available.
#' @param boot_cutoff Selection frequency needed for a mark on the bootstrap
#'   row. Default 0.5.
#' @param rf_lm Logical. If TRUE (default), refit `lm()` with each VSURF set
#'   and add a row for each, marked at p < `alpha`.
#' @param rf_parallel Logical. If TRUE, run VSURF in parallel. Default FALSE.
#' @param ncores Cores for VSURF when `rf_parallel = TRUE`.
#' @param quiet Logical. If TRUE, suppress progress messages.
#'
#' @return An object of class `classir_compare`, with `grid` (the marks),
#'   `coefficients`, `fits` (every fitted object), `subset_freq` when
#'   bootstrapped, and `n`.
#'
#' @section Runtime:
#' VSURF fits a great many forests and is by far the slowest step -- minutes to
#' hours on a large dataset. Dropping `"rf"` from `methods` will speed things
#' up.
#'
#' @section Interpreting the grid:
#' The intention is to read and interpret the whole grid, to examine robustness
#' of different effects across analysis methods. This is not intended for
#' running all methods and reporting whichever agrees with your hypothesis.
#'
#' P-values on the RF refit rows do not account for VSURF having chosen the
#' predictors, so they are anticonservative. Treat those rows as descriptive.
#'
#' @examples
#' \dontrun{
#' compare_methods(d, "P_Iconicity", pred_vars, seed = 999)
#' compare_methods(d, "P_Iconicity", pred_vars,
#'                 methods = c("cor", "lm", "step", "alasso"))
#' }
#'
#' @export
compare_methods <- function(d,
                            dv,
                            predictors,
                            methods = c("cor", "lm", "step", "alasso",
                                        "subsets", "rf"),
                            alpha = 0.05,
                            scale = TRUE,
                            scale_coded = TRUE,
                            seed = NULL,
                            n_cv = 11,
                            nvmax = NULL,
                            subsets_boot = FALSE,
                            subsets_B = 100,
                            subsets_method = c("exhaustive", "backward",
                                               "forward", "seqrep"),
                            boot_cutoff = 0.5,
                            rf_lm = TRUE,
                            rf_args = list(),
                            rf_parallel = FALSE,
                            ncores = NULL,
                            quiet = FALSE) {

  methods <- match.arg(methods, several.ok = TRUE)
  subsets_method <- match.arg(subsets_method)

  # once per session
  if (is.null(.classir_session$warned_compare)) {
    message("compare_methods() shows how robust an effect is across analysis ",
            "methods. It is not for choosing which analysis to report based ",
            "on the pattern of results.")
    .classir_session$warned_compare <- TRUE
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
  if (length(pred) < 2L) stop("Need at least two predictors.", call. = FALSE)
  if (!is.numeric(d[[dv]])) stop("`dv` must be numeric.", call. = FALSE)

  # --- complete cases and scaling ------------------------------------------
  n_before <- nrow(d)
  keep <- stats::complete.cases(d[, c(dv, pred), drop = FALSE])
  dd <- d[keep, c(dv, pred), drop = FALSE]
  rownames(dd) <- NULL
  n <- nrow(dd)

  if (n < 10L) stop("Only ", n, " complete row(s).", call. = FALSE)

  cat_pred <- pred[vapply(dd[pred], function(z) {
    is.factor(z) || is.character(z) || is.logical(z)
  }, logical(1))]

  # categorical predictors: two levels get effects coded, more than two stop
  coded <- character(0)
  for (v in cat_pred) {
    lv <- if (is.factor(dd[[v]])) {
      levels(droplevels(dd[[v]]))
    } else {
      sort(unique(as.character(dd[[v]])))
    }

    if (length(lv) > 2L) {
      stop("`", v, "` has ", length(lv), " levels. Dummy coding splits it ",
           "into ", length(lv) - 1L, " columns, and the correlations, ",
           "regression, adaptive lasso, best subsets and VSURF all treat ",
           "those columns as separate predictors -- so the factor can be ",
           "partly selected and partly dropped, which is not interpretable. ",
           "Stepwise regression instead adds and drops the factor as a whole, ",
           "so the grid would not be comparing like with like. Reduce `", v,
           "` to a single numeric contrast, or leave it out, before calling ",
           "compare_methods().", call. = FALSE)
    }

    if (length(lv) < 2L) {
      stop("`", v, "` has only one level among the complete cases.",
           call. = FALSE)
    }

    dd[[v]] <- ifelse(as.character(dd[[v]]) == lv[1], -0.5, 0.5)
    coded <- c(coded, v)
    warning("Effects coded `", v, "`: ", lv[1], " = -0.5, ", lv[2], " = 0.5. ",
            if (scale && scale_coded) {
              "Standardised with the other predictors, so its coefficient is a per-SD change."
            } else {
              "Left unscaled, so its coefficient is the difference between the two groups."
            },
            call. = FALSE)
  }
  cat_pred <- character(0)

  if (scale) {
    scale_these <- if (scale_coded) pred else setdiff(pred, coded)
    for (v in scale_these) {
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
  terms_all <- colnames(x)

  # a data frame version of the design matrix, for the RF refits
  xdf <- as.data.frame(x)
  safe <- make.names(terms_all, unique = TRUE)
  names(xdf) <- safe
  xdf[[".y"]] <- y

  say <- function(...) if (!quiet) message(...)

  row_names <- character(0)
  marks <- list()
  fits  <- list()
  subset_freq <- NULL

  blank_row <- function() stats::setNames(rep(NA_real_, length(terms_all)),
                                          terms_all)

  short_names <- character(0)
  add_row <- function(nm, short, row) {
    row_names   <<- c(row_names, nm)
    short_names <<- c(short_names, short)
    marks[[nm]] <<- row
  }

  full_fml <- stats::as.formula(paste(dv, "~", paste(pred, collapse = " + ")))

  # --- zero-order correlations ---------------------------------------------
  if ("cor" %in% methods) {
    say("Computing zero-order correlations...")

    rs <- ps <- stats::setNames(rep(NA_real_, length(terms_all)), terms_all)
    for (tm in terms_all) {
      ct <- suppressWarnings(tryCatch(stats::cor.test(x[, tm], y),
                                      error = function(e) NULL))
      if (!is.null(ct)) {
        rs[[tm]] <- unname(ct$estimate)
        ps[[tm]] <- ct$p.value
      }
    }
    fits$cor <- data.frame(term = terms_all, r = unname(rs), p = unname(ps),
                           p_bh = stats::p.adjust(ps, method = "BH"),
                           row.names = NULL, stringsAsFactors = FALSE)

    row_r <- blank_row()
    row_r[!is.na(ps) & ps < alpha] <- rs[!is.na(ps) & ps < alpha]
    add_row("Correlation", "Corr", row_r)

    p_bh <- stats::p.adjust(ps, method = "BH")
    row_b <- blank_row()
    row_b[!is.na(p_bh) & p_bh < alpha] <- rs[!is.na(p_bh) & p_bh < alpha]
    add_row("Correlation (BH)", "Corr-BH", row_b)
  }

  # --- multiple regression --------------------------------------------------
  if ("lm" %in% methods) {
    say("Fitting multiple regression...")
    m_lm <- stats::lm(full_fml, data = dd)
    fits$lm <- m_lm

    sm  <- summary(m_lm)$coefficients
    row <- blank_row()
    for (tm in terms_all) {
      if (tm %in% rownames(sm) && sm[tm, 4] < alpha) row[[tm]] <- sm[tm, 1]
    }
    add_row("Regression", "Reg", row)
  }

  # --- stepwise BIC ---------------------------------------------------------
  if ("step" %in% methods) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Method 'step' requires the 'MASS' package.", call. = FALSE)
    }
    say("Running stepwise BIC (both directions)...")
    m_step <- MASS::stepAIC(stats::lm(full_fml, data = dd),
                            direction = "both", k = log(n), trace = 0)
    fits$step <- m_step

    cf  <- stats::coef(m_step)
    row <- blank_row()
    for (tm in terms_all) if (tm %in% names(cf)) row[[tm]] <- cf[[tm]]
    add_row("Stepwise BIC", "Stepwise", row)
  }

  # --- adaptive lasso -------------------------------------------------------
  if ("alasso" %in% methods) {
    say("Running adaptive lasso...")
    al <- alasso(dd, dv = dv, predictors = pred, n_cv = n_cv, seed = seed,
                 scale = FALSE, quiet = TRUE)
    fits$alasso <- al

    cf  <- al$coefficients
    row <- blank_row()
    for (tm in terms_all) {
      if (tm %in% names(cf) && cf[[tm]] != 0) row[[tm]] <- cf[[tm]]
    }
    add_row("Adaptive lasso", "aLasso", row)
  }

  # --- best subsets ---------------------------------------------------------
  if ("subsets" %in% methods) {
    if (!requireNamespace("leaps", quietly = TRUE)) {
      stop("Method 'subsets' requires the 'leaps' package.", call. = FALSE)
    }
    say("Running best subsets...")
    nv <- if (is.null(nvmax)) ncol(x) else nvmax

    rs_fit <- leaps::regsubsets(x = x, y = y, nvmax = nv,
                                method = subsets_method,
                                really.big = ncol(x) > 25)
    best <- which.min(summary(rs_fit)$bic)
    cf   <- stats::coef(rs_fit, best)
    fits$subsets <- rs_fit

    row <- blank_row()
    for (tm in terms_all) if (tm %in% names(cf)) row[[tm]] <- cf[[tm]]
    add_row("Best subsets", "Subsets", row)

    if (subsets_boot) {
      say("Bootstrapping best subsets (", subsets_B, " resamples)...")
      if (!is.null(seed)) set.seed(seed)

      hits <- stats::setNames(rep(0L, length(terms_all)), terms_all)
      for (b in seq_len(subsets_B)) {
        i <- sample.int(n, n, replace = TRUE)
        fb <- tryCatch(
          leaps::regsubsets(x = x[i, , drop = FALSE], y = y[i], nvmax = nv,
                            method = subsets_method,
                            really.big = ncol(x) > 25),
          error = function(e) NULL
        )
        if (is.null(fb)) next
        kb <- which.min(summary(fb)$bic)
        nb <- names(stats::coef(fb, kb))
        hits[intersect(terms_all, nb)] <- hits[intersect(terms_all, nb)] + 1L
      }
      subset_freq <- hits / subsets_B

      row_b <- blank_row()
      sel <- names(subset_freq)[subset_freq >= boot_cutoff]
      for (tm in sel) {
        row_b[[tm]] <- if (tm %in% names(cf)) cf[[tm]] else
          stats::coef(stats::lm(stats::reformulate(safe[match(tm, terms_all)],
                                                   response = ".y"),
                                data = xdf))[[2]]
      }
      add_row("Subsets (boot)", "Subs-Boot", row_b)
    }
  }

  # --- VSURF ----------------------------------------------------------------
  if ("rf" %in% methods) {
    if (!requireNamespace("VSURF", quietly = TRUE)) {
      stop("Method 'rf' requires the 'VSURF' package.", call. = FALSE)
    }
    say("Running VSURF -- this is the slow one...")
    if (!is.null(seed)) set.seed(seed)

    vs_args <- c(list(x = x, y = y), rf_args)
    if (rf_parallel) {
      vs_args$parallel <- TRUE
      if (!is.null(ncores)) vs_args$ncores <- ncores
    }
    vs <- do.call(VSURF::VSURF, vs_args)
    fits$rf <- vs

    interp <- if (length(vs$varselect.interp) > 0L) {
      terms_all[vs$varselect.interp]
    } else character(0)

    predst <- if (length(vs$varselect.pred) > 0L) {
      terms_all[vs$varselect.pred]
    } else character(0)

    if (length(predst) == 0L) {
      say("  VSURF returned no prediction set; that row will be blank.")
    }

    row_i <- blank_row(); row_i[interp] <- 1
    row_p <- blank_row(); row_p[predst] <- 1
    add_row("RF interpretation", "RF-Interp", row_i)
    add_row("RF prediction", "RF-Pred", row_p)

    if (rf_lm) {
      refit_row <- function(sel) {
        row <- blank_row()
        if (length(sel) == 0L) return(list(row = row, fit = NULL))
        nm  <- safe[match(sel, terms_all)]
        mod <- stats::lm(stats::reformulate(nm, response = ".y"), data = xdf)
        sm  <- summary(mod)$coefficients
        for (i in seq_along(sel)) {
          if (nm[i] %in% rownames(sm) && sm[nm[i], 4] < alpha) {
            row[[sel[i]]] <- sm[nm[i], 1]
          }
        }
        list(row = row, fit = mod)
      }

      say("Refitting lm() with each VSURF set...")
      ri <- refit_row(interp)
      rp <- refit_row(predst)
      if (!is.null(ri$fit)) fits$rf_lm_interp <- ri$fit
      if (!is.null(rp$fit)) fits$rf_lm_pred   <- rp$fit
      add_row("RF interp \u2192 lm", "RF-Int-Reg", ri$row)
      add_row("RF pred \u2192 lm", "RF-Pred-Reg", rp$row)
    }
  }

  say("Done.")

  grid <- do.call(rbind, marks[row_names])
  rownames(grid) <- row_names

  structure(
    list(grid         = grid,
         coefficients = grid,
         fits         = fits,
         subset_freq  = subset_freq,
         n            = n,
         n_dropped    = n_before - n,
         dv           = dv,
         alpha        = alpha,
         short        = stats::setNames(short_names, row_names),
         terms        = terms_all,
         rf_rows      = c("RF interpretation", "RF prediction")),
    class = "classir_compare"
  )
}


#' Print a compare_methods grid
#'
#' Variables run down the side, methods across the top.
#'
#' @param x A `classir_compare` object.
#' @param colour Logical. Use ANSI colour. Defaults to `interactive()`.
#' @param abbrev Optional integer. Truncate variable names to this many
#'   characters.
#' @param width Console width to wrap at. Defaults to `getOption("width")`.
#' @param ... Ignored.
#'
#' @export
print.classir_compare <- function(x, colour = interactive(), abbrev = NULL,
                                  width = NULL, ...) {

  g       <- x$grid
  methods <- rownames(g)
  terms   <- colnames(g)

  short <- if (!is.null(x$short)) x$short[methods] else methods
  names(short) <- methods

  lab <- terms
  if (!is.null(abbrev)) {
    long <- nchar(lab) > abbrev
    lab[long] <- substr(lab[long], 1, abbrev)
  }

  green <- function(s) if (colour) paste0("\033[32m", s, "\033[39m") else s
  red   <- function(s) if (colour) paste0("\033[31m", s, "\033[39m") else s

  # the plain glyph, so widths are computed before any escape codes are added
  glyph <- function(val, method) {
    if (is.na(val)) return(" ")
    if (method %in% x$rf_rows) return("\u2713")
    if (val > 0) "+" else "\u2212"
  }

  # pad first, colour second -- escape codes have width on paper but not screen
  cell <- function(val, method, w) {
    txt <- centre_str(glyph(val, method), w)
    if (is.na(val)) return(txt)
    if (method %in% x$rf_rows || val > 0) green(txt) else red(txt)
  }

  lab_w <- max(nchar(lab))
  col_w <- pmax(nchar(short), 3L)
  scr   <- if (is.null(width)) getOption("width", 80) else width
  avail <- max(scr - lab_w - 2L, 20L)

  # --- header ---------------------------------------------------------------
  cat("\n  DEPENDENT VARIABLE: ", x$dv, "\n", sep = "")
  cat(sprintf("  %s observations", format(x$n, big.mark = ",")))
  if (x$n_dropped > 0L) {
    cat(sprintf(", %s dropped", format(x$n_dropped, big.mark = ",")))
  }
  cat(sprintf("   alpha = %s\n", format(x$alpha)))

  # --- grid, wrapped by method if need be -----------------------------------
  blocks <- list()
  i <- 1L
  while (i <= length(methods)) {
    used <- 0L
    j <- i
    while (j <= length(methods) && used + col_w[j] + 1L <= avail) {
      used <- used + col_w[j] + 1L
      j <- j + 1L
    }
    if (j == i) j <- i + 1L
    blocks[[length(blocks) + 1L]] <- i:(j - 1L)
    i <- j
  }

  for (ch in blocks) {
    cat("\n", strrep(" ", lab_w), " ", sep = "")
    cat(paste(mapply(centre_str, short[ch], col_w[ch]), collapse = " "),
        "\n", sep = "")

    for (v in seq_along(terms)) {
      cat(formatC(lab[v], width = -lab_w), " ", sep = "")
      cells <- vapply(ch, function(m) {
        cell(g[methods[m], terms[v]], methods[m], col_w[m])
      }, character(1))
      cat(paste(cells, collapse = " "), "\n", sep = "")
    }
  }

  # --- legend ---------------------------------------------------------------
  cat("\n  + positive   \u2212 negative   \u2713 selected   blank not selected\n")
  cat("  ", paste(sprintf("%s = %s", short, methods), collapse = "   "),
      "\n\n", sep = "")

  invisible(x)
}


# Internal: flags for things that should happen once per session
.classir_session <- new.env(parent = emptyenv())

#' Summarise one method from a compare_methods grid
#'
#' Prints the full output of a single method. Call it with no `method` to see
#' the labels available -- they are the same short labels used as column
#' headings in the grid.
#'
#' @param object A `classir_compare` object.
#' @param method The method to summarise, e.g. `"Reg"` or `"aLasso"`. Case is
#'   ignored and partial names are matched.
#' @param ... Ignored.
#'
#' @return Invisibly, the underlying fitted object, so it can be passed on to
#'   other functions.
#'
#' @examples
#' \dontrun{
#' res <- compare_methods(d, "iconicity", preds)
#' summary(res)             # what's available
#' summary(res, "Reg")      # the multiple regression
#' summary(res, "aLasso")   # the adaptive lasso
#' }
#'
#' @export
summary.classir_compare <- function(object, method = NULL, ...) {

  short <- object$short
  keys  <- c("Correlation"       = "cor",
             "Correlation (BH)"  = "cor",
             "Regression"        = "lm",
             "Stepwise BIC"      = "step",
             "Adaptive lasso"    = "alasso",
             "Best subsets"      = "subsets",
             "Subsets (boot)"    = "subsets",
             "RF interpretation" = "rf",
             "RF prediction"     = "rf",
             "RF interp \u2192 lm"  = "rf_lm_interp",
             "RF pred \u2192 lm"    = "rf_lm_pred")

  avail <- names(short)[names(short) %in% names(keys)]
  avail <- avail[keys[avail] %in% names(object$fits)]

  if (is.null(method)) {
    cat("\nMethods in this comparison:\n\n")
    for (m in avail) {
      cat(sprintf("  %-12s %s\n", short[[m]], m))
    }
    cat("\nsummary(x, \"", short[[avail[1]]], "\") for any of them.\n\n",
        sep = "")
    return(invisible(NULL))
  }

  hit <- which(tolower(short[avail]) == tolower(method) |
               tolower(avail) == tolower(method))
  if (length(hit) == 0L) {
    hit <- grep(paste0("^", tolower(method)), tolower(short[avail]))
  }
  if (length(hit) == 0L) {
    stop("No method matching '", method, "'. Available: ",
         paste(short[avail], collapse = ", "), call. = FALSE)
  }
  if (length(hit) > 1L) {
    stop("'", method, "' matches several: ",
         paste(short[avail][hit], collapse = ", "), call. = FALSE)
  }

  full <- avail[hit]
  fit  <- object$fits[[keys[[full]]]]

  cat("\n", full, "\n\n", sep = "")

  if (keys[[full]] == "cor") {
    tab <- fit
    tab <- tab[order(abs(tab$r), decreasing = TRUE), ]
    tab$r    <- round(tab$r, 3)
    tab$p    <- signif(tab$p, 3)
    tab$p_bh <- signif(tab$p_bh, 3)
    print(tab, row.names = FALSE)

  } else if (keys[[full]] == "alasso") {
    cf  <- fit$coefficients
    sel <- cf[names(cf) %in% fit$selected]
    cat("lambda = ", signif(fit$lambda, 4), "\n\n", sep = "")
    print(round(c(`(Intercept)` = unname(cf[["(Intercept)"]]),
                  sort(sel, decreasing = TRUE)), 4))
    if (length(fit$dropped)) {
      cat("\nDropped: ", paste(fit$dropped, collapse = ", "), "\n", sep = "")
    }

  } else if (keys[[full]] == "subsets") {
    sm   <- summary(fit)
    best <- which.min(sm$bic)
    cat("Model chosen by BIC (", best, " predictor",
        if (best == 1L) "" else "s", "):\n\n", sep = "")
    print(round(stats::coef(fit, best), 4))
    if (!is.null(object$subset_freq)) {
      cat("\nBootstrap selection frequencies:\n")
      print(round(sort(object$subset_freq, decreasing = TRUE), 3))
    }

  } else if (keys[[full]] == "rf") {
    print(summary(fit))

  } else {
    print(summary(fit))
  }

  cat("\n")
  invisible(fit)
}

