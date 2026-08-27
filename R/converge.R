#' Get a non-converging lmer model to converge
#'
#' @description
#' Works through a fixed sequence of remedies for a mixed model that failed to
#' converge, stopping as soon as one works:
#'
#' 1. Refit with a different optimizer and more iterations.
#' 2. Drop the correlations between random intercepts and slopes (`||`), which
#'    requires refitting through `afex::lmer_alt()`.
#' 3. Drop random slopes one at a time -- highest-order term first, and among
#'    those the one with the smallest variance -- refitting after each.
#'
#' This is the approach reported in Sidhu et al. (2026).
#'
#' Sidhu, D. M., Parajuli, P., & Muraki, E. J. (2026). That sounds exciting!
#' Emotional iconicity facilitates processing and recall. \emph{Journal of
#' Memory and Language}, \emph{150}, 104769.
#'
#' Once a model converges, `restore = TRUE` tries to walk back the last two
#' steps: first putting the correlations back, then returning to the original
#' optimizer settings, keeping each only if convergence survives.
#'
#' As with any automated analysis, this is a tool that should be used along with
#' careful thought. Please don't rely on this without understanding the steps
#' being taken and their purpose.
#'
#' Only `lmer` models are handled. `converge()` stops on a `glmer` model rather
#' than simplifying it: convergence trouble in a generalized model often comes
#' from separation or predictor scaling, which pruning random slopes will not
#' fix.
#'
#' @param m A fitted `lmerMod` (or `lmerModLmerTest`) object, or a formula --
#'   the same one you would give `lme4::lmer()`. A formula is fitted first with
#'   `optimizer` and `maxfun`, so the first line of output is always the result
#'   of that initial fit.
#' @param data The data to fit or refit on. Required when `m` is a formula;
#'   recovered from the model call when `m` is already a fitted model.
#' @param optimizer Optimizer to try at step 1. Default `"bobyqa"`.
#' @param maxfun Iteration limit to try at step 1. Default 2e5.
#' @param singular_fails Logical. If TRUE (default), a singular fit counts as a
#'   failure and simplification continues.
#' @param var_method How to summarise variance for a term that occupies several
#'   rows in `VarCorr()` -- a factor with more than two levels, whose contrast
#'   columns each get their own variance. `"min"` (default) judges the factor by
#'   its lowest-variance slope; `"sum"` adds them.
#' @param restore Logical. If TRUE (default), try to undo the last remedies
#'   once the model converges.
#' @param max_drops Maximum number of random slopes to drop before giving up.
#'   Default 20.
#' @param p_values Logical. If TRUE (default), convert the final model with
#'   `lmerTest::as_lmerModLmerTest()` so that `summary()` reports Satterthwaite
#'   degrees of freedom and p-values for the fixed effects.
#' @param verbose Logical. If TRUE (default), report each step.
#' @return The converged model, or the last model tried if nothing worked.
#'
#' @param reml Logical. Used only when `m` is a formula: fit with REML
#'   (default TRUE) or maximum likelihood. Ignored when `m` is already a fitted
#'   model, whose own REML setting is read instead.
#'
#' @examples
#' \dontrun{
#' m1 <- lmer(RT ~ Iconicity * Freq + (1 + Iconicity | Subject) +
#'                                    (1 + Freq | Item), data = d)
#' m2 <- converge(m1)
#'
#' # or hand it the formula directly
#' m2 <- converge(RT ~ Iconicity * Freq + (1 + Iconicity | Subject) +
#'                                        (1 + Freq | Item), data = d)
#' }
#'
#' @export
converge <- function(m,
                     data = NULL,
                     reml = TRUE,
                     optimizer = "bobyqa",
                     maxfun = 2e5,
                     singular_fails = TRUE,
                     var_method = c("min", "sum"),
                     restore = TRUE,
                     max_drops = 20,
                     p_values = TRUE,
                     verbose = TRUE) {

  var_method <- match.arg(var_method)

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("`converge()` requires the 'lme4' package.", call. = FALSE)
  }

  log <- character(0)
  say <- function(...) {
    txt <- paste0(...)
    log <<- c(log, txt)
    if (verbose) message(txt)
  }

  # a formula gets fitted first, with the same optimizer settings the rest of
  # the function would try in step 1 -- so the log always starts from a real
  # attempt rather than silently doing the fit no one saw
  skip_step1 <- FALSE

  if (inherits(m, "formula")) {
    if (is.null(data)) {
      stop("`data` is required when `m` is a formula.", call. = FALSE)
    }
    say("Fitting the formula with optimizer = '", optimizer,
        "', maxfun = ", format(maxfun, scientific = FALSE), ".")

    ctrl <- lme4::lmerControl(optimizer = optimizer,
                              optCtrl = list(maxfun = maxfun))
    m <- tryCatch(
      lme4::lmer(m, data = data, control = ctrl, REML = reml),
      error = function(e) {
        stop("Could not fit that formula: ", conditionMessage(e),
             call. = FALSE)
      }
    )

    if (conv_ok(m, singular_fails)) {
      say("  Converged on the first fit.")
      return(converge_out(m, log, verbose, p_values))
    }
    say("  Still not converging.")
    skip_step1 <- TRUE   # step 1 would just repeat the fit above
  }

  if (!inherits(m, "merMod")) {
    stop("`m` must be a model fitted with lmer(), or a formula.",
         call. = FALSE)
  }
  if (inherits(m, "glmerMod")) {
    fam <- tryCatch(stats::family(m)$family, error = function(e) NULL)
    stop("`converge()` is currently only built for linear models.\n",
         "  This is a generalized mixed model",
         if (!is.null(fam)) paste0(" (family = ", fam, ")") else "",
         ", and refitting it with lmer()\n",
         "  would silently treat the outcome as continuous.",
         call. = FALSE)
  }

  # --- recover the data -----------------------------------------------------
  if (is.null(data)) {
    data <- tryCatch(
      eval(stats::getCall(m)$data, environment(stats::formula(m))),
      error = function(e) NULL
    )
    if (is.null(data)) {
      stop("Could not recover the data from the model call; pass `data`.",
           call. = FALSE)
    }
  }

  use_reml   <- lme4::isREML(m)
  orig_form  <- stats::formula(m)
  orig_ctrl  <- eval(stats::getCall(m)$control)

  # --- already fine? --------------------------------------------------------
  if (conv_ok(m, singular_fails)) {
    say("Model already converged; nothing to do.")
    return(converge_out(m, log, verbose, p_values))
  }

  # --- step 1: optimizer ----------------------------------------------------
  if (skip_step1) {
    say("Step 1: already tried above with these settings; moving on.")
    fit <- m
  } else {
    say("Step 1: refitting with optimizer = '", optimizer,
        "', maxfun = ", format(maxfun, scientific = FALSE), ".")

    fit <- try_fit(orig_form, data, optimizer, maxfun, use_reml, decor = FALSE)

    if (!is.null(fit) && conv_ok(fit, singular_fails)) {
      say("  Converged.")
      return(converge_out(fit, log, verbose, p_values))
    }
    say(if (is.null(fit)) "  Refit failed." else "  Still not converging.")
  }

  bars  <- bars_of(orig_form)
  fixed <- deparse_one(lme4::nobars(orig_form))

  # --- step 2: drop correlations --------------------------------------------
  say("Step 2: dropping intercept-slope correlations (||, via afex::lmer_alt).")

  if (!requireNamespace("afex", quietly = TRUE)) {
    stop("Dropping correlations requires the 'afex' package.", call. = FALSE)
  }

  decor <- sub("\\|", "||", bars)
  form2 <- build_form(fixed, decor)

  fit <- try_fit(form2, data, optimizer, maxfun, use_reml, decor = TRUE)

  if (!is.null(fit) && conv_ok(fit, singular_fails)) {
    say("  Converged.")
    fit <- maybe_restore(fit, fixed, decor, data, optimizer, maxfun, use_reml,
                         orig_ctrl, singular_fails, restore, say)
    return(converge_out(fit, log, verbose, p_values))
  }
  say(if (is.null(fit)) "  Refit failed." else "  Still not converging.")

  # --- step 3: drop slopes --------------------------------------------------
  current <- decor
  last    <- fit

  for (step in seq_len(max_drops)) {

    if (is.null(last)) {
      say("  Refit failed outright; cannot rank variances. Stopping.")
      break
    }

    drop <- pick_slope(last, current, var_method, data)

    if (is.null(drop)) {
      say("Step 3: no random slopes left to drop.")
      break
    }

    say("Step 3.", step, ": dropping (", drop$term, " | ", drop$group, ")",
        if (is.na(drop$var)) " [variance unmatched]" else
          sprintf(" [variance %.5f]", drop$var))

    current <- drop$bars
    if (all(!nzchar(current))) {
      say("  No random effects remain. Stopping.")
      break
    }

    form3 <- build_form(fixed, current)
    fit   <- try_fit(form3, data, optimizer, maxfun, use_reml, decor = TRUE)

    if (!is.null(fit) && conv_ok(fit, singular_fails)) {
      say("  Converged.")
      fit <- maybe_restore(fit, fixed, current, data, optimizer, maxfun,
                           use_reml, orig_ctrl, singular_fails, restore, say)
      return(converge_out(fit, log, verbose, p_values))
    }
    say(if (is.null(fit)) "  Refit failed." else "  Still not converging.")
    last <- fit
  }

  say("Gave up. Returning the last model tried -- treat it as unconverged.")
  converge_out(if (is.null(fit)) m else fit, log, verbose, p_values)
}


# --------------------------------------------------------------------------
# Internals
# --------------------------------------------------------------------------

# Internal: has this fit converged?
conv_ok <- function(fit, singular_fails = TRUE) {
  if (is.null(fit)) return(FALSE)
  msgs <- fit@optinfo$conv$lme4$messages
  ok   <- is.null(msgs) || length(msgs) == 0L
  if (ok && singular_fails && lme4::isSingular(fit)) ok <- FALSE
  ok
}


# Internal: deparse to a single string
deparse_one <- function(x) paste(deparse(x), collapse = " ")


# Internal: random-effect terms as strings, e.g. "1 + a | subj"
bars_of <- function(form) {
  vapply(lme4::findbars(form), deparse_one, character(1))
}


# Internal: rebuild a formula from its fixed part and bar strings
build_form <- function(fixed, bars) {
  bars <- bars[nzchar(bars)]
  stats::as.formula(
    paste(fixed, "+", paste0("(", bars, ")", collapse = " + ")),
    env = parent.frame()
  )
}


# Internal: fit, returning NULL rather than erroring
try_fit <- function(form, data, optimizer, maxfun, reml, decor) {
  ctrl <- lme4::lmerControl(optimizer = optimizer,
                            optCtrl = list(maxfun = maxfun))
  suppressWarnings(suppressMessages(tryCatch(
    do.call(if (decor) afex::lmer_alt else lme4::lmer,
            list(formula = form, data = data, control = ctrl, REML = reml)),
    error = function(e) NULL)))
}


# Internal: split a bar string into its slope terms and grouping factor
split_bar <- function(bar) {
  parts <- strsplit(bar, "\\|\\|?")[[1]]
  lhs   <- trimws(parts[1])
  group <- trimws(parts[length(parts)])

  # expand `*` into its main effects and interactions, so a slope written
  # (1 + a * b | g) is three droppable terms rather than one atomic one
  terms <- tryCatch(
    attr(stats::terms(stats::as.formula(paste("~", lhs))), "term.labels"),
    error = function(e) {
      x <- trimws(strsplit(lhs, "\\+")[[1]])
      x[nzchar(x) & !x %in% c("1", "0")]
    }
  )

  list(terms = terms, group = group, double = grepl("\\|\\|", bar))
}


# Internal: which model-matrix columns belong to each random-effect term
# "Cond" with levels low/mid/high -> c("Condmid", "Condhigh")
re_colnames <- function(terms, data) {
  if (length(terms) == 0L) return(list())
  f  <- stats::as.formula(paste("~", paste(terms, collapse = " + ")))
  mm <- stats::model.matrix(f, data = data)
  tl <- attr(stats::terms(f), "term.labels")
  asg <- attr(mm, "assign")

  out <- lapply(seq_along(tl), function(i) colnames(mm)[asg == i])
  names(out) <- tl
  out
}


# Internal: variance of a random term, from VarCorr
# `cols` are the model-matrix column names belonging to the term, so a factor
# is matched exactly rather than by guessing at suffixes. With method "min"
# a factor is judged by its lowest-variance contrast slope.
term_var <- function(fit, group, cols, method = "min") {
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) return(NA_real_)

  gnames <- names(vc)
  if (is.null(gnames)) return(NA_real_)

  # afex::lmer_alt splits `||` terms into Subject.1, Subject.2, ...
  belongs <- vapply(gnames, function(g) {
    if (identical(g, group)) return(TRUE)
    if (!startsWith(g, paste0(group, "."))) return(FALSE)
    grepl("^[0-9]+$", substring(g, nchar(group) + 2L))
  }, logical(1))

  # afex::lmer_alt renames columns: condb -> re1.condb, a:b -> re1.a_by_b
  unrename <- function(x) gsub("_by_", ":", sub("^re[0-9]+\\.", "", x))

  vals <- numeric(0)
  for (g in gnames[belongs]) {
    v  <- diag(as.matrix(vc[[g]]))
    nm <- names(v)
    if (is.null(nm)) next
    vals <- c(vals, v[unrename(nm) %in% cols])
  }

  if (length(vals) == 0L) return(NA_real_)
  if (method == "sum") sum(vals) else min(vals)
}


# Internal: choose the slope to drop -- highest order, then lowest variance
pick_slope <- function(fit, bars, var_method, data) {

  cand <- list()
  for (i in seq_along(bars)) {
    sb <- split_bar(bars[i])
    if (length(sb$terms) == 0L) next

    cols_map <- tryCatch(re_colnames(sb$terms, data),
                         error = function(e) list())

    for (tm in sb$terms) {
      cols <- if (!is.null(cols_map[[tm]])) cols_map[[tm]] else tm
      cand[[length(cand) + 1L]] <- list(
        i     = i,
        term  = tm,
        group = sb$group,
        order = length(strsplit(tm, ":")[[1]]),
        var   = term_var(fit, sb$group, cols, var_method)
      )
    }
  }
  if (length(cand) == 0L) return(NULL)

  ord  <- vapply(cand, function(z) z$order, integer(1))
  cand <- cand[ord == max(ord)]

  vars <- vapply(cand, function(z) z$var, numeric(1))
  pick <- if (all(is.na(vars))) cand[[1]] else cand[[which.min(vars)]]

  # rebuild the bar without that term
  sb    <- split_bar(bars[pick$i])
  keep  <- setdiff(sb$terms, pick$term)
  # (1 || g) is not valid; with no slopes left the bar must use a single |
  pipe  <- if (sb$double && length(keep) > 0L) "||" else "|"

  new_bars <- bars
  new_bars[pick$i] <- if (length(keep) == 0L) {
    paste("1", pipe, sb$group)
  } else {
    paste(paste(c("1", keep), collapse = " + "), pipe, sb$group)
  }

  list(term = pick$term, group = pick$group, var = pick$var, bars = new_bars)
}


# Internal: try putting correlations and original settings back
maybe_restore <- function(fit, fixed, bars, data, optimizer, maxfun, reml,
                          orig_ctrl, singular_fails, restore, say) {

  if (!restore) return(fit)

  # 1. correlations back
  recor <- gsub("\\|\\|", "|", bars)
  if (!identical(recor, bars)) {
    say("Restore: putting correlations back.")
    alt <- try_fit(build_form(fixed, recor), data, optimizer, maxfun, reml,
                   decor = FALSE)
    if (conv_ok(alt, singular_fails)) {
      say("  Kept -- still converges.")
      fit  <- alt
      bars <- recor
    } else {
      say("  Reverted.")
    }
  }

  # 2. original optimizer settings back
  if (!is.null(orig_ctrl)) {
    say("Restore: returning to the original optimizer settings.")
    alt <- suppressWarnings(suppressMessages(tryCatch(
      lme4::lmer(build_form(fixed, gsub("\\|\\|", "|", bars)),
                 data = data, control = orig_ctrl, REML = reml),
      error = function(e) NULL
    )))
    if (conv_ok(alt, singular_fails)) {
      say("  Kept -- still converges.")
      fit <- alt
    } else {
      say("  Reverted.")
    }
  }

  fit
}


# Internal: attach p-values, report, and return the model
converge_out <- function(fit, log, verbose, p_values = TRUE) {

  if (p_values && !inherits(fit, "lmerModLmerTest")) {
    if (requireNamespace("lmerTest", quietly = TRUE)) {
      fit <- tryCatch(lmerTest::as_lmerModLmerTest(fit),
                      error = function(e) fit)
    } else if (verbose) {
      message("Install the 'lmerTest' package for p-values.")
    }
  }

  if (verbose) {
    message("\nFinal formula:\n  ", deparse_one(stats::formula(fit)))
    cf <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
    if (!is.null(cf)) {
      cat("\nFixed effects:\n")
      stats::printCoefmat(cf, digits = 4, signif.stars = TRUE,
                          has.Pvalue = ncol(cf) >= 5L,
                          P.values = ncol(cf) >= 5L)
      cat("\n")
    }
  }

  fit
}
