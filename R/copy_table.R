#' Copy a table to the clipboard, ready to paste into Excel
#'
#' @description
#' Takes model output -- or any table -- and puts it on the clipboard as
#' tab-separated text, so pasting into Excel drops each value into its own cell
#' without going through Text to Columns. Numbers are rounded and written in
#' plain notation, so nothing arrives as `1.2e-16`, and row names become a first
#' column so predictor names come across too.
#'
#' `p_nice = TRUE`, the default, formats p-value columns the way a paper would:
#' significance stars, anything below the rounding threshold as `< .001`, and no
#' zero before the decimal point.
#'
#' `names_nice = TRUE`, the default, tidies the term names. Interactions are
#' joined with a multiplication sign rather than a colon, and a categorical
#' predictor's terms are written out against their reference level, so
#' `PoSverb` becomes `PoS (Verb vs. Noun)`.
#'
#' `rename` relabels terms, with the old name on the left as elsewhere in the
#' package: `rename = c(PoS = "Part of Speech")`.
#'
#' `drop_cols` removes columns you do not report, and `file` writes the table
#' out instead of only copying it.
#'
#' @details
#' Works with `lm`, `glm`, `lmer` and `lmerTest` models, `anova` tables, and
#' anything that is already a data frame or matrix. For a model, the fixed
#' effects table is used -- the same thing `summary()` prints.
#'
#' On macOS this uses `pbcopy` and on Windows the system clipboard. On Linux it
#' needs `xclip` or `xsel`; without one, the text is printed to copy by hand.
#'
#' @param x A model, an anova table, a data frame or a matrix.
#' @param digits Decimal places. Default 3.
#' @param p_nice Logical. If TRUE (default), format p-values with stars, as
#'   `< .001` where they are too small to show, and without a leading zero.
#' @param names_nice Logical. If TRUE (default), tidy the term names.
#' @param rename Named character vector relabelling terms, old name on the
#'   left, e.g. `c(PoS = "Part of Speech")`. Applied after `names_nice`.
#' @param drop_cols Character vector of columns to leave out, e.g.
#'   `c("t value", "df")`.
#' @param file Optional path to write the table to. `.csv` is written
#'   comma-separated, anything else tab-separated.
#' @param row_name Name for the column holding the row names. Default `"Term"`.
#'   `NULL` leaves row names out.
#' @param quiet Logical. If TRUE, don't print the table that was copied.
#'
#' @return Invisibly, the formatted data frame.
#'
#' @examples
#' \dontrun{
#' m <- lm(iconicity ~ freq * pos, data = demo_words)
#' copy_table(m)
#'
#' copy_table(m, drop_cols = "t value",
#'            rename = c(freq = "Frequency", pos = "Part of Speech"))
#'
#' copy_table(m, file = "table1.csv")
#' }
#'
#' @export
copy_table <- function(x,
                       digits = 3,
                       p_nice = TRUE,
                       names_nice = TRUE,
                       rename = NULL,
                       drop_cols = NULL,
                       file = NULL,
                       row_name = "Term",
                       quiet = FALSE) {

  # --- get a table out of whatever was passed -------------------------------
  tab <- if (inherits(x, c("lm", "glm", "merMod", "lmerModLmerTest"))) {
    stats::coef(summary(x))
  } else if (inherits(x, c("summary.lm", "summary.merMod"))) {
    stats::coef(x)
  } else if (is.matrix(x) || is.data.frame(x)) {
    x
  } else {
    tryCatch(as.data.frame(x), error = function(e) {
      stop("Don't know how to turn a ", class(x)[1], " into a table. Pass a ",
           "model, a data frame or a matrix.", call. = FALSE)
    })
  }

  tab <- as.data.frame(tab, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(tab) == 0L) {
    stop("Nothing to copy -- the table is empty.", call. = FALSE)
  }

  # --- format the numbers ---------------------------------------------------
  is_p   <- grepl("^(pr\\(|p[. _-]?val|p$)", tolower(names(tab)))
  cutoff <- 10^(-digits)

  out <- tab
  for (j in seq_along(tab)) {
    v <- tab[[j]]
    if (!is.numeric(v)) {
      out[[j]] <- as.character(v)
      next
    }
    if (is_p[j] && p_nice) {
      out[[j]] <- nice_p(v, digits)
    } else {
      txt <- formatC(v, format = "f", digits = digits)   # never scientific
      txt[is.na(v)] <- ""
      out[[j]] <- trimws(txt)
    }
  }

  # --- row names become a real column ---------------------------------------
  rn <- rownames(tab)
  has_rn <- !is.null(row_name) && !is.null(rn) &&
    !identical(rn, as.character(seq_len(nrow(tab))))

  if (has_rn) {
    terms <- rn
    if (names_nice) terms <- nice_terms(terms, factor_levels(x))
    if (!is.null(rename)) terms <- apply_rename(terms, rename, rn)
    out <- cbind(stats::setNames(data.frame(terms, stringsAsFactors = FALSE),
                                 row_name),
                 out)
  }
  rownames(out) <- NULL

  # --- drop columns ---------------------------------------------------------
  if (!is.null(drop_cols)) {
    missing_cols <- setdiff(drop_cols, names(out))
    if (length(missing_cols) > 0L) {
      warning("`drop_cols` names column(s) not in the table: ",
              paste(missing_cols, collapse = ", "), ". Columns are: ",
              paste(names(out), collapse = ", "), call. = FALSE)
    }
    keep <- setdiff(names(out), drop_cols)
    if (length(keep) == 0L) {
      stop("`drop_cols` would remove every column.", call. = FALSE)
    }
    out <- out[, keep, drop = FALSE]
  }

  # --- write out, and to the clipboard --------------------------------------
  if (!is.null(file)) {
    if (grepl("\\.csv$", tolower(file))) {
      utils::write.csv(out, file, row.names = FALSE)
    } else {
      utils::write.table(out, file, sep = "\t", row.names = FALSE,
                         quote = FALSE)
    }
  }

  txt <- paste(c(paste(names(out), collapse = "\t"),
                 apply(out, 1, paste, collapse = "\t")),
               collapse = "\n")

  ok <- write_clipboard(txt)

  if (!quiet) {
    print(out, row.names = FALSE)
    if (ok) {
      cat("\nCopied. Paste straight into Excel.\n")
    } else {
      cat("\nCouldn't reach the clipboard. The tab-separated text is:\n\n")
      cat(txt, "\n")
    }
    if (!is.null(file)) cat("Written to: ", file, "\n", sep = "")
    cat("\n")
  }

  invisible(out)
}


# Internal: p-values as a paper would write them -- stars, "< .001", and no
# zero before the decimal point
nice_p <- function(v, digits) {
  cutoff <- 10^(-digits)

  txt <- formatC(v, format = "f", digits = digits)
  txt <- sub("^(-?)0\\.", "\\1.", txt)                    # .023, not 0.023

  small <- !is.na(v) & v < cutoff
  txt[small] <- paste0("< ", sub("^0\\.", ".",
                                 formatC(cutoff, format = "f",
                                         digits = digits)))

  star <- rep("", length(v))
  star[!is.na(v) & v < 0.05]  <- "*"
  star[!is.na(v) & v < 0.01]  <- "**"
  star[!is.na(v) & v < 0.001] <- "***"

  out <- trimws(paste0(txt, star))
  out[is.na(v)] <- ""
  out
}


# Internal: capitalise the first letter, leaving the rest alone
cap_first <- function(x) {
  ifelse(nzchar(x),
         paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))),
         x)
}


# Internal: the factor levels a model used, so terms can be written out
# against their reference level. Empty list if they can't be recovered.
factor_levels <- function(x) {
  lv <- tryCatch({
    if (!is.null(x$xlevels)) {
      x$xlevels
    } else if (inherits(x, "merMod")) {
      fr <- x@frame
      f  <- vapply(fr, is.factor, logical(1))
      lapply(fr[f], levels)
    } else {
      list()
    }
  }, error = function(e) list())

  if (is.null(lv)) list() else lv
}


# Internal: PoSverb -> "PoS (Verb vs. Noun)"; condb:freq -> "Cond (B vs. A) x Freq"
nice_terms <- function(terms, flev) {

  # longest variable names first, so PoS is not matched inside PoSType
  vars <- names(flev)
  vars <- vars[order(nchar(vars), decreasing = TRUE)]

  one <- function(p) {
    for (v in vars) {
      if (startsWith(p, v)) {
        lev <- substring(p, nchar(v) + 1L)
        if (lev %in% flev[[v]]) {
          return(sprintf("%s (%s vs. %s)", v, cap_first(lev),
                         cap_first(flev[[v]][1])))
        }
      }
    }
    cap_first(p)
  }

  vapply(terms, function(tm) {
    if (identical(tm, "(Intercept)")) return("Intercept")
    parts <- strsplit(tm, ":", fixed = TRUE)[[1]]
    paste(vapply(parts, one, character(1)), collapse = " \u00D7 ")
  }, character(1), USE.NAMES = FALSE)
}


# Internal: relabel terms, matching against the original names as well as the
# tidied ones, so `rename` works whether or not `names_nice` ran
apply_rename <- function(terms, rename, original) {
  if (is.null(names(rename)) || any(!nzchar(names(rename)))) {
    stop("`rename` must be named, e.g. c(PoS = \"Part of Speech\").",
         call. = FALSE)
  }
  for (old in names(rename)) {
    new <- unname(rename[[old]])
    hit <- original == old | terms == old
    terms[hit] <- new
    # also swap the variable name inside a compound label
    terms <- sub(paste0("\\b", old, "\\b"), new, terms)
  }
  terms
}


# Internal: put text on the system clipboard; FALSE if there is no way to
write_clipboard <- function(txt) {
  os <- Sys.info()[["sysname"]]

  if (identical(os, "Darwin")) {
    con <- pipe("pbcopy", "w")
    on.exit(close(con), add = TRUE)
    writeLines(txt, con)
    return(TRUE)
  }

  if (identical(os, "Windows")) {
    return(tryCatch({
      utils::writeClipboard(strsplit(txt, "\n", fixed = TRUE)[[1]])
      TRUE
    }, error = function(e) FALSE))
  }

  for (tool in c("xclip -selection clipboard", "xsel --clipboard --input")) {
    if (nzchar(Sys.which(strsplit(tool, " ")[[1]][1]))) {
      con <- pipe(tool, "w")
      on.exit(close(con), add = TRUE)
      writeLines(txt, con)
      return(TRUE)
    }
  }

  FALSE
}
