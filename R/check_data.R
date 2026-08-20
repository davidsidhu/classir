#' Check a dataset for likely problems
#'
#' @description
#' Scans every column for things that usually indicate a data problem: columns
#' stored as the wrong type, heavy missingness, constant columns, values that
#' look like missing-value placeholders, stray whitespace, case-only
#' duplicates, and strange cells -- individual values that don't look like the
#' rest of their column. Nothing is modified; suggested fixes are printed as
#' ready-to-paste code.
#'
#' Every check here is a heuristic. A flag means "worth a look", not "wrong".
#'
#' @param d A data.frame or data.table.
#' @param missing_prop Flag columns with at least this proportion missing.
#'   Default 0.2.
#' @param sd_cutoff For numeric columns, flag values further than this many
#'   standard deviations from the mean. Default 5. Set to `Inf` to skip.
#' @param rare_prop For text columns, flag values whose character pattern
#'   occurs in fewer than this proportion of the column. Default 0.01.
#' @param factor_max Character columns with this many distinct values or fewer
#'   are suggested as factors. Default `Inf`, so every character column is
#'   suggested. Set a number to restrict it, or 0 to skip the check.
#' @param unique_max Numeric columns with this many distinct values or fewer
#'   are flagged as possibly categorical. Default 5. Set to 0 to skip.
#' @param sentinels Numeric values commonly used as missing-value codes.
#'   Flagged where they sit outside a column's range.
#'   Default `c(-9999, -999, -99, -9, 999, 9999)`.
#' @param id Optional column name to check for duplicate identifiers.
#' @param ask_drop Logical. If TRUE (default) and the session is interactive,
#'   offer to drop any stray header rows that are found. Set FALSE in scripts.
#' @param examples How many example values to show per strange-cell flag.
#'   Default 3.
#' @param quiet Logical. If TRUE, return the issues without printing.
#'
#' @return Invisibly, a list with `issues` (a data.frame of column, check, and
#'   detail) and `suggestions` (paste-ready conversion code).
#'
#' @section Strange cells:
#' For numeric columns this covers values far from the mean, non-whole numbers
#' in an otherwise whole-numbered column, and negatives in an otherwise
#' non-negative one. For text columns it compares each value's character
#' pattern -- letters, digits and punctuation, with runs collapsed -- against
#' the rest of the column, and flags patterns that are rare. A column of words
#' containing one entry with a digit in it, or one entry three times longer
#' than everything else, shows up here.
#'
#' @examples
#' \dontrun{
#' check_data(d2)
#' check_data(d2, id = "Word", sd_cutoff = 4)
#' }
#'
#' @export
check_data <- function(d,
                       missing_prop = 0.2,
                       sd_cutoff = 5,
                       rare_prop = 0.01,
                       factor_max = Inf,
                       unique_max = 5,
                       sentinels = c(-9999, -999, -99, -9, 999, 9999),
                       id = NULL,
                       examples = 3,
                       ask_drop = TRUE,
                       quiet = FALSE) {

  d_name  <- deparse(substitute(d))
  d_valid <- length(d_name) == 1L && make.names(d_name) == d_name
  if (!d_valid) d_name <- "d"

  is_dt <- inherits(d, "data.table")
  d <- as.data.frame(d)

  if (ncol(d) == 0L || nrow(d) == 0L) {
    stop("`d` has no rows or no columns.", call. = FALSE)
  }

  n <- nrow(d)
  issues <- list()
  add <- function(column, check, detail) {
    issues[[length(issues) + 1L]] <<-
      data.frame(column = column, check = check, detail = detail,
                 stringsAsFactors = FALSE)
  }

  show_vals <- function(v, k = examples) {
    v <- unique(v)
    txt <- paste(utils::head(v, k), collapse = ", ")
    if (length(v) > k) txt <- paste0(txt, ", ...")
    txt
  }

  na_strings <- c("", "na", "n/a", "n.a.", "nan", "null", "none", "missing",
                  ".", "-", "--", "?", "#n/a", "#null!", "unknown")

  # --- whole-dataset checks -------------------------------------------------
  n_dup_rows <- sum(duplicated(d))
  if (n_dup_rows > 0L) {
    add("(all)", "duplicate rows",
        sprintf("%d fully duplicated row(s)", n_dup_rows))
  }

  if (!is.null(id)) {
    if (!id %in% names(d)) {
      stop("`id` '", id, "' not found in `d`.", call. = FALSE)
    }
    k <- sum(duplicated(d[[id]]))
    if (k > 0L) add(id, "duplicate ids", sprintf("%d repeated value(s)", k))
  }

  # stray header rows: a row whose cells repeat the column names, which is what
  # a second file's header looks like after a careless rbind or paste
  hdr_rows <- integer(0)
  if (nrow(d) > 1L && ncol(d) >= 3L) {
    nm <- tolower(trimws(names(d)))
    hits <- vapply(seq_len(nrow(d)), function(i) {
      cell <- tolower(trimws(as.character(unlist(d[i, ], use.names = FALSE))))
      sum(!is.na(cell) & nzchar(cell) & cell %in% nm)
    }, integer(1))
    hdr_rows <- which(hits >= max(3L, ceiling(0.2 * ncol(d))))
  }

  if (length(hdr_rows) > 0L) {
    add("(all)", "stray header row",
        sprintf("row%s %s hold%s column names, not data",
                if (length(hdr_rows) == 1L) "" else "s",
                paste(format(hdr_rows, big.mark = ","), collapse = ", "),
                if (length(hdr_rows) == 1L) "s" else ""))
  }

  # --- per-column checks ----------------------------------------------------
  to_num <- character(0)
  to_fac <- character(0)

  for (v in names(d)) {
    z  <- d[[v]]
    na <- is.na(z)

    if (all(na)) {
      add(v, "all missing", "every value is NA")
      next
    }
    p_na <- mean(na)
    if (p_na >= missing_prop) {
      add(v, "missing", sprintf("%.1f%% missing (%d of %d)",
                                100 * p_na, sum(na), n))
    }

    zz <- z[!na]

    if (length(unique(zz)) == 1L) {
      add(v, "constant", sprintf("only one value: %s", format(zz[1])))
    }

    if (is.character(z) || is.factor(z)) {

      chr <- as.character(zz)

      hits <- chr[tolower(trimws(chr)) %in% na_strings]
      if (length(hits) > 0L) {
        add(v, "missing as text",
            sprintf("%d value(s) like %s -- real NA?",
                    length(hits), show_vals(hits)))
      }

      k <- sum(chr != trimws(chr))
      if (k > 0L) {
        add(v, "whitespace",
            sprintf("%d value(s) with leading/trailing spaces", k))
      }

      u  <- unique(trimws(chr))
      ul <- unique(tolower(u))
      if (length(ul) < length(u)) {
        add(v, "case mismatch",
            sprintf("%d value(s) differ only in capitalisation",
                    length(u) - length(ul)))
      }

      # strange cells: rare character patterns
      if (rare_prop > 0 && length(unique(chr)) > 1L) {
        shp <- value_shape(chr)
        tabs <- table(shp)
        rare_shapes <- names(tabs)[tabs / length(chr) < rare_prop]
        if (length(rare_shapes) > 0L) {
          odd <- chr[shp %in% rare_shapes]
          add(v, "strange cells",
              sprintf("%d value(s) shaped unlike the rest: %s",
                      length(odd), show_vals(odd)))
        }
      }

      if (is.character(z)) {
        if (looks_numeric(z)) {
          to_num <- c(to_num, v)
          add(v, "type", "character, but all values parse as numbers")
        } else {
          k_lev <- length(unique(chr))
          if (factor_max > 0L && k_lev <= factor_max) {
            to_fac <- c(to_fac, v)
            add(v, "type",
                sprintf("character with %s distinct values -- factor?",
                        format(k_lev, big.mark = ",")))
          }
        }
      }

    } else if (is.numeric(z)) {

      k_unique <- length(unique(zz))
      if (unique_max > 0L && k_unique <= unique_max) {
        add(v, "type", sprintf("numeric with only %d distinct value(s) -- factor?",
                               k_unique))
      }

      sent_hit <- intersect(sentinels, unique(zz))
      if (length(sent_hit) > 0L) {
        core <- zz[!zz %in% sentinels]
        if (length(core) > 0L) {
          rng <- range(core)
          odd <- sent_hit[sent_hit < rng[1] - 1 | sent_hit > rng[2] + 1]
          if (length(odd) > 0L) {
            add(v, "sentinel value",
                sprintf("contains %s, outside the rest of the range -- missing code?",
                        paste(odd, collapse = ", ")))
          }
        }
      }

      # strange cells: far from the mean
      if (is.finite(sd_cutoff) && k_unique > 2L) {
        spread <- stats::sd(zz)
        if (!is.na(spread) && spread > 0) {
          zsc <- abs(zz - mean(zz)) / spread
          hit <- zsc > sd_cutoff
          if (any(hit)) {
            add(v, "strange cells",
                sprintf("%d value(s) beyond %g SDs: %s",
                        sum(hit), sd_cutoff,
                        show_vals(format(zz[order(zsc, decreasing = TRUE)][
                          seq_len(min(examples, sum(hit)))]))))
          }
        }
      }

      # strange cells: a few non-whole numbers among whole ones
      whole <- zz == round(zz)
      if (any(!whole) && mean(whole) > 0.95) {
        add(v, "strange cells",
            sprintf("%d non-whole value(s) in an otherwise whole-numbered column: %s",
                    sum(!whole), show_vals(format(zz[!whole]))))
      }

      # strange cells: a few negatives among non-negatives
      neg <- zz < 0
      if (any(neg) && mean(!neg) > 0.95) {
        add(v, "strange cells",
            sprintf("%d negative value(s) in an otherwise non-negative column: %s",
                    sum(neg), show_vals(format(zz[neg]))))
      }
    }
  }

  # --- assemble -------------------------------------------------------------
  issue_tab <- if (length(issues) > 0L) {
    do.call(rbind, issues)
  } else {
    data.frame(column = character(0), check = character(0),
               detail = character(0), stringsAsFactors = FALSE)
  }

  suggestions <- character(0)
  if (length(to_num) > 0L) {
    suggestions <- c(suggestions, conv_code(d_name, to_num, "as.numeric", is_dt))
  }
  if (length(to_fac) > 0L) {
    suggestions <- c(suggestions, conv_code(d_name, to_fac, "as.factor", is_dt))
  }

  # --- print ----------------------------------------------------------------
  if (!quiet) {
    cat(sprintf("\n%s: %d rows, %d columns\n\n", d_name, n, ncol(d)))

    if (nrow(issue_tab) == 0L) {
      cat("No issues flagged.\n\n")
    } else {
      cat(sprintf("%d issue(s) flagged\n\n", nrow(issue_tab)))
      for (ck in unique(issue_tab$check)) {
        sub <- issue_tab[issue_tab$check == ck, , drop = FALSE]
        cat(toupper(ck), "\n", sep = "")
        for (i in seq_len(nrow(sub))) {
          cat(sprintf("  %-28s %s\n", sub$column[i], sub$detail[i]))
        }
        cat("\n")
      }
    }

    if (length(suggestions) > 0L) {
      cat("SUGGESTED CONVERSIONS\n")
      cat(paste0("  ", suggestions, collapse = "\n"), "\n\n")
    }

    cat("All checks are heuristics -- a flag means worth a look, not wrong.\n")
  }

  if (length(hdr_rows) > 0L && ask_drop && interactive() && d_valid) {
    cat("\n  Row", if (length(hdr_rows) == 1L) "" else "s", " ",
        paste(format(hdr_rows, big.mark = ","), collapse = ", "),
        " appear", if (length(hdr_rows) == 1L) "s" else "",
        " to be a stray header rather than data:\n\n", sep = "")
    print(utils::head(d[hdr_rows, seq_len(min(6L, ncol(d))), drop = FALSE]))
    cat("\n  Type y to drop", if (length(hdr_rows) == 1L) " it" else " them",
        ". To leave the data alone, leave blank\n  and press enter.\n\n",
        sep = "")

    if (tolower(substr(trimws(readline("  Selection: ")), 1, 1)) == "y") {
      out <- d[-hdr_rows, , drop = FALSE]
      rownames(out) <- NULL
      if (is_dt) data.table::setDT(out)
      assign(d_name, out, envir = parent.frame())
      cat("\n  Dropped ", length(hdr_rows), " row",
          if (length(hdr_rows) == 1L) "" else "s", ". The code for this was:\n\n",
          sep = "")
      cat("    ", d_name, " <- ", d_name, "[-c(",
          paste(hdr_rows, collapse = ", "), "), ]\n\n", sep = "")
    } else {
      cat("\n  Left alone.\n\n")
    }
  }

  invisible(list(issues = issue_tab, suggestions = suggestions))
}


# Internal: reduce a string to its character pattern, runs collapsed
# "abandon" -> "a+", "COVID-19" -> "a+-9+", "3.5kg" -> "9+.9+a+"
value_shape <- function(s) {
  s <- gsub("[A-Za-z]", "a", s)
  s <- gsub("[0-9]", "9", s)
  s <- gsub("\\s", "_", s)
  gsub("(.)\\1+", "\\1+", s)
}
