#' Look over a dataset
#'
#' @description
#' Prints a quick look at variables in a dataset. Variables are numbered by
#' their position. Numeric variables get mean, SD, median and range, with skew
#' and kurtosis shown when they exceed conventional thresholds; categorical
#' variables get levels listed with counts (up to `max_levels`).
#'
#' `cors = TRUE` adds a correlation table of every numeric variable, with
#' asterisks marking significance. Which correlation is used is set by
#' `cor_method`: `"pearson"` is the default and measures linear association,
#' while `cor_method = "spearman"` ranks the values first, so it picks up any
#' monotonic relationship and is much less affected by skew or by a handful of
#' extreme values -- often the safer summary for variables like raw frequency
#' counts, unless they have already been log-transformed.
#'
#' @details
#' Columns where almost every value is unique get a level total and a few
#' examples instead.
#'
#' Columns that look like they might need converting to factors or numeric
#' variables are listed, and you can convert them on the spot; nothing else in
#' the dataset is touched. When the prompt is turned off with `ask_conv = FALSE`,
#' or the session is not interactive, ready-to-paste conversion code is printed
#' instead and nothing is modified at all. Columns with no variance are excluded
#' from the correlation table and named in a message, rather than silently
#' producing missing values.
#'
#' @param d A data.frame or data.table.
#' @param cors Logical. If TRUE, print a correlation table of all numeric
#'   variables, with asterisks marking significance. Default FALSE.
#' @param digits Number of decimal places. Default 2.
#' @param max_levels Maximum number of levels listed for a categorical
#'   variable, in order of frequency. Any remaining levels are summarised on an
#'   ellipsis row. Default 5, matching the five rows of a numeric block.
#' @param factor_max Character columns with this many distinct values or fewer
#'   are suggested as factors. Default 12. Independent of `max_levels`.
#' @param examples Number of example values shown for near-unique columns --
#'   those where almost every value occurs once, such as word lists or IDs.
#'   Default 3.
#' @param trunc Values longer than this are truncated. Default 22.
#' @param normality Logical. If TRUE (default), show skew and kurtosis for
#'   numeric variables that exceed `skew_max` / `kurt_max`.
#' @param skew_max Absolute skew above which a variable is flagged. Default 2.
#' @param kurt_max Absolute excess kurtosis above which a variable is flagged.
#'   Default 7.
#' @param ask_conv Logical. If TRUE (default), and the session is interactive,
#'   any columns that look like they want converting are listed with numbers and
#'   you are asked which to convert. Set FALSE in scripts and R Markdown, where
#'   a prompt would stall.
#' @param suggest Logical. If TRUE (default), print suggested conversion code.
#'   Only used when the prompt is off or unavailable.
#' @param cor_method Correlation method, `"pearson"` (default) or
#'   `"spearman"`.
#'
#' @return Invisibly, a list with one element per variable, plus
#'   `suggestions` and (when `cors = TRUE`) `cors`.
#'
#' @examples
#' \dontrun{
#' look(d2)
#' look(d2, cors = TRUE)
#' }
#'
#' @export
look <- function(d,
                 cors = FALSE,
                 digits = 2,
                 max_levels = 5,
                 factor_max = 12,
                 examples = 3,
                 trunc = 22,
                 normality = TRUE,
                 skew_max = 2,
                 kurt_max = 7,
                 ask_conv = TRUE,
                 suggest = TRUE,
                 cor_method = c("pearson", "spearman")) {

  cor_method <- match.arg(cor_method)

  d_name  <- deparse(substitute(d))
  d_valid <- length(d_name) == 1L && make.names(d_name) == d_name
  if (!d_valid) d_name <- "d"

  is_dt <- inherits(d, "data.table")
  d <- as.data.frame(d)

  if (ncol(d) == 0L) stop("`d` has no columns.", call. = FALSE)

  n   <- nrow(d)
  num <- function(x) formatC(x, format = "f", digits = digits, big.mark = ",")
  cnt <- function(x) formatC(x, format = "d", big.mark = ",")
  cut_str <- function(s) {
    long <- nchar(s) > trunc
    s[long] <- paste0(substr(s[long], 1, trunc - 1), "\u2026")
    s
  }

  headers <- character(ncol(d))
  blocks  <- vector("list", ncol(d))
  out     <- list()
  to_num  <- character(0)
  to_fac  <- character(0)

  for (j in seq_len(ncol(d))) {

    v    <- names(d)[j]
    z    <- d[[j]]
    na   <- is.na(z)
    n_na <- sum(na)
    zz   <- z[!na]

    headers[j] <- sprintf("(%d) %s <%s>", j, cut_str(v), class(z)[1])

    labs <- character(0)
    vals <- character(0)
    put  <- function(l, x) {
      labs <<- c(labs, l)
      vals <<- c(vals, x)
    }

    if (length(zz) == 0L) {
      put("", "all values missing")
      blocks[[j]] <- list(labs = labs, vals = vals)
      next
    }

    k_unique <- length(unique(zz))
    constant <- k_unique == 1L

    if (is.numeric(zz)) {

      m  <- mean(zz)
      s  <- if (length(zz) > 1L) stats::sd(zz) else NA_real_
      md <- stats::median(zz)
      rg <- range(zz)

      put("Mean",   num(m))
      put("SD",     if (is.na(s)) "-" else num(s))
      put("Min",    num(rg[1]))
      put("Median", num(md))
      put("Max",    num(rg[2]))

      sk <- kt <- NA_real_
      if (normality && !constant && length(zz) > 3L && !is.na(s) && s > 0) {
        sk <- skewness(zz)
        kt <- kurtosis(zz)
        if (abs(sk) > skew_max) {
          put("Skew", sprintf("%s (%s)", num(sk),
                              if (sk > 0) "bunched left" else "bunched right"))
        }
        if (abs(kt) > kurt_max) {
          put("Kurt", sprintf("%s (%s)", num(kt),
                              if (kt > 0) "peaked" else "flat"))
        }
      }

      if (constant) put("", "All values identical!")

      out[[v]] <- list(class = class(z)[1], n = length(zz), na = n_na,
                       mean = m, sd = s, median = md,
                       min = rg[1], max = rg[2], skew = sk, kurtosis = kt)

    } else {

      chr <- as.character(zz)
      tab <- sort(table(chr), decreasing = TRUE)
      k   <- length(tab)
      nms <- cut_str(names(tab))

      # near-unique columns (IDs, word lists) get a compact summary instead
      near_unique <- k > max_levels && mean(tab == 1) >= 0.9

      if (near_unique) {
        put("Lvls", cnt(k))
        put("e.g.", paste(nms[seq_len(min(examples, k))], collapse = ", "))
      } else {
        shown <- min(k, max_levels)
        for (i in seq_len(shown)) {
          put(nms[i], sprintf("%s (%.1f%%)", cnt(tab[i]),
                              100 * tab[i] / length(chr)))
        }
        if (k > shown) {
          put("...", sprintf("%s more level%s (%s total)", cnt(k - shown),
                             if (k - shown == 1L) "" else "s", cnt(k)))
        }
      }

      out[[v]] <- list(class = class(z)[1], n = length(zz), na = n_na,
                       counts = tab)

      if (is.character(z)) {
        if (looks_numeric(z)) {
          to_num <- c(to_num, v)
        } else if (k <= factor_max) {
          to_fac <- c(to_fac, v)
        }
      }
    }

    if (n_na > 0L) {
      put("NA", sprintf("%s (%.1f%%)", cnt(n_na), 100 * n_na / n))
    }

    blocks[[j]] <- list(labs = labs, vals = vals)
  }

  # --- render each block to lines -------------------------------------------
  lines <- lapply(blocks, function(b) {
    if (length(b$labs) == 0L) return(character(0))
    w <- max(nchar(b$labs))
    sprintf("  %-*s  %s", w, b$labs, b$vals)
  })

  widths <- vapply(seq_along(lines), function(j) {
    max(nchar(c(headers[j], lines[[j]])))
  }, integer(1))

  avail <- max(getOption("width", 80), 40L)

  cat(sprintf("\n%s: %s rows, %d columns\n", d_name, cnt(n), ncol(d)))

  j <- 1L
  while (j <= length(lines)) {
    used <- 0L
    k <- j
    while (k <= length(lines) && used + widths[k] + 3L <= avail) {
      used <- used + widths[k] + 3L
      k <- k + 1L
    }
    if (k == j) k <- j + 1L
    idx <- j:(k - 1L)

    cat("\n")
    cat(paste(mapply(function(s, w) formatC(s, width = -w),
                     headers[idx], widths[idx]), collapse = "   "),
        "\n", sep = "")

    depth <- max(vapply(lines[idx], length, integer(1)))
    for (r in seq_len(depth)) {
      row <- vapply(idx, function(jj) {
        s <- if (r <= length(lines[[jj]])) lines[[jj]][r] else ""
        formatC(s, width = -widths[jj])
      }, character(1))
      cat(paste(row, collapse = "   "), "\n", sep = "")
    }

    j <- k
  }

  cat("\n")

  # --- suggested conversions ------------------------------------------------
  num_code <- if (length(to_num) > 0L) {
    conv_code(d_name, to_num, "as.numeric", is_dt)
  } else character(0)

  fac_code <- if (length(to_fac) > 0L) {
    conv_code(d_name, to_fac, "as.factor", is_dt)
  } else character(0)

  suggestions <- c(num_code, fac_code)
  n_cand <- length(to_fac) + length(to_num)

  asked <- FALSE

  if (ask_conv && interactive() && d_valid && n_cand > 0L) {

    asked <- TRUE
    cand  <- c(to_fac, to_num)
    kind  <- c(rep("factor", length(to_fac)), rep("numeric", length(to_num)))

    cat("POSSIBLE CONVERSIONS\n\n")

    i <- 0L
    if (length(to_fac) > 0L) {
      cat("  These look like factors:\n    ")
      cat(paste(sprintf("%d: %s", seq_along(to_fac), to_fac),
                collapse = "    "), "\n\n", sep = "")
      i <- length(to_fac)
    }
    if (length(to_num) > 0L) {
      cat("  These look like numeric:\n    ")
      cat(paste(sprintf("%d: %s", i + seq_along(to_num), to_num),
                collapse = "    "), "\n\n", sep = "")
    }

    cat("  Type numbers of variables you want to convert separated by",
        "commas\n  (e.g. 1, 3), or \"all\" to convert all. To proceed without",
        "any\n  conversions, leave blank and press enter.\n\n")
    raw <- trimws(readline("  Selection: "))

    pick <- integer(0)
    if (tolower(raw) %in% c("all", "a")) {
      pick <- seq_along(cand)
    } else if (nzchar(raw)) {
      nums <- suppressWarnings(
        as.integer(trimws(strsplit(raw, "[,[:space:]]+")[[1]])))
      nums <- nums[!is.na(nums)]
      bad  <- nums[nums < 1 | nums > length(cand)]
      if (length(bad) > 0L) {
        message("  Ignoring out-of-range: ", paste(bad, collapse = ", "))
      }
      pick <- nums[nums >= 1 & nums <= length(cand)]
    }

    if (length(pick) > 0L) {
      dd <- d
      for (k in pick) {
        dd[[cand[k]]] <- if (kind[k] == "factor") {
          as.factor(dd[[cand[k]]])
        } else {
          as.numeric(dd[[cand[k]]])
        }
      }
      if (is_dt) data.table::setDT(dd)
      assign(d_name, dd, envir = parent.frame())

      did_fac <- cand[pick][kind[pick] == "factor"]
      did_num <- cand[pick][kind[pick] == "numeric"]
      cat("\n")
      if (length(did_fac) > 0L) {
        cat("  Converted to factor: ", and_list(did_fac), "\n", sep = "")
      }
      if (length(did_num) > 0L) {
        cat("  Converted to numeric: ", and_list(did_num), "\n", sep = "")
      }

      cat("\n  The same conversions written out, in case this is part of a",
          "script you\n  will run again. Paste these in and call",
          sprintf("look(%s, ask_conv = FALSE)\n  so the prompt does not stop",
                  d_name),
          "the script:\n\n")
      if (length(did_num) > 0L) {
        cat("    ", conv_code(d_name, did_num, "as.numeric", is_dt), "\n",
            sep = "")
      }
      if (length(did_fac) > 0L) {
        cat("    ", conv_code(d_name, did_fac, "as.factor", is_dt), "\n",
            sep = "")
      }
      cat("\n")
    } else {
      cat("\n  Nothing converted.\n\n")
    }
  }

  if (!asked && suggest && length(suggestions) > 0L) {
    cat("SUGGESTED CONVERSIONS\n\n")

    if (length(to_num) > 0L) {
      cat(sprintf("  %s stored as text but hold%s only numbers.\n",
                  and_list(to_num), if (length(to_num) == 1L) "s" else ""))
      cat("  Convert with:\n")
      cat("    ", num_code, "\n\n", sep = "")
    }

    if (length(to_fac) > 0L) {
      cat(sprintf("  %s ha%s few distinct values and may be factor%s.\n",
                  and_list(to_fac),
                  if (length(to_fac) == 1L) "s" else "ve",
                  if (length(to_fac) == 1L) "" else "s"))
      cat("  Convert with:\n")
      cat("    ", fac_code, "\n\n", sep = "")
    }
  }

  # --- correlations ---------------------------------------------------------
  cor_tab <- NULL
  if (cors) {
    is_num <- vapply(d, is.numeric, logical(1))
    num_nm <- names(d)[is_num]

    flat <- num_nm[vapply(d[num_nm], function(z) {
      sdv <- stats::sd(z, na.rm = TRUE)
      is.na(sdv) || sdv == 0
    }, logical(1))]

    if (length(flat) > 0L) {
      message("Excluded from correlations (no variance): ",
              paste(flat, collapse = ", "))
      num_nm <- setdiff(num_nm, flat)
    }

    if (length(num_nm) < 2L) {
      message("Fewer than two usable numeric variables; no correlation table.")
    } else {
      m <- as.matrix(d[num_nm])
      cor_tab <- cor_stars(m, digits = digits, method = cor_method)

      cat(sprintf("CORRELATIONS (%s, pairwise complete)\n", cor_method))
      print(as.data.frame(cor_tab), quote = FALSE)
      cat("  * p<.05   ** p<.01   *** p<.001\n\n")
    }
  }

  out$suggestions <- suggestions
  out$cors <- cor_tab
  invisible(out)
}


# Internal: sample skewness (g1)
skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  m <- mean(x)
  s <- sqrt(sum((x - m)^2) / n)
  if (s == 0) return(NA_real_)
  sum((x - m)^3) / (n * s^3)
}


# Internal: excess kurtosis (g2)
kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4L) return(NA_real_)
  m <- mean(x)
  s <- sqrt(sum((x - m)^2) / n)
  if (s == 0) return(NA_real_)
  sum((x - m)^4) / (n * s^4) - 3
}


# Internal: "a", "a and b", "a, b and c"
and_list <- function(x) {
  k <- length(x)
  if (k == 0L) return("")
  if (k == 1L) return(x)
  if (k == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-k], collapse = ", "), " and ", x[k])
}


# Internal: build a paste-ready conversion line, in the right dialect
conv_code <- function(d_name, cols, fun, is_dt) {
  vec <- paste0("c(", paste0("\"", cols, "\"", collapse = ", "), ")")

  if (is_dt) {
    sprintf("%s[, %s := lapply(.SD, %s), .SDcols = %s]",
            d_name, vec, fun, vec)
  } else {
    sprintf("%s[%s] <- lapply(%s[%s], %s)",
            d_name, vec, d_name, vec, fun)
  }
}


# Internal: lower-triangular correlation matrix with significance stars
cor_stars <- function(m, digits = 2, method = "pearson") {

  vars <- colnames(m)
  k    <- length(vars)
  r    <- stats::cor(m, use = "pairwise.complete.obs", method = method)

  out <- matrix("", k, k, dimnames = list(vars, vars))

  for (i in seq_len(k)) {
    out[i, i] <- "-"
    if (i == 1L) next
    for (j in seq_len(i - 1L)) {
      rv <- r[i, j]
      n  <- sum(stats::complete.cases(m[, c(i, j)]))

      p <- if (is.na(rv) || n < 4L || abs(rv) >= 1) {
        NA_real_
      } else {
        tstat <- rv * sqrt((n - 2) / (1 - rv^2))
        2 * stats::pt(-abs(tstat), df = n - 2)
      }

      star <- if (is.na(p)) "" else
              if (p < .001) "***" else
              if (p < .01)  "**"  else
              if (p < .05)  "*"   else ""

      out[i, j] <- if (is.na(rv)) "NA" else
                   paste0(formatC(rv, format = "f", digits = digits), star)
    }
  }

  out
}
