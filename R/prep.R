#' Get a dataset ready to analyse
#'
#' @description
#' One function to prepare data for analysis. Rows with missing values on
#' identified columns are dropped first. Then categorical variables are prepped
#' by setting reference levels and effects coding. Then numeric variables are
#' standardised. Columns are then renamed.
#'
#' `vars` names the variables an analysis will actually use. It changes what
#' `TRUE` means elsewhere: `complete = TRUE` requires those columns to be
#' complete rather than all of them, and `scale = TRUE` standardises the numeric
#' ones among them. Without `vars`, `TRUE` still means every column.
#'
#' `complete` names the columns that must have no missing values, or `TRUE` for
#' every column. Any row missing one of them is dropped, so everything
#' downstream works from the same set of observations.
#'
#' `subset` keeps only the rows you want, written as an ordinary expression --
#' `subset = pos == "noun" & freq > 1`. It is applied after the missing-value
#' cut, so any standardising happens on the rows that survive both.
#'
#' `scale` names the numeric columns to standardise, or `TRUE` for all of them.
#' This happens after the row cut, so the means and SDs come from the rows that
#' remain. `no_scale` names columns to leave alone, which is the easy way to
#' standardise everything bar an id or a count: `scale = TRUE, no_scale =
#' "trial_number"`. Columns that were effects coded are never standardised.
#'
#' `ref` sets the reference level of a factor, written as
#' `ref = c(PoS = "noun")`. Several can be set at once.
#'
#' `effects` names two-level factors to effects code, applying contrasts of
#' -0.5 and 0.5. The column stays a factor, so it still works with `emmeans` and
#' anything else that expects one.
#'
#' `rename` renames columns, written as `rename = c(Log_Freq_HAL = "freq")`,
#' with the old name on the left.
#'
#' `drop_others = TRUE` keeps only the columns named in the other arguments,
#' plus any named in `keep`.
#'
#' @details
#' Character columns holding only numbers are detected and, in an interactive
#' session, listed so they can be converted on the spot. `ask_conv = FALSE`
#' turns the prompt off, which is what you want in a script.
#'
#' A report of what was done is printed unless `quiet = TRUE`.
#'
#' @param d A data.frame or data.table.
#' @param vars Character or numeric: the variables an analysis will use. When
#'   supplied, `complete = TRUE` and `scale = TRUE` apply to these rather than
#'   to every column, and they are kept by `drop_others`.
#' @param complete Character or numeric: columns that must have no missing
#'   values. Rows missing any of them are dropped. `TRUE` requires every column
#'   to be complete.
#' @param subset An unquoted expression selecting rows to keep, written the way
#'   you would inside `d[...]`, e.g. `pos == "noun" & freq > 1`. Applied after
#'   the missing-value cut and before anything else.
#' @param scale Character or numeric: numeric columns to standardise. `TRUE`
#'   standardises every numeric column.
#' @param no_scale Character or numeric: columns never to standardise, even if
#'   `scale` would otherwise include them. Useful with `scale = TRUE`.
#' @param ref Named character vector: the reference level for each factor, e.g.
#'   `c(PoS = "noun")`.
#' @param effects Character vector: two-level factors to effects code at
#'   -0.5 / 0.5.
#' @param rename Named character vector, old name on the left, e.g.
#'   `c(Log_Freq_HAL = "freq")`.
#' @param keep Character vector: extra columns to retain when
#'   `drop_others = TRUE`.
#' @param drop_others Logical. If TRUE, keep only the columns named in the other
#'   arguments. Default FALSE.
#' @param ask_conv Logical. If TRUE (default) and the session is interactive,
#'   offer to convert character columns that hold only numbers.
#' @param quiet Logical. If TRUE, suppress the report. Default FALSE.
#'
#' @return The prepared data. A data.table in, a data.table out.
#'
#' @examples
#' \dontrun{
#' # everything applies to the named variables only
#' d <- prep(demo_words,
#'           vars     = c("freq", "aoa", "concreteness"),
#'           complete = TRUE,
#'           scale    = TRUE,
#'           keep     = "word",
#'           drop_others = TRUE)
#'
#' d <- prep(demo_words,
#'           complete = c("freq", "aoa", "concreteness"),
#'           subset   = pos != "adjective",
#'           scale    = TRUE,
#'           no_scale = "length",
#'           ref      = c(pos = "noun"),
#'           effects  = "sound_symbolic",
#'           rename   = c(concreteness = "conc"))
#' }
#'
#' @export
prep <- function(d,
                 vars = NULL,
                 complete = NULL,
                 subset,
                 scale = NULL,
                 no_scale = NULL,
                 ref = NULL,
                 effects = NULL,
                 rename = NULL,
                 keep = NULL,
                 drop_others = FALSE,
                 ask_conv = TRUE,
                 quiet = FALSE) {

  was_dt <- inherits(d, "data.table")
  d <- as.data.frame(d)

  if (ncol(d) == 0L || nrow(d) == 0L) {
    stop("`d` has no rows or no columns.", call. = FALSE)
  }

  as_names <- function(x, what) {
    if (is.null(x)) return(character(0))
    nm <- if (is.numeric(x)) names(d)[x] else as.character(x)
    miss <- setdiff(nm, names(d))
    if (length(miss) > 0L) {
      stop("`", what, "` names column(s) not in `d`: ",
           paste(miss, collapse = ", "), call. = FALSE)
    }
    nm
  }

  vars <- as_names(vars, "vars")
  all_or_vars <- if (length(vars) > 0L) vars else names(d)

  complete <- if (isTRUE(complete)) {
    all_or_vars
  } else if (isFALSE(complete)) {
    character(0)
  } else {
    as_names(complete, "complete")
  }
  scale_c <- if (isTRUE(scale)) {
    all_or_vars[vapply(d[all_or_vars], is.numeric, logical(1))]
  } else if (isFALSE(scale)) {
    character(0)
  } else {
    as_names(scale, "scale")
  }
  no_scale_c <- as_names(no_scale, "no_scale")
  effects  <- as_names(effects, "effects")
  keep     <- as_names(keep, "keep")
  if (!is.null(ref))    as_names(names(ref), "ref")
  if (!is.null(rename)) as_names(names(rename), "rename")

  log <- character(0)
  note <- function(...) log <<- c(log, paste0(...))

  # --- 0. character columns that hold only numbers --------------------------
  chr_num <- names(d)[vapply(d, looks_numeric, logical(1))]
  if (length(chr_num) > 0L) {
    if (ask_conv && interactive()) {
      d <- convert_prompt(d, chr_num)
      done <- chr_num[vapply(d[chr_num], is.numeric, logical(1))]
      if (length(done) > 0L) {
        note("Converted to numeric: ", and_list(done))
      }
    } else {
      note("Character but numeric, left alone: ", and_list(chr_num))
    }
  }

  # --- 1. complete cases ----------------------------------------------------
  n_before <- nrow(d)
  if (length(complete) > 0L) {
    ok <- stats::complete.cases(d[, complete, drop = FALSE])
    d <- d[ok, , drop = FALSE]
    rownames(d) <- NULL
    note(sprintf("Kept %s of %s rows complete on %s.",
                 format(nrow(d), big.mark = ","),
                 format(n_before, big.mark = ","),
                 and_list(complete)))
    if (nrow(d) == 0L) {
      stop("No rows are complete on all of: ", and_list(complete),
           call. = FALSE)
    }
  }

  # --- 1b. subset -----------------------------------------------------------
  if (!missing(subset)) {
    n_pre <- nrow(d)
    sel <- eval(substitute(subset), d, parent.frame())
    if (!is.logical(sel)) {
      stop("`subset` must give a TRUE/FALSE value for each row.", call. = FALSE)
    }
    if (length(sel) != nrow(d)) {
      stop("`subset` gave ", length(sel), " values but there are ", nrow(d),
           " rows.", call. = FALSE)
    }
    sel[is.na(sel)] <- FALSE
    d <- d[sel, , drop = FALSE]
    rownames(d) <- NULL

    if (nrow(d) == 0L) stop("`subset` kept no rows.", call. = FALSE)

    note(sprintf("Subset kept %s of %s rows.",
                 format(nrow(d), big.mark = ","),
                 format(n_pre, big.mark = ",")))

    fac <- names(d)[vapply(d, is.factor, logical(1))]
    for (v in fac) {
      if (nlevels(d[[v]]) != nlevels(droplevels(d[[v]]))) {
        gone <- setdiff(levels(d[[v]]), levels(droplevels(d[[v]])))
        d[[v]] <- droplevels(d[[v]])
        note("Dropped unused level(s) of ", v, ": ", and_list(gone))
      }
    }
  }

  # --- 2. reference levels --------------------------------------------------
  if (!is.null(ref)) {
    if (is.null(names(ref)) || any(!nzchar(names(ref)))) {
      stop("`ref` must be named, e.g. c(PoS = \"noun\").", call. = FALSE)
    }
    for (v in names(ref)) {
      z <- d[[v]]
      if (!is.factor(z)) z <- as.factor(z)
      if (!ref[[v]] %in% levels(z)) {
        stop("`", ref[[v]], "` is not a level of `", v, "`. Levels are: ",
             paste(levels(z), collapse = ", "), call. = FALSE)
      }
      d[[v]] <- stats::relevel(z, ref = ref[[v]])
      note("Reference level of ", v, " set to ", ref[[v]], ".")
    }
  }

  # --- 3. effects coding ----------------------------------------------------
  for (v in effects) {
    z <- d[[v]]
    if (!is.factor(z)) z <- as.factor(z)
    z <- droplevels(z)
    if (nlevels(z) != 2L) {
      stop("`", v, "` has ", nlevels(z), " levels; effects coding here is for ",
           "two-level factors only.", call. = FALSE)
    }
    stats::contrasts(z) <- c(-0.5, 0.5)
    d[[v]] <- z
    note("Effects coded ", v, ": ", levels(z)[1], " = -0.5, ",
         levels(z)[2], " = 0.5.")
  }

  # --- 4. scaling -----------------------------------------------------------
  scale_c <- setdiff(scale_c, c(effects, no_scale_c))
  if (length(scale_c) > 0L) {
    done <- character(0)
    for (v in scale_c) {
      if (!is.numeric(d[[v]])) {
        warning("`", v, "` is not numeric; not scaled.", call. = FALSE)
        next
      }
      s <- stats::sd(d[[v]], na.rm = TRUE)
      if (is.na(s) || s == 0) {
        warning("`", v, "` has no variance; not scaled.", call. = FALSE)
        next
      }
      d[[v]] <- as.vector(base::scale(d[[v]]))
      done <- c(done, v)
    }
    if (length(done) > 0L) note("Standardised: ", and_list(done))
  }

  # --- 5. drop other columns ------------------------------------------------
  if (length(keep) > 0L && !drop_others) {
    warning("`keep` only has an effect when `drop_others = TRUE`; ",
            "no columns were dropped.", call. = FALSE)
  }

  if (drop_others) {
    wanted <- unique(c(keep, vars, complete, scale_c, effects, names(ref),
                       names(rename)))
    wanted <- intersect(names(d), wanted)
    if (length(wanted) == 0L) {
      stop("`drop_others = TRUE` but no columns were named to keep.",
           call. = FALSE)
    }
    dropped <- setdiff(names(d), wanted)
    d <- d[, wanted, drop = FALSE]
    if (length(dropped) > 0L) {
      note(sprintf("Dropped %d other column%s.", length(dropped),
                   if (length(dropped) == 1L) "" else "s"))
    }
  }

  # --- 6. rename, last ------------------------------------------------------
  if (!is.null(rename)) {
    if (is.null(names(rename)) || any(!nzchar(names(rename)))) {
      stop("`rename` must be named, e.g. c(old_name = \"new_name\").",
           call. = FALSE)
    }
    hit <- intersect(names(rename), names(d))
    if (length(hit) > 0L) {
      clash <- intersect(unname(rename[hit]), setdiff(names(d), hit))
      if (length(clash) > 0L) {
        stop("Renaming would produce duplicate column name(s): ",
             paste(clash, collapse = ", "), call. = FALSE)
      }
      names(d)[match(hit, names(d))] <- unname(rename[hit])
      note("Renamed: ",
           and_list(paste0(hit, " \u2192 ", unname(rename[hit]))))
    }
  }

  # --- report ---------------------------------------------------------------
  if (!quiet && length(log) > 0L) {
    cat("\n")
    for (l in log) cat("  ", l, "\n", sep = "")
    cat("\n")
  }

  if (was_dt) data.table::setDT(d)
  d
}


# Internal: offer to convert character columns that hold only numbers
convert_prompt <- function(d, cand) {

  cat("\nThese columns are text but hold only numbers:\n    ")
  cat(paste(sprintf("%d: %s", seq_along(cand), cand), collapse = "    "),
      "\n\n", sep = "")
  cat("  Type numbers of variables you want to convert separated by commas\n",
      "  (e.g. 1, 3), or \"all\" to convert all. To proceed without any\n",
      "  conversions, leave blank and press enter.\n\n", sep = "")

  raw <- trimws(readline("  Selection: "))

  pick <- integer(0)
  if (tolower(raw) %in% c("all", "a")) {
    pick <- seq_along(cand)
  } else if (nzchar(raw)) {
    nums <- suppressWarnings(
      as.integer(trimws(strsplit(raw, "[,[:space:]]+")[[1]])))
    nums <- nums[!is.na(nums)]
    pick <- nums[nums >= 1 & nums <= length(cand)]
  }

  for (k in pick) d[[cand[k]]] <- as.numeric(d[[cand[k]]])
  cat("\n")
  d
}
