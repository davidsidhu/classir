#' Add values from a lookup dataset by id
#'
#' @description
#' Looks each id (`id = `) up in a second dataset (`lookup = `), matching on a
#' column in that dataset (`lookup_id = `) and adds the requested columns
#' (`lookup_cols = `).
#'
#' The lookup can be a data frame already in R or a path to a file, and .csv,
#' .tsv, .txt, .rds, .xlsx and .xls are all read without issue. If
#' neither id column is named, the leftmost column of each dataset is used,
#' with a message saying so.
#'
#' You can choose to simply add all columns by using `add_all = TRUE`.
#'
#' `new_cols` renames the incoming columns, either by position or by name so
#' that only some are renamed. By position, give one name per column in
#' `lookup_cols`: `new_cols = c("freq", "len")`. By name, give only the ones to
#' rename, with the old name on the left: `new_cols = c(Log_Freq_HAL =
#' "freq")`.
#'
#' `add_subset` restricts the lookup to a subset of rows -- for example `PoS ==
#' "verb"` -- written as an ordinary expression. Rows outside the subset are
#' kept, with missing values in the new columns.
#'
#' `fix_types = TRUE` converts incoming character columns whose values all
#' parse as numbers, reporting what it converted -- useful when the lookup has
#' been through a spreadsheet.
#'
#' `complete_only = TRUE` keeps only rows with values on every incoming
#' column.
#'
#' @param d A data.frame/data.table, or a character vector of ids.
#' @param id Character: name of the id column in `d`. Defaults to the leftmost
#'   column, with a message.
#' @param lookup Either a data.frame or a path to a file to read
#'   (.csv, .tsv/.txt, .rds, .xlsx/.xls).
#' @param lookup_id Character: name of the id column in `lookup`. Defaults to
#'   `id` if present, otherwise the leftmost column, with a message.
#' @param lookup_cols Character vector: names of the columns in `lookup` to add.
#'   Not needed when `add_all = TRUE`.
#' @param add_all Logical. If TRUE, every column in `lookup` except `lookup_id`
#'   is added, and `lookup_cols` is ignored. Default FALSE.
#' @param new_cols Optional character vector of names for the added columns.
#'   Either unnamed and the same length as `lookup_cols` (matched by position),
#'   or named, where the names are entries in `lookup_cols` and the values are
#'   their new names (allowing a subset to be renamed).
#' @param add_subset Optional unquoted expression evaluated in `d`, e.g.
#'   `PoS == "verb"`. Only rows where it is TRUE are looked up; all other rows
#'   are kept, with `NA` in the added columns. `NA` in the condition counts as
#'   FALSE.
#' @param multiple What to do when an id appears more than once in `lookup`.
#'   `"first"` (default) keeps the first match and warns; `"all"` returns one
#'   row per match, changing the row count; `"error"` stops.
#' @param fix_types Logical. If TRUE, character columns whose non-missing
#'   values all parse as numbers are converted to numeric, with a message.
#'   Default FALSE.
#' @param complete_only Logical. If TRUE, keep only rows with values on all of
#'   the added columns. Default FALSE.
#' @param ignore_case Logical. If TRUE (default), match on lower-cased,
#'   whitespace-trimmed ids.
#' @param quiet Logical. If TRUE, suppress the report. Default FALSE.
#'
#' @return `d` with the requested columns added. A data.table in, a data.table
#'   out; otherwise a data.frame. Keys are not preserved.
#'
#' @examples
#' \dontrun{
#' # both files use their leftmost column as the id
#' d2 <- add_values(d2, lookup = "ELP.csv",
#'                  lookup_cols = c("Log_Freq_HAL", "Length"))
#'
#' # everything in the file
#' d2 <- add_values(d2, lookup = "ELP.csv", add_all = TRUE)
#'
#' # explicit, renamed, verbs only
#' d2 <- add_values(d2, "Word", "ELP.csv", "Word",
#'                  c("Log_Freq_HAL", "Length"),
#'                  new_cols   = c("freq", "len"),
#'                  add_subset = PoS == "verb",
#'                  fix_types  = TRUE)
#' }
#'
#' @export
add_values <- function(d,
                       id,
                       lookup,
                       lookup_id,
                       lookup_cols,
                       add_all = FALSE,
                       new_cols = NULL,
                       add_subset,
                       multiple = c("first", "all", "error"),
                       fix_types = FALSE,
                       complete_only = FALSE,
                       ignore_case = TRUE,
                       quiet = FALSE) {

  multiple <- match.arg(multiple)

  # --- d: character vector or data frame ------------------------------------
  if (is.character(d) && is.null(dim(d))) {
    was_dt <- FALSE
    if (missing(id)) id <- "id"
    d <- data.frame(x = d, stringsAsFactors = FALSE)
    names(d) <- id
  } else {
    was_dt <- inherits(d, "data.table")
    d <- as.data.frame(d)
  }

  if (ncol(d) == 0L || nrow(d) == 0L) {
    stop("`d` has no rows or no columns.", call. = FALSE)
  }

  if (missing(id)) {
    id <- names(d)[1]
    if (!quiet) message("`id` not supplied; using the leftmost column: '", id, "'.")
  }
  if (!is.character(id) || length(id) != 1L) {
    stop("`id` must be a single character string.", call. = FALSE)
  }
  if (!id %in% names(d)) {
    stop("`id` '", id, "' not found in `d`.", call. = FALSE)
  }

  # --- lookup ---------------------------------------------------------------
  if (missing(lookup)) stop("`lookup` must be supplied.", call. = FALSE)

  lk <- if (is.character(lookup) && length(lookup) == 1L) {
    read_lookup(lookup)
  } else {
    as.data.frame(lookup)
  }

  if (missing(lookup_id)) {
    lookup_id <- if (id %in% names(lk)) id else names(lk)[1]
    if (!quiet) message("`lookup_id` not supplied; using: '", lookup_id, "'.")
  }
  if (!lookup_id %in% names(lk)) {
    stop("`lookup_id` '", lookup_id, "' not found in the lookup data.",
         call. = FALSE)
  }

  # --- which columns to add -------------------------------------------------
  if (add_all) {
    if (!missing(lookup_cols) && !quiet) {
      message("`add_all = TRUE`; ignoring `lookup_cols`.")
    }
    lookup_cols <- setdiff(names(lk), lookup_id)
    if (length(lookup_cols) == 0L) {
      stop("`add_all = TRUE` but the lookup data has no columns besides '",
           lookup_id, "'.", call. = FALSE)
    }
    if (!quiet) {
      message("`add_all = TRUE`: adding ", length(lookup_cols), " column(s).")
    }
  } else {
    if (missing(lookup_cols)) {
      stop("`lookup_cols` must be supplied, or set `add_all = TRUE`.",
           call. = FALSE)
    }
    missing_cols <- setdiff(lookup_cols, names(lk))
    if (length(missing_cols) > 0L) {
      stop("These columns are not in the lookup data: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }

  # --- names for the added columns ------------------------------------------
  final_names <- lookup_cols

  if (!is.null(new_cols)) {
    if (!is.character(new_cols)) {
      stop("`new_cols` must be a character vector.", call. = FALSE)
    }
    if (is.null(names(new_cols))) {
      if (length(new_cols) != length(lookup_cols)) {
        stop("Unnamed `new_cols` must be the same length as `lookup_cols` (",
             length(lookup_cols), ").", call. = FALSE)
      }
      final_names <- new_cols
    } else {
      if (any(!nzchar(names(new_cols)))) {
        stop("`new_cols` must be either fully named or fully unnamed.",
             call. = FALSE)
      }
      unknown <- setdiff(names(new_cols), lookup_cols)
      if (length(unknown) > 0L) {
        stop("`new_cols` refers to columns not in `lookup_cols`: ",
             paste(unknown, collapse = ", "), call. = FALSE)
      }
      final_names[match(names(new_cols), lookup_cols)] <- unname(new_cols)
    }
    if (anyDuplicated(final_names)) {
      stop("`new_cols` would produce duplicate column names: ",
           paste(unique(final_names[duplicated(final_names)]), collapse = ", "),
           call. = FALSE)
    }
  }

  clash <- intersect(final_names, names(d))
  if (length(clash) > 0L) {
    stop("These columns already exist in `d`: ",
         paste(clash, collapse = ", "),
         ". Rename or drop them first, or use `new_cols`.", call. = FALSE)
  }

  # --- which rows to look up ------------------------------------------------
  if (missing(add_subset)) {
    sel <- rep(TRUE, nrow(d))
  } else {
    sel <- eval(substitute(add_subset), d, parent.frame())
    if (!is.logical(sel)) {
      stop("`add_subset` must evaluate to a logical vector.", call. = FALSE)
    }
    if (length(sel) != nrow(d)) {
      stop("`add_subset` produced ", length(sel), " values but `d` has ",
           nrow(d), " rows.", call. = FALSE)
    }
    sel[is.na(sel)] <- FALSE
  }
  n_sel <- sum(sel)

  # --- keys -----------------------------------------------------------------
  key_d <- trimws(as.character(d[[id]]))
  key_s <- trimws(as.character(lk[[lookup_id]]))
  if (ignore_case) {
    key_d <- tolower(key_d)
    key_s <- tolower(key_s)
  }

  dup_keys <- unique(key_s[duplicated(key_s)])
  hit_dups <- intersect(dup_keys, key_d[sel])

  if (length(hit_dups) > 0L && multiple == "error") {
    stop(length(hit_dups), " id(s) appear more than once in the lookup data, ",
         "including: ",
         paste(utils::head(hit_dups, 5), collapse = ", "),
         if (length(hit_dups) > 5) ", ..." else "",
         ". Set `multiple` to \"first\" or \"all\".", call. = FALSE)
  }

  # --- match ----------------------------------------------------------------
  n_before <- nrow(d)

  if (multiple == "all" && length(hit_dups) > 0L) {

    by_key <- split(seq_along(key_s), key_s)
    hits <- vector("list", nrow(d))
    for (i in seq_len(nrow(d))) {
      if (!sel[i]) {
        hits[[i]] <- NA_integer_
      } else {
        h <- by_key[[key_d[i]]]
        hits[[i]] <- if (is.null(h)) NA_integer_ else h
      }
    }
    row_rep <- rep(seq_len(nrow(d)), lengths(hits))
    idx     <- unlist(hits, use.names = FALSE)
    base    <- d[row_rep, , drop = FALSE]

  } else {

    if (length(hit_dups) > 0L) {
      warning(length(hit_dups), " id(s) appear more than once in the lookup ",
              "data; keeping the first match for each.", call. = FALSE)
      keep  <- !duplicated(key_s)
      lk    <- lk[keep, , drop = FALSE]
      key_s <- key_s[keep]
    }
    idx <- match(key_d, key_s)
    idx[!sel] <- NA_integer_
    base <- d
  }

  new <- lk[idx, lookup_cols, drop = FALSE]
  names(new) <- final_names
  rownames(new) <- NULL

  # --- optional type repair -------------------------------------------------
  n_conv <- character(0)
  if (fix_types) {
    conv <- names(new)[vapply(new, looks_numeric, logical(1))]
    if (length(conv) > 0L) {
      new[conv] <- lapply(new[conv], as.numeric)
      n_conv <- conv
    }
  }

  out <- cbind(base, new)
  rownames(out) <- NULL

  n_matched <- sum(!is.na(idx))
  types     <- vapply(new, function(z) class(z)[1], character(1))
  na_counts <- vapply(new, function(z) sum(is.na(z)), integer(1))
  complete  <- stats::complete.cases(out[, final_names, drop = FALSE])

  if (complete_only) {
    out <- out[complete, , drop = FALSE]
    rownames(out) <- NULL
  }

  # --- report ---------------------------------------------------------------
  if (!quiet) {
    cat("\nSUCCESS!\n")

    cat(sprintf("  Matched %s of %s row(s).\n",
                format(n_matched, big.mark = ","),
                format(n_sel, big.mark = ",")))

    if (n_sel < n_before) {
      cat(sprintf("  %d row(s) not selected by `add_subset`; left as NA.\n",
                  n_before - n_sel))
    }
    if (multiple == "all" && nrow(out) != n_before) {
      cat(sprintf("  multiple = \"all\": %d rows in, %d rows out.\n",
                  n_before, nrow(out)))
    }
    if (length(n_conv) > 0L) {
      cat("  Converted to numeric: ", paste(n_conv, collapse = ", "), "\n",
          sep = "")
    }
    if (complete_only) {
      cat(sprintf("  complete_only = TRUE: kept %d of %d rows.\n",
                  sum(complete), length(complete)))
    }

    cat(sprintf("  Added %d column%s:\n", length(types),
                if (length(types) == 1L) "" else "s"))
    w <- max(nchar(names(types)))
    for (i in seq_along(types)) {
      cat(sprintf("    %-*s  %s, %s missing\n", w, names(types)[i], types[i],
                  format(na_counts[i], big.mark = ",")))
    }
    cat("\n")
  }

  if (was_dt) data.table::setDT(out)

  out
}


# Internal: TRUE if a character column's non-missing values all parse as numbers
looks_numeric <- function(z) {
  if (!is.character(z)) return(FALSE)
  z <- z[!is.na(z) & nzchar(trimws(z))]
  if (!length(z)) return(FALSE)
  !any(is.na(suppressWarnings(as.numeric(z))))
}


# Internal: read a lookup file based on its extension
read_lookup <- function(file) {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))

  switch(
    ext,
    csv = utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
    tsv = ,
    txt = utils::read.delim(file, stringsAsFactors = FALSE, check.names = FALSE),
    rds = as.data.frame(readRDS(file)),
    xls = ,
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reading '.", ext, "' files requires the 'readxl' package.",
             call. = FALSE)
      }
      as.data.frame(readxl::read_excel(file))
    },
    stop("Unsupported file type: '", ext, "'.", call. = FALSE)
  )
}
