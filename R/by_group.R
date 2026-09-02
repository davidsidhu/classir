#' Mean and SD table by group
#'
#' For looking at values of a variable by group (e.g., average reaction time by
#' age group). Builds a table with one column per group. A numeric variable
#' gets one row, each cell formatted as "M (SD)"; a categorical variable gets a
#' row per level, each cell holding that level's count. Two grouping variables
#' can be crossed, giving one column per combination.
#'
#' @param d A data.frame or data.table.
#' @param vars Integer or character: columns to summarise (e.g. `5:12`).
#' @param by Character: one or two grouping column names.
#' @param var_name Name of the variable column in the exported table. Default
#'   `"Dimension"`.
#' @param digits Decimal places for means and SDs. Default 2.
#' @param sep Separator between levels when two grouping variables are crossed.
#'   Default `": "`.
#' @param empty String used where a cell has no data. Default an em dash.
#' @param file Optional path to write the main table as CSV.
#' @param n_file Optional path to write the n table as CSV.
#'
#' @return An object of class `classir_by_group`, with elements `table`
#'   (the "M (SD)" cells), `n` (group sizes), and `n_var` (non-missing n per
#'   variable per group). Printing is handled by a custom method; access the
#'   elements directly to export them.
#'
#' @examples
#' \dontrun{
#' by_group(d, "Response", "FriendCond")
#' by_group(d, c("RT", "Accuracy"), c("FriendCond", "WordType"))
#' }
#'
#' @export
by_group <- function(d,
                     vars,
                     by,
                     var_name = "Dimension",
                     digits = 2,
                     sep = ": ",
                     empty = "\u2014",
                     file = NULL,
                     n_file = NULL) {

  was_dt <- inherits(d, "data.table")
  d <- as.data.frame(d)

  # --- grouping columns -----------------------------------------------------
  if (!is.character(by) || !length(by) %in% 1:2) {
    stop("`by` must be one or two column names.", call. = FALSE)
  }
  missing_grp <- setdiff(by, names(d))
  if (length(missing_grp) > 0L) {
    stop("Grouping column(s) not found in `d`: ",
         paste(missing_grp, collapse = ", "), call. = FALSE)
  }

  # --- variables ------------------------------------------------------------
  vars_chr <- if (is.numeric(vars)) names(d)[vars] else vars
  vars_chr <- intersect(vars_chr, names(d))
  if (length(vars_chr) == 0L) {
    stop("No valid columns found in `vars`.", call. = FALSE)
  }
  vars_chr <- setdiff(vars_chr, by)

  is_num <- vapply(d[vars_chr], is.numeric, logical(1))
  if (length(vars_chr) == 0L) {
    stop("No columns left in `vars`.", call. = FALSE)
  }

  # a numeric variable gets one row of M (SD); a categorical one gets a row per
  # level, holding counts. rows is what the table is actually built from.
  rows_spec <- list()
  for (i in seq_along(vars_chr)) {
    v <- vars_chr[i]
    if (is_num[i]) {
      rows_spec[[length(rows_spec) + 1L]] <-
        list(var = v, level = NA_character_, label = v)
    } else {
      z  <- d[[v]]
      lv <- if (is.factor(z)) levels(droplevels(z)) else
        sort(unique(as.character(z[!is.na(z)])))
      for (l in lv) {
        rows_spec[[length(rows_spec) + 1L]] <-
          list(var = v, level = l, label = paste0(v, ": ", l))
      }
    }
  }

  row_labels <- vapply(rows_spec, function(r) r$label, character(1))

  # --- group labels, in a stable order --------------------------------------
  lev <- lapply(by, function(g) {
    z <- d[[g]]
    if (is.factor(z)) levels(droplevels(z))
    else sort(unique(as.character(z[!is.na(z)])))
  })

  if (length(by) == 1L) {
    combos <- data.frame(a = lev[[1]], stringsAsFactors = FALSE)
    names(combos) <- by
    labels <- combos[[1]]
    key_d  <- as.character(d[[by[1]]])
  } else {
    combos <- expand.grid(lev[[2]], lev[[1]], stringsAsFactors = FALSE)
    combos <- combos[, c(2, 1), drop = FALSE]
    names(combos) <- by
    labels <- paste0(combos[[1]], sep, combos[[2]])
    key_d  <- paste0(as.character(d[[by[1]]]), sep,
                     as.character(d[[by[2]]]))
  }

  key_combo <- if (length(by) == 1L) combos[[1]] else labels

  present <- key_combo %in% unique(key_d)
  if (!any(present)) {
    stop("No rows fall into any group combination.", call. = FALSE)
  }
  if (any(!present)) {
    combos    <- combos[present, , drop = FALSE]
    labels    <- labels[present]
    key_combo <- key_combo[present]
  }

  # --- cells ----------------------------------------------------------------
  mat <- matrix(empty, nrow = length(rows_spec), ncol = length(labels),
                dimnames = list(row_labels, labels))
  n_mat <- matrix(0L, nrow = length(rows_spec), ncol = length(labels),
                  dimnames = list(row_labels, labels))

  fmt <- paste0("%.", digits, "f (%.", digits, "f)")

  for (j in seq_along(key_combo)) {
    rows <- which(key_d == key_combo[j])
    for (i in seq_along(rows_spec)) {
      spec <- rows_spec[[i]]
      z <- d[rows, spec$var]

      if (is.na(spec$level)) {                       # numeric: M (SD)
        z <- z[!is.na(z)]
        n_mat[i, j] <- length(z)
        if (length(z) == 0L) next
        m <- mean(z)
        s <- if (length(z) > 1L) stats::sd(z) else NA_real_
        mat[i, j] <- if (is.na(s)) {
          sprintf(paste0("%.", digits, "f (", empty, ")"), m)
        } else {
          sprintf(fmt, m, s)
        }
      } else {                                       # categorical: a count
        k <- sum(as.character(z) == spec$level, na.rm = TRUE)
        n_mat[i, j] <- k
        mat[i, j]   <- format(k, big.mark = ",", trim = TRUE)
      }
    }
  }

  tab <- data.frame(v = row_labels, mat, check.names = FALSE,
                    stringsAsFactors = FALSE)
  names(tab)[1] <- var_name
  rownames(tab) <- NULL

  n_group <- vapply(key_combo, function(k) sum(key_d == k, na.rm = TRUE),
                    integer(1))
  n_tab <- combos
  n_tab$n <- unname(n_group)
  rownames(n_tab) <- NULL

  n_var <- data.frame(v = row_labels, n_mat, check.names = FALSE,
                      stringsAsFactors = FALSE)
  names(n_var)[1] <- var_name
  rownames(n_var) <- NULL

  if (!is.null(file))   utils::write.csv(tab,   file,   row.names = FALSE)
  if (!is.null(n_file)) utils::write.csv(n_tab, n_file, row.names = FALSE)

  if (was_dt) {
    data.table::setDT(tab)
    data.table::setDT(n_tab)
    data.table::setDT(n_var)
  }

  structure(
    list(table = tab, n = n_tab, n_var = n_var,
         labels = labels, n_group = unname(n_group), vars = row_labels),
    class = "classir_by_group"
  )
}


#' Print a by_group table
#'
#' @param x A `classir_by_group` object.
#' @param abbrev Optional integer. Group labels longer than this are
#'   truncated, which fits more columns per block.
#' @param width Console width to wrap at. Defaults to `getOption("width")`.
#' @param ... Ignored.
#'
#' @export
print.classir_by_group <- function(x, abbrev = NULL, width = NULL, ...) {

  tab    <- as.data.frame(x$table)
  labels <- x$labels
  vars   <- x$vars
  ns     <- paste0("n = ", format(x$n_group, big.mark = ",", trim = TRUE))

  if (!is.null(abbrev)) {
    long <- nchar(labels) > abbrev
    labels[long] <- paste0(substr(labels[long], 1, abbrev - 1), "\u2026")
  }

  lab_w <- max(nchar(vars))
  gap   <- "  "
  avail <- max((if (is.null(width)) getOption("width", 80) else width) -
                 lab_w - nchar(gap), 20L)

  # column widths: header, cells, and the n string
  widths <- vapply(seq_along(labels), function(j) {
    max(nchar(c(labels[j], tab[[j + 1L]], ns[j])))
  }, integer(1))

  # split columns into blocks that fit
  blocks <- list()
  j <- 1L
  while (j <= length(labels)) {
    used <- 0L
    k <- j
    while (k <= length(labels) &&
           used + widths[k] + nchar(gap) <= avail) {
      used <- used + widths[k] + nchar(gap)
      k <- k + 1L
    }
    if (k == j) k <- j + 1L
    blocks[[length(blocks) + 1L]] <- j:(k - 1L)
    j <- k
  }

  cat("\n")

  for (idx in blocks) {

    # header
    cat(strrep(" ", lab_w), gap, sep = "")
    cat(paste(mapply(centre_str, labels[idx], widths[idx]), collapse = gap),
        "\n", sep = "")

    # one row per variable
    for (i in seq_along(vars)) {
      cat(formatC(vars[i], width = -lab_w), gap, sep = "")
      cells <- vapply(idx, function(j) centre_str(tab[i, j + 1L], widths[j]),
                      character(1))
      cat(paste(cells, collapse = gap), "\n", sep = "")
    }

    # n row
    cat(strrep(" ", lab_w), gap, sep = "")
    cat(paste(mapply(centre_str, ns[idx], widths[idx]), collapse = gap),
        "\n", sep = "")

    cat("\n")
  }

  invisible(x)
}


# Internal: centre a string in a field of given width
centre_str <- function(s, width) {
  s <- as.character(s)
  pad <- width - nchar(s)
  if (pad <= 0L) return(s)
  left <- pad %/% 2L
  paste0(strrep(" ", left), s, strrep(" ", pad - left))
}
