#' Keep complete cases on selected columns, with optional trimming and scaling
#'
#' @description
#' Drops any row missing a value on selected columns.
#'
#' `drop_others = TRUE` discards every column except the ones named, which is
#' useful when a wide dataset is being narrowed to the variables in an analysis.
#'
#' `scale_numeric = TRUE` standardises the numeric columns among `cols`, and
#' does so after the rows have been cut, so the means and SDs come from the rows
#' that remain.
#'
#' `drop_others = TRUE` discards every column that was not named, apart from an
#' identifier column named in `id_col`. This is the quickest way to reduce a
#' wide dataset to the variables an analysis actually uses, and it makes the
#' resulting object small enough to inspect.
#'
#' @param d A data.frame or data.table.
#' @param cols Character or numeric: the columns that must be complete.
#' @param id_col Optional character: an identifier column to retain when
#'   `drop_others = TRUE`.
#' @param drop_others Logical. If TRUE, keep only `cols` and `id_col`. Default
#'   FALSE.
#' @param scale_numeric Logical. If TRUE, standardize the numeric columns among
#'   `cols` after the rows have been cut. Default FALSE.
#' @param quiet Logical. If TRUE, suppress the summary message. Default FALSE.
#'
#' @return The filtered data. A data.table in, a data.table out; otherwise a
#'   data.frame.
#'
#' @examples
#' \dontrun{
#' # keep rows with values on all the predictors
#' d_use <- keep_complete(demo_words, c("freq", "aoa", "concreteness"))
#'
#' # narrow to just those columns, keeping the word, and standardise them
#' d_use <- keep_complete(demo_words,
#'                        cols = c("freq", "aoa", "concreteness"),
#'                        id_col = "word",
#'                        drop_others = TRUE,
#'                        scale_numeric = TRUE)
#' }
#'
#' @export
keep_complete <- function(d,
                          cols,
                          id_col = NULL,
                          drop_others = FALSE,
                          scale_numeric = FALSE,
                          quiet = FALSE) {

  was_dt <- inherits(d, "data.table")
  d <- as.data.frame(d)

  cols_chr <- if (is.numeric(cols)) names(d)[cols] else cols
  missing_cols <- setdiff(cols_chr, names(d))
  if (length(missing_cols) > 0L) {
    stop("These columns are not in `d`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (length(cols_chr) == 0L) {
    stop("No columns supplied in `cols`.", call. = FALSE)
  }

  if (!is.null(id_col)) {
    if (!is.character(id_col) || length(id_col) != 1L) {
      stop("`id_col` must be a single character string.", call. = FALSE)
    }
    if (!id_col %in% names(d)) {
      stop("`id_col` '", id_col, "' not found in `d`.", call. = FALSE)
    }
  }

  n_before <- nrow(d)
  keep_rows <- stats::complete.cases(d[, cols_chr, drop = FALSE])
  out <- d[keep_rows, , drop = FALSE]
  rownames(out) <- NULL

  if (drop_others) {
    out <- out[, unique(c(id_col, cols_chr)), drop = FALSE]
  }

  n_scaled <- 0L
  if (scale_numeric) {
    for (v in cols_chr) {
      if (is.numeric(out[[v]])) {
        s <- stats::sd(out[[v]])
        if (is.na(s) || s == 0) {
          warning("Column '", v, "' has zero variance; left unscaled.",
                  call. = FALSE)
          next
        }
        out[[v]] <- as.vector(scale(out[[v]]))
        n_scaled <- n_scaled + 1L
      }
    }
  }

  if (!quiet) {
    message(sprintf("Kept %d of %d rows (%.1f%%); %d columns.",
                    nrow(out), n_before, 100 * nrow(out) / n_before,
                    ncol(out)))
    if (scale_numeric) {
      message(sprintf("Scaled %d numeric column(s).", n_scaled))
    }
  }

  if (was_dt) data.table::setDT(out)

  out
}
