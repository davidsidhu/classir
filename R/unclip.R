#' Read data copied from Excel
#'
#' @description
#' Reads whatever is on the clipboard as a table, so a block copied out of
#' Excel becomes a data frame in one call:
#'
#' \preformatted{
#' d <- unclip()
#' }
#'
#' The counterpart to [copy_table()], which goes the other way.
#'
#' @details
#' A few defaults differ from `read.table()`, because clipboard data from a
#' spreadsheet is not the same as a well-behaved text file. Apostrophes are not
#' treated as quote characters, so a word like `don't` does not break the
#' parse; `#` is not treated as a comment, so it does not truncate a line; and
#' empty cells become `NA` rather than empty strings.
#'
#' Column names are left as Excel had them, spaces and all, rather than being
#' mangled into `Word.Count`. Set `check_names = TRUE` for R's usual behaviour.
#'
#' On macOS this uses `pbpaste` and on Windows the system clipboard. On Linux
#' it needs `xclip` or `xsel` installed.
#'
#' @param sep Column separator. Default a tab, which is what Excel puts on the
#'   clipboard.
#' @param header Logical. Does the first row hold column names? Default TRUE.
#' @param check_names Logical. If TRUE, run column names through
#'   `make.names()`, which replaces spaces with dots. Default FALSE.
#' @param dt Logical. If TRUE, return a data.table. Defaults to TRUE when
#'   data.table is installed, since that is likely what you want it as.
#' @param quiet Logical. If TRUE, don't report what was read.
#' @param ... Passed to `utils::read.table()`.
#'
#' @return A data frame, or a data.table when `dt = TRUE`.
#'
#' @examples
#' \dontrun{
#' # copy a block in Excel, then
#' d <- unclip()
#'
#' # no header row
#' d <- unclip(header = FALSE)
#' }
#'
#' @export
unclip <- function(sep = "\t",
                   header = TRUE,
                   check_names = FALSE,
                   dt = requireNamespace("data.table", quietly = TRUE),
                   quiet = FALSE,
                   ...) {

  txt <- read_clipboard()

  if (is.null(txt)) {
    stop("Couldn't reach the clipboard.\n",
         "  On Linux this needs 'xclip' or 'xsel' installed.", call. = FALSE)
  }
  if (length(txt) == 0L || all(!nzchar(trimws(txt)))) {
    stop("The clipboard is empty. Copy something in Excel first.",
         call. = FALSE)
  }

  out <- tryCatch(
    utils::read.table(text = txt,
                      sep          = sep,
                      header       = header,
                      quote        = "\"",        # not ', so don't is safe
                      comment.char = "",          # not #, so it can't truncate
                      na.strings   = c("NA", ""),
                      check.names  = check_names,
                      stringsAsFactors = FALSE,
                      ...),
    error = function(e) {
      stop("Couldn't read the clipboard as a table: ", conditionMessage(e),
           "\n  Is it a rectangular block, with every row the same width?",
           call. = FALSE)
    }
  )

  if (!quiet) {
    cat(sprintf("Read %s rows, %d column%s.\n",
                format(nrow(out), big.mark = ","), ncol(out),
                if (ncol(out) == 1L) "" else "s"))
    if (ncol(out) == 1L && header) {
      cat("  Only one column -- if that looks wrong, the separator may not be",
          "a tab.\n")
    }
  }

  if (dt) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      warning("data.table is not installed; returning a data.frame.",
              call. = FALSE)
    } else {
      data.table::setDT(out)
    }
  }

  out
}


# Internal: read the system clipboard as lines; NULL if there is no way to
read_clipboard <- function() {
  os <- Sys.info()[["sysname"]]

  if (identical(os, "Darwin")) {
    con <- pipe("pbpaste", "r")
    on.exit(close(con), add = TRUE)
    return(readLines(con, warn = FALSE))
  }

  if (identical(os, "Windows")) {
    return(tryCatch(utils::readClipboard(), error = function(e) NULL))
  }

  for (tool in c("xclip -selection clipboard -o", "xsel --clipboard --output")) {
    if (nzchar(Sys.which(strsplit(tool, " ")[[1]][1]))) {
      con <- pipe(tool, "r")
      on.exit(close(con), add = TRUE)
      return(readLines(con, warn = FALSE))
    }
  }

  NULL
}
