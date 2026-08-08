#' Add values from another dataset, step by step
#'
#' @description
#' A guided version of [add_values()] for people who would rather answer a few
#' questions than write out a function call. It asks where the new values are
#' coming from, which columns to match on, and which columns to add, then does
#' the work and prints the equivalent `add_values()` command so the step can be
#' put in a script.
#'
#' The result is assigned back to the dataset you passed in.
#'
#' Only the basics are offered here. For renaming columns as they arrive,
#' restricting to a subset of rows, or handling repeated ids, use
#' [add_values()] directly.
#'
#' Columns are chosen from numbered menus rather than typed, which removes the
#' most common source of error, and the prompt for a file path shows the
#' directory R is currently looking in along with examples of relative and
#' absolute paths. Quotes are stripped if they are typed anyway. Any step can
#' be abandoned by pressing 0, or by leaving the answer blank and pressing
#' enter, and nothing is changed until
#' the whole sequence completes.
#'
#' When it finishes, the equivalent `add_values()` call is printed with named
#' arguments, so the step can be pasted into a script and the analysis stays
#' reproducible.
#'
#' The function is interactive by design and stops with an explanatory error if
#' called from a script or an R Markdown document.
#'
#' @param d A data.frame or data.table to add columns into.
#'
#' @return Invisibly, the updated dataset. It is also assigned back to `d` in
#'   the calling environment.
#'
#' @examples
#' \dontrun{
#' add_values_guided(d2)
#' }
#'
#' @export
add_values_guided <- function(d) {

  if (!interactive()) {
    stop("`add_values_guided()` asks questions, so it only works in an ",
         "interactive session. Use add_values() in scripts.", call. = FALSE)
  }

  d_name <- deparse(substitute(d))
  if (length(d_name) != 1L || make.names(d_name) != d_name) {
    stop("Pass a named dataset, e.g. add_values_guided(d2).", call. = FALSE)
  }

  dd <- as.data.frame(d)
  if (ncol(dd) == 0L || nrow(dd) == 0L) {
    stop("`", d_name, "` has no rows or no columns.", call. = FALSE)
  }

  cancelled <- function() {
    message("\nCancelled -- nothing was changed.")
    invisible(NULL)
  }

  cat("\nThis adds columns from another dataset into ", d_name,
      ", matching rows by a\nshared column \u2014 words, IDs, or something else ",
      "the two have in common.\n\n", sep = "")

  # --- 1. where the values come from ----------------------------------------
  src <- utils::menu(c("a file on my computer",
                       "something already loaded in R"),
                     title = "Where are the new values coming from?")
  if (src == 0L) return(cancelled())

  if (src == 1L) {
    cat("\nPath to the file (no quotes needed).\n")
    cat("  R is currently looking in:\n      ", getwd(), "\n\n", sep = "")
    cat("  - If the file is in that folder, just type its name: ELP.csv\n")
    cat("  - If it's in a folder inside that one, include it: data/ELP.csv\n")
    cat("  - If it's somewhere else entirely, paste the full path,\n")
    cat("    or drag the file from Finder onto the console\n\n")

    path <- trimws(readline("Path: "))
    path <- gsub("^[\"']|[\"']$", "", path)   # in case they quote it anyway
    if (!nzchar(path)) return(cancelled())

    if (!file.exists(path)) {
      stop("Can't find a file at '", path, "'.\n",
           "  R is looking in: ", getwd(), call. = FALSE)
    }

    lk <- read_lookup(path)
    lk_label <- paste0("\"", path, "\"")
    lk_shown <- path

  } else {
    objs <- ls(envir = parent.frame())
    is_df <- vapply(objs, function(o) {
      is.data.frame(get(o, envir = parent.frame()))
    }, logical(1))
    objs <- setdiff(objs[is_df], d_name)

    if (length(objs) == 0L) {
      stop("No other data frames found in your environment.", call. = FALSE)
    }

    pick <- utils::menu(objs, title = "\nWhich dataset?")
    if (pick == 0L) return(cancelled())

    lk <- as.data.frame(get(objs[pick], envir = parent.frame()))
    lk_label <- objs[pick]
    lk_shown <- objs[pick]
  }

  cat("  Read ", format(nrow(lk), big.mark = ","), " rows, ",
      ncol(lk), " columns.\n", sep = "")

  # --- 2. matching columns --------------------------------------------------
  i_id <- utils::menu(names(dd),
                      title = paste0("\nWhich column in ", d_name,
                                     " should be used to match on?"))
  if (i_id == 0L) return(cancelled())
  id <- names(dd)[i_id]

  i_lid <- utils::menu(names(lk),
                       title = paste0("\nWhich column in ", lk_shown,
                                      " holds the matching values?"))
  if (i_lid == 0L) return(cancelled())
  lookup_id <- names(lk)[i_lid]

  # --- 3. columns to add ----------------------------------------------------
  avail <- setdiff(names(lk), lookup_id)
  if (length(avail) == 0L) {
    stop("`", lk_shown, "` has no columns besides '", lookup_id, "'.",
         call. = FALSE)
  }

  cat("\nWhich columns do you want to add into ", d_name, "?\n",
      "  Type numbers separated by commas. To cancel, leave blank and\n",
      "  press enter.\n\n", sep = "")
  for (i in seq_along(avail)) {
    cat(sprintf("  %d: %s\n", i, avail[i]))
  }

  raw <- trimws(readline("Selection: "))
  if (!nzchar(raw)) return(cancelled())

  nums <- suppressWarnings(as.integer(trimws(strsplit(raw, "[,[:space:]]+")[[1]])))
  nums <- nums[!is.na(nums)]

  if (length(nums) == 0L || any(nums < 1 | nums > length(avail))) {
    stop("Didn't understand '", raw, "'. Give numbers from 1 to ",
         length(avail), ", separated by commas.", call. = FALSE)
  }

  lookup_cols <- avail[nums]

  # --- 4. do it -------------------------------------------------------------
  cat("\n")
  out <- add_values(dd,
                    id = id,
                    lookup = lk,
                    lookup_id = lookup_id,
                    lookup_cols = lookup_cols)

  if (inherits(d, "data.table")) data.table::setDT(out)

  assign(d_name, out, envir = parent.frame())

  # --- 5. show the command --------------------------------------------------
  cols_txt <- if (length(lookup_cols) == 1L) {
    paste0("\"", lookup_cols, "\"")
  } else {
    paste0("c(", paste0("\"", lookup_cols, "\"", collapse = ", "), ")")
  }

  pad <- strrep(" ", nchar(d_name) + nchar(" <- add_values("))

  cat("\nDone. The command for this was:\n\n")
  cat(sprintf("  %s <- add_values(%s,\n", d_name, d_name))
  cat(sprintf("  %sid = \"%s\",\n", pad, id))
  cat(sprintf("  %slookup = %s,\n", pad, lk_label))
  cat(sprintf("  %slookup_id = \"%s\",\n", pad, lookup_id))
  cat(sprintf("  %slookup_cols = %s)\n\n", pad, cols_txt))

  invisible(out)
}
