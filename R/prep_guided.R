#' Prepare a dataset for analysis, step by step
#'
#' @description
#' A guided version of [prep()] for people who would rather answer questions
#' than write out a function call. It walks through each preparation step in
#' turn -- naming the variables of interest, dropping incomplete rows,
#' subsetting, setting reference levels, effects coding, standardising, and
#' dropping unused columns -- and any step can be skipped by leaving it blank
#' and pressing enter.
#'
#' Naming variables of interest at the start narrows every later step to those
#' columns, so they only have to be picked once.
#'
#' The prepared data is assigned back to the dataset you passed in, and the
#' equivalent `prep()` call is printed so the step can be pasted into a script.
#'
#' @details
#' Renaming columns is not offered here; use [prep()] directly for that.
#'
#' The function is interactive by design and stops with an explanatory error if
#' called from a script or an R Markdown document.
#'
#' @param d A data.frame or data.table.
#'
#' @return Invisibly, the prepared data. It is also assigned back to `d` in the
#'   calling environment.
#'
#' @examples
#' \dontrun{
#' prep_guided(demo_words)
#' }
#'
#' @export
prep_guided <- function(d) {

  if (!interactive()) {
    stop("`prep_guided()` asks questions, so it only works in an interactive ",
         "session. Use prep() in scripts.", call. = FALSE)
  }

  d_name <- deparse(substitute(d))
  if (length(d_name) != 1L || make.names(d_name) != d_name) {
    stop("Pass a named dataset, e.g. prep_guided(d2).", call. = FALSE)
  }

  dd <- as.data.frame(d)
  if (ncol(dd) == 0L || nrow(dd) == 0L) {
    stop("`", d_name, "` has no rows or no columns.", call. = FALSE)
  }

  is_num <- vapply(dd, is.numeric, logical(1))
  is_cat <- vapply(dd, function(z) is.factor(z) || is.character(z) ||
                     is.logical(z), logical(1))

  cat("\nPreparing ", d_name, " (", format(nrow(dd), big.mark = ","),
      " rows, ", ncol(dd), " columns).\n",
      "To skip a step, leave it blank and press enter.\n", sep = "")

  args <- list()

  # ---------------------------------------------------------------- step 0
  cat("\n\u2014\u2014 1. Which variables will your analysis use? \u2014\u2014\n\n")
  cat("  Include your outcome variable and all predictors. Naming them\n",
      "  here saves picking them again later.\n\n", sep = "")
  vars <- pick_columns(names(dd), dd, "Variables of interest:")
  if (length(vars) > 0L) args$vars <- vars

  offer <- if (length(vars) > 0L) vars else names(dd)

  # ---------------------------------------------------------------- step 1
  cat("\n\u2014\u2014 2. Drop rows with missing values \u2014\u2014\n\n")
  if (length(vars) > 0L) {
    cat("  Your variables of interest are:\n    ", and_list(vars), "\n\n",
        sep = "")
    cat("  Type y to drop rows missing any of them. To choose columns\n",
        "  yourself, leave blank and press enter.\n\n", sep = "")
    ans <- trimws(readline("  Selection: "))
    if (tolower(substr(ans, 1, 1)) == "y") {
      args$complete <- TRUE
    } else {
      complete <- pick_columns(names(dd), dd,
                               "Which columns must have no missing values?")
      if (length(complete) > 0L) args$complete <- complete
    }
  } else {
    complete <- pick_columns(names(dd), dd,
                             "Which columns must have no missing values?")
    if (length(complete) > 0L) args$complete <- complete
  }

  # ---------------------------------------------------------------- step 2
  cat("\n\u2014\u2014 3. Keep only certain rows \u2014\u2014\n\n")
  cat("  Type a condition describing the rows to keep. For example:\n\n")
  cat("    ==   equal to          pos == \"noun\"\n")
  cat("    !=   not equal to      pos != \"adjective\"\n")
  cat("    >    greater than      freq > 1\n")
  cat("    <    less than         aoa < 10\n")
  cat("    &    and               pos == \"noun\" & freq > 1\n")
  cat("    |    or                pos == \"noun\" | pos == \"verb\"\n\n")
  cat("  Put text in quotes; column names go in as they are.\n\n")

  cat("  To skip this step, leave blank and press enter.\n\n")
  sub_txt <- trimws(readline("  Condition: "))
  if (nzchar(sub_txt)) {
    expr <- tryCatch(str2lang(sub_txt), error = function(e) NULL)
    if (is.null(expr)) {
      stop("Could not read that as a condition: ", sub_txt, call. = FALSE)
    }
    args$subset <- expr
  }

  # ---------------------------------------------------------------- step 3
  cat("\n\u2014\u2014 4. Set reference levels \u2014\u2014\n\n")
  cat_cols <- intersect(offer, names(dd)[is_cat])
  if (length(cat_cols) == 0L) {
    cat("  No categorical columns; skipping.\n")
  } else {
    chosen <- pick_columns(cat_cols, dd,
                           "Which factors need a reference level set?")
    refs <- character(0)
    for (v in chosen) {
      lv <- levels(as.factor(dd[[v]]))
      cat("\n  Levels of ", v, ":\n", sep = "")
      for (i in seq_along(lv)) cat(sprintf("    %d: %s\n", i, lv[i]))
      cat("  To skip this one, leave blank and press enter.\n")
      k <- trimws(readline("  Which should be the reference level? "))
      k <- suppressWarnings(as.integer(k))
      if (!is.na(k) && k >= 1 && k <= length(lv)) {
        refs[[v]] <- lv[k]
      } else {
        cat("  Skipped.\n")
      }
    }
    if (length(refs) > 0L) args$ref <- refs
  }

  # ---------------------------------------------------------------- step 4
  cat("\n\u2014\u2014 5. Effects code two-level factors \u2014\u2014\n\n")
  two_lev <- cat_cols[vapply(cat_cols, function(v) {
    length(unique(dd[[v]][!is.na(dd[[v]])])) == 2L
  }, logical(1))]
  if (length(two_lev) == 0L) {
    cat("  No two-level columns; skipping.\n")
  } else {
    eff <- pick_columns(two_lev, dd, "Which should be effects coded?")
    if (length(eff) > 0L) args$effects <- eff
  }

  # ---------------------------------------------------------------- step 5
  cat("\n\u2014\u2014 6. Standardise numeric variables \u2014\u2014\n\n")
  num_cols <- intersect(offer, names(dd)[is_num])
  if (length(num_cols) == 0L) {
    cat("  No numeric columns; skipping.\n")
  } else if (length(vars) > 0L) {
    cat("  Numeric variables of interest:\n    ", and_list(num_cols), "\n\n",
        sep = "")
    cat("  Type y to standardise all of them. To choose yourself, leave\n",
        "  blank and press enter.\n\n", sep = "")
    ans <- trimws(readline("  Selection: "))
    if (tolower(substr(ans, 1, 1)) == "y") {
      args$scale <- TRUE
      cat("\n  You will usually want to leave your outcome variable\n",
          "  unstandardised, so its coefficients stay in its own units.\n\n",
          sep = "")
      skip <- pick_columns(num_cols, dd,
                           "Which variables do you want to leave unstandardised?")
      if (length(skip) > 0L) args$no_scale <- skip
    } else {
      sc <- pick_columns(num_cols, dd, "Which should be standardised?")
      if (length(sc) > 0L) args$scale <- sc
    }
  } else {
    sc <- pick_columns(names(dd)[is_num], dd, "Which should be standardised?")
    if (length(sc) > 0L) args$scale <- sc
  }

  # ---------------------------------------------------------------- step 6
  cat("\n\u2014\u2014 7. Drop columns you're not using \u2014\u2014\n\n")
  used <- unique(c(vars,
                   if (!isTRUE(args$complete)) args$complete,
                   if (!isTRUE(args$scale)) args$scale,
                   args$effects, names(args$ref)))
  if (length(used) == 0L) {
    cat("  No columns named so far; skipping.\n")
  } else {
    cat("  Columns used so far: ", and_list(used), "\n\n", sep = "")
    cat("  Type y to keep only these. To keep everything, leave blank\n",
        "  and press enter.\n\n", sep = "")
    ans <- trimws(readline("  Selection: "))
    if (tolower(substr(ans, 1, 1)) == "y") {
      args$drop_others <- TRUE
      extra <- pick_columns(setdiff(names(dd), used), dd,
                            "Any others to keep as well? (e.g. an id column)")
      if (length(extra) > 0L) args$keep <- extra
    }
  }

  # -------------------------------------------------------------- do it
  if (length(args) == 0L) {
    cat("\nNothing selected -- ", d_name, " is unchanged.\n\n", sep = "")
    return(invisible(d))
  }

  cl  <- as.call(c(list(quote(prep), as.name(d_name)), args))
  out <- eval(cl, parent.frame())

  assign(d_name, out, envir = parent.frame())

  # ------------------------------------------------------- show the code
  txt <- deparse(cl, width.cutoff = 60L)
  cat("Done. The command for this was:\n\n")
  cat("  ", d_name, " <- ", txt[1], "\n", sep = "")
  if (length(txt) > 1L) {
    pad <- strrep(" ", nchar(d_name) + 4L)
    for (l in txt[-1]) cat("  ", pad, trimws(l), "\n", sep = "")
  }
  cat("\n")

  invisible(out)
}


# Internal: numbered multi-select over column names
pick_columns <- function(choices, d, prompt) {

  if (length(choices) == 0L) return(character(0))

  cat("  ", prompt, "\n\n", sep = "")
  w <- max(nchar(choices))
  for (i in seq_along(choices)) {
    cat(sprintf("    %d: %-*s  <%s>\n", i, w, choices[i],
                class(d[[choices[i]]])[1]))
  }
  cat("\n  Type numbers separated by commas, or \"all\" for every one.\n",
      "  To skip this step, leave blank and press enter.\n\n", sep = "")

  raw <- trimws(readline("  Selection: "))

  if (tolower(raw) %in% c("all", "a")) return(choices)
  if (!nzchar(raw)) return(character(0))

  nums <- suppressWarnings(
    as.integer(trimws(strsplit(raw, "[,[:space:]]+")[[1]])))
  nums <- nums[!is.na(nums)]
  bad  <- nums[nums < 1 | nums > length(choices)]
  if (length(bad) > 0L) {
    message("  Ignoring out-of-range: ", paste(bad, collapse = ", "))
  }
  choices[nums[nums >= 1 & nums <= length(choices)]]
}
