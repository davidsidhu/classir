#' Quick report for best_subsets_boot results
#'
#' Convenience helper to print the regression summary and the
#' sorted selection frequencies from a best_subsets_boot() result.
#'
#' @param x A list returned by best_subsets_boot().
#' @param digits Number of digits to round selection frequencies.
#'
#' @return Invisibly returns the sorted selection frequencies.
#' @export
best_subsets_report <- function(x, digits = 3) {
  if (!is.list(x) || is.null(x$model) || is.null(x$sel_freq)) {
    stop("x does not look like a best_subsets_boot() result.", call. = FALSE)
  }

  cat("Best-subsets regression model\n\n")
  mod_sum <- summary(x$model)
  print(mod_sum)

  cat("\nSelection frequencies (sorted):\n\n")
  freq <- sort(x$sel_freq, decreasing = TRUE)
  print(round(freq, digits = digits))

  invisible(freq)
}
