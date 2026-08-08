#' Permutation null for best_subsets_boot selection frequencies
#'
#' Repeatedly permutes the outcome variable, runs \code{best_subsets_boot()}
#' for each permutation, and summarizes how often each predictor is selected
#' under the null (no true relationship).
#'
#' @param data A data.frame or data.table containing the variables.
#' @param outcome Character scalar. Name of the outcome variable to permute.
#' @param predictors Integer or character vector. Predictors to pass to
#'   \code{best_subsets_boot()}.
#' @param subset Optional subset argument passed to \code{best_subsets_boot()}.
#' @param n_perm Integer. Number of permutations to run (default 100).
#' @param seed Optional integer. If supplied, used to set the random seed.
#' @param shuffled_name Character scalar. Name of the temporary permuted
#'   outcome column to add to the data (internal only).
#' @param ... Further arguments passed to \code{best_subsets_boot()}.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{sel_freq_perm}: Named numeric vector giving the mean selection
#'     frequency of each predictor across permutations.
#'   \item \code{sel_mat}: Matrix of selection frequencies (predictors x
#'     permutations).
#'   \item \code{n_perm}: Number of permutations.
#'   \item \code{call}: The matched function call.
#' }
#'
#' @export
best_subsets_perm <- function(data,
                              outcome,
                              predictors,
                              subset = NULL,
                              n_perm = 100L,
                              seed = NULL,
                              shuffled_name = ".perm_outcome",
                              ...) {

  if (!is.character(outcome) || length(outcome) != 1L) {
    stop("'outcome' must be a single character string.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (shuffled_name %in% names(data)) {
    stop("Temporary column name '", shuffled_name, "' already exists in 'data'. ",
         "Choose a different 'shuffled_name'.")
  }

  # Work on a local copy so we never modify the user's object
  if (inherits(data, "data.table")) {
    data_local <- data.table::copy(data)
  } else {
    data_local <- data
  }

  predictor_names <- if (is.numeric(predictors)) {
    names(data_local)[predictors]
  } else {
    predictors
  }

  if (is.null(predictor_names) || any(!predictor_names %in% names(data_local))) {
    stop("All 'predictors' must correspond to columns in 'data'.")
  }

  sel_mat <- matrix(
    0,
    nrow = length(predictor_names),
    ncol = n_perm,
    dimnames = list(predictor_names, NULL)
  )

  for (p in seq_len(n_perm)) {

    # Permute outcome and store in temp column *inside the local copy*
    data_local[[shuffled_name]] <- sample(data_local[[outcome]])

    xm <- best_subsets_boot(
      data       = data_local,
      outcome    = shuffled_name,
      predictors = predictors,
      subset     = subset,
      ...
    )

    sf <- xm$sel_freq

    common <- intersect(names(sf), predictor_names)
    if (length(common) > 0L) {
      sel_mat[common, p] <- sf[common]
    }
  }

  sel_freq_perm <- rowMeans(sel_mat)

  out <- list(
    sel_freq_perm = sel_freq_perm,
    sel_mat       = sel_mat,
    n_perm        = n_perm,
    call          = match.call()
  )

  class(out) <- c("best_subsets_perm", class(out))
  out
}
