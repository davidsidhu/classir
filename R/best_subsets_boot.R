#' Best-subsets regression with BIC and bootstrap stability
#'
#' Runs best-subsets regression using BIC to choose model size,
#' then uses bootstrapping to estimate how often each predictor
#' is selected as part of the best-BIC model.
#'
#' @param data A data frame containing the outcome and predictors.
#' @param outcome A string giving the name of the outcome variable
#'   (e.g., `"Agreeableness"`).
#' @param predictors Optional predictor specification. Can be:
#'   \itemize{
#'     \item `NULL` (default): use all columns except `outcome`
#'     \item character vector: predictor column names
#'     \item numeric vector: predictor column positions
#'   }
#' @param subset Optional logical or numeric vector used to subset rows
#'   of `data` before fitting models. Logical vectors must have length
#'   `nrow(data)`; numeric vectors are treated as row indices.
#' @param B Number of bootstrap samples. Default is 300.
#' @param nvmax Maximum number of predictors considered by
#'   `leaps::regsubsets()`. Default is `length(predictors)`.
#' @param seed Optional integer seed for reproducibility. If `NULL`,
#'   the current RNG state is used.
#' @param progress Logical; if TRUE (default), show a text progress bar
#'   for the bootstrap iterations.
#' @param ... Additional arguments passed to `leaps::regsubsets()`.
#'
#' @return A list with components:
#' \describe{
#'   \item{model}{The `lm` object refit on the full (possibly subset) data
#'     using the best-BIC model.}
#'   \item{formula}{The `formula` used for `model`.}
#'   \item{sel_freq}{Named numeric vector of selection frequencies
#'     (between 0 and 1) for each predictor.}
#'   \item{regsubsets}{The original `leaps::regsubsets` object.}
#'   \item{summary}{The `summary()` of the original regsubsets call.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example: restrict to females and specific columns by index
#' af <- best_subsets_boot(
#'   data       = d,
#'   outcome    = "Agreeableness",
#'   predictors = 11:44,
#'   subset     = d$Gender == "Female",
#'   B          = 300,
#'   seed       = 123
#' )
#'
#' af$formula
#' summary(af$model)
#' sort(af$sel_freq, decreasing = TRUE)
#'
#' # Same but pre-subsetted data and automatic predictors:
#' d_a <- d[d$Gender == "Female", c(5, 11:44)]
#' af2 <- best_subsets_boot(
#'   data    = d_a,
#'   outcome = "Agreeableness",
#'   B       = 300,
#'   seed    = 123
#' )
#' }
#'
#' @importFrom leaps regsubsets
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
best_subsets_boot <- function(
    data,
    outcome,
    predictors = NULL,
    subset = NULL,
    B = 300,
    nvmax = NULL,
    seed = NULL,
    progress = TRUE,
    ...
) {
  # Basic checks -------------------------------------------------------------
  if (!outcome %in% names(data)) {
    stop("Outcome '", outcome, "' not found in data.", call. = FALSE)
  }

  # Handle predictors argument (NULL, names, or numeric indices) ------------
  if (is.null(predictors)) {
    # default: all columns except outcome
    predictors <- setdiff(names(data), outcome)
  } else {
    if (is.numeric(predictors)) {
      # interpret as column positions
      if (any(predictors < 1 | predictors > ncol(data))) {
        stop("'predictors' numeric indices out of range.", call. = FALSE)
      }
      predictors <- names(data)[predictors]
    }
    # if character, assume they are names
    missing_pred <- setdiff(predictors, names(data))
    if (length(missing_pred) > 0L) {
      stop(
        "These predictors are not columns in 'data': ",
        paste(missing_pred, collapse = ", "),
        call. = FALSE
      )
    }
    # in case outcome accidentally included, drop it
    predictors <- setdiff(predictors, outcome)
  }

  if (length(predictors) == 0L) {
    stop("No predictors supplied (predictors has length 0).", call. = FALSE)
  }

  if (is.null(nvmax)) {
    nvmax <- length(predictors)
  }

  # Optional row filtering ---------------------------------------------------
  if (!is.null(subset)) {
    if (is.logical(subset)) {
      if (length(subset) != nrow(data)) {
        stop("Logical 'subset' must have length nrow(data).", call. = FALSE)
      }
      data <- data[subset, , drop = FALSE]
    } else if (is.numeric(subset)) {
      data <- data[subset, , drop = FALSE]
    } else {
      stop("'subset' must be NULL, logical, or numeric.", call. = FALSE)
    }
  }

  # Use only the outcome + predictors columns -------------------------------
  data_use <- data[, c(outcome, predictors), drop = FALSE]

  # 1. Best BIC model on the full sample ------------------------------------
  fml_all <- stats::reformulate(predictors, response = outcome)

  subsets <- leaps::regsubsets(
    fml_all,
    data   = data_use,
    nbest  = 1,
    nvmax  = nvmax,
    method = "exhaustive",
    ...
  )

  sum_sub   <- summary(subsets)
  best_id   <- which.min(sum_sub$bic)
  vars_best <- names(sum_sub$which[best_id, ])[sum_sub$which[best_id, ]]
  vars_best <- setdiff(vars_best, "(Intercept)")

  fml_best <- stats::reformulate(vars_best, response = outcome)
  model    <- stats::lm(fml_best, data = data_use)

  # 2. Bootstrap stability of selected predictors ---------------------------
  if (!is.null(seed)) set.seed(seed)

  var_names  <- predictors
  sel_counts <- stats::setNames(numeric(length(var_names)), var_names)

  n <- nrow(data_use)

  # Progress bar ------------------------------------------------------------
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = B, style = 3)
  }

  for (b in seq_len(B)) {
    if (progress) utils::setTxtProgressBar(pb, b)

    idx       <- sample.int(n, replace = TRUE)
    data_boot <- data_use[idx, ]

    subsets_b <- leaps::regsubsets(
      fml_all,
      data   = data_boot,
      nbest  = 1,
      nvmax  = nvmax,
      method = "exhaustive",
      ...
    )

    sum_b     <- summary(subsets_b)
    best_id_b <- which.min(sum_b$bic)

    vars_b <- names(sum_b$which[best_id_b, ])[sum_b$which[best_id_b, ]]
    vars_b <- setdiff(vars_b, "(Intercept)")

    sel_counts[vars_b] <- sel_counts[vars_b] + 1
  }

  if (progress) close(pb)

  sel_freq <- sel_counts / B

  # Return everything useful -----------------------------------------------
  list(
    model      = model,
    formula    = fml_best,
    sel_freq   = sel_freq,
    regsubsets = subsets,
    summary    = sum_sub
  )
}
