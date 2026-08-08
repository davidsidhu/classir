#' Simulated lexical data
#'
#' A made-up dataset of 500 invented words with lexical characteristics and two
#' outcomes, used in the examples and README. Nothing in it is real: the words
#' are generated strings and the values are simulated to have roughly the shape
#' and correlations of published lexical norms, so that example output looks
#' realistic without redistributing anyone else's data.
#'
#' A few deliberate problems are included -- missing values, a case mismatch in
#' `pos`, and a sentinel value in `rt` -- so that [check_data()] has something
#' to find.
#'
#' @format A data frame with 500 rows and 10 columns:
#' \describe{
#'   \item{word}{Invented word form.}
#'   \item{length}{Number of letters.}
#'   \item{freq}{Frequency, right-skewed.}
#'   \item{old20}{Orthographic neighbourhood measure.}
#'   \item{aoa}{Age of acquisition, in years.}
#'   \item{concreteness}{Concreteness rating, 1 to 5.}
#'   \item{pos}{Part of speech: noun, verb or adjective.}
#'   \item{sound_symbolic}{1 if the word was generated as sound-symbolic, else 0.}
#'   \item{iconicity}{Iconicity rating.}
#'   \item{rt}{Lexical decision response time, in milliseconds.}
#' }
#'
#' @source Simulated. See `data-raw/demo_words.R` for the code that made it.
"demo_words"
