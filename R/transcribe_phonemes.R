# ---------------------------------------------------------------------------
# Internal helpers: IPA cleaning and segmentation
# ---------------------------------------------------------------------------

# Internal helper: strip IPA diacritics
strip_ipa_diacritics <- function(s) {
  if (is.na(s) || s == "") return(s)

  # Remove ALL combining marks (Unicode "Mark" category)
  s <- gsub("\\p{M}+", "", s, perl = TRUE)

  # Remove common spacing diacritics/modifier letters (conservative for English)
  spacing_diacritics <- c(
    "\u02D0", "\u02D1",                         # length
    "\u02B0", "\u02B2", "\u02B7",               # aspiration / palatalization / labialization
    "\u02E0", "\u02E4", "\u02DE",               # velar / pharyngeal / rhoticity
    "\u02BC", "\u02C0", "\u02E1"                # ejective / glottal / lateral release
  )
  if (length(spacing_diacritics)) {
    s <- gsub(paste0("[", paste(spacing_diacritics, collapse = ""), "]"), "", s)
  }

  s
}


# Internal: IPA -> segments (diacritics stripped; diphthongs kept as units)
ipa_to_segments <- function(
    ipa_string,
    keep_stress = FALSE,
    extra_multichar = character(0)
) {
  if (is.na(ipa_string) || ipa_string == "") return(character(0))

  s <- gsub("\\s+", "", ipa_string)
  s <- strip_ipa_diacritics(s)

  # Multi-character units (longest-match first)
  multichar <- unique(c(
    # affricates (after diacritic stripping, tie-bar versions collapse to these)
    "t\u0283", "d\u0292",
    # diphthongs treated as single units
    "o\u028A", "a\u026A", "a\u028A", "\u0254\u026A", "e\u026A",
    extra_multichar
  ))
  multichar <- multichar[order(nchar(multichar), decreasing = TRUE)]

  drop <- c(".", "\u2016", "|")
  if (!keep_stress) drop <- c(drop, "\u02C8", "\u02CC")

  out <- character(0)
  i <- 1L
  n <- nchar(s)

  while (i <= n) {
    matched <- FALSE
    for (pat in multichar) {
      k <- nchar(pat)
      if (k > 0 && i + k - 1L <= n && substr(s, i, i + k - 1L) == pat) {
        out <- c(out, pat)
        i <- i + k
        matched <- TRUE
        break
      }
    }
    if (matched) next

    ch <- substr(s, i, i)
    if (ch %in% drop) {
      i <- i + 1L
      next
    }

    out <- c(out, ch)
    i <- i + 1L
  }

  out
}


# ---------------------------------------------------------------------------
# ARPAbet -> IPA mapping and dictionary construction
# ---------------------------------------------------------------------------

# Internal: ARPAbet -> IPA, stress-insensitive
arpabet_ipa <- c(
  AA = "\u0251",       AE = "\u00E6",       AH = "\u028C",
  AO = "\u0254",       AW = "a\u028A",      AY = "a\u026A",
  B  = "b",            CH = "t\u0283",      D  = "d",
  DH = "\u00F0",       EH = "\u025B",       ER = "\u025D",
  EY = "e\u026A",      F  = "f",            G  = "\u0261",
  HH = "h",            IH = "\u026A",       IY = "i",
  JH = "d\u0292",      K  = "k",            L  = "l",
  M  = "m",            N  = "n",            NG = "\u014B",
  OW = "o\u028A",      OY = "\u0254\u026A", P  = "p",
  R  = "\u0279",       S  = "s",            SH = "\u0283",
  T  = "t",            TH = "\u03B8",       UH = "\u028A",
  UW = "u",            V  = "v",            W  = "w",
  Y  = "j",            Z  = "z",            ZH = "\u0292"
)

# Internal: unstressed variants, used when stress_sensitive = TRUE
# AH0 -> schwa, ER0 -> r-coloured schwa
arpabet_ipa_stress <- c(
  AH0 = "\u0259",      ER0 = "\u025A"
)


#' Build a word -> IPA lookup table from CMUdict
#'
#' Parses a CMUdict file (`cmudict.dict`, from
#' \url{https://github.com/cmusphinx/cmudict}) and converts each ARPAbet
#' transcription to IPA. Alternate pronunciations -- the `word(2)`, `word(3)`
#' entries -- are dropped, keeping only the first pronunciation per word.
#'
#' Most of the time you want `cmu_dict()` instead, which handles downloading
#' and caching. Use this directly only if you have your own copy of the file.
#'
#' @param file Path to `cmudict.dict`.
#' @param stress_sensitive Logical. If TRUE, CMUdict's stress digits are used
#'   to distinguish unstressed vowels: `AH0` maps to schwa and `ER0` to
#'   r-coloured schwa, rather than collapsing with their stressed
#'   counterparts. Default FALSE.
#' @param quiet Logical. If TRUE, suppress the summary message. Default FALSE.
#'
#' @return A data.frame with columns `word` (lower-cased) and `ipa`.
#'
#' @export
build_cmu_ipa <- function(file, stress_sensitive = FALSE, quiet = FALSE) {

  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  lines <- lines[!grepl("^;;;", lines)]
  lines <- lines[nzchar(trimws(lines))]

  word <- sub("\\s.*$", "", lines)
  rest <- sub("^\\S+\\s+", "", lines)
  rest <- sub("\\s*#.*$", "", rest)          # strip trailing comments

  # drop alternate pronunciations: cat(2), cat(3), ...
  alt  <- grepl("\\(\\d+\\)$", word)
  word <- word[!alt]
  rest <- rest[!alt]

  phones <- strsplit(trimws(rest), "\\s+")

  unmapped <- character(0)

  ipa <- vapply(phones, function(arp) {
    if (stress_sensitive) {
      # try the stress-specific map first, then fall back to the plain one
      out  <- arpabet_ipa_stress[arp]
      need <- is.na(out)
      out[need] <- arpabet_ipa[gsub("[0-9]", "", arp[need])]
    } else {
      out <- arpabet_ipa[gsub("[0-9]", "", arp)]
    }
    if (any(is.na(out))) {
      unmapped <<- c(unmapped, arp[is.na(out)])
      out <- out[!is.na(out)]
    }
    paste(out, collapse = "")
  }, character(1))

  if (length(unmapped) > 0L) {
    warning("Unmapped ARPAbet symbols dropped: ",
            paste(sort(unique(unmapped)), collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    word = tolower(word),
    ipa  = ipa,
    stringsAsFactors = FALSE
  )

  # spot check
  if (!quiet) {
    message(sprintf("Built dictionary: %d words, %d empty transcriptions.",
                    nrow(out), sum(!nzchar(out$ipa))))
  }

  out
}


# ---------------------------------------------------------------------------
# Dictionary download and caching
# ---------------------------------------------------------------------------

# Internal: session-level cache so the dictionary is read from disk only once
.classir_cache <- new.env(parent = emptyenv())

# Internal: where the built dictionary lives on this machine
cmu_cache_path <- function(stress_sensitive = FALSE) {
  file.path(
    tools::R_user_dir("classir", which = "cache"),
    if (stress_sensitive) "cmu_ipa_stress.rds" else "cmu_ipa.rds"
  )
}


#' Get the CMU pronouncing dictionary, downloading it if needed
#'
#' Returns a word -> IPA lookup table. On first use the CMU pronouncing
#' dictionary is downloaded and converted, then cached on disk, so the download
#' happens only once per machine. Later calls read the cached copy, and
#' repeated calls within a session reuse the version already in memory.
#'
#' @param ask Logical. If TRUE (the default in interactive sessions), prompt
#'   before downloading. If FALSE, download without prompting.
#' @param refresh Logical. If TRUE, re-download and rebuild even if a cached
#'   copy exists. Default FALSE.
#' @param stress_sensitive Passed to `build_cmu_ipa()`. Cached separately from
#'   the stress-insensitive version.
#'
#' @return A data.frame with columns `word` and `ipa`.
#'
#' @export
cmu_dict <- function(ask = interactive(),
                     refresh = FALSE,
                     stress_sensitive = FALSE) {

  key  <- if (stress_sensitive) "stress" else "plain"
  path <- cmu_cache_path(stress_sensitive)

  # 1. already in memory?
  if (!refresh && !is.null(.classir_cache[[key]])) {
    return(.classir_cache[[key]])
  }

  # 2. already on disk?
  if (!refresh && file.exists(path)) {
    d <- readRDS(path)
    .classir_cache[[key]] <- d
    return(d)
  }

  # 3. needs downloading
  url <- "https://raw.githubusercontent.com/cmusphinx/cmudict/master/cmudict.dict"

  if (ask) {
    message("The CMU pronouncing dictionary hasn't been downloaded yet.")
    message("It's a few MB, downloaded once and cached at:")
    message("  ", path)
    ans <- utils::menu(c("Yes, download it now", "No, cancel"),
                       title = "Download it?")
    if (!identical(ans, 1L)) {
      stop("Cancelled. Supply your own dictionary via `dict`, or run ",
           "cmu_dict(ask = FALSE) to download without prompting.",
           call. = FALSE)
    }
  }

  tmp <- tempfile(fileext = ".dict")
  on.exit(unlink(tmp), add = TRUE)

  tryCatch(
    utils::download.file(url, tmp, mode = "wb", quiet = FALSE),
    error = function(e) {
      stop("Could not download the dictionary: ", conditionMessage(e),
           "\nCheck your connection, or download ", url,
           " manually and build it with build_cmu_ipa().", call. = FALSE)
    }
  )

  d <- build_cmu_ipa(tmp, stress_sensitive = stress_sensitive)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(d, path)
  message("Dictionary cached at: ", path)

  .classir_cache[[key]] <- d
  d
}


# ---------------------------------------------------------------------------
# Word -> IPA
# ---------------------------------------------------------------------------

#' Look up IPA transcriptions for spelled words
#'
#' English spelling does not map predictably onto pronunciation, so this is a
#' dictionary lookup rather than a derivation. Words absent from the
#' dictionary return `NA`.
#'
#' @param words Character vector of spelled words.
#' @param dict A data.frame with `word` and `ipa` columns. If `NULL` (default),
#'   `cmu_dict()` is used, which downloads and caches the CMU dictionary on
#'   first use.
#' @param quiet Logical. If TRUE, suppress the coverage message. Default FALSE.
#'
#' @return Character vector of IPA strings, the same length as `words`, with
#'   `NA` where the word was not found.
#'
#' @export
word_to_ipa <- function(words, dict = NULL, quiet = FALSE) {

  if (is.null(dict)) dict <- cmu_dict()

  dict <- as.data.frame(dict)
  if (!all(c("word", "ipa") %in% names(dict))) {
    stop("`dict` must have columns `word` and `ipa`.", call. = FALSE)
  }

  dup <- duplicated(dict$word)
  if (any(dup)) {
    warning(sum(dup), " duplicate word(s) in `dict`; keeping the first of each.",
            call. = FALSE)
    dict <- dict[!dup, , drop = FALSE]
  }

  key <- tolower(trimws(as.character(words)))
  out <- dict$ipa[match(key, dict$word)]

  n_miss <- sum(is.na(out))
  if (!quiet) {
    message(sprintf("Transcribed %d of %d words (%.1f%%).",
                    length(out) - n_miss, length(out),
                    100 * (length(out) - n_miss) / length(out)))
  }
  if (n_miss > 0L) {
    miss <- unique(words[is.na(out)])
    warning(n_miss, " word(s) not found, including: ",
            paste(utils::head(miss, 5), collapse = ", "),
            if (length(miss) > 5) ", ..." else "", call. = FALSE)
  }

  out
}


# ---------------------------------------------------------------------------
# Phoneme coding
# ---------------------------------------------------------------------------

#' English phoneme inventory (IPA)
#'
#' The 39 phonemes of the CMUdict English inventory, in the IPA forms produced
#' by `build_cmu_ipa()`. Pass this as `inventory` in `code_phonemes()` or
#' `transcribe_phonemes()` so that separately coded datasets end up with
#' identical columns.
#'
#' @export
english_phonemes <- c(
  # consonants
  "b", "t\u0283", "d", "\u00F0", "f", "\u0261", "h", "d\u0292", "k", "l",
  "m", "n", "\u014B", "p", "\u0279", "s", "\u0283", "t", "\u03B8", "v",
  "w", "j", "z", "\u0292",
  # monophthongs
  "\u0251", "\u00E6", "\u028C", "\u0254", "\u025B", "\u025D", "\u026A",
  "i", "\u028A", "u",
  # diphthongs
  "a\u028A", "a\u026A", "e\u026A", "o\u028A", "\u0254\u026A"
)


#' Code the presence or count of each phoneme
#'
#' Segments IPA strings (keeping affricates and diphthongs intact) and returns
#' one column per phoneme.
#'
#' @param ipa Character vector of IPA strings. `NA` entries produce a row of
#'   `NA`s.
#' @param inventory Optional character vector of phonemes to use as columns. If
#'   `NULL` (default), every segment observed in `ipa` is used -- convenient
#'   for a single dataset, but see `english_phonemes` when columns need to
#'   match across datasets.
#' @param counts Logical. If TRUE (default), each cell is the number of times
#'   the phoneme occurs. If FALSE, cells are 0/1 presence indicators.
#' @param prefix Prefix for the new column names. Default `"ph_"`.
#' @param ... Passed to `ipa_to_segments()`, e.g. `extra_multichar` or
#'   `keep_stress`.
#'
#' @return A data.frame with one row per element of `ipa` and one column per
#'   phoneme in `inventory`.
#'
#' @export
code_phonemes <- function(ipa,
                          inventory = NULL,
                          counts = TRUE,
                          prefix = "ph_",
                          ...) {

  ipa <- as.character(ipa)

  segs <- lapply(ipa, function(s) {
    if (is.na(s)) character(0) else ipa_to_segments(s, ...)
  })

  observed <- sort(unique(unlist(segs)))

  if (is.null(inventory)) {
    inventory <- observed
  } else {
    extra <- setdiff(observed, inventory)
    if (length(extra) > 0L) {
      warning("Segments observed but not in `inventory`, so not coded: ",
              paste(extra, collapse = ", "), call. = FALSE)
    }
  }

  if (length(inventory) == 0L) {
    stop("No phonemes to code -- `ipa` may be empty or all NA.", call. = FALSE)
  }

  mat <- matrix(
    0L,
    nrow = length(segs),
    ncol = length(inventory),
    dimnames = list(NULL, paste0(prefix, inventory))
  )

  for (i in seq_along(segs)) {
    if (length(segs[[i]]) == 0L) next
    tab <- table(factor(segs[[i]], levels = inventory))
    mat[i, ] <- if (counts) as.integer(tab) else as.integer(tab > 0L)
  }

  if (any(is.na(ipa))) mat[is.na(ipa), ] <- NA_integer_

  as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# Wrapper
# ---------------------------------------------------------------------------

#' Transcribe words to IPA, optionally coding phonemes
#'
#' @description
#' Looks each word up in a pronunciation dictionary and adds an IPA
#' transcription column. Optionally also adds one column per phoneme.
#'
#' `add_columns = TRUE` adds a column for each phoneme, holding either counts or
#' presence, set by `counts`. The default, `counts = TRUE`, records how many
#' times each phoneme occurs in the word; `counts = FALSE` records only whether
#' it occurs at all, as 0 or 1.
#'
#' For most English word lists the two give nearly the same thing, because a
#' phoneme seldom occurs more than once in a short word. Presence is then the
#' simpler choice: a coefficient reads directly as the difference associated
#' with a word containing that sound. Counts are worth using when repetition is
#' itself of interest, or when the words are long enough for repeats to be
#' common. One mechanical consequence is worth knowing either way -- the counts
#' across a row sum to the number of phonemes in the word, so count columns
#' carry word length with them in a way presence columns largely do not, which
#' matters if length is also a predictor in the model.
#'
#' The dictionary can be built with `stress_sensitive = TRUE`, which
#' distinguishes unstressed vowels -- schwa from its stressed counterpart --
#' rather than collapsing them. A different dictionary can be supplied
#' entirely, provided it has word and IPA columns.
#'
#' The pieces are also available separately: `word_to_ipa()` for transcription
#' alone, `code_phonemes()` for coding IPA strings that came from somewhere
#' else, and `cmu_dict()` for the dictionary itself.
#'
#' @param d A data.frame/data.table, or a plain character vector of words.
#' @param word_col Character: name of the word column in `d`. If `d` is a
#'   character vector, this is used as the name of the resulting column.
#'   Default `"word"`.
#' @param dict Pronunciation dictionary; see `word_to_ipa()`. If `NULL`
#'   (default), `cmu_dict()` downloads and caches the CMU dictionary on first
#'   use.
#' @param add_columns Logical. If FALSE (default), only the transcription
#'   column is added. If TRUE, a column is also added for each phoneme,
#'   containing the number of times it occurs in the word.
#' @param counts Logical. Only used when `add_columns = TRUE`. If TRUE
#'   (default), phoneme columns hold counts; if FALSE, 0/1 presence.
#' @param inventory Character vector of phonemes to use as columns. Defaults to
#'   `english_phonemes`, the full CMUdict inventory, so that every phoneme gets
#'   a column whether or not it occurs in these particular words. Pass `NULL`
#'   to use only the segments actually observed.
#' @param ipa_col Name of the transcription column to add. Default `"ipa"`.
#' @param prefix Prefix for the phoneme column names. Default `"ph_"`.
#' @param quiet Logical. If TRUE, suppress the coverage message. Default FALSE.
#' @param ... Passed to `ipa_to_segments()` via `code_phonemes()`.
#'
#' @return `d` with a transcription column added, plus one column per phoneme
#'   if `add_columns = TRUE`. Row order is preserved. A data.table in, a
#'   data.table out; otherwise a data.frame.
#'
#' @examples
#' \dontrun{
#' # transcription only
#' transcribe_phonemes(c("cat", "judge", "boat"))
#'
#' # with phoneme counts
#' transcribe_phonemes(c("cat", "judge", "boat"), add_columns = TRUE)
#'
#' # on an existing data frame, with columns that match across datasets
#' stimuli <- transcribe_phonemes(
#'   stimuli,
#'   word_col    = "Word",
#'   add_columns = TRUE,
#'   inventory   = english_phonemes
#' )
#' }
#'
#' @export
transcribe_phonemes <- function(d,
                                word_col = "word",
                                dict = NULL,
                                add_columns = FALSE,
                                counts = TRUE,
                                inventory = english_phonemes,
                                ipa_col = "ipa",
                                prefix = "ph_",
                                quiet = FALSE,
                                ...) {

  # d can be a character vector or a data frame
  if (is.character(d) && is.null(dim(d))) {
    was_dt <- FALSE
    d <- data.frame(x = d, stringsAsFactors = FALSE)
    names(d) <- word_col
  } else {
    was_dt <- inherits(d, "data.table")
    d <- as.data.frame(d)
  }

  if (!is.character(word_col) || length(word_col) != 1L) {
    stop("`word_col` must be a single character string.", call. = FALSE)
  }
  if (!word_col %in% names(d)) {
    stop("`word_col` '", word_col, "' not found in `d`.", call. = FALSE)
  }
  if (ipa_col %in% names(d)) {
    stop("`d` already has a column called '", ipa_col,
         "'. Rename it, or set `ipa_col` to something else.", call. = FALSE)
  }

  ipa <- word_to_ipa(d[[word_col]], dict = dict, quiet = quiet)

  out <- d
  out[[ipa_col]] <- ipa

  if (add_columns) {
    ph <- code_phonemes(
      ipa,
      inventory = inventory,
      counts    = counts,
      prefix    = prefix,
      ...
    )

    clash <- intersect(names(ph), names(out))
    if (length(clash) > 0L) {
      stop("These phoneme columns already exist in `d`: ",
           paste(clash, collapse = ", "),
           ". Rename them, or use a different `prefix`.", call. = FALSE)
    }

    out <- cbind(out, ph)
  }

  rownames(out) <- NULL

  if (was_dt) data.table::setDT(out)

  out
}
