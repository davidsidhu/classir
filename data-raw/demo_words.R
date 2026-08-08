# Simulates the `demo_words` dataset used in the README and examples.
# Run this once, then usethis::use_data(demo_words, overwrite = TRUE).
#
# Nothing here is real. The words are invented and the values are generated
# to have roughly the shape and correlations of lexical norms, so that the
# examples show realistic-looking output.

set.seed(2026)

n <- 500

# --- invented words --------------------------------------------------------
onsets <- c("b", "d", "f", "g", "h", "k", "l", "m", "n", "p", "r", "s", "t",
            "v", "w", "z", "br", "cl", "dr", "fl", "gr", "pl", "sn", "st",
            "sw", "tr")
nuclei <- c("a", "e", "i", "o", "u", "ai", "ee", "oa", "ou", "ea")
codas  <- c("", "b", "d", "g", "k", "l", "m", "n", "p", "r", "s", "t",
            "ng", "nk", "sh", "ch", "st", "lt", "nd")

make_word <- function() {
  k <- sample(1:2, 1, prob = c(0.65, 0.35))          # one or two syllables
  paste0(replicate(k, paste0(sample(onsets, 1),
                             sample(nuclei, 1),
                             sample(codas, 1))),
         collapse = "")
}

word <- character(0)
while (length(word) < n) {
  word <- unique(c(word, replicate(n, make_word())))
}
word <- sort(word[seq_len(n)])

# --- lexical characteristics ----------------------------------------------
length_ <- nchar(word)

# frequency: heavily right-skewed, negatively related to length
z <- function(x) as.numeric(scale(x))

freq <- round(exp(rnorm(n, mean = 2.2 - 0.12 * z(length_), sd = 1.6)), 2)

# orthographic neighbourhood: shorter words have more neighbours
old20 <- round(pmax(1, 1.2 + 0.28 * length_ + rnorm(n, 0, 0.45)), 2)

aoa <- round(pmin(18, pmax(2,
        9.5 - 0.9 * z(log(freq + 1)) + 0.25 * z(length_) +
        rnorm(n, 0, 2.2))), 2)

concreteness <- round(pmin(5, pmax(1, 3.2 + rnorm(n, 0, 0.95))), 2)

pos <- sample(c("noun", "verb", "adjective"), n, replace = TRUE,
              prob = c(0.5, 0.3, 0.2))

sound_symbolic <- rbinom(n, 1, 0.12)

# --- outcomes --------------------------------------------------------------
# iconicity: higher for shorter, later-acquired, sound-symbolic words
iconicity <- round(
  3.1 -
  0.35 * z(log(freq + 1)) +
  0.20 * z(aoa) +
  0.18 * z(concreteness) +
  0.85 * sound_symbolic +
  rnorm(n, 0, 0.85), 2)

# lexical decision RT: the usual frequency, length and neighbourhood effects
rt <- round(
  650 -
  38 * z(log(freq + 1)) +
  14 * z(length_) +
  11 * z(old20) +
  rnorm(n, 0, 45), 1)

demo_words <- data.frame(
  word         = word,
  length       = length_,
  freq         = freq,
  old20        = old20,
  aoa          = aoa,
  concreteness = concreteness,
  pos          = pos,
  sound_symbolic = sound_symbolic,
  iconicity    = iconicity,
  rt           = rt,
  stringsAsFactors = FALSE
)

# --- a little mess, so check_data() has something to find ------------------
demo_words$aoa[sample(n, 40)] <- NA              # ordinary missingness
demo_words$concreteness[sample(n, 12)] <- NA

demo_words$pos[sample(n, 3)] <- "Noun"           # case mismatch
demo_words$rt[sample(n, 2)] <- -999              # sentinel value

# spot check -- print() so these show when the file is source()d
print(str(demo_words))
print(colSums(is.na(demo_words)))
print(utils::head(demo_words, 3))

# Run this at the console AFTER sourcing, not from inside the file:
#   usethis::use_data(demo_words, overwrite = TRUE)
