############################################################
## classir workflow cheat sheet
## Save this file in the package root (with DESCRIPTION).
############################################################

## Install these once (if not already done) ----------------
## install.packages(c("devtools", "usethis", "roxygen2", "leaps"))

library(devtools)
library(usethis)

############################################################
## 1. Adding a new function
############################################################

## Step 1: Create a new R file (only once per function)
## This opens R/<name>.R where you paste your function + roxygen.
# usethis::use_r("my_new_function")

## Step 2: In that new file, write something like:
##
## #' Short description of the function
## #'
## #' Longer description, what it does, etc.
## #'
## #' @param x Description of x
## #' @return What it returns
## #' @examples
## #' my_new_function(1)
## #'
## #' @export
## my_new_function <- function(x) {
##   x + 1
## }

############################################################
## 2. Update docs & namespace after changing functions
############################################################

## Run this whenever you:
##  - add a new function
##  - change roxygen comments
##  - add/remove @export, @import, etc.
devtools::document()

############################################################
## 3. Install / reload the package
############################################################

## Install the package into your library (so library(classir) works)
devtools::install()

## In a fresh R session, load it like any other package:
# library(classir)

############################################################
## 4. Quick helper: rebuild + reload in one go
############################################################

reload_classir <- function() {
  devtools::document()
  devtools::install()
  library(classir)
}

## Then in any session (from the package project):
## reload_classir()

############################################################
## 5. Best-subsets helper (your function reminder)
############################################################

## Example usage of best_subsets_boot (assuming it's in the package):

## d_a <- d[d$Gender == "Female", c(5, 11:44)]
##
## res <- best_subsets_boot(
##   data    = d_a,
##   outcome = "Agreeableness",
##   B       = 300,
##   seed    = 123
## )
##
## res$formula                    # see chosen model
## summary(res$model)             # regression summary
## sort(res$sel_freq, TRUE)       # most stable predictors


############################################################
## 6. Managing dependencies
############################################################

## If you use a new external package in your functions, declare it:
## (Run once per dependency)
# usethis::use_package("leaps")   # already done for regsubsets
# usethis::use_package("dplyr")

############################################################
## 7. Optional: tests & README
############################################################

## Set up testthat (once):
# usethis::use_testthat()
# usethis::use_test("best_subsets_boot")  # creates a test file

## Create a README.Rmd (once):
# usethis::use_readme_rmd()

############################################################
## End of cheat sheet
############################################################
