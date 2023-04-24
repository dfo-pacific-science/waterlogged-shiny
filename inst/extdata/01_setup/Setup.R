#----------------------------------------------------
# Build and Tests
#----------------------------------------------------
library(testthat)
library(devtools)
library(usethis)

# Loading unfinished package to memory...
rm(list = ls())
devtools::load_all()
devtools::document()
# devtools::check() # Operating system test

devtools::test()  # Run tests

devtools::check() # Operating system test



# remove.packages("FDLToolbox")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::installInteraction_github("essatech/FDLToolbox")


library(goodpractice)
goodpractice::gp()
# library(inteRgrate)
# inteRgrate::check_pkg()

# use_vignette("run-fdl-calculator-from-r", "Run FDL Calculator from R")



# Build Manual
# devtools::build_manual(pkg = ".", path = "../")

# Installing unfinsihed package to computer...
# remove.packages("JoeModelCE")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::installInteraction_github("essatech/JoeModelCE")
# library(JoeModelCE)
# ?SystemCapacity

# Check on cran against LTR version of R
# devtools::check_win_release()



#----------------------------------------------------
# More Serious Tests
#----------------------------------------------------
# Generate cran-comments.md file
# results <- rhub::check_for_cran()
# Get the summary of your results
# results$cran_summary()

# Check for good practices
library(goodpractice)
goodpractice::gp()

# Build Manual
# devtools::build_manual(pkg = ".", path = "../")

library(inteRgrate)
inteRgrate::check_pkg()



#----------------------------------------------------
# pkgdown Creation
#----------------------------------------------------
# https://pkgdown.r-lib.org/
# install.packages("pkgdown")
library(pkgdown)
# usethis::use_pkgdown()
# pkgdown::build_site()
# usethis::use_pkgdown_github_pages()


# use_vignette("setup-and-installation", "Setup and Installation")
# use_mit_license(copyright_holder = NULL)


