install.packages("devtools")
devtools::install_github("hadley/devtools")
devtools::install_github("klutometis/roxygen")
install.packages("testthat")
install.packages("rlang")
install.packages("roxygen2")
library(devtools)
library(roxygen2)
library(testthat)
library(rlang)
install.packages("RCurl")
library(RCurl)

install.packages("installr")
installr::updateR()

install.packages("git2r")
install.packages("glue")
install.packages("usethis")
install.packages("stringi")
library(git2r)
library(glue)
library(usethis)
library(stringi)


installed.packages()

install.packages("rtools")

setwd("C:/Users/Roth/Documents/PSC-MB")
?create_description

devtools::install_version("roxygen2", version = "6.0.1")

create("C:/Users/Roth/Documents/PSCrepfx", description = getOption("devtools.desc"), check = FALSE,
       rstudio = TRUE, quiet = FALSE)

devtools::load_all()
devtools::document()
?file

setwd("~/PSC-MB")

devtools::load_all()
roxygen2::roxygenise()
roxygenise(clean=TRUE)

devtools::use_vignette()
update.packages()
remove.packages(setdiff(dir(.libPaths()), .packages(all = TRUE)))

version
##aap <- available.packages()
setdiff(dir(.libPaths()), .packages(all = TRUE))



warnings()

search()


library(tidyverse)

install.packages("data.table")
library(knitr)
library(yaml)

library(openxlsx)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
install.packages("tidyverse")

library(lubridate)

install.packages(c("rlang", "devtools", "roxygen2", "testthat", "knitr"))
devtools::install_github("hadley/devtools")
devtools::install_github("klutometis/roxygen")

install.packages("stringi")
library(stringi)
install.packages("rlang")


install.packages("roxygen2")


install.packages("yaml")

install.packages("Rtools")

install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")

sessionInfo()
