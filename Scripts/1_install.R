# Install INLA from their repo (not on CRAN)
install.packages("INLA",
                 repos=c(getOption("repos"),
                         INLA="https://inla.r-inla-download.org/R/stable"), 
                 dep=TRUE) 

install.packages("remotes")
remotes::install_github("inlabru-org/inlabru", ref = "stable")
install.packages("fmesher") # this should be installed with inlabru but just in case

# additional packages to be able to run the code
install.packages("tidyverse")
install.packages("terra")
install.packages("tidyterra")
install.packages("patchwork")
install.packages("sf")