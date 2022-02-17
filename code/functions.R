

# INSTALL PACKAGES -------------------------------------
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # other tidyverse
  "tidyr",
  "dplyr",
  "magrittr",
  "readr",
  "janitor", 
  "googledrive", 
  
  # Text Management
  "stringr")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}


# Other user functions -------------------------------------
