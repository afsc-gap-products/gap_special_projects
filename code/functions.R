

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

edit_data <- function(data0) {
  data1 <- data0 %>%
    dplyr::mutate(affiliation = gsub(pattern = " - ",
                                     replacement = "-",
                                     x = as.character(affiliation),
                                     fixed = TRUE),
                  affiliation = gsub(pattern = "-",
                                     replacement = " - ",
                                     x = affiliation,
                                     fixed = TRUE),
                  survey = gsub(pattern = ";", replacement = ", ", x = survey)) %>%
    tidyr::separate(data = ., col = requester_name, into = c("first_name", "last_name"),
                    sep = " ", remove = FALSE) %>% 
    dplyr::mutate( # TOLEDO, need to double check!
      target = dplyr::case_when(
        is.na(minimum_number_of_specimens) & is.na(maximum_number_of_specimens) ~
          "No maximum or minimum quantity. ",
        is.na(minimum_number_of_specimens) & !is.na(maximum_number_of_specimens) ~ 
          paste0("Maximum speciment quantity: ", maximum_number_of_specimens), 
        is.na(maximum_number_of_specimens) & !is.na(minimum_number_of_specimens) ~ 
          paste0("Minimum speciment quantity: ", minimum_number_of_specimens), 
        # is.character(minimum_number_of_specimens) ~ minimum_number_of_specimens, 
        # is.numeric(minimum_number_of_specimens) & is.numeric(maximum_number_of_specimens) ~ 
        #   paste0(minimum_number_of_specimens," - ",maximum_number_of_specimens), 
        TRUE ~ paste0(minimum_number_of_specimens," - ",maximum_number_of_specimens)
      ), 
      across(where(is.character), 
             gsub, pattern = " , ", replace = ", "), 
      across(where(is.character), 
             gsub, pattern = " ; ", replace = "; "), 
      across(where(is.character), 
             gsub, pattern = "none", replace = "[None]"), 
      across(where(is.character), 
             ~tidyr::replace_na(data = ., replace = "[None]")))
  # dplyr::mutate(across(is.character(.), ~tidyr::replace_na(., "None. ")))
  
  return(data1)
  
}
