#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------

# Load Data ----------------------------------------------------

# Google drive Spreadsheets
# https://drive.google.com/drive/folders/1Vbe_mH5tlnE6eheuiSVAFEnsTJvdQGD_?usp=sharing
# a <- googledrive::drive_ls(path = googledrive::as_id("1Vbe_mH5tlnE6eheuiSVAFEnsTJvdQGD_"), type = "spreadsheet")
# for (i in 1:nrow(a)){
#   googledrive::drive_download(file = googledrive::as_id(a$id[i]), 
#                               type = "xlsx", 
#                               overwrite = TRUE, 
#                               path = paste0(dir_out_rawdata, "/", a$name[i]))
# }


## Local Data -------------------------------------------------------------
a<-list.files(path = here::here("data"))
a<-a[a != "empty.txt"]
for (i in 1:length(a)){
  print(a[i])
  b <- readr::read_csv(file = paste0(here::here("data", a[i]))) %>% 
    janitor::clean_names(.)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  # assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0("data0")), value = b)
}

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
  