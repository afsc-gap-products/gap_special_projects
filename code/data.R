#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------


# Downoad Google drive Spreadsheets -------------------------------------
if (access_to_internet) {
  googledrive::drive_download(file = googledrive::as_id(dir_gspecial),
                              type = "xlsx",
                              overwrite = TRUE,
                              path = paste0("./data/special"))
  
  googledrive::drive_download(file = googledrive::as_id(dir_gcore),
                              type = "xlsx",
                              overwrite = TRUE,
                              path = paste0("./data/core"))
}

# Load Data --------------------------------------------------------------------

all_na <- function(x) any(!is.na(x))

special0 <- xlsx::read.xlsx(file = paste0(here::here("data", "special.xlsx")), 
                            sheetName = "Copy of ALL SURVEYS") %>% 
  janitor::clean_names()

stomachs0 <- xlsx::read.xlsx(file = paste0(here::here("data", "core.xlsx")), 
                            sheetName = "Stomachs", startRow = 2) %>% 
  dplyr::select_if(all_na) %>% 
  dplyr::filter(year == maxyr)

otoliths0 <- xlsx::read.xlsx(file = paste0(here::here("data", "core.xlsx")), 
                            sheetName = "Otoliths", startRow = 2) %>% 
  dplyr::select_if(all_na) %>% 
  dplyr::filter(year == maxyr)

crab0 <- xlsx::read.xlsx(file = paste0(here::here("data", "core.xlsx")), 
                        sheetName = "Crab", startRow = 2) %>% 
  dplyr::select_if(all_na) %>% 
  dplyr::filter(year == maxyr)

# Wrangle special project data -------------------------------------------------

special <- special0 %>% 
    dplyr::filter(!is.na(email_address)) %>% 
  dplyr::mutate(affiliation = gsub(pattern = " - ",
                                   replacement = "-",
                                   x = as.character(affiliation),
                                   fixed = TRUE),
                affiliation = gsub(pattern = "-",
                                   replacement = " - ",
                                   x = affiliation,
                                   fixed = TRUE),
                survey = gsub(pattern = ";", replacement = ", ", x = survey)) %>%
  dplyr::rename(first_name = requester_s_first_name,
                last_name = requester_s_last_name) %>% 
  # tidyr::separate(data = ., col = requester_name, into = c("first_name", "last_name"),
  #                 sep = " ", remove = FALSE) %>% 
  dplyr::mutate( # TOLEDO, need to double check!
    requestor_name = paste0(first_name, " ", last_name), 
    target = dplyr::case_when(
      is.na(minimum_number_of_specimens) & is.na(maximum_number_of_specimens) ~
        "No maximum or minimum quantity. ",
      is.na(minimum_number_of_specimens) & !is.na(maximum_number_of_specimens) ~ 
        paste0("Maximum specimen quantity: ", maximum_number_of_specimens), 
      is.na(maximum_number_of_specimens) & !is.na(minimum_number_of_specimens) ~ 
        paste0("Minimum specimen quantity: ", minimum_number_of_specimens), 
      # is.character(minimum_number_of_specimens) ~ minimum_number_of_specimens, 
      # is.numeric(minimum_number_of_specimens) & is.numeric(maximum_number_of_specimens) ~ 
      #   paste0(minimum_number_of_specimens," - ",maximum_number_of_specimens), 
      TRUE ~ paste0(minimum_number_of_specimens," - ",maximum_number_of_specimens)
    ), 
    dplyr::across(where(is.character),
                  gsub, pattern = " , ", replace = ", "),
    dplyr::across(where(is.character), 
                  gsub, pattern = " ; ", replace = "; "), 
    dplyr::across(where(is.character), 
                  gsub, pattern = "none", replace = "[None]"), 
    dplyr::across(where(is.character), 
                  ~tidyr::replace_na(data = ., replace = "[None]"))) %>%
  dplyr::rename(numeric_priority = order_of_importance, 
                short_procedures = short_on_deck_collection_instructions_675_character_limit) %>% 
  dplyr::mutate(vessel = toupper(vessel), 
                numeric_priority = as.numeric(numeric_priority), 
                numeric_priority = ifelse(is.na(numeric_priority),
                                          (max(numeric_priority, na.rm = TRUE)+1),
                                          numeric_priority)) 

# if (subset_to_accepted_projects){
# special <- special %>% 
#   dplyr::mutate(!is.na(numeric_priority))
#   # dplyr::mutate(project_accepted_t_f = ifelse(is.na(project_accepted_t_f), TRUE, FALSE)) %>%
#   # dplyr::filter(project_accepted_t_f == TRUE)
# }

vess <- unique(special$vessel)[!grepl(pattern = ",", x = unique(special$vessel)) & 
                               (unique(special$vessel) != "[NONE]")]
for (i in 1:length(vess)) {
  special$temp <- ifelse(grepl(pattern = vess[i], x = special$vessel, fixed = TRUE), 
                       TRUE, FALSE)
  names(special)[names(special) == "temp"] <- paste0("vess_", janitor::make_clean_names(vess[i]))
}

special <- special %>% 
  dplyr::mutate(
    sap_gap = dplyr::case_when(
      grepl(pattern = "Aleutian Islands", x = survey) == TRUE ~ "gap", 
      grepl(pattern = "Gulf of Alaska", x = survey) == TRUE ~ "gap", 
      animal_type == "crab" ~ "sap", 
      TRUE ~ "gap"),     
    preserve_freeze = 
      ifelse(unlist(lapply(X = specimen_preservation_method, grepl, 
                           pattern = "freeze")), "freeze", NA), 
    preserve_etoh = 
      ifelse(unlist(lapply(X = specimen_preservation_method, grepl, 
                           pattern = "95% Ethanol")), "EtOH", NA),
    # preserve_glycerol = 
    #   ifelse(unlist(lapply(X = specimen_preservation_method, grepl, 
    #                        pattern = "glycerol or thymol")), "glycerol/thymol", NA),
    preserve_live = 
      ifelse(unlist(lapply(X = specimen_preservation_method, grepl, 
                           pattern = "live")), "live", NA)) %>%  
  tidyr::unite(data = ., col = "preserve", 
               remove = FALSE, 
               na.rm = TRUE, 
               dplyr::starts_with("preserve_"), 
               sep = " or ") %>% 
  dplyr::mutate(preserve = ifelse(is.na(preserve), "", preserve))

# comb <- unique(strsplit(x = paste(special$vessel, collapse = ", "), split = ", ", fixed = TRUE)[[1]])
# for (i in 1:length(comb)) {
#   special <- special %>% 
#     dplyr::mutate(temp = unlist(lapply(X = vessel, grepl, pattern = comb[i]))) 
#   names(special)[names(special) == "temp"] <- paste0("vess_", comb[i])
# }

s <- data.frame(srvy = c("NBS", "EBS", "GOA", "AI", "BSSlope"), 
                survey = c("Northern Bering Sea", 
                           "Eastern Bering Sea Shelf", 
                           "Gulf of Alaska", 
                           "Aleutian Islands", 
                           "Bering Sea Slope") )

comb <- unique(strsplit(x = paste(special$survey, collapse = ", "), split = ", ", fixed = TRUE)[[1]])
for (i in 1:length(comb)) {
  special <- special %>% 
    dplyr::mutate(temp = unlist(lapply(X = survey, grepl, pattern = comb[i]))) 
  names(special)[names(special) == "temp"] <- paste0("srvy_", s$srvy[s$srvy == comb[i]])
}

