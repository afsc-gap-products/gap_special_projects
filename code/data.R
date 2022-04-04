#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------


# Downoad Google drive Spreadsheets -------------------------------------
if (access_to_internet) {
  googledrive::drive_download(file = googledrive::as_id(dir_gspecial),
                              type = "csv",
                              overwrite = TRUE,
                              path = paste0("./data/special"))
  
  googledrive::drive_download(file = googledrive::as_id(dir_gcore),
                              type = "xlsx",
                              overwrite = TRUE,
                              path = paste0("./data/core"))
}

# Load Data -------------------------------------------------------------

all_na <- function(x) any(!is.na(x))

special0 <- readr::read_csv(file = paste0(here::here("data", "special.csv"))) %>% 
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

# Wrangle special project data -----------------------------------------------------------------

special <- edit_data(data0 = special0) %>% 
  dplyr::mutate(vessel = toupper(vessel))

if (subset_to_accepted_projects){
special <- special %>% 
  dplyr::mutate(project_accepted_t_f = ifelse(is.na(project_accepted_t_f), TRUE, FALSE)) %>%
  dplyr::filter(project_accepted_t_f == TRUE)
}

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
  names(special)[names(special) == "temp"] <- paste0("srvy_", s$srvy[s$survey == comb[i]])
}

