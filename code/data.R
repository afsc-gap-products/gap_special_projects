#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------

# Run example ------------------------------------------------------------------

data0_ex <- readr::read_csv(file = paste0(here::here("example", "dat.csv"))) %>%
  janitor::clean_names(.)
if (names(data0_ex)[1] %in% "x1"){
  data0_ex$x1<-NULL
}
data1 <- edit_data(data0 = data0_ex)

for (i in 1:nrow(data1)) {
  
  dat <- data1[i,]
  
  file_name <- paste0(maxyr, "-", dat$last_name, "-", 
                      janitor::make_clean_names(dat$title), ".docx")
  
  rmarkdown::render(paste0("./code/template.Rmd"),
                    output_dir = here::here("example"),
                    output_file = file_name)
  
  temp <- strsplit(x = dat$survey, split = ", ")[[1]]
  
  for (ii in 1:length(temp)) {
    
    dir.create(paste0(dir_out, temp[ii]), showWarnings = F)
    
    file.copy(from = paste0(dir_out, file_name), 
              to = paste0(dir_out, temp[ii], "/", file_name), 
              overwrite = TRUE)
    
  }
  
}


# Downoad Google drive Spreadsheets -------------------------------------
if (access_to_internet) {
  googledrive::drive_download(file = googledrive::as_id(dir_gdrive),
                              type = "csv",
                              overwrite = TRUE,
                              path = paste0("./data/dat"))
}

# Load Data -------------------------------------------------------------

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

# Wrangle data -----------------------------------------------------------------

data1 <- edit_data(data0 = data0) %>% 
  dplyr::mutate(
    
    # TOLEDO - fake for poster
    title_short = title,  
    notes = "[no notes]", # TOLEDO - fake for poster
    vessel = rep_len(x = c("a", "b", "a, b"), length.out = nrow(.)), # TOLEDO - fake vessel assignments 
    desc = substr(x = paste0(detailed_collection_procedures, "\n", 
                             gsub(pattern = ", ", replacement = "\n", x = species_name)), 
                  start = 1, stop = 1000), # TOLEDO - fake short description) %>% 
    
    # real
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

comb <- unique(strsplit(x = paste(data1$vessel, collapse = ", "), split = ", ", fixed = TRUE)[[1]])
for (i in 1:length(comb)) {
  data1 <- data1 %>% 
    dplyr::mutate(temp = unlist(lapply(X = vessel, grepl, pattern = comb[i]))) 
  names(data1)[names(data1) == "temp"] <- paste0("vess_", comb[i])
}

s <- data.frame(srvy = c("NBS", "EBS", "GOA", "AI", "BSSlope"), 
                survey = c("Northern Bering Sea", 
                           "Eastern Bering Sea Shelf", 
                           "Gulf of Alaska", 
                           "Aleutian Islands", 
                           "Bering Sea Slope") )

comb <- unique(strsplit(x = paste(data1$survey, collapse = ", "), split = ", ", fixed = TRUE)[[1]])
for (i in 1:length(comb)) {
  data1 <- data1 %>% 
    dplyr::mutate(temp = unlist(lapply(X = survey, grepl, pattern = comb[i]))) 
  names(data1)[names(data1) == "temp"] <- paste0("srvy_", s$srvy[s$survey == comb[i]])
}

