#' ---------------------------------------------
#' title: Create special project sheets
#' author: L Dawson (Liz.Dawson AT noaa.gov) and EH Markowitz (emily.markowitz AT noaa.gov)
#' start date: 2022-02-16
#' last modified: 2022-03-01
#' Notes: 
#' ---------------------------------------------

# START ------------------------------------------------------------------------

# source("./code/run.R")
# 1

# *** REPORT KNOWNS ------------------------------------------------------------

maxyr <- 2022
dir_out <- paste0("./output/", Sys.Date(),"/")
dir.create(dir_out, showWarnings = F)

subset_to_accepted_projects <- FALSE

access_to_internet <- TRUE
googledrive::drive_auth()
1
dir_gspecial <- "https://docs.google.com/spreadsheets/d/1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k/edit?usp=sharing"

# 2022 CORE COLLECTIONS REQUESTS:  Otoliths, Crab, Food Habits
# dir_gcore <- "1W8bKigNipl5IdQIQH5Kyq4twJxZKLi1zeS9UNE_JAsU" # https://docs.google.com/spreadsheets/d/1W8bKigNipl5IdQIQH5Kyq4twJxZKLi1zeS9UNE_JAsU/edit?usp=sharing

dir_gcore <- "https://docs.google.com/spreadsheets/d/1WHyetA20twlq6uhp5VR-sHtm2VR1uecOcaLOmztK9Zs/edit?usp=sharing"

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

source('./code/functions.R')
source('./code/ex.R')
source('./code/data.R')


# Run RMarkdowns to create word docs from google spreadsheet --------------------

for (i in 1:nrow(data1)) {
  
  dat <- special[i,]
  
  file_name <- paste0(maxyr, "-", dat$last_name, "-", 
                      janitor::make_clean_names(dat$title), ".docx")
  
  rmarkdown::render(paste0("./code/template.Rmd"),
                    output_dir = dir_out,
                    output_file = paste0(dir_out, file_name))
  
  temp <- strsplit(x = dat$survey, split = ", ")[[1]]
  
  for (ii in 1:length(temp)) {
    
    dir.create(paste0(dir_out, temp[ii]), showWarnings = F)
    
    file.copy(from = paste0(dir_out, file_name), 
              to = paste0(dir_out, temp[ii], "/", file_name), 
              overwrite = TRUE)
    
  }
  
}

# move all loose files to the "all" folder
a <- list.files(path = dir_out, pattern = ".docx", full.names = TRUE)
dir.create(path = paste0(dir_out, "/all/"))
file.copy(from = a, to = paste0(dir_out, "/all/"))
file.remove(a)

# Special projects posters by vessel and survey ------------------------------------

comb <- tidyr::crossing( # the different combination of posters we will need to make
  srvy = special %>%
    dplyr::select(dplyr::starts_with("srvy_")) %>% 
    names(), #  %>% gsub(pattern = "srvy_", replacement = "", x = .)
  vess = special %>%
    dplyr::select(dplyr::starts_with("vess_")) %>% 
    names(), 
  sap_gap = unique(special$sap_gap)) #  %>% gsub(pattern = "vess_", replacement = "", x = .)


path0 <-paste0(dir_out, "posters_special/")
dir.create(path = path0)

for (i in 1:nrow(comb)) {
  
  # subset the data
  dat0 <- special[(special[,comb$srvy[i]] == TRUE & 
                     special[,comb$vess[i]] == TRUE & 
                     special$sap_gap == comb$sap_gap[i]), ] %>% 
    dplyr::arrange(desc(numeric_priority)) %>% 
    dplyr::select(short_name, last_name, preserve, short_procedures) 
  
  if (nrow(dat0) != 0) {
    
    file_name0 <-paste0(comb$srvy[i],"-",comb$vess[i],"-",comb$sap_gap[i])
    
    title <- paste0(
      maxyr, " ",  
      gsub(pattern = "srvy_", replacement = "", x = comb$srvy[i]), 
      " F/V ", 
      gsub(pattern = "Ak", replacement = "AK", x = stringr::str_to_title(
        gsub(pattern = "_", replacement = " ", 
             gsub(pattern = "vess_", replacement = "", x = comb$vess[i])))), " ",
      toupper(comb$sap_gap[i]), " ", #"\n",
      "Special Collections Tally Sheet")
    
    # PDF
    flextable::save_as_image(x = poster_special(dat0 = dat0,
                                                textsize = 10,
                                                title = title,
                                                spacing = 1.5,
                                                pad = 10),
                             path = paste0(path0, file_name0,".pdf"),
                             zoom = 1, expand = 1)
    
    # PDF
    # ft <- poster_special(dat0 = dat0, 
    #            textsize = 10,
    #            pad = 5,
    #            spacing = 1.4,
    #            title = title)
    # 
    # rmarkdown::render(paste0("./code/template_pdf.Rmd"),
    #                   output_dir = path0,
    #                   output_file = paste0(file_name0,".pdf"))
    
    # PPTX
    ft <- poster_special(dat0 = dat0, 
                         textsize = 40,
                         pad = 10,
                         spacing = 1.4,
                         title = title)
    
    rmarkdown::render(paste0("./code/template_pptx.Rmd"),
                      output_dir = path0,
                      output_file = paste0(file_name0,".pptx"))
    
  }
}


# Otolith projects posters by survey -------------------------------------------

comb <- otoliths0 %>% # the different combination of posters we will need to make
  dplyr::select(survey) %>% 
  dplyr::distinct()

path0 <-paste0(dir_out, "posters_otoliths/")
dir.create(path = path0)

for (i in 1:nrow(comb)) {
  
  file_name0 <- comb$survey[i]
  
  # subset the data
  dat0 <- otoliths0 %>% 
    dplyr::filter(survey == comb$survey[i] &
                    plan != "no collection this year") %>%
    dplyr::arrange(plan) %>% 
    dplyr::select(plan, species, species, n_per_haul, criteria) %>% 
    dplyr::mutate(plan = toupper(plan))
  
  # dat0$plan <- gsub(pattern = "random/haul", replacement = "random/\nhaul", x = dat0$plan)
  
  if (nrow(dat0) != 0) {
    
    title <- paste0(
      maxyr, " ",  
      gsub(pattern = "srvy_", replacement = "", x = comb$survey[i]), 
      " Survey Core Otoliths")
    
    # PDF
    flextable::save_as_image(x = poster_otolith(dat0 = dat0,
                                                textsize = 40,
                                                title = title,
                                                spacing = 1.5,
                                                pad = 10),
                             path = paste0(path0, file_name0,".pdf"),
                             zoom = 1, expand = 1)
    
    # PDF
    # ft <- poster_otolith(dat0 = dat0, 
    #            textsize = 10,
    #            pad = 5,
    #            spacing = 1.4,
    #            title = title)
    # 
    # rmarkdown::render(paste0("./code/template_pdf.Rmd"),
    #                   output_dir = path0,
    #                   output_file = paste0(file_name0,".pdf"))
    
    # PPTX
    ft <- poster_otolith(dat0 = dat0, 
                         textsize = 40,
                         pad = 10,
                         spacing = 1.4,
                         title = title)
    
    rmarkdown::render(paste0("./code/template_pptx.Rmd"),
                      output_dir = path0,
                      output_file = paste0(file_name0,".pptx"))
    
  }
}

