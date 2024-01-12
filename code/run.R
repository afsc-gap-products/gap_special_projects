#' -----------------------------------------------------------------------------
#' title: Create special project sheets
#' author: A Dowlin (alexandra.dowlin AT noaa.gov) and EH Markowitz (emily.markowitz AT noaa.gov)
#' start date: 202-02-16
#' last modified: 2024-01-12
#' Notes: 
#' -----------------------------------------------------------------------------

# run reports with one click
# source("./code/run.R")
# 1

# REPORT KNOWNS ----------------------------------------------------------------

# maxyr <- 2023
# comb <- data.frame(srvy = c("srvy_GOA", "srvy_GOA", 
#                             "srvy_EBS", "srvy_EBS", 
#                             "srvy_NBS", "srvy_NBS"), 
#                    vessel = c("Ocean Explorer", "Alaska Provider", 
#                               "Alaska Knight", "Northwest Explorer", 
#                               "Alaska Knight", "Northwest Explorer"), 
#                    vess = c("vess_oex", "vess_akp", 
#                             "vess_akk", "vess_nwe", 
#                             "vess_akk", "vess_nwe"), 
#                    sap_gap = c("gap", "gap", "gap", "gap", "gap", "gap")) # possibility for SAP, too

maxyr <- 2024
comb <- data.frame(srvy = c("srvy_AI", "srvy_AI", 
                            "srvy_EBS", "srvy_EBS"), 
                   vessel = c("Ocean Explorer", "Alaska Provider", 
                              "Alaska Knight", "Northwest Explorer"), 
                   vess = c("vess_oex", "vess_akp", 
                            "vess_akk", "vess_nwe"), 
                   sap_gap = c("gap", "gap", "gap", "gap")) 

# GOOGLE SPREADSHEET LINKS -----------------------------------------------------

# Google drive folder for this project: https://drive.google.com/drive/folders/1wNkH1gSOeiCSSwWOIObvqSnxjAWEF1DY?usp=share_link
dir_gcore <- "https://docs.google.com/spreadsheets/d/1WHyetA20twlq6uhp5VR-sHtm2VR1uecOcaLOmztK9Zs/edit?usp=sharing" # all years

# 2022: 
# dir_gspecial <- "https://docs.google.com/spreadsheets/d/1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k/edit?usp=sharing" 

# 2023: 
# dir_gspecial <- "https://docs.google.com/spreadsheets/d/1DaU7AxlOf3MjDA-LV46at_PZeVaZiHPiDvZjhRSW1xE" 

# 2024: 
dir_gspecial <- "https://docs.google.com/spreadsheets/d/1CWc5QEPOsd3EN0_VGhViwi6jx7RvExEIxWBal_HvyBk" 

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------

subset_to_accepted_projects <- TRUE
access_to_internet <- TRUE
source(here::here('code/functions.R'))
googledrive::drive_auth()
2
# source('./code/ex.R') # for README
source(here::here('code/data.R'))

# Run RMarkdowns to create word docs from google spreadsheet -------------------

srvy0 <- unique(gsub(pattern = "srvy_", replacement = "", x = comb$srvy))

for (i in 1:length(srvy0)) {
  
  srvy <- srvy0[i]
  vess <- unique(comb$vess[comb$srvy == paste0("srvy_", srvy)])
  vess_not <- unique(toupper(gsub(pattern = "vess_", replacement = "", x = comb$vess[!(comb$vess %in% vess)])))
  
  dir_out_srvy <- paste0(substr(x = dir_out, start = 1, stop = (nchar(dir_out)-1)), "_", srvy, "/")
  dir.create(dir_out_srvy, showWarnings = F)

  # project summary sheet without description
  filename0 <- paste0(maxyr, "-00-summarynodesc-", srvy, ".docx")
  rmarkdown::render(here::here("code/template_summary_nodesc.Rmd"),
                    output_dir = dir_out,
                    output_file = filename0)
  file.copy(from = paste0(dir_out, filename0),   # copy summary files to survey folders
            to = paste0(dir_out_srvy, "/", filename0), 
            overwrite = TRUE)
    
  # project summary sheet with description
  filename0 <- paste0(maxyr, "-00-summarydesc-", srvy, ".docx")
  rmarkdown::render(here::here("code/template_summary_desc.Rmd"),
                    output_dir = dir_out,
                    output_file = filename0)
  file.copy(from = paste0(dir_out, filename0),   # copy summary files to survey folders
            to = paste0(dir_out_srvy, "/", filename0), 
            overwrite = TRUE)  
}  

# individual project description pages
for (i in 1:nrow(special)) {

  dat <- special[i,]
  
  file_name <- paste0(maxyr, "-", 
                      stringr::str_pad(i, nchar(nrow(special)), pad = "0"), "_", 
                      dat$last_name, "-", 
                      janitor::make_clean_names(dat$short_title), ".docx")
  
  rmarkdown::render(here::here("code/template.Rmd"),
                    output_dir = dir_out,
                    output_file = file_name)
  
  temp <- strsplit(x = dat$srvy, split = ", ")[[1]]
  
  for (ii in 1:length(temp)) {
    
    # dir.create(paste0(dir_out, temp[ii]), showWarnings = F)
    
    file.copy(from = here::here(dir_out, file_name), 
              to = paste0(substr(x = dir_out, start = 1, stop = (nchar(dir_out)-1)), "_", temp[ii], "/", file_name), 
              overwrite = TRUE)
    
  }
  
}

# move all loose files to the "all" folder
a <- list.files(path = dir_out, pattern = ".docx", full.names = TRUE)
dir.create(path = paste0(dir_out, "/all/"))
file.copy(from = a, to = paste0(dir_out, "/all/"))
file.remove(a)

# Special projects posters by vessel and survey --------------------------------

path0 <-paste0(dir_out, "posters_special/")
dir.create(path = path0)

for (i in 1:nrow(comb)) {
  
  # subset the data
  dat0 <- special[(special[,comb$srvy[i]] == TRUE & 
                     special[,comb$vess[i]] == TRUE # & 
                   #   special$sap_gap == comb$sap_gap[i]
                   ), ] %>% 
    dplyr::select(short_title, first_name, last_name, preserve, short_procedures, numeric_priority) 
  
  if (nrow(dat0) != 0) {
    
    file_name0 <- gsub(pattern = "vess_", 
                       replacement = "", 
                       x = paste0(comb$srvy[i],"-",comb$vess[i],"-",comb$sap_gap[i]))
    
    title <- paste0(
      maxyr, " ",  
      gsub(pattern = "srvy_", replacement = "", x = comb$srvy[i]), 
      " F/V ", comb$vessel[i], 
      " ",
      toupper(comb$sap_gap[i]), " ", 
      "Special Collections Tally Sheet")
    
    ft <- poster_special(dat0 = dat0,
                         subheader_size = 36,
                         header_size = 60,
                         body_size = 28,
                         pgwidth = 46,
                         font = "Arial",
                         title = title,
                         spacing = 1.25,
                         pad = 20,
                         fig_size = .5)

    flextable::save_as_pptx(
      " " = ft,
      path = paste0(path0, file_name0,".pptx"))


    rmarkdown::render(here::here("code/template_pptx_landscape.Rmd"), # fix sizing of page
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
  
  dat0 <- otoliths0 %>% # subset the data
    dplyr::filter(survey == comb$survey[i] &
                    plan != "no collection this year") %>%
    dplyr::arrange(plan) %>% 
    dplyr::select(plan, species, species, n_per_haul, criteria) %>% 
    dplyr::mutate(plan = toupper(plan))
  
  if (nrow(dat0) != 0) {  # if there is data, run scripts
    
    title <- paste0(
      maxyr, " ",  
      gsub(pattern = "srvy_", replacement = "", x = comb$survey[i]), 
      " Survey Core Otolith Collections")
    
    ft <- poster_otolith(dat0 = dat0,
                         title = title,
                         header = 80,
                         subheader_size = 80,
                         body_size = 72,
                         spacing = 1.2,
                         pad = 10, 
                         pgwidth = 34,
                         col_spacing = c(0.3,0.2,0.4)) 
    
    flextable::save_as_pptx(
      " " = ft,
      path = paste0(path0, file_name0,".pptx"))

    # fix sizing of page
    rmarkdown::render(here::here("code/template_pptx.Rmd"),
                      output_dir = path0,
                      output_file = paste0(file_name0,".pptx"))
    
  }
}

