#' ---------------------------------------------
#' title: Create special project sheets
#' author: L Dawson (Liz.Dawson AT noaa.gov) and EH Markowitz (emily.markowitz AT noaa.gov)
#' start date: 2022-02-16
#' last modified: 2022-03-01
#' Notes: 
#' ---------------------------------------------

# START ------------------------------------------------------------------------

# *** REPORT KNOWNS ------------------------------------------------------------

maxyr <- 2022
dir_out <- paste0("./output/", Sys.Date(),"/")

googledrive::drive_auth()
1
dir_gdrive <- "1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k" # https://docs.google.com/spreadsheets/d/1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k/edit?usp=sharing

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

source('./code/functions.R')
source('./code/data.R')

# Check work -------------------------------------------------------------------

for (i in 1:nrow(data1)) {
  
  dat <- data1[i,]
  
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

