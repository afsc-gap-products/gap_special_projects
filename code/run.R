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

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

dir_out <- paste0("./output/", Sys.Date(),"/")
source('./code/functions.R')
source('./code/data.R')

# Check work -------------------------------------------------------------------

for (i in 1:nrow(data1)) {
  dat <- data1[i,]
  rmarkdown::render(paste0("./code/template.Rmd"),
                    output_dir = dir_out,
                    output_file = paste0(dir_out,
                                         maxyr, "-", dat$last_name, "-", 
                                         janitor::make_clean_names(dat$title), ".docx"))
}
