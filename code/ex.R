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

