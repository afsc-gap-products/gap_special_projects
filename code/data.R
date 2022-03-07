#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------

# Run example ------------------------------------------------------------------

data0_ex <- readr::read_csv(file = paste0(here::here("example", "dat.csv"))) %>%
  janitor::clean_names(.)
if (names(b)[1] %in% "x1"){
  b$x1<-NULL
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

data1 <- edit_data(data0 = data0)

