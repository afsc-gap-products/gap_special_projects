#' -----------------------------------------------------------------------------
#' title: Create special project sheets
#' author: L Dawson (Liz.Dawson AT noaa.gov) and EH Markowitz (emily.markowitz AT noaa.gov)
#' start date: 2022-02-16
#' last modified: 2022-03-01
#' Notes: 
#' -----------------------------------------------------------------------------

# REPORT KNOWNS ----------------------------------------------------------------

# Google drive folder for this project
# https://drive.google.com/drive/folders/1wNkH1gSOeiCSSwWOIObvqSnxjAWEF1DY?usp=share_link

# source("./code/run.R")
# 1

maxyr <- 2023
subset_to_accepted_projects <- TRUE
access_to_internet <- TRUE

# dir_gspecial <- "https://docs.google.com/spreadsheets/d/1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k/edit?usp=sharing" # 2022
dir_gspecial <- "https://docs.google.com/spreadsheets/d/1DaU7AxlOf3MjDA-LV46at_PZeVaZiHPiDvZjhRSW1xE" # 2023
dir_gcore <- "https://docs.google.com/spreadsheets/d/1WHyetA20twlq6uhp5VR-sHtm2VR1uecOcaLOmztK9Zs/edit?usp=sharing" # all years

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------

source('./code/functions.R')
googledrive::drive_auth()
1
# source('./code/ex.R') # for README
source('./code/data.R')

# Run RMarkdowns to create word docs from google spreadsheet -------------------

for (i in 1:nrow(special)) {
  
  dat <- special[i,]
  
  file_name <- paste0(maxyr, "-", dat$last_name, "-", 
                      janitor::make_clean_names(dat$short_title), ".docx")
  
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

# Special projects posters by vessel and survey --------------------------------

comb <- data.frame(srvy = c("srvy_GOA", "srvy_GOA", 
                            "srvy_EBS", "srvy_EBS", 
                            "srvy_NBS", "srvy_NBS"), 
                   vess = c("vess_oex", "vess_akp", 
                            "vess_akk", "vess_nwe", 
                            "vess_akk", "vess_nwe"), 
                   sap_gap = c("gap", "gap", "gap", "gap", "gap", "gap")) # possibility for SAP, too

path0 <-paste0(dir_out, "posters_special/")
dir.create(path = path0)

for (i in 1:nrow(comb)) {
  
  # subset the data
  dat0 <- special[(special[,comb$srvy[i]] == TRUE & 
                     special[,comb$vess[i]] == TRUE & 
                     special$sap_gap == comb$sap_gap[i]), ] %>% 
    dplyr::select(short_title, requestor_name, preserve, short_procedures, numeric_priority) 
  
  if (nrow(dat0) != 0) {
    
    file_name0 <- gsub(pattern = "vess_", 
                       replacement = "", 
                       x = paste0(comb$srvy[i],"-",comb$vess[i],"-",comb$sap_gap[i]))
    
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
    
    # ft <- poster_special(dat0 = dat0,
    #                      header_size = 14,
    #                      subheader_size = 10,
    #                      body_size = 8,
    #                      pgwidth = 11,
    #                      font = "Arial",
    #                      title = title,
    #                      spacing = 1.1,
    #                      pad = 40, 
    #                      fig_size = .2, 
    #                      in_markdown = FALSE)
    # 
    # flextable::save_as_image(x = ft,
    #                          path = paste0(path0, file_name0,".pdf"),
    #                          webshot = "webshot", zoom = 1)
    
    
    # might not be the best rule, but we will see
    adj0 <- ifelse(nrow(dat0)<=12 | 
                     (max(nchar(dat0$short_procedures)) < 360), 0, -2)
    
    ft <- poster_special(dat0 = dat0,
                         title = title,
                         header_size = 12,
                         subheader_size = 8+adj0,
                         body_size = 7+adj0,
                         pgwidth = 11.6,
                         font = "Arial",
                         spacing = 1.6,
                         pad = 40, 
                         fig_size = .1, 
                         in_markdown = TRUE)
    
    rmarkdown::render(paste0("./code/template_pdf_landscape.Rmd"),
                      output_dir = path0,
                      output_file = paste0(file_name0,".pdf"))
    
    # PPTX
    
    # ft <- poster_special(dat0 = dat0,
    #                      subheader_size = 36,
    #                      header_size = 60,
    #                      body_size = 28,
    #                      pgwidth = 46,
    #                      font = "Arial",
    #                      title = title,
    #                      spacing = 1.5,
    #                      pad = 20, 
    #                      fig_size = .5)
    # 
    # flextable::save_as_pptx(
    #   " " = ft,
    #   path = paste0(path0, file_name0,".pptx"))
    # 
    # rmarkdown::render(paste0("./code/template_pptx_landscape.Rmd"),
    #                   output_dir = path0,
    #                   output_file = paste0(file_name0,".pptx"))
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
      " Survey Core Otolith Collections")
    
    # PDF
    
    # ft <- poster_otolith(dat0 = dat0,
    #                      title = title,
    #                      header_size = 24,
    #                      subheader_size = 18,
    #                      body_size = 14,
    #                      spacing = 1.2,
    #                      pad = 1,
    #                      pgwidth = 11.6,
    #                      col_spacing = c(3,1.5,7.1))
    
    
    ft <- poster_otolith(dat0 = dat0,
                         title = title,
                         header_size = 14,
                         subheader_size = 12,
                         body_size = 12,
                         spacing = .9,
                         pad = 1, # doesnt work, maybe next year https://ardata-fr.github.io/flextable-book/rendering.html#note_pdf
                         pgwidth = 7.6,
                         col_spacing = c(1.5,1.4,4.2)) # must sum to pgwidth

    rmarkdown::render(paste0("./code/template_pdf_portrait.Rmd"),
                      output_dir = path0,
                      output_file = paste0(file_name0,".pdf"))
    
    
    
    # ft <- poster_otolith(dat0 = dat0,
    #                      title = title,
    #                      header_size = 22,
    #                      subheader_size = 20,
    #                      body_size = 18,
    #                      spacing = 1.2,
    #                      pad = 10,
    #                      pgwidth = 7.6,
    #                      col_spacing = c(1.5,1.2,4.6))
    # 
    # flextable::save_as_image(x = ft,
    #                          path = paste0(path0, file_name0,"_1.pdf"), 
    #                          zoom = 1,expand = 1, webshot = "webshot")
    
    # flextable_to_rmd(x = ft, )
    
    # ft <- poster_otolith(dat0 = dat0,
    #                      title = title,
    #                      header_size = 28, 
    #                      body_size = 26,
    #                      spacing = 1.2,
    #                      pad = 10, 
    #                      pgwidth = 7.8, 
    #                      col_spacing = c(1.8,1,5)) # c(1.8,1,5)
    # 
    # 
    # flextable::save_as_image(x =ft,
    #                          path = paste0(path0, file_name0,".pdf"))
    
    # flextable::save_as_image(x = poster_otolith(dat0 = dat0,
    #                                             textsize = 35,
    #                                             title = title,
    #                                             spacing = 1.2,
    #                                             pad = 10),
    #                          path = paste0(path0, file_name0,".pdf"),
    #                          zoom = 1, expand = 1)
    
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
    # ft <- poster_otolith(dat0 = dat0, 
    #                      textsize = 40,
    #                      pad = 10,
    #                      spacing = 1.2,
    #                      title = title)
    # 
    # rmarkdown::render(paste0("./code/template_pptx.Rmd"),
    #                   output_dir = path0,
    #                   output_file = paste0(file_name0,".pptx"))
    
  }
}

