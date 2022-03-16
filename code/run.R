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
access_to_internet <- TRUE
googledrive::drive_auth()
1
dir_gdrive <- "1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k" # https://docs.google.com/spreadsheets/d/1svA3mD8nV3nRnkmIF8MZqeIC4tLpjn1ltTBVjvYoB1k/edit?usp=sharing

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

source('./code/functions.R')
source('./code/data.R')

# Run RMarkdowns to create word docs from google spreadsheet --------------------

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

# Create vessel/survey specific posters from google spreadsheet --------------------

# the different combination of posters we will need to make
comb <- tidyr::crossing(
  srvy = data1 %>%
    dplyr::select(dplyr::starts_with("srvy_")) %>% 
    names(), #  %>% gsub(pattern = "srvy_", replacement = "", x = .)
  vess = data1 %>%
    dplyr::select(dplyr::starts_with("vess_")) %>% 
    names(), 
  sap_gap = unique(data1$sap_gap)) #  %>% gsub(pattern = "vess_", replacement = "", x = .)

dir.create(path = paste0(dir_out, "/posters/"))

for (i in 1:nrow(comb)) {
  
  # subset the data
  dat0 <- data1[(data1[comb$srvy[i]] & 
                   data1[comb$vess[i]] & 
                   data1$sap_gap == comb$sap_gap[i]), ]
  list_tab <- list()
  list_flex <- list()
  list_gg <- list()
  
  if (nrow(dat0)>1) {
    for (ii in 1:nrow(dat0)) {
      
      dat <- dat0[ii, ]
      
      d <- data.frame(matrix(data = c(
        paste0(toupper(dat$last_name)#, 
               # paste(rep_len(x = " ", 
               #               length.out = 50-(nchar(dat$last_name)+nchar(dat$preserve))), 
               #       collapse = ""), 
               # dat$preserve
        ), 
        # ifelse(is.na(dat$notes), "", dat$notes), 
        paste0(dat$preserve, "\n", dat$notes), 
        dat$desc), 
        # gsub(pattern = ", ", replacement = "\n",
        #      x = dat$species_name, fixed = TRUE)),
        ncol = 1))
      names(d) <- paste0(dat$title_short)#, " ", dat$preserve_freeze)
      list_tab$temp <- d
      
      dd <- d %>%
        flextable::flextable(data = .) %>% 
        # header
        flextable::bg(x = ., part = "header", bg = "#5781B2") %>% 
        flextable::color(x = ., part = "header", color = "white") %>% 
        flextable::bold(x = ., part = "header") %>%
        # requestor line
        flextable::color(x = ., i = 1, part = "body", color = "#0B6693") %>% 
        flextable::bold(x = ., i = 1, part = "body") %>% 
        # preservation line
        flextable::color(x = ., i = 2, part = "body", color = "#0B6693") %>% 
        flextable::fontsize(x = ., i = 2, part = "body", size = 8) %>% 
        flextable::italic(x = ., i = 2, part = "body") %>%
        #species lines
        flextable::color(x = ., i = 2, part = "body", color = "#0B6693") %>% 
        flextable::fontsize(x = ., i = 2, part = "body", size = 8) %>% 
        flextable::italic(x = ., i = 2, part = "body") %>% 
        flextable::width(x = ., width = 5, unit = "in") 
      
      list_flex$temp <- 
        ggplot() +
        theme_void() +
        annotation_custom(rasterGrob(dd %>% flextable::as_raster()), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
      
      # library(ggpubr)
      # 
      # d[,1] <- str_wrap(d[,1], 40)
      # list_gg$temp <- ggpubr::ggtexttable(d, rows = NULL,
      #                             theme = ttheme("mBlue"))
      
      names(list_tab)[names(list_tab) == "temp"] <- 
        paste0(dat$last_name, "-", ii)
      
      names(list_flex)[names(list_flex) == "temp"] <- 
        paste0(dat$last_name, "-", ii)
      
      names(list_gg)[names(list_gg) == "temp"] <- 
        paste0(dat$last_name, "-", ii)
    }
    
    
    # list_flex$title <- ggdraw() + 
    #   draw_label(
    #     paste0(gsub(toupper(pattern = "srvy_", replacement = "", x = comb$srvy[i])), " ",
    #            gsub(pattern = "vess_", replacement = "", x = comb$vess[i]), " ",
    #                 toupper(comb$sap_gap[i]), "\n", 
    #                 "Special Collections Tally Sheet"),
    #     fontface = 'bold',
    #     x = 0,
    #     hjust = 0
    #   ) +
    #   theme(
    #     # add margin on the left of the drawing canvas,
    #     # so title is aligned with left edge of first plot
    #     plot.margin = margin(0, 0, 0, 7)
    #   )
    # 
    # plot_grid(
    #   title, plot_row,
    #   ncol = 1,
    #   # rel_heights values control vertical title margins
    #   rel_heights = c(0.1, 1)
    # )
    
    title <- paste0(
          maxyr, " ",  
          gsub(pattern = "srvy_", replacement = "", x = comb$srvy[i]), " ",
          gsub(pattern = "vess_", replacement = "", x = comb$vess[i]), " ",
          toupper(comb$sap_gap[i]), "\n",
          "Special Collections Tally Sheet")
    
    cowplot::plot_grid(plotlist = list_flex,
                       align = "hv", greedy = TRUE) %>%
      # grid::grid.text(title, x = 0.5, y = 0.5)
      # ggplot2::annotate("text", x = 12.5, y = 3.5, label = title) 
      # 
      # 
      # cowplot::draw_text(
      #   text = , 
      #   x = 0, y = 0, size = 30) %>%
      ggplot2::ggsave(
        filename = paste0(dir_out, "/posters/", comb$srvy[i],"-",comb$vess[i],"-",comb$sap_gap[i],".pdf"),
        plot = .,
        width = 48, height = 36, units = "in",
        device = "pdf")
    
  }
}


