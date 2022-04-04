

# INSTALL PACKAGES -------------------------------------
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # other tidyverse
  "tidyr",
  "dplyr",
  "magrittr",
  "readr",
  "janitor", 
  "googledrive", 
  "ggplot2", 
  "cowplot", 
  "magick",
  
  "xlsx", 
  "readr",
  
  "grid", 
  "gridExtra", 
  
  "flextable", 
  "bookdown", 
  # "posterdown", # remotes::install_github("brentthorne/posterdown")
  
  # Text Management
  "stringr")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}


# Other user functions -------------------------------------

edit_data <- function(data0) {
  data1 <- data0 %>%
    dplyr::mutate(affiliation = gsub(pattern = " - ",
                                     replacement = "-",
                                     x = as.character(affiliation),
                                     fixed = TRUE),
                  affiliation = gsub(pattern = "-",
                                     replacement = " - ",
                                     x = affiliation,
                                     fixed = TRUE),
                  survey = gsub(pattern = ";", replacement = ", ", x = survey)) %>%
    tidyr::separate(data = ., col = requester_name, into = c("first_name", "last_name"),
                    sep = " ", remove = FALSE) %>% 
    dplyr::mutate( # TOLEDO, need to double check!
      target = dplyr::case_when(
        is.na(minimum_number_of_specimens) & is.na(maximum_number_of_specimens) ~
          "No maximum or minimum quantity. ",
        is.na(minimum_number_of_specimens) & !is.na(maximum_number_of_specimens) ~ 
          paste0("Maximum speciment quantity: ", maximum_number_of_specimens), 
        is.na(maximum_number_of_specimens) & !is.na(minimum_number_of_specimens) ~ 
          paste0("Minimum speciment quantity: ", minimum_number_of_specimens), 
        # is.character(minimum_number_of_specimens) ~ minimum_number_of_specimens, 
        # is.numeric(minimum_number_of_specimens) & is.numeric(maximum_number_of_specimens) ~ 
        #   paste0(minimum_number_of_specimens," - ",maximum_number_of_specimens), 
        TRUE ~ paste0(minimum_number_of_specimens," - ",maximum_number_of_specimens)
      ), 
      across(where(is.character), 
             gsub, pattern = " , ", replace = ", "), 
      across(where(is.character), 
             gsub, pattern = " ; ", replace = "; "), 
      across(where(is.character), 
             gsub, pattern = "none", replace = "[None]"), 
      across(where(is.character), 
             ~tidyr::replace_na(data = ., replace = "[None]")))
  # dplyr::mutate(across(is.character(.), ~tidyr::replace_na(., "None. ")))
  
  return(data1)
  
}



#' @importFrom officer fp_border fp_par
#' @export
#' @title Apply vanilla theme
#' @description Apply theme vanilla to a flextable:
#' The external horizontal lines of the different parts of
#' the table (body, header, footer) are black 2 points thick,
#' the external horizontal lines of the different parts
#' are black 0.5 point thick. Header text is bold,
#' text columns are left aligned, other columns are
#' right aligned.
#' @param x a flextable object
#' @param pgwidth a numeric. The width in inches the table should be. Default = 6, which is ideal for A4 (8.5x11 in) portrait paper.
#' @param row_lines T/F. If True, draws a line between each row.
#' @param font String. Default = "Times New Roman". Instead, you may want "Arial".
#' @param body_size Numeric. default = 11.
#' @param header_size Numeric. default = 11.
#' @param spacing table spacing. default = 1
#' @param pad padding around each element. default = 0.1
#' @family functions related to themes
#' @examples
#' ft <- flextable::flextable(head(airquality))
#' ft <- NMFSReports::theme_flextable_nmfstm(ft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_theme_vanilla_1.png}{options: width=60\%}}
theme_poster <- function(x,
                         ncol0,
                         pgwidth = 6,
                         row_lines = TRUE,
                         body_size = 11,
                         header_size = 11,
                         font = "Arial",
                         spacing = 1,
                         pad = 0.1, 
                         even_body = "#CFCFCF", 
                         odd_body = "#EFEFEF") {
  
  if (!inherits(x, "flextable")) {
    stop("theme_flextable_nmfstm supports only flextable objects.")
  }
  
  FitFlextableToPage <- function(x, pgwidth = 6){
    # https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
    ft_out <- x %>% flextable::autofit()
    
    ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable::flextable_dim(ft_out)$widths))
    return(ft_out)
  }
  
  std_b <- officer::fp_border(width = 2, color = "grey10")
  thin_b <- officer::fp_border(width = 0.5, color = "grey10")
  
  if (row_lines == TRUE) {
    x <- flextable::hline(x = x, border = thin_b, part = "body")
  }
  
  b_nrow <- flextable::nrow_part(x, "body")
  x <- flextable::border_remove(x)
  x <- flextable::align(x = x, align = "center", part = "header")
  x <- flextable::valign(x, valign = "top", part = "body")
  
  if (b_nrow > 0) {
    even <- seq_len(b_nrow)%%2 == 0
    odd <- !even
    
    x <- bg(x = x, i = odd, j = seq_len(ncol0)%%2 != 0, bg = odd_body, part = "body")
    x <- bg(x = x, i = odd, j = seq_len(ncol0)%%2 == 0, bg = even_body, part = "body")
    x <- bg(x = x, i = even, j = seq_len(ncol0)%%2 != 0, bg = even_body, part = "body") 
    x <- bg(x = x, i = even, j = seq_len(ncol0)%%2 == 0, bg = odd_body, part = "body") 
  }
  
  x <- flextable::hline_top(x = x, border = std_b, part = "header")
  x <- flextable::hline_bottom(x = x, border = std_b, part = "header")
  x <- flextable::hline_bottom(x = x, border = std_b, part = "body")
  x <- flextable::bold(x = x, bold = TRUE, part = "header")
  x <- flextable::align_text_col(x = x, align = "left", header = TRUE)
  x <- flextable::align_nottext_col(x = x, align = "right", header = TRUE)
  x <- flextable::padding(x = x, padding = pad, part = "all") # remove all line spacing in a flextable
  x <- flextable::font(x = x, fontname = font, part = "all")
  x <- flextable::fontsize(x = x, size = body_size, part = "body")
  x <- flextable::fontsize(x = x, size = header_size, part = "header")
  x <- FitFlextableToPage(x = x, pgwidth = pgwidth)
  x <- flextable::line_spacing(x = x, space = spacing, part = "all")
  
  x <- flextable::fix_border_issues(x = x)
  
  return(x)
}


poster_special <- function(dat0, 
                           textsize = 16, 
                           pad = 5, 
                           spacing = 1.2, 
                           row_lines = TRUE, 
                           title = ""){
  
  if (nrow(dat0) <= 8) {
    nnn <- 2
  } else if (nrow(dat0) <= 12)  {
    nnn <- 3
  } else {
    nnn <- 4
  }
  
  comb1 <- data.frame(from = seq(1,nrow(dat0)+nnn, nnn)[1:length(seq(nnn,nrow(dat0)+nnn, nnn))], 
                      to = seq(nnn,nrow(dat0)+nnn, nnn))
  
  if (comb1$from[nrow(comb1)] > nrow(dat0)) {
    comb1 <- comb1[-nrow(comb1),]
  }
  
  if (comb1$to[nrow(comb1)]-nrow(dat0) != 0) {
    d <- data.frame(matrix(data = "", 
                           nrow = (comb1$to[nrow(comb1)]-nrow(dat0)),
                           ncol = nnn))
    names(d) <- names(dat0)
    dat0 <- dplyr::bind_rows(dat0, d)
  }
  
  dat0000 <- data.frame()
  
  for (ii in 1:nrow(comb1)) {
    d <- dat0[c((comb1$from[ii]):(comb1$to[ii])),]
    names(d) <- paste0(names(dat0), ii)
    if (ii == 1) {
      dat0000 <- d
    } else {
      dat0000 <- dplyr::bind_cols(dat0000, d)
    }
  }
  
  cc <- 1:nrow(comb1)
  
  ft <- eval(parse(text = paste0('flextable::flextable(dat0000, 
                                     col_keys = paste0("dummy", 1:nrow(comb1)))', 
                                 paste0(' %>%
    flextable::compose(j = "dummy',cc,'",
                       value = as_paragraph(
                         flextable::as_chunk(paste0((short_name',cc,'), "\n"), 
                                  props = fp_text_default(color = "#006699", # "white", 
                                                          bold = TRUE, 
                                                          # shading.color	= "#006699",
                                                          font.size = ',textsize+8,')),
                          flextable::as_chunk(toupper(paste0(last_name',cc,', "\n")), 
                                  props = fp_text_default(color = "black",
                                                          bold = TRUE, 
                                                          font.size = ',textsize+2,')),
  flextable::as_i(paste0(preserve',cc,', "\n\n")),
  paste0(short_procedures',cc,')))', collapse = ""), collapse = "") )) %>%
    # flextable::autofit(x, add_w = 0.1, add_h = 0.1, part = c("body", "header"), unit = "in") %>% 
    # flextable::fontsize(x = ., size = 20) %>% 
    theme_poster(body_size = textsize, 
                 header_size = textsize+8, 
                 pad = 5, 
                 ncol0 = nrow(comb1), 
                 spacing = 1.2, 
                 row_lines = FALSE) %>% 
    flextable::merge_h(., part = "header") %>%
    flextable::color(x = ., color = "grey20") %>%
    flextable::set_header_labels(x = .,
                                 "dummy1" = title)  %>%
    flextable::width(x = ., width = 48/4, unit = "in") %>%
    flextable::line_spacing(x = ., space = spacing, part = "all") %>% 
    flextable::padding(x = ., padding = pad, part = "all") %>% 
    flextable::hline(x = ., 
                     border = officer::fp_border(width = 0.5, color = "grey10"), 
                     part = "body")
}


poster_otolith <- function(dat0, 
                           textsize = 16, 
                           pad = 5, 
                           spacing = 1.2, 
                           row_lines = TRUE, 
                           title = ""){
  
  ft <- #flextable::as_grouped_data(x = dat0000, groups = c("plan")) %>%
    flextable::flextable(data = dat0) %>%
    flextable::theme_zebra(x = ., 
                           odd_header = "darkolivegreen3", 
                           even_header  = "darkolivegreen3",
                           odd_body = "lightblue") %>%
    flextable::add_header(x = ., 
                          n_per_haul = title,
                          criteria = title,
                          plan = title, 
                          species = title, top = TRUE ) %>%
    flextable::merge_h(x = ., i = 1, part = "header") %>%
    flextable::merge_v(x = ., j="plan") %>%
    # flextable::bold(x = ., j = "plan", bold = TRUE) %>%
    flextable::set_header_labels(x = .,
                                 "plan" = "", 
                                 "species" = "Species", 
                                 "n_per_haul" = "Target Number per Plan", 
                                 "criteria" = "Rules") %>% 
    # flextable::set_caption(x = ., caption = title) %>%
    flextable::font(x = ., fontname = "Arial") %>%
    flextable::fontsize(x = ., size = textsize, part = "body") %>% 
    flextable::fontsize(x = ., i = 1, size = textsize+20, part = "header") %>% 
    flextable::fontsize(x = ., i = 2, size = textsize+4, part = "header") %>% 
    flextable::width(x = ., width = 48/4, unit = "in") %>%
    flextable::line_spacing(x = ., space = spacing, part = "all") %>% 
    flextable::padding(x = ., padding = pad, part = "all") %>% 
    flextable::hline(x = ., 
                     border = officer::fp_border(width = 0.5, color = "grey10"), 
                     part = "body")
  # flextable::autofit()
  
  return(ft)
  
}
