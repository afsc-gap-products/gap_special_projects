

# INSTALL PACKAGES -------------------------------------
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # other tidyverse
  "tidyr",
  # "plyr",
  "dplyr",
  "magrittr",
  "readr",
  "janitor", 
  "googledrive", 
  # "ggplot2", 
  # "cowplot", 
  # "magick",
  # "webshot2", 
  # "webshot",
  
  "xlsx", 
  "readr",
  
  "grid", 
  "gridExtra", 
  
  "flextable", 
  # "bookdown", 
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
          paste0("Maximum specimen quantity: ", maximum_number_of_specimens), 
        is.na(maximum_number_of_specimens) & !is.na(minimum_number_of_specimens) ~ 
          paste0("Minimum specimen quantity: ", minimum_number_of_specimens), 
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
    
    x <- flextable::bg(x = x, i = odd, j = seq_len(ncol0)%%2 != 0, bg = odd_body, part = "body")
    x <- flextable::bg(x = x, i = odd, j = seq_len(ncol0)%%2 == 0, bg = even_body, part = "body")
    x <- flextable::bg(x = x, i = even, j = seq_len(ncol0)%%2 != 0, bg = even_body, part = "body") 
    x <- flextable::bg(x = x, i = even, j = seq_len(ncol0)%%2 == 0, bg = odd_body, part = "body") 
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
                           title = "", 
                           header_size = 30,
                           subheader_size = 16, 
                           body_size = 11,
                           pad = 5, 
                           font = "Arial",
                           spacing = 1.2, 
                           pgwidth = 48,
                           even_body = "#CFCFCF", 
                           odd_body = "#EFEFEF", 
                           fig_size = .5, 
                           in_markdown = TRUE){
  
  # Data restacking

  if (nrow(dat0) <= 1) {
    nnn <- 1
  } else if (nrow(dat0) <= 8) {
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
                           ncol = ncol(dat0)))
    names(d) <- names(dat0)
    dat0 <- dplyr::bind_rows(dat0, 
                             d %>% 
                               dplyr::mutate(numeric_priority = as.numeric(numeric_priority)))
    # } else if (comb1$to[nrow(comb1)]-nrow(dat0) > 0) {
    #   comb1$to[nrow(comb1)] <- nrow(dat0)
  }
  
  dat0 <- dat0 %>% 
    dplyr::mutate(numeric_priority = ifelse(grepl(pattern = "outreach", x = short_name, ignore.case = TRUE), 
                                            (max(numeric_priority, na.rm = TRUE)+1), 
                                            numeric_priority), 
                  numeric_priority = ifelse(is.na(numeric_priority), 
                                            (max(numeric_priority, na.rm = TRUE)+1), 
                                            numeric_priority), 
                  nchar_proc = nchar(short_procedures)) %>%
    dplyr::arrange(numeric_priority, desc(nchar_proc) # 
                   # plyr::round_any(nchar(short_procedures), 10, f = ceiling)
                   ) %>% 
    dplyr::mutate(col = rep_len(length.out = nrow(.), x = 1:nrow(comb1))) %>% 
    dplyr::select(-numeric_priority, nchar_proc)
  
  dat0000 <- data.frame()
  
  for (ii in 1:nrow(comb1)) {
  # arrange L->R, then Top->Down
    d <- dat0[dat0$col == ii,]
    names(d) <- paste0(names(dat0), ii)
    # # arrange Top->Down, then L->R 
    #   d <- dat0[c((comb1$from[ii]):(comb1$to[ii])),]
    #   names(d) <- paste0(names(dat0), ii)
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
                          flextable::as_chunk(paste0((short_name',cc,')), 
                                  props = fp_text_default(color = "#006699", # "white", 
                                                          bold = TRUE, 
                                                          # shading.color	= "#006699",
                                                          font.size = ',subheader_size+2,')), 
                          "\n",
                          flextable::as_chunk(toupper(paste0(pi',cc,', "\n")), 
                                  props = fp_text_default(color = "black",
                                                          bold = TRUE, 
                                                          font.size = ',subheader_size,')),
                          flextable::as_chunk(paste0(preserve',cc,'), 
                                  props = fp_text_default(color = "#006699",
                                                          italic = TRUE, 
                                                          font.size = ',body_size,')),
                          "  ",                                 
                          flextable::as_image(src =
                                     ifelse(grepl(x = preserve',cc,', pattern = "freeze"),
                                     "',ifelse(in_markdown, ".", ""),'./img/snowflake.png",
                                     "',ifelse(in_markdown, ".", ""),'./img/blank.png"),
                                                   width = ',fig_size,', height = ',fig_size,'),  
                          "\n",
                          flextable::as_chunk((paste0(short_procedures',cc,', "\n")), 
                                  props = fp_text_default(color = "black",
                                                          bold = TRUE, 
                                                          font.size = ',body_size,')) ))', 
                  collapse = ""), collapse = "") )) %>%

    # table aesthetics
    flextable::align_text_col(x = ., align = "left", header = TRUE) %>% 
    flextable::align_nottext_col(x = ., align = "right", header = TRUE) %>% 
    flextable::padding(x = ., padding = pad, part = "all") %>% 
    flextable::line_spacing(x = ., space = spacing, part = "all") %>% 
    flextable::font(x = ., fontname = font, part = "all") %>%
    flextable::valign(x = ., valign = "top", part = "body") %>%
  
    # header
    flextable::merge_h(., part = "header") %>%
    flextable::bold(x = ., bold = TRUE, part = "header") %>% 
    flextable::fontsize(x = ., size = header_size, part = "header") %>%
    flextable::align(x = ., align = "center", part = "header") %>% 
    flextable::color(x = ., color = "grey20") %>%
    flextable::set_header_labels(x = .,
                                 "dummy1" = title) %>%
    # flextable::hline(x = ., 
    #                  border = officer::fp_border(width = 0.5, color = "grey10"), 
    #                  part = "body") %>%   
    
    # table sizing 
    flextable::fit_to_width(x = .,
                            max_width = pgwidth,
                            # max_iter = 50,
                     unit = "in") %>%
    flextable::width(x = .,
                     width = rep_len(x = pgwidth/nrow(comb1), length.out = nrow(comb1)),
                     j = 1:nrow(comb1),
                     unit = "in") 
  
    
# Line borders
      ft <- flextable::hline(x = ft, 
                             border = officer::fp_border(width = 0.5, color = "grey80"), 
                             part = "body")
      ft <- flextable::vline(x = ft, 
                             border = officer::fp_border(width = 0.5, color = "grey80"), 
                             part = "body")
    
    # # alternating colors
    b_nrow <- flextable::nrow_part(ft, "body")
    
    ncol0 <- nrow(comb1)
    
    if (b_nrow > 0) {
      even <- seq_len(b_nrow)%%2 == 0
      odd <- !even
      
      ft <- flextable::bg(x = ft, i = odd, j = seq_len(ncol0)%%2 != 0, 
                          bg = odd_body, part = "body")
      ft <- flextable::bg(x = ft, i = odd, j = seq_len(ncol0)%%2 == 0, 
                          bg = even_body, part = "body")
      ft <- flextable::bg(x = ft, i = even, j = seq_len(ncol0)%%2 != 0, 
                          bg = even_body, part = "body") 
      ft <- flextable::bg(x = ft, i = even, j = seq_len(ncol0)%%2 == 0, 
                          bg = odd_body, part = "body") 
    }
}


poster_otolith <- function(dat0, 
                           header_size = 30,
                           subheader_size = 28,
                           body_size = 16, 
                           pad = 5, 
                           spacing = 1.2, 
                           # row_lines = TRUE, 
                           title = "", 
                           pgwidth = 12, 
                           col_spacing = c(2,1,5)){
  
  # as_flextable(x, col_keys = NULL, hide_grouplabel = FALSE, ...)
  
  # plan <- plan0 <- ifelse(length(unique(dat0$plan)) == 1, 
  #                 "Sampling plan for all", 
  #                 "Sampling plan")
  
  dat000 <- dat0 %>% 
    dplyr::arrange(plan, species, n_per_haul) %>% 
    dplyr::mutate(
      species = paste0("\n", species, "\n"), 
      n_per_haul = paste0("\n", n_per_haul, "\n"), 
      n_per_haul = gsub(pattern = "\nNA\n", replacement = NA, n_per_haul), 
      criteria = paste0("\n", criteria, "\n"))
  
  title <- paste0(title) # , "\n"
  
  names(dat000)[names(dat0) == "plan"] <- "Sampling plan"
  
  # prep for merged cells
  n_missing <- which(is.na(dat000$n_per_haul))
  dat000$n_per_haul[n_missing] <- dat000$criteria[n_missing]
  

  ft <- flextable::as_grouped_data(
      x = dat000, 
      groups = c("Sampling plan")) %>%
    flextable::as_flextable(x = .) %>% 
    flextable::theme_zebra(x = ., 
                           odd_header = "darkolivegreen3", 
                           even_header  = "darkolivegreen3",
                           odd_body = "lightblue") %>%
    
    
    # header
    flextable::add_header(x = ., 
                          n_per_haul = title,
                          criteria = title,
                          # "Sampling plan" = title,
                          species = title,
                          top = TRUE ) %>%
    flextable::merge_h(x = ., part = "header", i = 1) %>% 
    flextable::set_header_labels(x = .,
                                 "species" = "\nSpecies\n", 
                                 "n_per_haul" = "\nTarget #/Plan\n", 
                                 "criteria" = "\nRules\n") %>% 
    flextable::fontsize(x = ., i = 1, size = header_size, part = "header") %>% 
    flextable::fontsize(x = ., i = 2, size = subheader_size, part = "header") %>% 
    
    
    # table aesthetics
    flextable::valign(x = ., valign = "center", part = "all") %>% 
    flextable::font(x = ., fontname = "Arial") %>%
    flextable::fontsize(x = ., size = body_size, part = "body") %>% 
    flextable::bg(x = ., i = 1, bg = "white", part = "header") %>% 
    flextable::line_spacing(x = ., space = spacing, part = "all") %>% 
    flextable::padding(x = ., padding = pad, part = "all") %>% 
    flextable::padding(x = ., j="species", padding.left = 5) %>% 
    flextable::align(x = ., align = "center", part = "all") %>%
    flextable::align(x = ., j = "criteria", align = "left", part = "body") %>%
    flextable::align(x = ., j = "species", align = "left", part = "body") %>%
    flextable::align(x = ., align = "center", i = ~!is.na(`Sampling plan`)) %>%
    # flextable::hline(x = ., 
    #                  border = officer::fp_border(width = 0.5, color = "grey10"), 
    #                  part = "body") %>% 
    flextable::bold(x = ., j = 1, i = ~ !is.na(`Sampling plan`), part = "body") %>% 
    flextable::bg(x = ., j = 1, i = ~ !is.na(`Sampling plan`), 
                    bg = "blanchedalmond", part = "body")  %>% 
    # table sizing
    flextable::width(x = ., width = col_spacing, unit = "in") %>% # width = c(6,4,10)
    flextable::fit_to_width(x = .,
                            max_width = pgwidth,
                            # max_iter = 50,
                            unit = "in") # %>%
  
  
  
  if (length(n_missing)>0) {
    for (iii in length(n_missing)) {
      ft <- ft %>% 
        flextable::merge_at(x = .,
                            i = n_missing[iii]+1, # +1 bc extra header line
                            j = c("n_per_haul", "criteria"),
                            part = "body") %>% 
        flextable::align(x = ., 
                         j = "n_per_haul", 
                         i = n_missing[iii]+1, 
                         align = "left", 
                         part = "body") 
    }
  }
  # if (plan == "Sampling plan for all") {
  #   ft <- ft  %>% 
  #     flextable::bold(x = ., j = 1, i = ~ !is.na(`Sampling plan for all`), part = "body")   %>% 
  #     flextable::bg(x = ., j = 1, i = ~ !is.na(`Sampling plan for all`), 
  #                   bg = "blanchedalmond", part = "body")
  # } else {
    # ft <- ft  
  # }
  # flextable::autofit()
  
  return(ft)
  
}
