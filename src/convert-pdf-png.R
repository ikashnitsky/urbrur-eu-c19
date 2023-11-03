#===============================================================================
# 2023-09-22 -- urb/rur
# convert pdf to png
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(pdftools)
library(fs)
library(tidyverse)

pdfs <- dir_ls("fig") %>%
    str_subset(".pdf")

convert_pdf_to_png <- function(path_to_pdf){
    path = path_to_pdf %>% paste
    pdf_convert(pdf = path, filenames = path %>% str_replace(".pdf", ".png"), dpi = 300)
}


pdfs %>% map(convert_pdf_to_png)
