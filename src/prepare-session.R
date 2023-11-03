#===============================================================================
# 2023-11-03 -- urbrur-eu-c19
# prepare session
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr)
library(eurostat)
library(sf)
library(cowplot)
library(patchwork)
library(hrbrthemes)
library(prismatic)
library(ggflags)

library(showtext)
sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()


# constants ---------------------------------------------------------------

# nuts 2016 remote
remote <- c(paste0('ES',c(63,64,70)),paste('FRY',1:5,sep=''),'PT20','PT30')


# visual theming ----------------------------------------------------------

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
theme_set(theme_ik(base_family = "ah"))

theme_for_facets <- theme_ik(base_family = font_rc)+
    theme(
        # plot.background = element_rect(fill = "#dadada", color = NA),
        legend.position = "none",
        panel.spacing = unit(.5, "lines"),
        panel.grid.minor = element_blank(),
        line = element_line(lineend = "round")
    )


# colors ------------------------------------------------------------------


# pal_3 <- c("#3C4565FF", "#4db6ac", "#FFE37CFF")
# 886457
# pal_3 <- c("#3C4565FF", "#82E2D8FF", "#B29906FF")
#
# pal_3 <- c("#03463A", "#9ccc65", "#81684D")

# pal_3 <- c("#4C1D4BFF", "#CB1B4FFF", "#F69C73FF" %>% clr_darken(.1))

# pal_3 <- viridis::viridis(3, begin = .2, end = .8, option = "F")

pal_3 <- viridis::viridis(3, begin = .05, end = .85, option = "D")


# pal_3 %>% clr_grayscale()
# pal_3 %>% clr_deutan()
# pal_3 %>% clr_deutan()%>% clr_grayscale()
