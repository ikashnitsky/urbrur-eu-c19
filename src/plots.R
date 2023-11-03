#===============================================================================
# 2023-11-03 -- urbrur-eu-c19
# plot the results
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================


source("src/prepare-session.R")

load("dat/life-tables.rda")



# small multiples –- changes in e0 ----------------------------------------



# BOTH sex -- all years ---------------------------------------------------

df_lt_b_0 <- lt_b %>%
    filter(age == 0) %>%
    mutate(
        country = cntr %>%
            str_replace("EL", "GR") %>%
            countrycode::countrycode(
                origin = "iso2c", destination = "country.name"
            ),
        type_c = type %>% as.factor() %>%
            lvls_revalue(c("URB", "IN", "RUR"))
    )



# plot small multiples
df_lt_b_0 %>%
    ggplot(aes(two_year, ex, color = type_c, group = type_c), shape = 1)+
    geom_line()+
    geom_point(size = 1)+
    scale_x_discrete(position = "top")+
    scale_y_continuous(breaks = 1:100)+
    scale_color_manual(
        values = pal_3,
        guide = guide_legend(direction = "horizontal")
    )+
    facet_wrap(~ country, ncol = 5, scales = "free_y")+
    theme_for_facets +
    theme(
        axis.text = element_blank(),
        legend.position = c(.7, .08)
    )+
    labs(
        x = "Two-year periods: 2014-15, 2016-17, 2018-19, 2020-21",
        y = "Life expectancy at birth",
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0-cntr-BOTH.pdf",
    width = 7, height = 4
)

# filter only counties with URB RUR
df_lt_b_0 %>%
    filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
    ggplot(aes(two_year, ex, color = type_c, group = type_c), shape = 1)+
    geom_line()+
    geom_point(size = 1)+
    scale_x_discrete(position = "top")+
    scale_y_continuous(breaks = 1:100)+
    scale_color_manual(
        values = pal_3,
        guide = guide_legend(direction = "horizontal")
    )+
    facet_wrap(~ country, ncol = 5, scales = "free_y")+
    theme_for_facets +
    theme(
        axis.text = element_blank(),
        legend.position = "bottom"
    )+
    labs(
        x = "Two-year periods: 2014-15, 2016-17, 2018-19, 2020-21",
        y = "Life expectancy at birth",
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0-cntr-filter-BOTH.pdf",
    width = 7, height = 4
)



# Females -- all years ----------------------------------------------------

df_lt_f_0 <- lt_f %>%
    filter(age == 0) %>%
    mutate(
        country = cntr %>%
            str_replace("EL", "GR") %>%
            countrycode::countrycode(
                origin = "iso2c", destination = "country.name"
            ),
        type_c = type %>% as.factor() %>%
            lvls_revalue(c("URB", "IN", "RUR"))
    )


# filter only counties with URB RUR
df_lt_f_0 %>%
    filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
    ggplot(aes(two_year, ex, color = type_c, group = type_c), shape = 1)+
    geom_line()+
    geom_point(size = 1)+
    scale_x_discrete(position = "top")+
    scale_y_continuous(breaks = 1:100)+
    scale_color_manual(
        values = pal_3,
        guide = guide_legend(direction = "horizontal")
    )+
    facet_wrap(~ country, ncol = 5, scales = "free_y")+
    theme_for_facets +
    theme(
        axis.text = element_blank(),
        legend.position = "bottom"
    )+
    labs(
        x = "Two-year periods: 2014-15, 2016-17, 2018-19, 2020-21",
        y = "FEMALE life expectancy at birth",
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0-cntr-filter-FEMALES.pdf",
    width = 7, height = 4
)


# MALES -- all years ----------------------------------------------------

df_lt_m_0 <- lt_m %>%
    filter(age == 0) %>%
    mutate(
        country = cntr %>%
            str_replace("EL", "GR") %>%
            countrycode::countrycode(
                origin = "iso2c", destination = "country.name"
            ),
        type_c = type %>% as.factor() %>%
            lvls_revalue(c("URB", "IN", "RUR"))
    )


# filter only counties with URB RUR
df_lt_m_0 %>%
    filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
    ggplot(aes(two_year, ex, color = type_c, group = type_c), shape = 1)+
    geom_line()+
    geom_point(size = 1)+
    scale_x_discrete(position = "top")+
    scale_y_continuous(breaks = 1:100)+
    scale_color_manual(
        values = pal_3,
        guide = guide_legend(direction = "horizontal")
    )+
    facet_wrap(~ country, ncol = 5, scales = "free_y")+
    theme_for_facets +
    theme(
        axis.text = element_blank(),
        legend.position = "bottom"
    )+
    labs(
        x = "Two-year periods: 2014-15, 2016-17, 2018-19, 2020-21",
        y = "MALE life expectancy at birth",
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0-cntr-filter-MALE.pdf",
    width = 7, height = 4
)


# confidence intervals ----------------------------------------------------



# load back
load("dat/sim_lt_5e2_BOTH.rda")
load("dat/sim_lt_5e2_FEMALES.rda")
load("dat/sim_lt_5e2_MALES.rda")


# MAIN -- both sex  -------------------------------------------------------

# # life expectancy with 95% CI
# df_e0_ci <- df_lt_sim %>%
#     filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
#     filter(age == 0) %>%
#     group_by(cntr, two_year, type) %>%
#     summarise(
#         e0_025 = ex %>% quantile(.025),
#         e0_5 = ex %>% quantile(.5),
#         e0_975 = ex %>% quantile(.975)
#     ) %>%
#     left_join(
#         lt_m %>%
#             filter(age == 0) %>%
#             mutate(type = type %>% paste) %>%
#             select(cntr, two_year, type, ex)
#     ) %>%
#     mutate(
#         country = cntr %>%
#             str_replace("EL", "GR") %>%
#             countrycode::countrycode(
#                 origin = "iso2c", destination = "country.name"
#             ),
#         type_c = type %>% as.factor() %>%
#             lvls_revalue(c("URB", "IN", "RUR"))
#     )


# changes in life expectancy 2018-2019 -- 2020-2021 with 95% CI
df_e0diff_ci_b <- df_lt_sim_b %>%
    filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
    filter(age == 0, two_year %in% c("2018-2019", "2020-2021")) %>%
    group_by(cntr, type, sim_id) %>%
    mutate(diff = ex %>% diff) %>%
    ungroup() %>%
    group_by(cntr, type) %>%
    summarise(
        diff_025 = diff %>% quantile(.025),
        diff_5 = diff %>% quantile(.5),
        diff_975 = diff %>% quantile(.975)
    ) %>%
    ungroup() %>%
    mutate(
        country = cntr %>%
            str_replace("EL", "GR") %>%
            countrycode::countrycode(
                origin = "iso2c", destination = "country.name"
            ),
        type_c = type %>% as.factor() %>%
            lvls_revalue(c("URB", "IN", "RUR"))
    )


df_e0diff_ci_b %>%
    mutate(country = country %>% as_factor %>% fct_reorder(diff_5)) %>%
    ggplot(aes(diff_5, y = country, color = type_c, group = type_c))+
    geom_hline(
        data = . %>%
            arrange(country) %>%
            distinct(country) %>%
            mutate(
                id = seq_along(country)
            ) %>%
            filter(!(id %% 2) == 0),
        aes(yintercept = country), size = 8, color = "#ccffff"
    )+
    geom_vline(xintercept = 0, size = 2, alpha = 3/4, color = "#aaffff")+
    geom_point(size = .1, alpha = .01)+
    geom_segment(
        data = . %>% filter(type_c == "URB"),
        position = position_nudge(y = .25),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "URB"),
        position = position_nudge(y = .25)
    )+
    geom_segment(
        data = . %>% filter(type_c == "IN"),
        position = position_nudge(y = 0),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "IN"),
        position = position_nudge(y = 0)
    )+
    geom_segment(
        data = . %>% filter(type_c == "RUR"),
        position = position_nudge(y = -.25),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "RUR"),
        position = position_nudge(y = -.25)
    )+
    scale_x_continuous(position = "top")+
    scale_color_manual(values = pal_3)+
    theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.text = element_text(face = 2)
    )+
    labs(
        x = "Difference in life expectancy at birth between 2018-19 and 2020-21",
        y = NULL,
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0diff-cntr-ci-BOTH.pdf",
    width = 7, height = 6
)



# FEMALES -----------------------------------------------------------------

# changes in life expectancy 2018-2019 -- 2020-2021 with 95% CI
df_e0diff_ci_f <- df_lt_sim_f %>%
    filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
    filter(age == 0, two_year %in% c("2018-2019", "2020-2021")) %>%
    group_by(cntr, type, sim_id) %>%
    mutate(diff = ex %>% diff) %>%
    ungroup() %>%
    group_by(cntr, type) %>%
    summarise(
        diff_025 = diff %>% quantile(.025),
        diff_5 = diff %>% quantile(.5),
        diff_975 = diff %>% quantile(.975)
    ) %>%
    ungroup() %>%
    mutate(
        country = cntr %>%
            str_replace("EL", "GR") %>%
            countrycode::countrycode(
                origin = "iso2c", destination = "country.name"
            ),
        type_c = type %>% as.factor() %>%
            lvls_revalue(c("URB", "IN", "RUR"))
    )


df_e0diff_ci_f %>%
    mutate(country = country %>% as_factor %>% fct_reorder(diff_5)) %>%
    ggplot(aes(diff_5, y = country, color = type_c, group = type_c))+
    geom_hline(
        data = . %>%
            arrange(country) %>%
            distinct(country) %>%
            mutate(
                id = seq_along(country)
            ) %>%
            filter(!(id %% 2) == 0),
        aes(yintercept = country), size = 8, color = "#ccffff"
    )+
    geom_vline(xintercept = 0, size = 2, alpha = 3/4, color = "#aaffff")+
    geom_point(size = .1, alpha = .01)+
    geom_segment(
        data = . %>% filter(type_c == "URB"),
        position = position_nudge(y = .25),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "URB"),
        position = position_nudge(y = .25)
    )+
    geom_segment(
        data = . %>% filter(type_c == "IN"),
        position = position_nudge(y = 0),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "IN"),
        position = position_nudge(y = 0)
    )+
    geom_segment(
        data = . %>% filter(type_c == "RUR"),
        position = position_nudge(y = -.25),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "RUR"),
        position = position_nudge(y = -.25)
    )+
    scale_x_continuous(position = "top")+
    scale_color_manual(values = pal_3)+
    theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.text = element_text(face = 2)
    )+
    labs(
        x = "Difference in FEMALE life expectancy at birth between 2018-19 and 2020-21",
        y = NULL,
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0diff-cntr-ci-FEMALES.pdf",
    width = 7, height = 6
)



# MALES -----------------------------------------------------------------

# changes in life expectancy 2018-2019 -- 2020-2021 with 95% CI
df_e0diff_ci_m <- df_lt_sim_m %>%
    filter(! cntr %in% c("CY", "LI", "LU", "ME", "EE", "IS")) %>%
    filter(age == 0, two_year %in% c("2018-2019", "2020-2021")) %>%
    group_by(cntr, type, sim_id) %>%
    mutate(diff = ex %>% diff) %>%
    ungroup() %>%
    group_by(cntr, type) %>%
    summarise(
        diff_025 = diff %>% quantile(.025),
        diff_5 = diff %>% quantile(.5),
        diff_975 = diff %>% quantile(.975)
    ) %>%
    ungroup() %>%
    mutate(
        country = cntr %>%
            str_replace("EL", "GR") %>%
            countrycode::countrycode(
                origin = "iso2c", destination = "country.name"
            ),
        type_c = type %>% as.factor() %>%
            lvls_revalue(c("URB", "IN", "RUR"))
    )


df_e0diff_ci_m %>%
    mutate(country = country %>% as_factor %>% fct_reorder(diff_5)) %>%
    ggplot(aes(diff_5, y = country, color = type_c, group = type_c))+
    geom_hline(
        data = . %>%
            arrange(country) %>%
            distinct(country) %>%
            mutate(
                id = seq_along(country)
            ) %>%
            filter(!(id %% 2) == 0),
        aes(yintercept = country), size = 8, color = "#ccffff"
    )+
    geom_vline(xintercept = 0, size = 2, alpha = 3/4, color = "#aaffff")+
    geom_point(size = .1, alpha = .01)+
    geom_segment(
        data = . %>% filter(type_c == "URB"),
        position = position_nudge(y = .25),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "URB"),
        position = position_nudge(y = .25)
    )+
    geom_segment(
        data = . %>% filter(type_c == "IN"),
        position = position_nudge(y = 0),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "IN"),
        position = position_nudge(y = 0)
    )+
    geom_segment(
        data = . %>% filter(type_c == "RUR"),
        position = position_nudge(y = -.25),
        aes(x = diff_025, xend = diff_975, yend = country),
        size = 2, alpha = .5
    )+
    geom_point(
        data = . %>% filter(type_c == "RUR"),
        position = position_nudge(y = -.25)
    )+
    scale_x_continuous(position = "top")+
    scale_color_manual(values = pal_3)+
    theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.text = element_text(face = 2)
    )+
    labs(
        x = "Difference in MALE life expectancy at birth between 2018-19 and 2020-21",
        y = NULL,
        color = "NUTS-3 urb/rur type:"
    )

ggsave(
    "fig/urbrur-e0diff-cntr-ci-MALES.pdf",
    width = 7, height = 6
)




