#===============================================================================
# 2023-11-03 -- urbrur-eu-c19
# prepare the data -- check data matchig
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

source("src/prepare-session.R")


load("dat/eu-geodata.rda")
load("dat/eu-type-urbrur.rda")
load("dat/pop.rda")
load("dat/death.rda")

# check matching nuts3
gd_n3 %>%
    left_join(eu_type, by = "id") %>%
    left_join(
        death %>% distinct(id) %>% mutate(match = 1)
    ) %>%
    ggplot()+
    geom_sf(aes(fill = match), size = NA)+
    geom_sf(data = gd_bord, color = "#ffffff")+
    scale_fill_viridis_c(option = "B", guide = guide_none())+
    theme_map()+
    labs(title = "Deaths")

p_match_d <- last_plot()

gd_n3 %>%
    left_join(eu_type, by = "id") %>%
    left_join(
        pop %>% distinct(id) %>% mutate(match = 1)
    ) %>%
    ggplot()+
    geom_sf(aes(fill = match), size = NA)+
    geom_sf(data = gd_bord, color = "#ffffff")+
    scale_fill_viridis_c(option = "G", guide = guide_none())+
    theme_map()+
    labs(title = "Population")

p_match_p <- last_plot()

p_match <- p_match_p + p_match_d


ggsave(
    "fig/matching-nuts3.png", p_match,
    width = 7, height = 4
)

# check
gd_n3 %>%
    left_join(eu_type, by = "id") %>%
    ggplot(aes(fill = type %>% paste))+
    geom_sf(size = NA)+
    scale_fill_viridis_d(option = "G")+
    theme_map()+
    labs(title = "Population")


# degurba map -------------------------------------------------------------

load("~/data/degurba/degurba-2018.rda")

degurba_s <- degurba %>%
    rmapshaper::ms_simplify(.2)

save(degurba_s, file = "dat/degurba_s.rda")


# degurba map
degurba_s %>%
    drop_na(dgurba) %>%
    filter(!dgurba == 9) %>%
    st_transform(crs = 3035) %>%
    ggplot(aes(fill = dgurba))+
    geom_sf(size = NA)+
    scale_fill_viridis_b(option = "D")+
    # scale_fill_manual(values = pal_3)+
    theme_map()+
    labs(fill = "DEGURBA")


ggsave(
    "fig/degurba-map.png",
    width = 12, height = 9
)

