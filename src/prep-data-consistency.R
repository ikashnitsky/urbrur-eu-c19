#===============================================================================
# 2023-11-03 -- urbrur-eu-c19
# Check the consistency of Eurostats counts between yearly and weekly
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================


source("src/prepare-session.R")

load("dat/death-y-raw.rda")
load("dat/death.rda")



# check consistency with yearly estimates
load("dat/death-y-raw.rda")
load("dat/death.rda")

check_count_consistency <- function(country) {
    left_join(
        death %>%
            filter(sex == "b",
                   id == country) %>%
            group_by(year) %>%
            summarise(tot = death %>% sum),
        death_y_raw %>%
            filter(sex == "T",
                   geo == country,
                   age == "TOTAL") %>%
            transmute(id = geo,
                      year = time %>% lubridate::year(),
                      yt = values)
    ) %>%
        mutate(diff = tot - yt,
               rel = (tot - yt) %>% abs %>%
                   divide_by(yt) %>%
                   multiply_by(1e2)) %>%
        view
}

check_count_consistency("FR")
check_count_consistency("DE")
check_count_consistency("DK")
check_count_consistency("BG")
check_count_consistency("EE")

# more elaborated in a separate script

# check_count_consistency <- function(country) {
#     left_join(
#         death %>%
#             filter(sex == "b",
#                    id == country) %>%
#             group_by(year) %>%
#             summarise(tot = death %>% sum),
#         death_y_raw %>%
#             filter(sex == "T",
#                    geo == country,
#                    age == "TOTAL") %>%
#             transmute(id = geo,
#                       year = time %>% lubridate::year(),
#                       yt = values)
#     ) %>%
#         mutate(diff = tot - yt,
#                rel = (tot - yt) %>% abs %>%
#                    divide_by(yt) %>%
#                    multiply_by(1e2)) %>%
#         view
# }
#
# check_count_consistency("FR")
# check_count_consistency("DE")
# check_count_consistency("DK")
# check_count_consistency("BG")
# check_count_consistency("EE")

# only both sex
df_check <- left_join(
    death %>%
        filter(sex == "b", !year == 2021) %>%
        group_by(id, year) %>%
        summarise(tot = death %>% sum),
    death_y_raw %>%
        filter(sex == "T",
               age == "TOTAL") %>%
        transmute(id = geo,
                  year = time %>% lubridate::year(),
                  yt = values)
) %>%
    mutate(diff = tot - yt,
           rel = (tot - yt) %>% abs %>%
               divide_by(yt) %>%
               multiply_by(1e2))

df_check %>%
    ggplot(aes(year, rel))+
    geom_jitter()+
    geom_segment(
        data = . %>%
            group_by(year) %>%
            summarise(avg = rel %>% mean(na.rm = T)) %>%
            ungroup(),
        aes(xend = year, y = avg, yend = avg),
        color = 2
    )

df_check %>% group_by(year, cntr = str_sub(id, 1, 2)) %>%
    summarise(avg = rel %>% mean(na.rm = T) %>% round(1)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = avg) %>% view

    df_check %>%
    mutate(cntr = str_sub(id, 1, 2)) %>%
    group_by(cntr, year) %>%
    summarise(
        mean = rel %>% mean(na.rm = T),
        mean_w = weighted.mean(rel, yt, na.rm = T),
        min = rel %>% min(na.rm = T),
        max = rel %>% max(na.rm = T),
        q10 = rel %>% quantile(.1, na.rm = T),
        g90 = rel %>% quantile(.9, na.rm = T)
    ) %>% view

my_cntr <- df_check %>%
    mutate(cntr = str_sub(id, 1, 2)) %>%
    pull(cntr) %>% unique()


