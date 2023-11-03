#===============================================================================
# 2023-11-03 -- urbrur-eu-c19
# prepare the data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

source("src/prepare-session.R")

# spatial data from EUROSTAT package -----------------------------------------------

gd_n3 <- eurostat_geodata_60_2016 %>%
    janitor::clean_names() %>%
    filter(
        ! str_sub(id, 1, 4) %in% remote,
        levl_code == 3
    ) %>%
    transmute(
        id, cntr = cntr_code, name = nuts_name
    ) %>%
    st_transform(crs = 3035)

gd_cntr <- gd_n3 %>%
    rmapshaper::ms_dissolve("cntr")

gd_bord <- gd_cntr %>% rmapshaper::ms_innerlines()


save(gd_n3, gd_cntr, gd_bord, file = "dat/eu-geodata.rda")


# eurostat n3 urb/rur -----------------------------------------------------

# https://ec.europa.eu/eurostat/web/nuts/history
# https://ec.europa.eu/eurostat/web/rural-development/methodology
# https://ec.europa.eu/eurostat/documents/35209/35256/Urban-rural-NUTS-2016.xlsx
eu_type <- readxl::read_excel(
    "dat/Urban-rural-NUTS-2016.xlsx",
    sheet = 2, skip = 4
) %>%
    set_colnames(c("id", "type", "cntr")) %>%
    mutate(type_c = type %>% paste)

save(eu_type, file = "dat/eu-type-urbrur.rda")


# check
gd_n3 %>%
    inner_join(eu_type, by = c("id", "cntr")) %>%
    ggplot(aes(fill = type_c))+
    geom_sf(size = NA)+
    # scale_fill_viridis_b()+
    theme_map()+
    theme(legend.position = c(.8, .5))

# get raw -----------------------------------------------------------------

pop_raw <- get_eurostat("demo_r_pjangrp3")
save(pop_raw, file = "dat/pop-raw.rda")

death_w_raw <- get_eurostat("demo_r_mweek3")
save(death_w_raw, file = "dat/death-w-raw.rda")

tictoc::tic()
load("dat/death-w-raw.rda")
tictoc::toc()

tictoc::tic()
load("dat/pop-raw.rda")
tictoc::toc()



# clean -------------------------------------------------------------------


# # show non-matching or missing categories
# anti_join(
#     pop_raw %>%
#         filter(!str_sub(geo, 3,3) == "X") %>%
#         mutate(cntr = str_sub(geo, 1,2)) %>%
#         distinct(cntr, age, time) %>%
#         complete(cntr, age, time) %>%
#         arrange(cntr, age, time)
#     ,
#     pop_raw %>%
#         filter(!str_sub(geo, 3,3) == "X") %>%
#         mutate(cntr = str_sub(geo, 1,2)) %>%
#         distinct(cntr, age, time) %>%
#         mutate(mark = "*")
# ) %>% view

pop <- pop_raw %>%
    mutate(cntr = str_sub(geo, 1, 2)) %>%
    filter(
        ! age %in% c("TOTAL", "UNK", "Y_GE85"),
        ! str_sub(geo, 3, 3) == "X",
        ! cntr == "AL", # open age is different 85+
        ! cntr == "UK", # no data for 2020 and 2021
        ! str_sub(geo, 1, 4) %in% remote
    ) %>%
    complete(geo, time, sex, age) %>%
    transmute(
        cntr,
        id = geo,
        year = time %>% lubridate::year(),
        sex = sex %>% str_to_lower() %>% str_replace("t", "b"),
        age = age %>%
            str_replace("Y_LT5", "Y0-4") %>%
            str_replace("Y_GE90", "Y90-99") %>%
            str_remove("Y"),
        pop = values
    ) %>%
    separate(age, into = c("age_begin", "age_end"), sep = "-") %>%
    arrange(id, year, sex) %>%
    mutate(
        sex = sex %>% as_factor,
        age_begin = age_begin %>% as.numeric,
        age_end = age_end %>% as.numeric
    ) %>%
    arrange(id, year, sex, age_begin)

save(pop, file = "dat/pop.rda")



# clean the huge dataframe of weekly deaths using {multidplyr} -------------------------------

library(multidplyr)
cluster <- new_cluster(7)


# 1. filter only the needed years
# partition
system.time(
    foo <- death_w %>% partition(cluster)
)
# execution
system.time(
    foo <- foo %>%
        # separate(time, into = c("year", "week"), sep = "W") %>%
        mutate(year = as.numeric(substr(time, 1, 4))) %>%
        filter(year %in% 2014:2021)
)
# collection
system.time(foo <- foo %>% collect())
# save the intermediate output
system.time(save(foo, file = "dat/death-w-filter.rda"))
# clear the previous dataframe
rm(death_w)

# 2. summarize over weeks -- x52 reduction
# partition
system.time(
    boo <- foo %>% partition(cluster)
)
# execution
system.time(
    boo <- boo %>%
        group_by(geo, sex, year, age) %>%
        summarise(death = sum(values, na.rm = TRUE)) %>%
        ungroup()
)
# collection
system.time(boo <- boo %>% collect())
# save the intermediate output
system.time(save(boo, file = "dat/death-w-summarize.rda"))
# clear the previous dataframe
rm(foo, cluster)

# 2.5 !!! apparently need to collapse over the 7 cores again
load("dat/death-w-summarize.rda")
# execution without {multidplyr}
system.time(
    boo <- boo %>%
        group_by(geo, sex, year, age) %>%
        summarise(death = sum(death, na.rm = TRUE)) %>%
        ungroup()
)

# save the intermediate output
system.time(save(boo, file = "dat/death-summarize.rda"))


# 3. final cleaning without parallel processing
system.time(
    death <- boo %>%
        mutate(
            cntr = str_sub(geo, 1, 2)
        ) %>%
        filter(
            ! age %in% c("TOTAL", "UNK", "Y_GE85"),
            ! str_sub(geo, 3, 3) == "X",
            ! cntr == "AL", # open age is different 85+
            ! cntr == "UK", # no data for 2020 and 2021
            ! str_sub(geo, 1, 4) %in% remote
        ) %>%
        transmute(
            cntr,
            id = geo,
            year,
            sex = sex %>% str_to_lower() %>% str_replace("t", "b"),
            age = age %>%
                str_replace("Y_LT5", "Y0-4") %>%
                str_replace("Y_GE90", "Y90-99") %>%
                str_remove("Y"),
            death
        ) %>%
        separate(age, into = c("age_begin", "age_end"), sep = "-") %>%
        arrange(id, year, sex) %>%
        mutate(
            sex = sex %>% as_factor,
            age_begin = age_begin %>% as.numeric,
            age_end = age_end %>% as.numeric
        ) %>%
        arrange(id, year, sex, age_begin)
)
# save the result
save(death, file = "dat/death.rda")
rm(boo)


# check consistency -------------------------------------------------------

# see in a separate elaborated script "prep-data-consistency.R"

# check data matching -----------------------------------------------------

# see in a separate elaborated script "prep-data-matching.R"



# join everything and aggregate -------------------------------------------

# aggregate over two-year periods and urbrur type

counts2y <- left_join(
    death %>%
        mutate(
            two_year = case_when(
                year %in% 2014:2015 ~ "2014-2015",
                year %in% 2016:2017 ~ "2016-2017",
                year %in% 2018:2019 ~ "2018-2019",
                year %in% 2020:2021 ~ "2020-2021",
            )
        ) %>%
        select(-year),
    pop %>%
        filter(year %in% seq(2015, 2021, 2)) %>%
        mutate(
            two_year = case_when(
                year %in% 2015 ~ "2014-2015",
                year %in% 2017 ~ "2016-2017",
                year %in% 2019 ~ "2018-2019",
                year %in% 2021 ~ "2020-2021",
            )
        )%>%
        select(-year)
) %>%
    right_join(eu_type, by = c("id", "cntr")) %>%
    group_by(cntr, type, sex, two_year, age_begin, age_end) %>%
    summarise(
        d = death %>% sum(na.rm = T),
        e = pop %>% sum(na.rm = T)
    ) %>%
    ungroup() %>%
    # remove Norway and Serbia since there is no population data for 2015 and 2021
    filter(!cntr %in% c("NO", "RS"))

save(counts2y, file = "dat/counts2y.rda")

# load back
load("dat/counts2y.rda")


# use topals --------------------------------------------------------------

# functions
source("src/fun-topals-fit.R")
source("src/fun-life-table.R")


# use HMD standards from
# https://schmert.net/topals-mortality/
# https://schmert.net/topals-mortality/data/HMDstd.csv
# # Just in case copy of the data in my gist ot the URL
# "https://gist.githubusercontent.com/ikashnitsky/71a6084808c1ac96ee96a2c187588105/raw/179a61e602f895b3feca3b61e56402eacc50863b/hmd-std.csv"
hmd_std <- read_csv(
    "https://schmert.net/topals-mortality/data/HMDstd.csv"
) %>%
    mutate(b = (m * f) %>% sqrt %>% multiply_by(-1))

hmd_std_b <- hmd_std$b %>% head(-1)
hmd_std_f <- hmd_std$f %>% head(-1)
hmd_std_m <- hmd_std$m %>% head(-1)

# data sex-specific
counts2y_f <- counts2y %>% filter(sex == "f")
counts2y_m <- counts2y %>% filter(sex == "m")
counts2y_b <- counts2y %>% filter(sex == "b")

# fit TOPALS
fit_f <- counts2y_f %>%
    group_by(cntr, type, two_year) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$e, D = .x$d,
            std = hmd_std_f,
            age_group_bounds = c(seq(0, 90, 5), 99),
            max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c() %>%
            tibble(logmx = .)
    })

fit_m <- counts2y_m %>%
    group_by(cntr, type, two_year) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$e, D = .x$d,
            std = hmd_std_m,
            age_group_bounds = c(seq(0, 90, 5), 99),
            max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c() %>%
            tibble(logmx = .)
    })

fit_b <- counts2y_b %>%
    group_by(cntr, type, two_year) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$e, D = .x$d,
            std = hmd_std_b,
            age_group_bounds = c(seq(0, 90, 5), 99),
            max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c() %>%
            tibble(logmx = .)
    })

# life tables
# calculate full life tables
lt_f <- fit_f %>%
    mutate(mx_fit = logmx %>% exp) %>%
    group_by(cntr, type, two_year) %>%
    group_modify(~ lt(mx = .x$mx_fit, sex = "f")) %>%
    ungroup()

lt_m <- fit_m %>%
    mutate(mx_fit = logmx %>% exp) %>%
    group_by(cntr, type, two_year) %>%
    group_modify(~ lt(mx = .x$mx_fit, sex = "m")) %>%
    ungroup()

lt_b <- fit_b %>%
    mutate(mx_fit = logmx %>% exp) %>%
    group_by(cntr, type, two_year) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()

save(lt_b, lt_f, lt_m, file = "dat/life-tables.rda")


load("dat/counts2y.rda")



# -------------------------------------------------------------------------
# THE FOLLOWING IS BOOTSTRAPING
# To lighten the github repository the ~2GB simulated data is not uploaded
# there. Please note, it will take considerable time to simulate this data
# locally again. The mentioned times are on my machine -- Mac Pro 2018
# -------------------------------------------------------------------------



# join standard and bootstrap death counts x500 from Poisson ~ 4 sec
tictoc::tic()
df_sim <- counts2y %>%
    mutate(type = type %>% paste) %>%
    left_join(
        hmd_std %>% pivot_longer(f:b, names_to = "sex"),
        by = c("sex", "age_begin" = "age")
    ) %>%
    drop_na() %>%
    group_by(cntr, two_year, sex, type) %>%
    expand_grid(sim_id = 1:5e2) %>%
    group_by(cntr, two_year, sex, type, age_begin) %>%
    mutate(death_sim = rpois(5e2, d)) %>%
    ungroup()
tictoc::toc()


# both sex –– MAIN analysis -----------------------------------------------


# fit TOPALS to the simulated death counts ~ took 7 min
tictoc::tic()
df_fit_sim_b <- df_sim %>%
    filter(sex == "b") %>%
    group_by(cntr, two_year, type, sim_id) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$e, D = .x$death_sim,
            std = hmd_std_m,
            age_group_bounds = c(seq(0, 90, 5), 99),
            max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c() %>%
            tibble(logmx = .)
    }) %>%
    ungroup()
tictoc::toc()
save(df_fit_sim_b, file = "dat/sim_fit_5e2_BOTH.rda")


# calculate life tables based on simulated data ~ 4 min
tictoc::tic()
df_lt_sim_b <- df_fit_sim_b %>%
    mutate(mx_fit = logmx %>% exp) %>%
    group_by(cntr, two_year, type, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()
save(df_lt_sim_b, file = "dat/sim_lt_5e2_BOTH.rda")


# FEMALES -----------------------------------------------------------------

# fit TOPALS to the simulated death counts ~ took 7 min
tictoc::tic()
df_fit_sim_f <- df_sim %>%
    filter(sex == "f") %>%
    group_by(cntr, two_year, type, sim_id) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$e, D = .x$death_sim,
            std = hmd_std_f,
            age_group_bounds = c(seq(0, 90, 5), 99),
            max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c() %>%
            tibble(logmx = .)
    }) %>%
    ungroup()
tictoc::toc()
save(df_fit_sim_f, file = "dat/sim_fit_5e2_FEMALES.rda")


# calculate life tables based on simulated data ~ 4 min
tictoc::tic()
df_lt_sim_f <- df_fit_sim_f %>%
    mutate(mx_fit = logmx %>% exp) %>%
    group_by(cntr, two_year, type, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()
save(df_lt_sim_f, file = "dat/sim_lt_5e2_FEMALES.rda")

# MALES -------------------------------------------------------------------


# fit TOPALS to the simulated death counts ~ took 7 min for males only
tictoc::tic()
df_fit_sim_m <- df_sim %>%
    filter(sex == "m") %>%
    group_by(cntr, two_year, type, sim_id) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$e, D = .x$death_sim,
            std = hmd_std_m,
            age_group_bounds = c(seq(0, 90, 5), 99),
            max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c() %>%
            tibble(logmx = .)
    }) %>%
    ungroup()
tictoc::toc()
save(df_fit_sim_m, file = "dat/sim_fit_5e2_MALES.rda")


# calculate life tables based on simulated data ~ 4 min
tictoc::tic()
df_lt_sim_m <- df_fit_sim_m %>%
    mutate(mx_fit = logmx %>% exp) %>%
    group_by(cntr, two_year, type, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()
save(df_lt_sim_m, file = "dat/sim_lt_5e2_MALES.rda")

