
library(babynames)
library(multidplyr)
cluster <- new_cluster(7)

foo <- babynames

# with multidplyr
system.time(
    boo <- foo %>%
        group_by(sex, name) %>% # need grouping before cluster
        partition(cluster)
)
# execution
system.time(
    boo <- boo %>%

        summarise(n_total = sum(n, na.rm = T)) %>%
        ungroup()
)
# collection
system.time(boo <- boo %>% collect())

# without multidplyr
system.time(
    coo <- foo %>%
        group_by(sex, name) %>%
        summarise(n_total = sum(n, na.rm = T)) %>%
        ungroup()
)

system.time(
    xoo <- boo %>%
        group_by(sex, name) %>%
        summarise(n_total = sum(n, na.rm = T)) %>%
        ungroup()
)

