
# load data on gas prices, gas taxes, and distances between cities
gas_weekly_clean <- read_csv(str_c(here(), "/data/tbl_gas_weekly_clean.csv"))
tax_clean <- read_csv(str_c(here(), "/data/tbl_tax_clean.csv"))
cities_distances_clean <- read_csv(str_c(here(), "/data/tbl_distance_clean.csv"))

# cities with missing wholesale price
gas_weekly_clean_missing <-
    gas_weekly_clean %>%
    filter(market == "wholesale", is.na(price))

gas_weekly_clean_missing %>%
    mutate(yr = year(date)) %>%
    count(yr, city_name, grade) %>%
    print(n = 50)

# for each city find 10 closest cities
cities_distances_ranked <-
    bind_rows(cities_distances_clean,
              cities_distances_clean %>%
                  rename(city_name_destination = city_name_origin,
                         state_destination = state_origin,
                         city_name_origin = city_name_destination,
                         state_origin = state_destination)) %>%
    # filter(state_origin %in% c("BC", "AB", "SK", "MB")) %>%
    arrange(city_name_origin, distance) %>%
    group_by(city_name_origin) %>%
    slice(1:10) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    select(-distance)

# for each city with missing wholesale price add the wholesale prices for 10 closest cities
gas_weekly_clean_missing_closest <-
    gas_weekly_clean_missing %>%
    left_join(cities_distances_ranked,
              by = c("city_name" = "city_name_origin")) %>%
    left_join(gas_weekly_clean %>%
                  filter(market == "wholesale") %>%
                  select(date, city_name_destination = city_name, grade, price_destination = price),
              by = c("date", "city_name_destination", "grade"))

# check which cities don't have any wholesale price info for 10 closest cities
gas_weekly_clean_missing_closest %>%
    group_by(date, city_name, grade) %>%
    filter(all(is.na(price_destination))) %>%
    filter(rank == 1) %>%
    ungroup() %>%
    # count(date, city_name, grade)
    mutate(yr = year(date)) %>%
    count(yr, city_name, grade) %>%
    print(n = 50)

# impute missing wholesale prices as wholesale price in the closest city
gas_weekly_clean_missing_imputed <-
    gas_weekly_clean_missing_closest %>%
    group_by(date, city_name, grade) %>%
    filter(!all(is.na(price_destination))) %>%
    mutate(rank_imputed = min(rank[!is.na(price_destination)], na.rm = TRUE),
           price_imputed = price_destination[rank == rank_imputed]) %>%
    filter(rank == rank_imputed) %>%
    ungroup() %>%
    select(date, city_name, state, state_name, grade, market, city_name_destination, rank_imputed, price_imputed)

# add gas tax information
combined_weekly_clean <-
    gas_weekly_clean %>%
    left_join(gas_weekly_clean_missing_imputed,
              by = c("date", "city_name", "state", "state_name", "grade", "market")) %>%
    left_join(tax_clean %>%
                  as_tsibble(index = date, key = id(state, state_name)) %>%
                  fill_gaps(.full = TRUE) %>%
                  fill(tax, .direction = "down") %>%
                  as_tibble(),
              by = c("state", "date")) %>%
    select(yr, date, city_name, state, state_name = state_name.x, market, grade, tax, price, price_imputed)



#### plots ####

combined_weekly_clean %>%
    filter(is.na(price), is.na(price_imputed)) %>%
    count(yr, city_name, market, grade) %>%
    print(n = 150)

combined_weekly_clean %>%
    filter(state_name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")) %>%
    ggplot(aes(x = date, y = price, group = city_name, col = state_name)) +
        geom_line(alpha = 0.5) +
        facet_grid(grade~market)
