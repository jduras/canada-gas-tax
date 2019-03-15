
gas_weekly_clean <- read_csv(str_c(here(), "/data/tbl_gas_weekly_clean.csv"))

# perform request to the Google Maps Distance Matrix API to obtain distances between cities with missing wholesale gas price
cities_distances_raw <-
    gas_weekly_clean %>%
    select(city_name, state) %>%
    distinct() %>%
    crossing(., .) %>%
    mutate(city_name_origin = str_replace_all(city_name, " ", fixed("+")),
           state_origin = state,
           city_name_destination = str_replace_all(city_name1, " ", fixed("+")),
           state_destination = state1) %>%
    filter(city_name_origin < city_name_destination) %>%
    mutate(api_link = str_c("https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial",
                            "&origins=", city_name_origin, ",", state_origin,
                            "&destinations=", city_name_destination, ",", state_destination,
                            "&key=", google_api_key),
           webpage = map(api_link, possibly(read_html, otherwise = NA)))

cities_distances_clean <-
    cities_distances_raw %>%
    mutate(record_json = map(webpage, ~html_nodes(.x, "p") %>%
                                 html_text() %>%
                                 fromJSON()),
           distance = map_chr(record_json, ~purrr::pluck(.x, "rows", "elements", 1 ,"distance", "text")) %>% parse_number()) %>%
    select(city_name_origin = city_name, state_origin, city_name_destination = city_name1, state_destination, distance)

save(cities_distances_raw, cities_distances_clean, file = str_c(here(), "/data/tbl_cities_distances_raw.Rdata"))
write_csv(cities_distances_clean, path = str_c(here(), "/data/tbl_distance_clean.csv"))
