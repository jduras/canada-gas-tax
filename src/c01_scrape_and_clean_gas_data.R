
# first and last year in sample
yfst <- 2013
ylst <- 2017

# scrape gasoline price data from https://charting.kentgroupltd.com/
gas_tbl_raw <-
    crossing(yr = yfst:ylst,
             grade = c("Unleaded", "Premium"),
             market = c("Retail%20(Incl.%20Tax)", "Retail%20(Excl.%20Tax)", "Wholesale")) %>%
    mutate(link = str_c("https://charting.kentgroupltd.com/WPPS/", grade, "/", market, "/WEEKLY/", yr, "/", grade, "_", market, "_WEEKLY_", yr, ".htm"),
           webpage = map(link, read_html),
           data = map(webpage, ~ .x %>% html_table() %>% purrr::pluck(1)))

# inspect the gasoline price data
gas_tbl_raw$data[[1]] %>%
    glimpse()

gas_tbl_raw$data[[1]] %>%
    slice(-(1:2)) %>%
    glimpse()

gas_tbl_raw$data[[1]] %>%
    slice(-(1:2)) %>%
    as_tibble() %>%
    select(1:2) %>%
    tail(15)

# tidy the gasoline price data
gas_tbl <-
    gas_tbl_raw %>%
    mutate(data_clean = map(data, ~.x %>%
                                as_tibble() %>%
                                slice(-(1:2)) %>%
                                filter(!str_detect(X1, "\\([SVP]\\)")) %>%
                                filter(!str_detect(X1, "S-Simple")) %>%
                                filter(!str_detect(X1, "prices as of")) %>%
                                filter(!str_detect(X1, "To print")) %>%
                                filter(X1 != "") %>%
                                distinct(X1, .keep_all = TRUE) %>%
                                gather(key, value, -X1) %>%
                                filter(key != key[value == "Average"]) %>%
                                group_by(key) %>%
                                mutate(year = X1[str_detect(X1, "DATE")] %>% parse_number(),
                                       date = value[str_detect(X1, "DATE")] %>% str_c(as.character(year), "/", .) %>% as.Date(format = "%Y/%m/%d")) %>%
                                filter(value != X1) %>%
                                filter(X1 != "CANADA AVERAGE") %>%
                                filter(!is.na(date)) %>%
                                ungroup() %>%
                                filter(!str_detect(X1, "DATE")) %>%
                                mutate(value = parse_number(value)) %>%
                                select(date, city = X1, price = value)))

save(gas_tbl_raw, gas_tbl, file = str_c(here(), "/data/gas_tbl_raw.Rdata"))


# cities, provinces and territories data
cities_tbl <-
    fromJSON("data/cities/jprichardson/cities.json") %>%
    as_tibble() %>%
    rename(city = V1,
           state = V2)

state_tbl <-
    bind_rows(fromJSON("data/cities/jprichardson/provinces.json") %>%
                  enframe(name = "state", value = "state_name") ,
              fromJSON("data/cities/jprichardson/territories.json") %>%
                  enframe(name = "state", value = "state_name")) %>%
    unnest()

# use string comparison algorithm by Jaro and Winkler to match city names with proper names
matched_cities_tbl <-
    crossing(gas_tbl %>%
                 select(yr, grade, market, data_clean) %>%
                 unnest() %>%
                 distinct(city),
             cities_tbl %>%
                 rename(city_name = city)) %>%
    mutate(jw = jarowinkler(city, city_name)) %>%
    group_by(city) %>%
    arrange(city, desc(jw)) %>%
    slice(1) %>%
    ungroup()

# check matches with imperfect fit (Jaro and Winkler similarity measure)
matched_cities_tbl %>%
    print(n = 100) %>%
    filter(jw < 1) %>%
    arrange(jw) %>%
    print(n = 100)

# cities with multiple different spellings in gasoline data
matched_cities_tbl %>%
    count(city_name) %>%
    filter(n > 1)

# add matched city names to gasoline data
gas_tbl_clean <-
    gas_tbl %>%
    select(yr, grade, market, data_clean) %>%
    unnest() %>%
    left_join(matched_cities_tbl, by = "city") %>%
    left_join(state_tbl, by = "state") %>%
    select(yr, date, city_name, city, state, state_name, grade, market, price)

# number of cities in gasoline data, by province/territory
gas_tbl_clean %>%
    count(state, state_name, city_name) %>%
    count(state, state_name) %>%
    filter(state %in% c("BC", "AB", "SK", "MB"))

gas_tbl_clean %>%
    filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    count(state, city_name)

gas_tbl_clean %>%
    filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    filter(grade == "Premium") %>%
    spread(market, price)

write_csv(gas_tbl_clean, path = str_c(here(), "/data/gas_tbl_clean.csv"))

