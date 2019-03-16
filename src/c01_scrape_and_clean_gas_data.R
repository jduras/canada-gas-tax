
# scrape gasoline price data from https://charting.kentgroupltd.com/

#### get daily data ####

# available in html format until June 2017 and in xls format from June 2017 onward
gas_daily_raw <-
    crossing(yr = yfst_daily:ylst_daily,
             grade = c("Unleaded", "Premium"),
             market = c("Retail%20(Incl.%20Tax)", "Retail%20(Excl.%20Tax)", "Wholesale")) %>%
    mutate(link = str_c("https://charting.kentgroupltd.com/WPPS/", grade, "/", market, "/DAILY/", yr, "/", grade, "_", market, "_DAILY_", yr, ".htm"),
           webpage = map(link, ~read_html(RETRY("GET", url = .x))),
           data = map(webpage, ~html_table(.x) %>% purrr::pluck(1)))

gas_daily_raw %>%
    filter(map_lgl(data, is.null))



#### get weekly gas data ####

# available in html format until June 2017 and in xls format from June 2017 onward
gas_weekly_raw <-
    crossing(yr = yfst_weekly:ylst_weekly,
             grade = c("Unleaded", "Premium"),
             market = c("Retail%20(Incl.%20Tax)", "Retail%20(Excl.%20Tax)", "Wholesale")) %>%
    mutate(link = str_c("https://charting.kentgroupltd.com/WPPS/", grade, "/", market, "/WEEKLY/", yr, "/", grade, "_", market, "_WEEKLY_", yr, ".htm"),
           webpage = map(link, ~read_html(RETRY("GET", url = .x))),
           data = map(webpage, ~html_table(.x) %>% purrr::pluck(1)))

gas_weekly_raw %>%
    filter(map_lgl(data, is.null))

# inspect the weekly gasoline price data
gas_weekly_raw$data[[1]] %>%
    glimpse()

gas_weekly_raw$data[[1]] %>%
    slice(-(1:2)) %>%
    glimpse()

gas_weekly_raw$data[[1]] %>%
    slice(-(1:2)) %>%
    as_tibble() %>%
    select(1:2) %>%
    tail(15)

# tidy the weekly gasoline price data
gas_weekly <-
    gas_weekly_raw %>%
    filter(!map_lgl(data, is.null)) %>%
    filter(yr >= 2010) %>%
    mutate(market = case_when(market == "Retail%20(Excl.%20Tax)" ~ "retail_exc_tax",
                              market == "Retail%20(Incl.%20Tax)" ~ "retail_inc_tax",
                              market == "Wholesale" ~ "wholesale"),
            data_clean = map(data, ~.x %>%
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

save(gas_weekly_raw, gas_weekly, file = str_c(here(), "/data/tbl_gas_weekly_raw.Rdata"))


# 2017 weekly gas data from https://charting.kentgroupltd.com/
gas_weekly_raw_2017 <-
    str_c(here(), "/data/gas/Premium_Retail (Excl. Tax)_WEEKLY_2017.xlsx") %>%
    read_xlsx()
gas_weekly_raw_2017



#### get cities, provinces and territories data ####

cities <-
    fromJSON("data/cities/jprichardson/cities.json") %>%
    as_tibble() %>%
    rename(city = V1,
           state = V2)

state <-
    bind_rows(fromJSON("data/cities/jprichardson/provinces.json") %>%
                  enframe(name = "state", value = "state_name") ,
              fromJSON("data/cities/jprichardson/territories.json") %>%
                  enframe(name = "state", value = "state_name")) %>%
    unnest() %>%
    mutate(state_name = str_to_title(state_name))



#### clean up city names ####

# use string comparison algorithm by Jaro and Winkler to match city names with proper names
matched_cities <-
    crossing(gas_weekly %>%
                 select(yr, grade, market, data_clean) %>%
                 unnest() %>%
                 distinct(city),
             cities %>%
                 rename(city_name = city)) %>%
    mutate(jw = jarowinkler(city, city_name)) %>%
    group_by(city) %>%
    arrange(city, desc(jw)) %>%
    slice(1) %>%
    ungroup()

# check matches with imperfect fit (Jaro and Winkler similarity measure)
matched_cities %>%
    print(n = 100) %>%
    filter(jw < 1) %>%
    arrange(jw) %>%
    print(n = 100)

# cities with multiple different spellings in gasoline data
matched_cities %>%
    count(city_name) %>%
    filter(n > 1)

# add matched city names to gasoline data
gas_weekly_clean <-
    gas_weekly %>%
    select(yr, grade, market, data_clean) %>%
    unnest() %>%
    left_join(matched_cities, by = "city") %>%
    left_join(state, by = "state") %>%
    mutate(city = str_to_title(city),
           city_name = str_to_title(city_name)) %>%
    select(yr, date, city_name, city, state, state_name, grade, market, price)

# number of cities in gasoline data, by province/territory
gas_weekly_clean %>%
    filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    count(state, state_name, city_name) %>%
    count(state, state_name)

gas_weekly_clean %>%
    filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    count(state, city_name)

write_csv(gas_weekly_clean, path = str_c(here(), "/data/tbl_gas_weekly_clean.csv"))
# gas_weekly_clean <- read_csv(str_c(here(), "/data/tbl_gas_weekly_clean.csv"))



#### plots ####

# plot prices as time series
gas_weekly_clean %>%
    mutate(is_AB = state == "AB") %>%
    ggplot(aes(x = date, y = price, group = city, col = is_AB)) +
        geom_line(alpha = 0.3) +
        scale_color_manual(values = c("gray50", "red")) +
        facet_grid(grade ~ market)

gas_weekly_clean %>%
    filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    ggplot(aes(x = date, y = price, group = city, col = state_name)) +
        geom_line(alpha = 0.5) +
        facet_grid(grade ~ market)
