
# scrape gasoline price data from https://charting.kentgroupltd.com/

#### scrape daily data ####

# available in html format until June 2017 and in xls format from June 2017 onward
gas_daily_raw_html <-
    crossing(yr = yfst_daily:ylst_daily,
             grade = c("Unleaded", "Premium"),
             market = c("Retail%20(Incl.%20Tax)", "Retail%20(Excl.%20Tax)", "Wholesale")) %>%
    mutate(link = str_c("https://charting.kentgroupltd.com/WPPS/", grade, "/", market, "/DAILY/", yr, "/", grade, "_", market, "_DAILY_", yr, ".htm"),
           webpage = map(link, ~read_html(RETRY("GET", url = .x))),
           data = map(webpage, ~html_table(.x) %>% purrr::pluck(1)))

gas_daily_raw_html %>%
    filter(map_lgl(data, is.null))



#### scrape weekly gas data ####

# available in html format until June 2017 and in xlsx format from January 2017 onward
gas_weekly_raw_html <-
    crossing(yr = yfst_weekly:2016,
             grade = c("Unleaded", "Premium"),
             market = c("Retail%20(Incl.%20Tax)", "Retail%20(Excl.%20Tax)", "Wholesale")) %>%
    mutate(link = str_c("https://charting.kentgroupltd.com/WPPS/", grade, "/", market, "/WEEKLY/", yr, "/", grade, "_", market, "_WEEKLY_", yr, ".htm"),
           webpage = map(link, ~read_html(RETRY("GET", url = .x))),
           data = map(webpage, ~html_table(.x) %>% purrr::pluck(1)))

gas_weekly_raw_html %>%
    filter(map_lgl(data, is.null))

# inspect the weekly gasoline price data
gas_weekly_raw_html$data[[1]] %>%
    glimpse()

gas_weekly_raw_html$data[[1]] %>%
    slice(-(1:2)) %>%
    glimpse()

gas_weekly_raw_html$data[[1]] %>%
    slice(-(1:2)) %>%
    as_tibble() %>%
    select(1:2) %>%
    tail(15)

# tidy the weekly gasoline price data
gas_weekly_html <-
    gas_weekly_raw_html %>%
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

# available in xlsx format from January 2017 onward
gas_weekly_raw_xlsx <-
    crossing(yr = 2017:2018,
         grade = c("Unleaded", "Premium"),
         market = c("Retail (Incl. Tax)", "Retail (Excl. Tax)", "Wholesale")) %>%
    mutate(file_name = str_c(here(), "/data/gas/", grade, "_", market, "_WEEKLY_", yr, ".xlsx"),
           data = map(file_name, read_xlsx, skip = 2))

# inspect the weekly gasoline price data
gas_weekly_raw_xlsx$data[[1]] %>%
    glimpse()

gas_weekly_raw_xlsx$data[[1]] %>%
    select(1:2) %>%
    tail(15)

# tidy the weekly gasoline price data
gas_weekly_xlsx <-
    gas_weekly_raw_xlsx %>%
    mutate(market = case_when(market == "Retail (Excl. Tax)" ~ "retail_exc_tax",
                              market == "Retail (Incl. Tax)" ~ "retail_inc_tax",
                              market == "Wholesale" ~ "wholesale"),
           data_clean = map(data, ~.x %>%
                                filter(!str_detect(X__1, "\\([SVP]\\)")) %>%
                                filter(!str_detect(X__1, "S-Simple")) %>%
                                filter(!is.na(X__1)) %>%
                                gather(md, price, -X__1) %>%
                                select(md, city = X__1, price)))

# combine the data from html and xlsx sources
gas_weekly <-
    bind_rows(gas_weekly_html %>%
                  select(yr, grade, market, data_clean) %>%
                  unnest(),
              gas_weekly_xlsx %>%
                  select(yr, grade, market, data_clean) %>%
                  unnest() %>%
                  mutate(date = str_c(as.character(yr), "/", md) %>% as.Date(format = "%Y/%m/%d")) %>%
                  select(yr, grade, market, date, city, price))

save(gas_weekly_raw_html, gas_weekly_raw_xlsx, gas_weekly, file = str_c(here(), "/data/tbl_gas_weekly_raw.Rdata"))


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
