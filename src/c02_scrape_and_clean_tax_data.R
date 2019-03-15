
# scrape tax data from https://charting.kentgroupltd.com/
tax_2016_raw <-
    "https://charting.kentgroupltd.com/WPPS/Taxation/Taxation%20Tables/HISTORICAL%20TAXATION/2016/Taxation_Taxation%20Tables_HISTORICAL%20TAXATION_2016.htm" %>%
    read_html() %>%
    html_table() %>%
    purrr::pluck(1)

tax_2016_raw %>%
    glimpse()

tax_2016_raw %>%
    slice(-(1:3)) %>%
    glimpse()

tax_2016_clean <-
    tax_2016_raw %>%
    mutate(X1 = if_else(row_number() == 4, "X1", X1)) %>%
    set_colnames(slice(., 4)) %>%
    slice(-c(1:4)) %>%
    mutate(group = (X1 == "") %>% cumsum()) %>%
    group_by(group) %>%
    mutate(state_name = nth(X1, 2)) %>%
    ungroup() %>%
    gather(date, value, -c(X1, group, state_name)) %>%
    filter(value != "") %>%
    mutate(state = recode(state_name,
                          Yukon = "YT",
                          `British Columbia` = "BC",
                          `North-West Territory` = "NT",
                          Alberta = "AB",
                          Saskatchewan = "SK",
                          Manitoba = "MB",
                          Ontario = "ON",
                          Québec = "QC",
                          `New Brunswick` = "NB",
                          `Nova Scotia` = "NS",
                          `Prince Edward Island` = "PE",
                          Newfoundland = "NL",
                          CANADA = "CA"),
           date = as.Date(date),
           value = str_replace(value, "%", "") %>% parse_double()) %>%
    filter(str_detect(X1, "Gasoline")) %>%
    select(state, state_name, date, tax = value)

tax_2016_clean %>%
     #filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    filter(state_name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")) %>%
    ggplot(aes(x = date, y = tax, color = state_name)) +
        geom_point() +
        geom_step()

write_csv(tax_2016_clean, path = str_c(here(), "/data/tbl_tax_2016_clean.csv"))


# 2019 tax data from https://charting.kentgroupltd.com/
tax_raw <-
    str_c(here(), "/data/taxes/Taxation_Taxation Tables_HISTORICAL TAXATION_2019.csv") %>%
    read_csv(col_names = FALSE)
tax_raw

tax_raw %>%
    slice(-(1:3)) %>%
    glimpse()

tax_clean <-
    tax_raw %>%
    select(-X50) %>%
    mutate(X1 = if_else(row_number() == 4, "X1", X1)) %>%
    set_colnames(slice(., 4)) %>%
    slice(-c(1:4)) %>%
    mutate(group = is.na(X1) %>% cumsum()) %>%
    group_by(group) %>%
    mutate(state_name = nth(X1, 2)) %>%
    ungroup() %>%
    gather(date, value, -c(X1, group, state_name)) %>%
    filter(value != "") %>%
    mutate(state = recode(state_name,
                          Yukon = "YT",
                          `British Columbia` = "BC",
                          `North-West Territory` = "NT",
                          Alberta = "AB",
                          Saskatchewan = "SK",
                          Manitoba = "MB",
                          Ontario = "ON",
                          Quebec = "QC",
                          `New Brunswick` = "NB",
                          `Nova Scotia` = "NS",
                          `Prince Edward Island` = "PE",
                          Newfoundland = "NL",
                          CANADA = "CA"),
           date = as.Date(date),
           value = str_replace(value, "%", "") %>% parse_double()) %>%
    filter(str_detect(X1, "Gasoline")) %>%
    select(state, state_name, date, tax = value)

tax_clean %>%
    #filter(date >= "2013-01-01", date <= "2017-12-31") %>%
    #filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    filter(state_name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")) %>%
    ggplot(aes(x = date, y = tax, color = state_name)) +
        geom_point() +
        geom_step()

write_csv(tax_clean, path = str_c(here(), "/data/tbl_tax_clean.csv"))
# tax_clean <- read_csv(str_c(here(), "/data/tbl_tax_clean.csv"))
