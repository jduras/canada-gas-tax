
# scrape tax data from https://charting.kentgroupltd.com/
tax_tbl_raw <-
    "https://charting.kentgroupltd.com/WPPS/Taxation/Taxation%20Tables/HISTORICAL%20TAXATION/2016/Taxation_Taxation%20Tables_HISTORICAL%20TAXATION_2016.htm" %>%
    read_html() %>%
    html_table() %>%
    purrr::pluck(1)

tax_tbl_raw %>%
    glimpse()

tax_tbl_raw %>%
    slice(-(1:3)) %>%
    glimpse()

tax_tbl_clean <-
    tax_tbl_raw %>%
    mutate(X1 = if_else(row_number() == 4, "X1", X1)) %>%
    set_colnames(slice(., 4)) %>%
    slice(-c(1:4)) %>%
    mutate(group = (X1 == "") %>% cumsum()) %>%
    group_by(group) %>%
    mutate(state = nth(X1, 2)) %>%
    ungroup() %>%
    gather(date, value, -c(X1, group, state)) %>%
    filter(value != "") %>%
    mutate(date = as.Date(date),
           value = str_replace(value, "%", "") %>% parse_double()) %>%
    filter(str_detect(X1, "Gasoline")) %>%
    select(state, date, tax = value)

tax_tbl_clean %>%
     #filter(state %in% c("BC", "AB", "SK", "MB")) %>%
    filter(state %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")) %>%
    ggplot(aes(x = date, y = tax, color = state)) +
        geom_point() +
        geom_step()



# 2019 tax data from https://charting.kentgroupltd.com/
tax_tbl_raw <-
    str_c(here(), "/data/taxes/Taxation_Taxation Tables_HISTORICAL TAXATION_2019.csv") %>%
    read_csv(col_names = FALSE)
tax_tbl_raw

tax_tbl_raw %>%
    slice(-(1:3)) %>%
    glimpse()

tax_tbl_clean <-
    tax_tbl_raw %>%
    select(-X50) %>%
    mutate(X1 = if_else(row_number() == 4, "X1", X1)) %>%
    set_colnames(slice(., 4)) %>%
    slice(-c(1:4)) %>%
    mutate(group = is.na(X1) %>% cumsum()) %>%
    group_by(group) %>%
    mutate(state = nth(X1, 2)) %>%
    ungroup() %>%
    gather(date, value, -c(X1, group, state)) %>%
    filter(value != "") %>%
    mutate(date = as.Date(date),
           value = str_replace(value, "%", "") %>% parse_double()) %>%
    filter(str_detect(X1, "Gasoline")) %>%
    select(state, date, tax = value)
