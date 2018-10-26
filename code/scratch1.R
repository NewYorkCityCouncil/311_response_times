library(tidyverse)
library(janitor)
library(lubridate)
library(cumstats)
dat_311 <- read_csv("311_Service_Requests_from_2010_to_Present.csv") %>% 
  clean_names()

dat_311_17 <- dat_311 %>% 
  mutate(created_date = mdy_hms(created_date),
         closed_date = mdy_hms(closed_date)) %>% 
  filter(year(created_date) == 2017,
         !str_detect(tolower(resolution_description), "canceled"))

rm(dat_311)

# 
# removed <- dat_311 %>% 
#   mutate(created_date = mdy_hms(created_date),
#          closed_date = mdy_hms(closed_date)) %>% 
#   filter(year(created_date) == 2018,
#          str_detect(tolower(resolution_description), "canceled"))
# 
# removed %>% View()

dups_311_17 <- dat_311_17 %>%
  mutate(day = as.Date(created_date)) %>% 
  drop_na(day, complaint_type, location, agency) %>% 
  group_by(day, complaint_type, location, agency) %>% 
  summarize(created_date = min(created_date),
            closed_date = min(closed_date),
            n = n(),
            dup = n > 1,
            resolutions = list(resolution_description)) %>% 
  mutate(response_time = closed_date - created_date) %>% 
  ungroup()

# dups_by_day <- dups_311_17 %>% 
#   filter(!is.na(response_time), is.finite(response_time), response_time > 0) %>% 
#   group_by(day, dup) %>%
#   summarize(med = median(response_time))
# 
# 
# dups_by_day %>% 
#   spread(dup, med) %>% 
#   mutate(diff = `FALSE` - `TRUE`) %>% ungroup() %>%  select(diff) %>% acf()
#   ggplot(aes(day, diff)) +
#   geom_line() +
#   geom_smooth()
# 
# dups_by_day %>% filter(dup) %>%
#   ungroup() %>% 
#   select(med) %>% 
#   acf()
# 
# dups_by_day %>% filter(!dup) %>%
#   ungroup() %>% 
#   select(med) %>% 
#   acf()

by_complaint <- dups_311_17 %>% 
  filter(!is.na(response_time), is.finite(response_time), response_time > 0) %>% 
  group_by(dup, complaint_type) %>% 
  summarize(med = median(response_time)/60^2, n = n()) %>% 
  ungroup() %>% 
  filter(n >= 100) %>% 
  select(-n) %>% 
  spread(dup, med) %>% 
  mutate(diff = `FALSE` - `TRUE`)

by_complaint %>% 
  mutate(complaint_type = reorder(complaint_type, diff),
         diff = diff/24/7) %>% 
  filter(!is.na(diff)) %>% 
  ggplot(aes(complaint_type, diff)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous()

by_agency <- dups_311_17 %>% 
  filter(!is.na(response_time), is.finite(response_time), response_time > 0) %>%
  group_by(dup, agency) %>% 
  summarize(med = median(response_time)/60^2) %>% 
  ungroup() %>% 
  spread(dup, med) %>% 
  mutate(diff = `FALSE` - `TRUE`)

by_agency %>% 
  mutate(agency = reorder(agency, diff),
         diff = diff/24/7) %>% 
  filter(!is.na(diff)) %>% 
  ggplot(aes(agency, diff)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous()

dups_311_17 %>% 
  filter(agency == "DPR") %>% 
  arrange(desc(n)) %>% 
  head(1)

