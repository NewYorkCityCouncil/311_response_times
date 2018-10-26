
ggplot(dat_dups_agg, aes(n)) +
  geom_histogram(binwidth = 1)

dat_dups_agg %>%
  group_by(n) %>% 
  summarize(num = n()) %>% 
  arrange(desc(num)) %>% 
  mutate(perc = num/sum(num), cum_perc = cumsum(perc))

dat_dups_agg %>% 
  filter(is.finite(response_time), response_time > 0) %>% 
  group_by(dup) %>% 
  summarize(med = median(response_time)/60^2)

dat_dups_agg %>% 
  filter(is.finite(response_time), response_time > 0) %>% 
  ggplot(aes(response_time)) +
  geom_histogram() +
  facet_wrap(~dup)


by_complaint <- dat_dups_agg %>% 
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

by_agency <- dat_dups_agg %>% 
  filter(!is.na(response_time), is.finite(response_time), response_time > 0) %>%
  group_by(dup, agency) %>% 
  summarize(med = median(response_time)/60^2, n = n()) %>%
  filter(n >=100) %>% 
  select(-n) %>% 
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

dat_dups_agg %>% 
  filter(agency == "DPR") %>% 
  arrange(desc(n)) %>% 
  head(1)

