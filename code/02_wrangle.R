
if(! all(c(unagg_filename, agg_filename) %in% list.files("data/processed_data", full.names = TRUE))){
  # Load C++ function
  sourceCpp("code/find_dups.cpp")
  
  # Read in all 311 data
  
  
  # window_size <- dat_311_17 %>% 
  #   group_by(date = as.Date(created_date)) %>% 
  #   summarize(n = n()) %>% 
  #   ungroup() %>% 
  #   summarize(mean = mean(n)) %>% 
  #   pull() %>% 
  #   round()
  # 
  calls_2017 <- dat_311_17 %>%
    arrange(desc(created_date)) %>%
    drop_na(location, complaint_type) %>% # Remove XXXX calls with missing info
    mutate(complaint_type = tolower(complaint_type))
  # 
  # dup_id <- find_dups(calls_2017$unique_key, calls_2017$complaint_type, calls_2017$location, window_size)
  # 
  # calls_2017[,"dup_id"] <- dup_id
  
  
  response_times <- calls_2017 %>%
    group_by(complaint_type) %>%
    summarize(q75 = quantile(closed_date - created_date, .75, na.rm = TRUE)/60^2/24)
  calls_per_day <- calls_2017 %>% 
    group_by(day = as.Date(created_date), complaint_type) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    group_by(complaint_type) %>% 
    summarize(mean = mean(n))
  windows <- response_times %>% 
    left_join(calls_per_day, by = "complaint_type") %>% 
    mutate(window = ceiling(as.numeric(q75 * mean)),
           window = case_when(window < 3 ~ 3,
                              is.na(window) ~ 3,
                              TRUE ~ window)) %>% 
    select(-q75)
  
  calls_2017_2 <- calls_2017 %>% 
    mutate(location2 = paste(round(latitude, 4), round(longitude, 4), sep = ", ")) %>%
    left_join(windows, by = "complaint_type") %>%
    group_by(complaint_type) %>% 
    arrange(desc(created_date)) %>% 
    mutate(dup_id = find_dups(unique_key, complaint_type, location2, unique(window)))
  
  calls_2017_agg <- calls_2017_2 %>%
    ungroup() %>% 
    group_by(dup_id) %>%
    summarize(n = n(),
              created_date = min(created_date),
              closed_date = min(closed_date),
              resolutions = list(resolution_description)) %>%
    mutate(response_time = closed_date - created_date,
           dup = n > 1) %>% 
    left_join(calls_2017 %>% 
                select(location, complaint_type, agency, unique_key),
              by = c("dup_id" = "unique_key"))
  
  save(calls_2017_2, file = unagg_filename)
  save(calls_2017_agg, file = agg_filename)
  
} else {
  load(unagg_filename)
  load(agg_filename)
}
