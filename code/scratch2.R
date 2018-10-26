library(tidyverse)
library(lubridate)
library(janitor)
library(Rcpp)

sourceCpp("find_dups.cpp")
dat_311 <- read_csv("311_Service_Requests_from_2010_to_Present.csv") %>% 
  clean_names()

dat_311_17 <- dat_311 %>% 
  mutate(created_date = mdy_hms(created_date),
         closed_date = mdy_hms(closed_date)) %>% 
  filter(year(created_date) == 2017,
    !str_detect(tolower(resolution_description), "canceled"))

window_size <- dat_311_17 %>% 
  group_by(date = as.Date(created_date)) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  summarize(mean = mean(n)) %>% 
  pull() %>% 
  round()

dat_dups <- dat_311_17 %>%
  arrange(desc(created_date)) %>% 
  drop_na(location, complaint_type) %>% 
  mutate(complaint_type = tolower(complaint_type))

test <- dat_311_17 %>%
  arrange(desc(created_date)) %>% 
  # drop_na(location, complaint_type) %>% 
  mutate(complaint_type = tolower(complaint_type))
nrow(test) - nrow(dat_dups)

excluded2 <- dat_dups %>% 
  anti_join(dat_311_17, by = c("unique_key"))

# dat_dups_original <- dat_dups

# p <- progress_estimated(nrow(dat_dups))

# library(foreach)
# library(doMC)
# registerDoMC(12)

# dat_dups2 <- dat_dups %>% head(10000)



dup_id <- find_dups(dat_dups$unique_key, dat_dups$complaint_type, dat_dups$location, window_size)

dat_dups[,"dup_id"] <- dup_id

# for(i in 1:nrow(dat_dups)){
#   
#   created <- dat_dups$created_date[i]
#   type <- dat_dups$complaint_type[i]
#   og_location <- dat_dups$location[i]
#   id <- dat_dups$unique_key[i]
#   
#   is_dup <- with(dat_dups[i:(i+min(window_size, nrow(dat_dups) - i)),], 
#                  is.na(dup_id) & 
#                    complaint_type == type & 
#                    location == og_location)
#   
#   dat_dups[which(is_dup) + (i-1), "dup_id"] <- id 
#   
#   p$tick()$print()
# }

write_csv(dat_dups, "with_dups.csv")
# 
# dat_dups <- read_csv("with_dups.csv")

# dat_dups_agg <- dat_dups %>%
#   group_by(dup_id) %>%
#   summarize(n = n(),
#             created_date = min(created_date),
#             closed_date = min(closed_date)) %>%
#   mutate(response_time = closed_date - created_date,
#          dup = n > 1)
# 
# dat_dups_agg <- dat_dups_agg %>% 
#   left_join(dat_dups_original %>% 
#               select(location, complaint_type, agency, unique_key),
#             by = c("dup_id" = "unique_key"))

# View(dat_dups)
# View(dat_dups_agg)


