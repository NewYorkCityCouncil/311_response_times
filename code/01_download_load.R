
filename <- "data/original_data/311_Service_Requests_from_2010_to_Present.csv"

if(! filename %in% list.files("data/original_data", full.names = TRUE)) {
  download.file(url = "https://data.cityofnewyork.us/api/views/erm2-nwe9/rows.csv?accessType=DOWNLOAD", destfile = filename)
}

dat_311 <- read_csv(filename) %>% 
  clean_names()

# Get 2017
dat_311_17 <- dat_311 %>% 
  mutate(created_date = mdy_hms(created_date),
         closed_date = mdy_hms(closed_date)) %>% 
  filter(year(created_date) == 2017,
         (!str_detect(tolower(resolution_description), "canceled")) | is.na(resolution_description)) # remove XXXX calls canceled by caller