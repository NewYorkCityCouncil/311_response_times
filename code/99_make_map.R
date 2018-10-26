library(leaflet)


# small_calls <- calls_2017_agg %>%
#   # arrange(desc(n), created_date) %>%
#   filter(n > 1) %>%
#   separate(location, into = c("lat", "lon"), sep = ",") %>%
#   mutate(lat = str_remove_all(lat, "\\(|\\)") %>% as.numeric(),
#          lon = str_remove_all(lon, "\\(|\\)") %>% as.numeric())
# 
# pal <- colorNumeric("RdGr", log(small_calls$n))
# 
# small_calls %>%
#   leaflet() %>%
#   addProviderTiles("Stamen.TonerLite") %>%
#   addCircleMarkers(color = ~pal(log(n)), popup = ~paste(complaint_type, n, created_date, sep = "<br>"),
#                    clusterOptions = markerClusterOptions()) %>%
#   addLegend(position = "bottomright", pal = pal, values = ~log(n))

library(sf)

options(tigris_class = "sf")
tracts <- tigris::tracts("NY", year = 2010)

complaint_types <- calls_2017_agg %>% 
  group_by(complaint_type) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  pull(complaint_type)

top_complaints <- calls_2017_agg %>% 
  filter(complaint_type %in% complaint_types) %>% 
  separate(location, into = c("lat", "lon"), sep = ",") %>%
  mutate(lat = str_remove_all(lat, "\\(|\\)") %>% as.numeric(),
         lon = str_remove_all(lon, "\\(|\\)") %>% as.numeric()) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(tracts))

tract_complaints <- tracts %>%
  st_join(top_complaints, st_contains, left = FALSE) %>% 
  as.data.frame() %>% 
  select(-geometry)



tract_complaints_agg <- tract_complaints %>% 
  # st_cast("POLYGON") %>% 
  # st_simplify() %>% 
  group_by(GEOID10, complaint_type) %>% 
  summarize(mean = mean(n)) %>% 
  left_join(tracts, by = "GEOID10") %>% 
  st_sf() %>%
  # mutate(mean = ifelse(mean < 5, mean, 5)) %>% 
  filter(mean < 5)

pal <- colorNumeric("RdYlGn", tract_complaints_agg$mean)

map <- tract_complaints_agg %>%
  # filter(complaint_type == "noise - residential") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  leaflet() %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  addPolygons(color = ~pal(mean), group = ~complaint_type, stroke = FALSE, fillOpacity = .8) %>% 
  addLayersControl(baseGroups = ~unique(complaint_type)) %>% 
  addLegend(position = "bottomright", pal = pal, values = ~mean)
