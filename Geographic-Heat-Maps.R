# Assuming we have geographic coordinates or region data
# First, summarize data by region
region_summary <- clean_data %>%
  group_by(comm)) %>%  # comm = community/region variable
  summarise(
    covid_cases = sum(pcr_results == 1, na.rm = TRUE),
    malaria_cases = sum(malaria == 1, na.rm = TRUE),
    dengue_cases = sum(dengue == 1, na.rm = TRUE),
    total_cases = n()
  ) %>%
  mutate(
    covid_rate = covid_cases / total_cases * 1000,
    malaria_rate = malaria_cases / total_cases * 1000,
    dengue_rate = dengue_cases / total_cases * 1000
  )

# If you have shapefiles for the regions:
# regions_sf <- st_read("path_to_shapefile.shp")
# map_data <- left_join(regions_sf, region_summary, by = c("name" = "comm"))

# For demonstration with fake coordinates (replace with real data)
set.seed(123)
map_data <- region_summary %>%
  mutate(
    lat = runif(n(), min = 5, max = 10),
    long = runif(n(), min = -2, max = 2)
  )

# COVID-19 heatmap
ggplot(map_data, aes(x = long, y = lat, size = covid_cases, color = covid_rate)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Geographic Distribution of COVID-19 Cases",
       x = "Longitude",
       y = "Latitude",
       size = "Case Count",
       color = "Case Rate (per 1000)") +
  theme_minimal()

# Malaria heatmap
ggplot(map_data, aes(x = long, y = lat, size = malaria_cases, color = malaria_rate)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Geographic Distribution of Malaria Cases",
       x = "Longitude",
       y = "Latitude",
       size = "Case Count",
       color = "Case Rate (per 1000)") +
  theme_minimal()