# 01_shift.R
# Builds the distribution shift variable from DATRAS trawl survey data,
# following Baudron et al. (2020). Shift is measured at the species level,
# so North Sea Herring and NSS Herring share the herring series.
# Input:  data/raw/datras.sqlite
# Output: data/cleaned/stock_shift.rds, plus shift and survey coverage figures

source(here::here("code", "00_setup.R"))


## Load survey data

con <- dbConnect(RSQLite::SQLite(), file.path(path_raw, "datras.sqlite"))

# weight = estimated CPUE (biomass per 60 min trawl)
query <- "
SELECT
  hh.haulID,
  hh.Survey,
  hh.Year,
  hh.ShootLat,
  hh.ShootLong,
  cpue.species,
  cpue.Aphia,
  cpue.weight
FROM cpue
JOIN hh ON cpue.haulID = hh.haulID
WHERE cpue.weight > 0
"

data <- dbGetQuery(con, query) %>%
  filter(Aphia %in% stocks$aphia)

dbDisconnect(con)

# A few hauls appear twice in the hh table
data <- distinct(data, haulID, Aphia, .keep_all = TRUE)


## Annual biomass-weighted centroids

annual_centroids <- data %>%
  group_by(species, Aphia, Year) %>%
  summarise(
    weighted_lat  = weighted.mean(ShootLat,  w = weight),
    weighted_long = weighted.mean(ShootLong, w = weight),
    n_hauls       = n(),
    .groups = "drop"
  )


## Baseline location and range

# Baseline = the first 5 years after each species first appears in the survey data
start_years <- annual_centroids %>%
  group_by(species) %>%
  summarise(start_year = min(Year))

baseline <- annual_centroids %>%
  left_join(start_years, by = "species") %>%
  filter(Year < start_year + n_baseline_years) %>%
  group_by(species) %>%
  summarise(
    base_lat  = mean(weighted_lat),
    base_long = mean(weighted_long)
  )

# Diagonal extent of the species' range during the baseline years
baseline_extent <- data %>%
  left_join(start_years, by = "species") %>%
  filter(Year < start_year + n_baseline_years) %>%
  group_by(species) %>%
  summarise(
    lat_range_km  = (max(ShootLat) - min(ShootLat)) * 111,
    long_range_km = (max(ShootLong) - min(ShootLong)) * 111 * cos(mean(ShootLat) * pi/180),
    range_km      = sqrt(lat_range_km^2 + long_range_km^2)
  )


## Shift from baseline

# Haversine distance (degrees to km)
haversine_km <- function(lat1, lon1, lat2, lon2) {
  R    <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a    <- sin(dlat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dlon/2)^2
  2 * R * asin(sqrt(a))
}

species_shift <- annual_centroids %>%
  inner_join(baseline,        by = "species") %>%
  inner_join(baseline_extent, by = "species") %>%
  mutate(
    shift_km      = haversine_km(base_lat, base_long, weighted_lat, weighted_long),
    percent_shift = (shift_km / range_km) * 100     # % of baseline range extent
  )

# Attach each species' shift to its stock; both herring stocks get the herring series
stock_shift <- stocks %>%
  select(stock, aphia) %>%
  inner_join(species_shift, by = c("aphia" = "Aphia")) %>%
  select(stock, species, year = Year, shift_km, percent_shift, n_hauls)


## Plots

ggplot(species_shift, aes(x = Year, y = percent_shift)) +
  geom_line(colour = "grey60") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Year", y = "Shift (% of baseline range)")

ggsave(file.path(path_figs, "shift_by_species.pdf"), width = 9, height = 6)

# Survey coverage check: which surveys contribute hauls to each species each year.
# When a survey starts or stops, the centroid can move even if the fish don't.
data %>%
  count(species, Year, Survey) %>%
  ggplot(aes(x = Year, y = n, fill = Survey)) +
  geom_col(width = 1) +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Year", y = "Hauls that caught the species")

ggsave(file.path(path_figs, "survey_coverage_by_species.pdf"), width = 10, height = 6)


# Save

saveRDS(stock_shift, file.path(path_clean, "stock_shift.rds"))

