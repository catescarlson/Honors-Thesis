# 02_coverage.R
# Builds the management coverage ratio: the share of each stock's area that
# falls in the EEZs of parties to a cooperative management plan, by year.
# Input:  data/raw/ICES_areas.zip, data/raw/World_EEZ_v12_20231025.zip,
#         data/raw/management_chronology.csv
# Output: data/cleaned/coverage.rds, output/figures/coverage_by_stock.pdf

source(here::here("code", "00_setup.R"))

# One of the EEZ polygons has a duplicate vertex that the s2 engine rejects,
# so use planar geometry (GEOS) instead
sf_use_s2(FALSE)


## ICES divisions

unzip(file.path(path_raw, "ICES_areas.zip"), exdir = file.path(tempdir(), "ices"))

area_union <- st_read(file.path(tempdir(), "ices", "ICES_Areas_20160601_cut_dense_3857.shp"),
                      quiet = TRUE) %>%
  st_make_valid() %>%
  mutate(division = paste(SubArea, Division, sep = ".")) %>%
  group_by(division) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_transform(3035)   # equal-area projection for Europe, so areas are in m^2

division_areas <- area_union %>%
  mutate(division_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry()


## EEZs 

unzip(file.path(path_raw, "World_EEZ_v12_20231025.zip"), exdir = file.path(tempdir(), "eez"))

countries_relevant <- c("Norway", "Russia", "Iceland", "Denmark", "United Kingdom",
                        "Ireland", "Spain", "France", "Germany", "Netherlands",
                        "Belgium", "Portugal", "Sweden")

# Crop to the Northeast Atlantic / Arctic
ne_atlantic_bbox <- st_bbox(c(xmin = -45, ymin = 35, xmax = 70, ymax = 82),
                            crs = st_crs(4326))

# Faroe Islands and Greenland belong to Denmark but negotiate separately and
# are not in the EU, so they are kept apart from mainland Denmark
eez <- st_read(file.path(tempdir(), "eez", "World_EEZ_v12_20231025", "eez_v12.shp"),
               quiet = TRUE) %>%
  filter(SOVEREIGN1 %in% countries_relevant) %>%
  mutate(territory = case_when(
    TERRITORY1 == "Faeroe"    ~ "Faroe Islands",
    TERRITORY1 == "Greenland" ~ "Greenland",
    TRUE                      ~ SOVEREIGN1
  )) %>%
  select(territory) %>%
  st_crop(ne_atlantic_bbox) %>%
  st_transform(3035)


## Share of each stock's area in each territory's EEZ 

# Share of each ICES division inside each territory's EEZ
division_coverage <- st_intersection(eez, area_union) %>%
  mutate(intersect_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(territory, division) %>%
  summarise(intersect_area = sum(intersect_area), .groups = "drop") %>%
  left_join(division_areas, by = "division") %>%
  mutate(pct_of_division = pmin(intersect_area / division_area, 1))

# Weight each division by its share of the stock's total area.
# This assumes each stock is spread evenly across its divisions.
stock_area <- stock_divisions %>%
  left_join(division_areas, by = "division") %>%
  group_by(stock) %>%
  mutate(division_weight = division_area / sum(division_area)) %>%
  ungroup()

eez_share <- division_coverage %>%
  inner_join(stock_area, by = "division", relationship = "many-to-many") %>%
  group_by(territory, stock) %>%
  summarise(pct_of_stock = sum(pct_of_division * division_weight), .groups = "drop")

# Per stock this should be <= 1 (the rest is high seas)
eez_share %>%
  group_by(stock) %>%
  summarise(total_eez = sum(pct_of_stock))


## Management chronology

mgmt <- read_csv(file.path(path_raw, "management_chronology.csv"), show_col_types = FALSE) %>%
  mutate(
    countries_under_plan = `Countries Under Plan` %>%
      str_replace_all("the Faroe Islands", "Faroe Islands") %>%
      str_remove(",?\\s*third party quota") %>%
      na_if("None"),
    under_review = if_else(`Under Review` == "Y", 1L, 0L, missing = 0L)
  ) %>%
  select(stock = Species, Year, countries_under_plan, under_review)


## Coverage ratio by stock and year

# Negotiating party for each territory. The UK negotiated as part of the EU
# until it left at the end of 2020.
eu_members <- c("Belgium", "Denmark", "France", "Germany", "Ireland",
                "Netherlands", "Portugal", "Spain", "Sweden")

coverage <- eez_share %>%
  inner_join(mgmt, by = "stock", relationship = "many-to-many") %>%
  mutate(
    party = case_when(
      territory %in% eu_members                    ~ "EU",
      territory == "United Kingdom" & Year <= 2020 ~ "EU",
      territory == "United Kingdom"                ~ "UK",
      TRUE                                         ~ territory
    ),
    in_plan = str_detect(replace_na(countries_under_plan, ""), fixed(party))
  ) %>%
  group_by(stock, Year) %>%
  summarise(
    coverage_ratio = sum(pct_of_stock[in_plan]),
    under_review   = first(under_review),
    .groups = "drop"
  )


## Plot and save

# Event study threshold (main specification in 06)
threshold <- 0.60

# Coverage over time
ggplot(coverage, aes(x = Year, y = coverage_ratio, colour = stock)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = threshold, linetype = "dashed", colour = "red") +
  labs(y = "Coverage ratio", colour = NULL)

ggsave(file.path(path_figs, "coverage_by_stock.pdf"), width = 8, height = 5)

# Range of coverage by stock (Figure 4): bar = min to max, diamond = mean.
# Uses every year up to year_max
coverage_range <- coverage %>%
  filter(Year <= year_max) %>%
  group_by(stock) %>%
  summarise(
    min_cov  = min(coverage_ratio),
    mean_cov = mean(coverage_ratio),
    max_cov  = max(coverage_ratio)
  ) %>%
  mutate(
    group = if_else(max_cov >= threshold,
                    paste0("Treated (reaches ", threshold * 100, "%)"),
                    "Never Treated"),
    stock = fct_reorder(stock, mean_cov)   # highest mean coverage at the top
  )

ggplot(coverage_range, aes(y = stock, colour = group)) +
  geom_segment(aes(x = min_cov, xend = max_cov, yend = stock),
               linewidth = 3, alpha = 0.7) +
  geom_point(aes(x = min_cov), size = 2.5) +
  geom_point(aes(x = max_cov), size = 2.5) +
  geom_point(aes(x = mean_cov), shape = 18, size = 4) +
  geom_vline(xintercept = threshold, linetype = "dashed", colour = "red", linewidth = 1) +
  annotate("text", x = threshold + 0.01, y = 0.6, hjust = 0, colour = "red",
           label = paste0(threshold * 100, "% Threshold")) +
  scale_colour_manual(values = c("#F0A04B", "#5B84B1")) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Range of Management Coverage by Stock",
       x = "Coverage Ratio", y = NULL, colour = NULL) +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave(file.path(path_figs, "coverage_range.pdf"), width = 8, height = 5)

saveRDS(coverage, file.path(path_clean, "coverage.rds"))
