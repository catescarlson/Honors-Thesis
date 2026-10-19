# 04_build_panel.R
# Merges shift, coverage and outcomes into the two regression datasets,
# then adds lagged coverage and mean-centred variables.
# Input:  data/cleaned/stock_shift.rds, coverage.rds, stock_outcomes.rds, catch_advice.rds
# Output: data/cleaned/panel_ffmsy.rds, panel_catch.rds, panel_catch_point.rds

source(here::here("code", "00_setup.R"))

stock_shift    <- readRDS(file.path(path_clean, "stock_shift.rds"))
coverage       <- readRDS(file.path(path_clean, "coverage.rds"))
stock_outcomes <- readRDS(file.path(path_clean, "stock_outcomes.rds"))
catch_advice   <- readRDS(file.path(path_clean, "catch_advice.rds"))


## Lagged coverage

# Coverage in year t-1, matched on the calendar year (not the previous row),
# so a gap in the data can't pair a year with the wrong lag
coverage_lag <- coverage %>%
  mutate(Year = Year + 1) %>%
  select(stock, Year, coverage_lag = coverage_ratio)


## 2. Merge

# Base for the F/FMSY regression: one row per stock-year with an F/FMSY value
panel_ffmsy <- stock_outcomes %>%
  left_join(coverage,     by = c("stock", "Year")) %>%
  left_join(coverage_lag, by = c("stock", "Year")) %>%
  left_join(stock_shift %>% select(stock, Year = year, percent_shift),
            by = c("stock", "Year")) %>%
  filter(!is.na(F_FMSY), !is.na(coverage_ratio), !is.na(percent_shift),
         Year >= year_min, Year <= year_max)

# Base for the Catch/Advised regression: rows flagged with data caveats are dropped
panel_catch <- catch_advice %>%
  filter(has_data_caveat == 0) %>%
  left_join(coverage,     by = c("stock", "Year")) %>%
  left_join(coverage_lag, by = c("stock", "Year")) %>%
  left_join(stock_shift %>% select(stock, Year = year, percent_shift),
            by = c("stock", "Year")) %>%
  left_join(stock_outcomes %>% select(stock, Year, Health),
            by = c("stock", "Year")) %>%
  filter(!is.na(catch_advised), !is.na(coverage_ratio), !is.na(percent_shift),
         Year >= year_min, Year <= year_max)

# Same sample, restricted to years where ICES gave a single advised catch value
# (drops years where advice was a ceiling "≤" or a range)
panel_catch_point <- panel_catch %>%
  filter(advice_type == "point")


## Mean-centre and build interactions

# Variables are centred within each stock, so the interaction is identified
# only from changes in coverage and shift within a stock over time, not from
# differences between stocks. The main effect of shift is then its effect when
# a stock is at its own average coverage.
center_vars <- function(df) {
  df %>%
    group_by(stock) %>%
    mutate(
      shift_c         = percent_shift  - mean(percent_shift),
      coverage_c      = coverage_ratio - mean(coverage_ratio),
      coverage_lag_c  = coverage_lag   - mean(coverage_lag, na.rm = TRUE),
      interaction     = shift_c * coverage_c,
      interaction_lag = shift_c * coverage_lag_c
    ) %>%
    ungroup()
}

# Each sample is centred on its own stock means
panel_ffmsy       <- center_vars(panel_ffmsy)
panel_catch       <- center_vars(panel_catch)
panel_catch_point <- center_vars(panel_catch_point)


## Check and save

# Observations per stock, 1988-2024
count(panel_ffmsy, stock)
count(panel_catch, stock, advice_type)

saveRDS(panel_ffmsy,       file.path(path_clean, "panel_ffmsy.rds"))
saveRDS(panel_catch,       file.path(path_clean, "panel_catch.rds"))
saveRDS(panel_catch_point, file.path(path_clean, "panel_catch_point.rds"))
