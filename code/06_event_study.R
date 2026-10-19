# 06_event_study.R
# Event study around the year a stock's coverage first crosses a threshold
# (60% in the main text, 50% and 75% as sensitivity checks in the Appendix).
# Input:  data/cleaned/stock_shift.rds, coverage.rds, stock_outcomes.rds
# Output: output/figures/event_study_50.png, _60.png, _75.png (where estimable)

source(here::here("code", "00_setup.R"))

stock_shift    <- readRDS(file.path(path_clean, "stock_shift.rds"))
coverage       <- readRDS(file.path(path_clean, "coverage.rds"))
stock_outcomes <- readRDS(file.path(path_clean, "stock_outcomes.rds"))


## Event study panel

# The event study uses every year (No 1988 min)
# (e.g. North Sea Herring in 1978 is included)
event_panel <- stock_outcomes %>%
  inner_join(coverage, by = c("stock", "Year")) %>%
  inner_join(stock_shift %>% select(stock, Year = year, percent_shift),
             by = c("stock", "Year")) %>%
  filter(Year <= year_max)

# Stocks observed in each year (a year with only one stock can't be used,
# because the year fixed effect absorbs it)
event_panel %>%
  count(Year, name = "n_stocks") %>%
  filter(n_stocks < 2)

## Event study function

# Treatment = the first year a stock's coverage reaches the threshold.
# Only stocks that reach the threshold are used;
# identification comes from their different crossing years.
run_event_study <- function(threshold) {

  first_treat <- event_panel %>%
    filter(coverage_ratio >= threshold) %>%
    group_by(stock) %>%
    summarise(first_treat = min(Year))

  print(first_treat)

  event_data <- event_panel %>%
    inner_join(first_treat, by = "stock") %>%
    mutate(
      rel_year = Year - first_treat,
      # Cap at +/- 5 years to preserve degrees of freedom
      rel_year_capped = pmax(pmin(rel_year, 5), -5)
    )

  model <- feols(F_FMSY ~ i(rel_year_capped, ref = -1) + Health + percent_shift | stock + Year,
                 data = event_data, vcov = "hetero")

  # If the crossing years have no other stock to compare against, the year
  # fixed effects absorb every event-time coefficient
  if (!any(str_detect(names(coef(model)), "rel_year_capped"))) {
    message("At ", threshold * 100, "%, no event-time effects can be estimated; skipping.")
    return(NULL)
  }

  png(file.path(path_figs, paste0("event_study_", threshold * 100, ".png")),
      width = 1400, height = 900, res = 150)
  iplot(model, main = paste0("Coverage crossing ", threshold * 100, "%"),
        xlab = "Years relative to treatment", ylab = "Estimated effect on F/FMSY")
  dev.off()

  model
}


## Run at each threshold

# Main specification: 60% 
es_60 <- run_event_study(0.60)

# Sensitivity checks
es_50 <- run_event_study(0.50)
es_75 <- run_event_study(0.75)
