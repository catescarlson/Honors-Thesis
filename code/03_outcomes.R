# 03_outcomes.R
# Builds the overfishing outcomes and the stock health control:
#   F/FMSY and SSB/Blim from the ICES stock assessment database (SAG)
#   Catch/Advised from ICES advice reports (entered manually)
# Input:  ICES SAG API (downloaded once and saved to data/raw/ices_stock_assessments.rds),
#         data/raw/catch_vs_advised.csv
# Output: data/cleaned/stock_outcomes.rds, data/cleaned/catch_advice.rds

source(here::here("code", "00_setup.R"))
library(httr2)


## Download ICES stock assessments

# ICES stock code for each stock. Matching on the stock code (rather than on
# ICES areas) keeps out other stocks of the same species in overlapping areas,
# e.g. the Norwegian coastal cod stocks.
stock_codes <- tribble(
  ~stock,              ~StockKeyLabel,
  "Barents Sea Cod",   "cod.27.1-2",
  "Blue Whiting",      "whb.27.1-91214",
  "NEA Mackerel",      "mac.27.nea",
  "NSS Herring",       "her.27.1-24a514a",
  "North Sea Haddock", "had.27.46a20",
  "North Sea Herring", "her.27.3a47d",
  "North Sea Saithe",  "pok.27.3a46"
)

fetch_stockdownload <- function(key) {
  request("https://sag.ices.dk/sag_api/api/StockDownload") %>%
    req_url_query(assessmentKey = key) %>%
    req_retry(max_tries = 3) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as_tibble() %>%
    select(StockKeyLabel, AssessmentYear, Year, FishingPressure, FMSY, StockSize, Blim) %>%
    mutate(across(-StockKeyLabel, as.numeric))    # the API returns numbers as text
}

raw_file <- file.path(path_raw, "ices_stock_assessments.rds")

if (!file.exists(raw_file)) {

  latest_list <- request("https://sag.ices.dk/sag_api/api/LatestStockAdviceList") %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)

  keys <- latest_list %>%
    filter(StockKeyLabel %in% stock_codes$StockKeyLabel) %>%
    pull(AssessmentKey)

  ices_raw <- map(keys, fetch_stockdownload) %>% list_rbind()
  saveRDS(ices_raw, raw_file)
}

ices_raw <- readRDS(raw_file)


## F/FMSY and stock health

stock_outcomes <- ices_raw %>%
  inner_join(stock_codes, by = "StockKeyLabel") %>%
  mutate(
    F_FMSY = FishingPressure / FMSY,
    Health = StockSize / Blim         # SSB relative to its biological limit
  ) %>%
  filter(!is.na(F_FMSY)) %>%
  select(stock, Year, F_FMSY, Health, AssessmentYear)

# Check: one row per stock-year
stock_outcomes %>%
  group_by(stock) %>%
  summarise(n = n(), duplicates = sum(duplicated(Year)),
            first_year = min(Year), last_year = max(Year))


## Catch relative to scientific advice

catch_advice <- read_csv(file.path(path_raw, "catch_vs_advised.csv"), show_col_types = FALSE) %>%
  mutate(
    # Sum catches across all area groups
    total_catch   = rowSums(across(c(Catches1, Catches_Area2, Catches_Area3, Catches_Area4)),
                            na.rm = TRUE),
    # How the advice was written: a single number ("point"), a ceiling ("≤776391")
    # or a range ("320000–420000"). Used for the point-advice-only column in Table 5.
    advice_type = case_when(
      str_detect(str_remove_all(Advised_Catch, " "), "^[0-9]+$") ~ "point",
      str_detect(Advised_Catch, "^\\s*[≤<]")                    ~ "ceiling",
      str_detect(Advised_Catch, "[0-9]\\s*[–-]\\s*[0-9]")       ~ "range",
      TRUE                                                      ~ NA_character_
    ),
    # Keep the last number (upper end) if there's a range or "≤"; "N/A" becomes NA.
    Advised_Catch = Advised_Catch %>% str_remove_all(" ") %>% str_extract("[0-9]+$") %>% as.numeric(),
    # > 1 means catches exceeded advice; undefined when the advice was zero catch
    catch_advised = if_else(Advised_Catch > 0, total_catch / Advised_Catch, NA_real_),

    # Flag rows with notes for sensitivity checks
    has_data_caveat = if_else(!is.na(Notes) & Notes != "", 1L, 0L)
  ) %>%
  select(stock = Species, Year, total_catch, Advised_Catch, advice_type, catch_advised, has_data_caveat)


## Plot and save

ggplot(stock_outcomes, aes(x = Year, y = F_FMSY)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  facet_wrap(~ stock) +
  labs(x = "Year", y = "F/FMSY")

ggsave(file.path(path_figs, "ffmsy_by_stock.pdf"), width = 9, height = 6)

saveRDS(stock_outcomes, file.path(path_clean, "stock_outcomes.rds"))
saveRDS(catch_advice,   file.path(path_clean, "catch_advice.rds"))
