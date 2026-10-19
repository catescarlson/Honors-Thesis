# 00_setup.R
# Each script starts with: source(here::here("code", "00_setup.R"))

library(tidyverse)
library(here)
library(sf)
library(DBI)
library(fixest)

## Paths
path_raw   <- here("data", "raw")
path_clean <- here("data", "cleaned")
path_figs  <- here("output", "figures")
path_tabs  <- here("output", "tables")

## Sample
year_min <- 1988
year_max <- 2024
n_baseline_years <- 5   # shift baseline = each stock's first 5 survey years

## Stocks
# aphia = species code in DATRAS, sag_species = species name in ICES assessments
stocks <- tribble(
  ~stock,              ~aphia, ~sag_species,
  "Barents Sea Cod",   126436, "Gadus morhua",
  "Blue Whiting",      126439, "Micromesistius poutassou",
  "NEA Mackerel",      127023, "Scomber scombrus",
  "NSS Herring",       126417, "Clupea harengus",
  "North Sea Haddock", 126437, "Melanogrammus aeglefinus",
  "North Sea Herring", 126417, "Clupea harengus",
  "North Sea Saithe",  126441, "Pollachius virens"
)

# ICES divisions in each stock's assessment area
stock_divisions <- tribble(
  ~stock,              ~division,
  "Barents Sea Cod",   "1.a, 1.b, 2.a, 2.b",
  "Blue Whiting",      "1.a, 1.b, 2.a, 2.b, 3.a, 4.a, 4.b, 4.c, 5.a, 5.b, 6.a, 6.b,
                        7.a, 7.b, 7.c, 7.d, 7.e, 7.f, 7.g, 7.h, 7.j, 7.k,
                        8.a, 8.b, 8.c, 8.d, 8.e, 9.a, 9.b, 12.a, 12.b, 12.c, 14.a, 14.b",
  "NEA Mackerel",      "1.a, 1.b, 2.a, 2.b, 3.a, 3.b, 3.c, 3.d, 4.a, 4.b, 4.c, 5.a, 5.b,
                        6.a, 6.b, 7.a, 7.b, 7.c, 7.d, 7.e, 7.f, 7.g, 7.h, 7.j, 7.k,
                        8.a, 8.b, 8.d, 8.e, 12.a, 12.b, 14.a, 14.b",
  "NSS Herring",       "1.a, 1.b, 2.a, 2.b, 4.a, 5.a, 5.b, 14.a",
  "North Sea Haddock", "4.a, 4.b, 4.c",
  "North Sea Herring", "3.a, 4.a, 4.b, 4.c, 7.d",
  "North Sea Saithe",  "3.a, 4.a, 4.b, 4.c, 6.a, 6.b"
) %>%
  separate_rows(division, sep = ",") %>%
  mutate(division = str_trim(division))

theme_set(theme_minimal())
