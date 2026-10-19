# Honors-Thesis
Replication code for honors thesis, *Cooperation under Climate Stress: Quantifying How Management Coverage Moderates Overfishing*. All code is in R.

## How to run

Open `Thesis Pipeline.Rproj` in RStudio, then run the scripts in `code/` in order (00 → 07).

| Script | Purpose |
|---|---|
| `00_setup.R` | Packages, paths, sample years, stock definitions |
| `01_shift.R` | Distribution shift of each species from DATRAS survey data |
| `02_coverage.R` | Share of each stock under cooperative management, by year |
| `03_outcomes.R` | F/F<sub>MSY</sub>, stock health and catch relative to advice |
| `04_build_panel.R` | Merge into regression panels |
| `05_regressions.R` | Fixed effects regressions (Table 5) and marginal effects (Figure 5) |
| `06_event_study.R` | Event study around coverage thresholds |
| `07_illustrative_calcs.R` | Schaefer-model yield and economic scenarios |

## Raw data

Some raw files are too large for GitHub and are not included. To run the full pipeline, download them and place them in `data/raw/`:

- `World_EEZ_v12_20231025.zip`: EEZ boundaries from [Marine Regions](https://www.marineregions.org/downloads.php) (used in `02`)
- `datras.sqlite`: ICES trawl survey data from [DATRAS](https://datras.ices.dk) (used in `01`)

The cleaned data in `data/cleaned/` are included, so scripts `04`–`07` run without these files.
