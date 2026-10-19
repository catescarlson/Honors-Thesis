# 07_illustrative_calcs.R
# Translates the Table 5 (column 1) estimates into yield and economic terms:
#   regression scenarios -> F/FMSY -> Schaefer yield -> landed value, GVA, total activity
# Input:  data/cleaned/reg_ffmsy_lag.rds, data/cleaned/panel_ffmsy.rds
# Output: output/tables/economic_scenarios.csv, output/figures/schaefer_model.pdf

source(here::here("code", "00_setup.R"))

model       <- readRDS(file.path(path_clean, "reg_ffmsy_lag.rds"))
panel_ffmsy <- readRDS(file.path(path_clean, "panel_ffmsy.rds"))

# Estimation sample (rows feols actually used)
sample_used <- panel_ffmsy[obs(model), ]


## Inputs

# Economic inputs
landed_value <- 750      # EUR million, 2023 EU-fleet landed value, six species (EUMOFA 2025 edition)
gva_share    <- 0.492    # direct GVA / revenue, EU catching sector 2022 (STECF AER)
multipliers  <- c(1.8, 2.5)   # Type II output multiplier range (Raffray et al. 2022)

# Scenario inputs:
#   shift_change    = one standard deviation of distribution shift
#   coverage_change = coverage falling from its sample mean to zero
baseline_F      <- mean(sample_used$F_FMSY)
shift_change    <- sd(sample_used$percent_shift)
coverage_change <- -mean(sample_used$coverage_lag)

b <- coef(model)


## Scenario F/FMSY

# A: stock at its own average shift and coverage
# B: +1 SD shift, coverage at its own average
# C: +1 SD shift and coverage falls to zero
scenarios <- tibble(
  scenario = c("A. Baseline", "B. +1 SD shift", "C. +1 SD shift, no coverage"),
  d_shift  = c(0, shift_change, shift_change),
  d_cov    = c(0, 0, coverage_change)
) %>%
  mutate(
    F_FMSY = baseline_F +
      b["shift_c"] * d_shift +
      b["coverage_lag_c"] * d_cov +
      b["interaction_lag"] * d_shift * d_cov
  )


## Schaefer yield and economic values

# Relative yield Y/YMSY = (F/FMSY)(2 - F/FMSY), floored at zero
# (Hilborn & Walters 1992, ch. 7)
schaefer_yield <- function(f) pmax(0, f * (2 - f))

scenarios <- scenarios %>%
  mutate(
    yield         = schaefer_yield(F_FMSY),
    yield_vs_A    = yield / yield[1],
    value         = landed_value * yield_vs_A,
    d_value       = value - landed_value,
    d_gva         = gva_share * d_value,
    d_activity_lo = multipliers[1] * d_value,
    d_activity_hi = multipliers[2] * d_value
  )

# Management benefit: value preserved by keeping coverage (B) rather than losing it (C)
benefit <- scenarios %>%
  summarise(
    scenario      = "Management benefit (B - C)",
    d_value       = d_value[2] - d_value[3],
    d_gva         = d_gva[2] - d_gva[3],
    d_activity_lo = d_activity_lo[2] - d_activity_lo[3],
    d_activity_hi = d_activity_hi[2] - d_activity_hi[3]
  )

economic_table <- bind_rows(scenarios, benefit) %>%
  select(-d_shift, -d_cov) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

economic_table

write_csv(economic_table, file.path(path_tabs, "economic_scenarios.csv"))


## Figure: Schaefer curve with scenarios

curve_df <- tibble(F_FMSY = seq(0, 2.2, by = 0.001)) %>%
  mutate(yield = schaefer_yield(F_FMSY))

# Shaded area: yield lost when coverage falls to zero (between scenarios B and C)
F_B <- scenarios$F_FMSY[2]
F_C <- scenarios$F_FMSY[3]

loss_area <- curve_df %>%
  filter(F_FMSY >= F_B, F_FMSY <= F_C) %>%
  mutate(yield_B = schaefer_yield(F_B))

ggplot(curve_df, aes(x = F_FMSY, y = yield)) +
  geom_ribbon(data = loss_area, aes(ymin = yield, ymax = yield_B),
              fill = "firebrick", alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  annotate("text", x = 1.02, y = 0.05, label = "F = FMSY", hjust = 0, colour = "grey40") +
  geom_vline(data = scenarios,
             aes(xintercept = F_FMSY, colour = scenario),
             linewidth = 1) +
  scale_colour_manual(values = c("seagreen", "steelblue", "firebrick")) +
  scale_x_continuous(breaks = seq(0, 2.2, by = 0.2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "F / FMSY", y = "Relative yield (% of MSY-level catch)",
       colour = NULL) +
  theme(legend.position = "bottom")

ggsave(file.path(path_figs, "schaefer_model.pdf"), width = 7, height = 7)
