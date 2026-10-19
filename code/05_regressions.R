# 05_regressions.R
# Core two-way fixed effects regressions (Table 5) and the marginal effect of
# shift by coverage level (Figure 5).
# Input:  data/cleaned/panel_ffmsy.rds, panel_catch.rds, panel_catch_point.rds
# Output: output/tables/table5.tex,
#         data/cleaned/reg_ffmsy_lag.rds (main model, used in 07),
#         output/figures/marginal_effect_plot.pdf

source(here::here("code", "00_setup.R"))

panel_ffmsy <- readRDS(file.path(path_clean, "panel_ffmsy.rds"))
panel_catch <- readRDS(file.path(path_clean, "panel_catch.rds"))
panel_catch_point <- readRDS(file.path(path_clean, "panel_catch_point.rds"))


## Table 5

# F/FMSY
reg_ffmsy_lag <- feols(F_FMSY ~ shift_c + coverage_lag_c + interaction_lag + Health | stock + Year,
                       data = panel_ffmsy, vcov = "hetero")

reg_ffmsy <- feols(F_FMSY ~ shift_c + coverage_c + interaction + Health | stock + Year,
                   data = panel_ffmsy, vcov = "hetero")

reg_ffmsy_review <- feols(F_FMSY ~ shift_c + coverage_c + interaction + Health + under_review | stock + Year,
                          data = panel_ffmsy, vcov = "hetero")

# Catch/Advised, lagged coverage
# Column 4: all advice years (upper end of ranges and ceilings)
reg_catch_lag <- feols(catch_advised ~ shift_c + coverage_lag_c + interaction_lag + Health | stock + Year,
                       data = panel_catch, vcov = "hetero")

# Column 5: only years with a single point value for advised catch
reg_catch_point <- feols(catch_advised ~ shift_c + coverage_lag_c + interaction_lag + Health | stock + Year,
                         data = panel_catch_point, vcov = "hetero")

table5 <- list(reg_ffmsy_lag, reg_ffmsy, reg_ffmsy_review, reg_catch_lag, reg_catch_point)

var_labels <- c(
  shift_c         = "Distribution Shift",
  coverage_lag_c  = "Lagged Coverage",
  interaction_lag = "Shift $\\times$ Lagged Coverage",
  coverage_c      = "Management Coverage",
  interaction     = "Shift $\\times$ Coverage",
  Health          = "Stock Health",
  under_review    = "Under Review"
)

stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

etable(table5, dict = var_labels, fitstat = c("ar2", "n"), signif.code = stars)

etable(table5, dict = var_labels, fitstat = c("ar2", "n"), signif.code = stars, tex = TRUE,
       file = file.path(path_tabs, "table5.tex"), replace = TRUE)

# Save the main specification for the economic scenarios in 07
saveRDS(reg_ffmsy_lag, file.path(path_clean, "reg_ffmsy_lag.rds"))


## Marginal effect of shift by coverage (Figure 5)

# Coverage is centred within each stock, so the x-axis is coverage relative to
# the stock's own average (0 = at its average, -0.5 = 50 points below it).
# Marginal effect of shift = b1 + b3 * (centred coverage)
# Var(b1 + b3*c) = Var(b1) + c^2 * Var(b3) + 2*c*Cov(b1, b3)
b1 <- coef(reg_ffmsy_lag)["shift_c"]
b3 <- coef(reg_ffmsy_lag)["interaction_lag"]
V  <- vcov(reg_ffmsy_lag)

plot_data <- tibble(coverage_c = seq(-0.5, 0.5, by = 0.01)) %>%
  mutate(
    marginal_effect = b1 + b3 * coverage_c,
    se = sqrt(V["shift_c", "shift_c"] +
              coverage_c^2 * V["interaction_lag", "interaction_lag"] +
              2 * coverage_c * V["shift_c", "interaction_lag"]),
    ci_lower = marginal_effect - 1.96 * se,
    ci_upper = marginal_effect + 1.96 * se
  )

ggplot(plot_data, aes(x = coverage_c, y = marginal_effect)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
  labs(x = "Lagged coverage relative to the stock's own average",
       y = "Marginal Effect of Shift on F/FMSY")

ggsave(file.path(path_figs, "marginal_effect_plot.pdf"), width = 7, height = 4.5)
