library(tidyverse)
load("R/results.RData")

mycols <- c(
  ML = "#E31A1C",
  lav = "#FB9A99",
  eRBM = "#A6CEE3",
  iRBM = "#1F78B4",
  Jackknife = "#C8DE00",
  Bootstrap = "#33A02C",
  `Ozenne et al.` = "#FDBF6F",
  REML = "#FF7F00"
)

p1 <-
  plot_df |> 
  filter(param %in% twofacpars) |> 
  summarise(
    B = mean(bias, na.rm = TRUE, trim = 0.05),
    rmse = sqrt(mean(bias ^ 2, na.rm = TRUE, trim = 0.05)),
    pu = mean(bias < 0),
    covr = mean(covered, na.rm = TRUE),
    .by = c(dist:param)
  ) |>
  pivot_longer(B:covr, names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(
      metric,
      levels = c("B", "rmse", "pu", "covr"),
      labels = c("Bias", "RMSE", "PU", "Coverage")
    )
  ) |>
  ggplot(aes(value, param, fill = method)) +
  geom_col(position = "dodge", width = 0.75) +
  geom_vline(
    data = tibble(
      metric = factor(c("Bias", "RMSE", "PU", "Coverage")),
      value = c(0, 0, 0.5, 0.95)
    ),
    aes(xintercept = value),
    linetype = "dashed"
  ) +
  scale_fill_manual(values = mycols) +
  facet_grid(n ~ metric, scales = "free_x") +
  ggh4x::facetted_pos_scales(
    x = list(
      scale_x_continuous(),
      scale_x_continuous(expand = c(0, 0, 0, 0.1)),
      scale_x_continuous(limits = c(0.3, 0.7), labels = scales::percent),
      scale_x_continuous(limits = c(0.6, 1), labels = scales::percent)
    )
  ) +
  scale_y_discrete(labels = rev(c(
    expression(theta["11"]),
    expression(psi["11"]),
    expression(psi["22"]),
    expression(beta),
    expression(lambda["21"])
  ))) +
  guides(fill = guide_legend(reverse = TRUE, position = "bottom")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7.5),
    legend.key.height = unit(1, "pt"), 
    legend.key.width = unit(9, "pt"),
    plot.margin = unit(c(0.05, 0.5, 0.05, 0.5), "cm")
  ) +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = glue::glue("{plot_df$dist[1]}, reliability = {gsub('Rel = ', '', plot_df$rel[1])}"))

p2 <-
  plot_drcomp |>
  mutate(param = forcats::fct_recode(param, 
                                     'theta[11]' = 'Theta["1,1"]',
                                     'psi[11]' = 'Psi["1,1"]',
                                     'psi[22]' = 'Psi["2,2"]',
                                     "beta" = "beta",
                                     'lambda[21]' = 'Lambda["2,1"]')) |>
  filter(model == "twofac", !method %in% c("lav")) |>
  mutate(method = fct_rev(method)) |>
  ggplot(aes(n, rmse, fill = method)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_x_continuous(breaks = 1:5, labels = c(15, 20, 50, 100, 1000)) +
  ggh4x::facet_nested(param ~ rel + dist, labeller = label_parsed) +
  scale_fill_manual(values = mycols) +
  guides(fill = guide_legend(nrow = 1, position = "bottom")) +
  theme_bw() +
  theme(legend.key.height = unit(1, "pt"), legend.key.width = unit(9, "pt"),
        plot.margin = unit(c(0.05, 0.5, 0.05, 0.5), "cm")) +
  labs(x = "Sample size (n)", y = "RMSE", fill = NULL)

p3 <- 
  plot_drcomp |>
  filter(model == "growth", !method %in% c("lav")) |>
  mutate(param = forcats::fct_recode(param, 
                                     'theta[11]' = 'Theta["1,1"]',
                                     'psi[11]' = 'Psi["1,1"]',
                                     'psi[22]' = 'Psi["2,2"]',
                                     'psi[12]' = 'Psi["1,2"]',)) |>
  ggplot(aes(n, relbias, col = method)) +
  geom_line(linewidth = 0.75, alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggh4x::facet_nested(param ~ rel + dist, labeller = label_parsed) +
  scale_color_manual(values = mycols) +
  scale_x_continuous(labels = c(15, 20, 50, 100, 1000)) +
  scale_y_continuous(labels = scales::percent) +
  # coord_cartesian(ylim = c(-0.25, 0.25)) +
  guides(colour = guide_legend(nrow = 1, reverse = TRUE, position = "top")) +
  theme_bw() +
  theme(plot.margin = unit(c(0.05, 0.5, 0.05, 0.5), "cm")) +
  labs(x = "Sample size (n)", y = "Relative mean bias", col = NULL)

