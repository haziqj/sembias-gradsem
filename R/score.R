# Illustration of random scores for normal variance 
library(tidyverse)

plot_sigma2_score <- function(n = 15, B = 100, sigma2 = 1, showbias = TRUE) {
  set.seed(123)
  X <- lapply(1:B, function(i) {
    rnorm(n, mean = 0, sd = sqrt(sigma2))
  })
  
  loglik <- function(theta, xx = x) sum(dnorm(xx, sd = sqrt(theta), log = TRUE))
  score <- function(theta, xx = x) {
    sum(-1 / (2 * theta) + (xx - 0) ^ 2 / (2 * theta ^ 2))
  }
  bump <- 3 / n # exagerate the bias, diminishes as n -> Inf
  
  # Prepare data for plotting
  plot_df <-
    tibble(b = 1:B) |>
    mutate(
      X = map(b, \(i) X[[i]]),
      theta = list(seq(0.32, 1.25, length = 1000)),
      score = map2(X, theta, \(xx, tt) 2 * map_dbl(tt + bump, score, xx))
    ) |>
    select(b, theta, score) |>
    unnest(c(theta, score))
  
  plot_df2 <- summarise(plot_df, score = mean(score), .by = theta)
  theta_hat <-
    plot_df2 |>
    mutate(absscore = abs(score)) |>
    filter(absscore == min(absscore)) |>
    slice(1) |>
    pull(theta)
  bias <- theta_hat - sigma2
  plot_df3 <-
    plot_df2 |>
    mutate(theta = theta - bias) |>
    filter(theta <= 1.25)
  
  x0 <- min(theta_hat, sigma2)
  x1 <- max(theta_hat, sigma2)
  y0 <- max(plot_df2$score, na.rm = TRUE) * 0.90   # pick a comfy height
  
  # Plot
  p <-
    ggplot(plot_df, aes(theta, score, group = b)) +
    geom_line(linewidth = 0.1, col = "#F18F00", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(
      data = plot_df2,
      aes(theta, score),
      col = "#F18F00",
      inherit.aes = FALSE,
      linewidth = 1
    ) +
    # geom_line(
    #   data = plot_df3,
    #   aes(theta, score),
    #   inherit.aes = FALSE,
    #   col = "red3",
    #   linewidth = 1
    # ) +
    geom_vline(xintercept = theta_hat, linetype = "dashed", col = "#F18F00",
               linewidth = 1) +
    geom_vline(xintercept = sigma2, linewidth = 1) +
    scale_x_continuous(
      expand = expansion(add = c(0, 0.05)) 
      # breaks = c(theta_hat, sigma2),
      # labels = c(expression(hat(theta), theta[0])),
      # name = NULL
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(hjust = 1, vjust = 8),
      plot.margin = margin_part(b = -10)
      # panel.grid.major.y = element_blank(),
      # panel.grid.minor.y = element_blank(),
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank()
    ) +
    # coord_cartesian(ylim = c(-8, 28)) +
    labs(
      y = expression(Score~s(sigma^2)),
      x = expression(sigma^2)
    ) +
    annotate(
      "text", col = "black",
      x = sigma2, y = max(plot_df2$score, na.rm = TRUE) * 0.96,  
      label = "bar(sigma)^2",
      hjust = -0.5, vjust = 0,          
      parse = TRUE,
      size = 4
    ) +
    annotate(
      "text", 
      x = theta_hat, 
      y = max(plot_df2$score, na.rm = TRUE) * 0.95,  
      label = "E(hat(sigma)^2)",
      hjust = 1.1, vjust = 0,           
      parse = TRUE,
      col = "#F18F00",
      size = 4
    )
             
  
  if (isTRUE(showbias)) {
    p + 
      annotate(
        "segment", x = x0, xend = x1, y = y0, yend = y0,
        linewidth = 0.6, colour = "grey40",
        arrow = arrow(length = unit(5, "pt"), ends = "both", type = "closed")
      ) +
      annotate(
        "text", x = (x0 + x1) / 2, y = y0 + 1.5,
        col = "grey40", label = "bias", fontface = "plain")
  } else {
    p
  }
  
}

# plot_sigma2_score(n = 15) + coord_cartesian(ylim = c(-8, 28))

