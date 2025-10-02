# Illustration of bias of MLE of sigma^2
library(tidyverse)

plot_sigma2_bias <- function(n = 10, B = 1000, sigma = 1, seed = 1) {
  set.seed(seed)
  
  res <- map_dbl (
    1:B,
    \(x) {
      x <- rnorm(n, mean = 0, sd = sigma)
      s2 <- mean((x - mean(x)) ^ 2)
      return(s2)
    },
    .progress = TRUE
  )
  
  df <- data.frame(x = res)
  ml_center <- mean(res)
  vlines <- data.frame(
    x = c(ml_center, sigma ^ 2),
    Estimator = c("ML", "Truth")
  )
  
  ggplot(df, aes(x = res)) +
    geom_density(
      aes(fill = "ML", col = "ML"), 
      alpha = 0.5, 
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_rug(
      aes(colour = "ML"), 
      length = grid::unit(10, "pt"),
      sides = "b", 
      alpha = 0.2, 
      size = 0.5, 
      show.legend = FALSE
    ) +
    geom_vline(
      data = vlines,
      aes(xintercept = x, col = Estimator, linetype = Estimator),
      linewidth = 0.8,
      key_glyph = "path"
    ) +
    annotate(
      "text", 
      x = ml_center, 
      y = max(density(df$x)$y) * 0.95,  
      label = "E(hat(sigma)^2)",
      hjust = 1.1, vjust = 0,           
      parse = TRUE,
      col = "#F18F00",
      size = 3.5
    ) +  
    annotate(
      "text", 
      x = sigma ^ 2, 
      y = max(density(df$x)$y) * 0.96,  
      label = "bar(sigma)^2",
      hjust = -0.5, vjust = 0,          
      parse = TRUE,
      col = "black",
      size = 3.5
    ) +        
    scale_colour_manual(
      values = c("ML" = "#F18F00", "Truth" = "black")
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("ML" = "#F18F00")
    ) +
    scale_linetype_manual(values = c(Truth = "solid", ML = "dashed")) +
    scale_y_continuous(expand = c(0.09, 0)) +
    guides(
      colour = guide_legend(override.aes = list(geom = "line", size = 5)),
      linetype = "none",
    ) +
    labs(
      x = expression(hat(sigma)^2),
      # x = NULL,
      y = "Density",
      col = "Based on 2000 replications of the ML fit"
    ) +
    theme_minimal() +
    theme(
      legend.position = c(0.8, 0.8),
      legend.key.width = unit(1.2, "cm"),
      legend.title = element_text(hjust = 1) # Right-align legend text
    ) 
}

