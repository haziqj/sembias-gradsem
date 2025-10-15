# Illustration of bias of MLE of sigma^2
library(tidyverse)

plot_sigma2_bias <- function(n = 10, B = 1000, sigma = 1, seed = 1,
                             showbias = FALSE) {
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
  
  p <- 
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
      y = "Density",
      col = "Based on 2000 replications of the ML fit"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(hjust = 1, vjust = 8),
      plot.margin = margin_part(b = -10),      
      legend.position = c(0.8, 0.8),
      legend.key.width = unit(1.2, "cm"),
      legend.title = element_text(hjust = 1) # Right-align legend text
    ) 
  
  if (isTRUE(showbias)) {
    x0 <- ml_center
    x1 <- sigma ^ 2
    y0 <- max(density(df$x)$y) * 0.95
    p + 
      annotate(
        "segment", x = x0, xend = x1, y = y0, yend = y0,
        linewidth = 0.6, colour = "grey40",
        arrow = arrow(length = unit(3, "pt"), ends = "both", type = "closed")
      ) +
      annotate(
        "text", x = (x0 + x1) / 2, y = y0 * 0.95, size = 3,
        col = "grey40", label = "bias", fontface = "plain")
  } else {
    p
  }
}

