# Illustration of bias of MLE of sigma^2
library(tidyverse)

n <- 10  # sample size
B <- 10000  # number of simulations
sigma <- 2  # true standard deviation
set.seed(123)

res <- map_dbl (
  1:B,
  \(x) {
    x <- rnorm(n, mean = 0, sd = sigma)
    s2 <- mean((x - mean(x)) ^ 2)
    return(s2)
  },
  .progress = TRUE
)

tibble(s2 = res) |>
  ggplot(aes(x = s2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = sigma^2, sd = sqrt(2 * sigma^4 / n)), color = "red", size = 1) +
  geom_vline(xintercept = sigma^2, color = "blue", linetype = "dashed", size = 1) +
  labs(title = expression(paste("Distribution of ", hat(sigma)^2)),
       x = expression(hat(sigma)^2),
       y = "Density") +
  theme_minimal()
