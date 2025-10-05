library(tidyverse)
n = 15
sigma2 = 1

set.seed(3)
X <- rnorm(n, mean = 0, sd = sqrt(sigma2))

# MLE for theta when mean=0 is known:
s2 <- mean(X ^ 2)

# Per-observation score and Hessian
score_i <- function(theta, xx) { -1 / (2 * theta) + xx ^ 2 / (2 * theta ^ 2) }
hess_i  <- function(theta, xx) {  1 / (2 * theta ^ 2) - xx ^ 2 / (theta ^ 3) }

# Aggregates
J_observed <- function(theta, xx) { -sum(hess_i(theta, xx)) }     
# j_expected <- function(theta, n)  {  n / (2 * theta^2) }
e_cross    <- function(theta, xx) { sum(score_i(theta, xx) ^ 2) }   
# S_total    <- function(theta, xx) { sum(score_i(theta, xx)) }     

j <- J_observed(s2, X)
e <- e_cross(s2, X)



s2 <- mean((X - mean(X)) ^ 2)
bias <- s2 - sigma2

loglik <- function(theta, xx = x) sum(dnorm(xx, sd = sqrt(theta), log = TRUE))
penloglik <- function(theta, xx = x) {
  theta <- theta + bias - 0.1
  pen <- -0.5 * e_cross(theta, xx) / J_observed(theta, xx)
  loglik(theta, xx) + pen
}


plot_df <-
  tibble(
    theta = seq(0.35, 1, length = 1000),
    loglik = map_dbl(theta + 0, loglik, xx = X),
    loglikpen = map_dbl(theta, \(tt) loglik(tt, xx = X) + 0.5 * 1 / tt - 0.4 * 1 / tt^2)
  )

s2_tilde <- 
  filter(plot_df, loglikpen == max(loglikpen)) |>
  pull(theta)

plot_df |>
  pivot_longer(cols = c(loglik, loglikpen)) |>
  ggplot(aes(theta, value, col = name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(loglik = "#F18F00", loglikpen = "#00A6AA"),
    labels = c(loglik = "Log-likelihood", loglikpen = "Penalized log-likelihood"),
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.posi
  )
  coord_cartesian(ylim = c(-18, -17.6), xlim = c(0.44, 0.9)) 





