library(tidyverse)

# Simulate data
set.seed(1)
n <- 100
X <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2))
x1 <- X[, 1]
x2 <- X[, 2]

s1sq <- mean(x1^2)
s2sq <- mean(x2^2)

# Log-likelihood for independent Normal(0, σ₁², σ₂²)
loglik <- function(t1, t2, s1sq, s2sq, n) {
  -(n/2)*(log(t1) + log(t2) + s1sq/t1 + s2sq/t2)
}

# Grid for contour plot
theta1_seq <- s1sq + seq(-0.5, 0.5, length.out = 50)
theta2_seq <- s2sq + seq(-0.5, 0.5, length.out = 50)
grid <- expand.grid(theta1 = theta1_seq, theta2 = theta2_seq)
grid$ll <- mapply(loglik, grid$theta1, grid$theta2,
                  MoreArgs = list(s1sq = s1sq, s2sq = s2sq, n = n))

# Observed info at MLE
j11 <- n / (2 * s1sq^2)
j22 <- n / (2 * s2sq^2)
J <- diag(c(j11, j22))

# MLE and illustrative bias
theta_hat <- c(s1sq, s2sq)
Ahat <- c(0.2, -0.1)
# theta_tilde <- theta_hat - solve(J, Ahat)
theta_tilde <- c(1, 1)

# Define a simple artificial "bias field" A(theta)
# Arrows point toward the corrected theta_tilde
A_field <- 
  expand.grid(theta1 = s1sq + seq(-0.5, 0.5, length.out = 18),
              theta2 = s2sq + seq(-0.5, 0.5, length.out = 18)) %>%
  # grid |>
  mutate(
    A1 = theta_tilde[1] - theta1,
    A2 = theta_tilde[2] - theta2
  ) %>%
  # normalize so all arrows same relative size
  mutate(mag = 5 * sqrt(A1^2 + A2^2),
         A1n = 0.15 * A1 / mag,
         A2n = 0.15 * A2 / mag)

# Labeled points
base_df <- data.frame(
  theta1 = c(theta_hat[1], theta_tilde[1]),
  theta2 = c(theta_hat[2], theta_tilde[2]),
  label  = c("MLE", "Bias-corrected")
)

p_erbm <-
  ggplot() +
  geom_contour_filled(
    data = grid, 
    aes(theta1, theta2, z = ll),
    col = "gray50", linetype = "dotted",
    bins = 15
  ) +
  geom_segment(
    aes(x = theta_hat[1], y = theta_hat[2], xend = theta_tilde[1], yend = theta_tilde[2]),
    arrow = arrow(length = unit(0.12, "inches")), 
    color = "#b10f2e", 
    size = 1.1
  ) +  
  geom_segment(
    data = A_field,
    aes(x = theta1, y = theta2,
        xend = theta1 + A1n, yend = theta2 + A2n),
    arrow = arrow(length = unit(0.05, "inches")),
    color = "gray30",
    alpha = 0.4
  ) +  
  annotate("point", theta_hat[1], theta_hat[2], size = 2) +
  annotate("point", theta_tilde[1], theta_tilde[2], col = "#b10f2e") +
  annotate("label", x = theta_hat[1], y = theta_hat[2],
           label = as.character(expression(hat(theta))),
           parse = TRUE, vjust = 0.5, hjust = 1.2, size = 6) +
  annotate("label", x = theta_tilde[1], y = theta_tilde[2],
           label = as.character(expression(tilde(theta))),
           parse = TRUE, vjust = 0.5, hjust = -0.2, size = 6, color = "#b10f2e") +

  scale_x_continuous(
    expand = c(0, 0),
    # breaks = theta_hat[1],
    # labels = expression(hat(theta)[1])
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    # breaks = c(theta_hat[2], theta_tilde[2]),
    # labels = c(expression(hat(theta)[2]), expression(tilde(theta)[2]))
  ) +  
  scale_fill_manual(values = terrain.colors(15)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # axis.text = element_text(size = 14),
    axis.text = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL, y = NULL)
