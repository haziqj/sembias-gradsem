plot_adjusted_score <- function(n = 15, B = 100, sigma2 = 1, showbias = TRUE) {
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
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(
      data = plot_df2,
      aes(theta, score),
      col = "#F18F00",
      inherit.aes = FALSE,
      linewidth = 1
    ) +
    geom_line(
      data = plot_df3,
      aes(theta, score),
      inherit.aes = FALSE,
      col = "#00A6AA",
      linewidth = 1
    ) +
    scale_x_continuous(
      breaks = c(theta_hat, sigma2),
      labels = c(expression(hat(vartheta), bar(vartheta))),
      name = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12)
    ) +
    labs(
      y = expression(Score~s(vartheta)),
      x = expression(theta)
    ) 
  
  # score at thetabar (= sigma2): s(thetabar)
  s_bar <- approx(plot_df2$theta, plot_df2$score, xout = sigma2)$y
  
  # triangle vertices: (thetahat, 0) -> (thetabar, 0) -> (thetabar, s(thetabar))
  tri_df <- tibble::tibble(
    x = c(theta_hat, sigma2, sigma2),
    y = c(0,          0,      s_bar)
  )
  
  # positions for labels
  x_mid_base <- (theta_hat + sigma2) / 2
  y_base_lab <- -0.35                         # a bit below the axis
  x_vert_lab <- sigma2 + 0.005                # tiny right offset
  y_vert_lab <- s_bar / 2
  
  # --- then in the ggplot chain `p <- ggplot(...) + ...` add these layers: ---
  p <- p +
    # filled triangle (light tint)
    ggplot2::geom_polygon(
      data = tri_df,
      aes(x, y),
      fill = "#80715D", alpha = 0.15, color = NA, inherit.aes = FALSE
    ) +
    # triangle edges
    annotate(
      "segment",
      x = theta_hat, y = 0, xend = sigma2, yend = 0,
      linewidth = 0.7, color = "#80715D"
    ) +
    annotate(
      "segment", x = sigma2, y = 0, xend = sigma2, yend = s_bar,
      linewidth = 0.7, color = "#80715D"
    ) +
    ggplot2::annotate(
      "text",
      x = x_mid_base, y = y_base_lab + 0.72,
      label = "B(vartheta)", parse = TRUE, size = 5
    ) +
    ggplot2::annotate(
      "text",
      x = x_vert_lab + 0.037, y = y_vert_lab - 0.45, 
      label = "B(vartheta) * I(vartheta)", parse = TRUE, size = 5
    )
  
  p + coord_cartesian(ylim = c(-4, 4), xlim = c(0.56, 1.25))
  
  
}
