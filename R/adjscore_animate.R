library(tidyverse)
library(gganimate)

plot_adjusted_score_anim <- function(
    n = 15, B = 100, sigma2 = 1,
    intro_frames = 6,         # show only the score
    grow_frames  = 12,        # duration to draw each segment
    pause_frames = 6,         # pause after each segment is completed
    morph_frames = 24         # duration of the curve shift
) {

  set.seed(123)
  X <- lapply(1:B, function(i) rnorm(n, mean = 0, sd = sqrt(sigma2)))
  
  score_fun <- function(theta, xx) sum(-1/(2*theta) + (xx^2)/(2*theta^2))
  bump <- 3 / n
  
  # Simulated scores (biased grid)
  plot_df <- tibble(b = 1:B) |>
    mutate(
      X = map(b, \(i) X[[i]]),
      theta = list(seq(0.32, 1.25, length.out = 1000)),
      score = map2(X, theta, \(xx, tt) 2 * map_dbl(tt + bump, score_fun, xx))
    ) |>
    select(b, theta, score) |>
    unnest(c(theta, score))
  
  mean_curve <- plot_df |>
    summarize(score = mean(score), .by = theta) |>
    arrange(theta)
  
  theta_hat <- mean_curve |>
    mutate(absscore = abs(score)) |>
    slice_min(absscore, with_ties = FALSE) |>
    pull(theta)
  
  bias  <- theta_hat - sigma2
  s_bar <- approx(mean_curve$theta, mean_curve$score, xout = sigma2)$y
  
  # Timeline
  f_intro_end   <- intro_frames
  f_base_start  <- f_intro_end + 1
  f_base_full   <- f_base_start + grow_frames - 1
  f_base_pause_end <- f_base_full + pause_frames
  
  f_vert_start  <- f_base_pause_end + 1
  f_vert_full   <- f_vert_start + grow_frames - 1
  f_vert_pause_end <- f_vert_full + pause_frames
  
  f_tri_start   <- f_vert_pause_end + 1
  f_morph_start <- f_tri_start + 1
  f_morph_end   <- f_morph_start + morph_frames - 1
  
  total_frames  <- f_morph_end
  
  # Helper to replicate df across a frame range
  rep_frames <- function(df, start, end) {
    if (start > end) return(df[0, , drop = FALSE])
    df |> 
      mutate(frame = list(seq.int(start, end))) |>
      unnest(frame)
  }
  
  # Baseline y = 0 for all frames
  hline_df <- rep_frames(tibble(y = 0), 1, total_frames)
  
  # Orange curve visible ALL frames (so it stays during the shift)
  df_orange <- rep_frames(mean_curve, 1, total_frames) |> 
    mutate(kind = "orange")
  
  # Teal morph curve (appears from f_morph_start onwards), shift left over frames
  t_seq <- seq(0, 1, length.out = morph_frames)
  df_teal <- map2_dfr(t_seq, seq_len(morph_frames), function(t, i) {
    tibble(theta = mean_curve$theta - t*bias,
           score = mean_curve$score,
           frame = f_morph_start + i - 1)
  }) |> 
    mutate(kind = "teal")
  
  # Base (horizontal) segment grows then pauses. Growth frames: interpolate xend
  # from theta_hat to sigma2
  if (theta_hat <= sigma2) {
    base_x0 <- theta_hat; base_x1 <- sigma2
  } else {
    base_x0 <- sigma2;    base_x1 <- theta_hat
  }
  base_g <- seq(0, 1, length.out = grow_frames)
  base_grow <- tibble(
    x    = base_x0,
    y    = 0,
    xend = base_x0 + base_g*(base_x1 - base_x0),
    yend = 0,
    frame = seq.int(f_base_start, f_base_full)
  )
  base_pause <- rep_frames(
    tibble(x = base_x0, y = 0, xend = base_x1, yend = 0),
    f_base_full + 1, total_frames
  )
  base_seg <- bind_rows(base_grow, base_pause)
  
  base_lab <- rep_frames(
    tibble(x = (theta_hat + sigma2)/2, y = 0.35, lab = "𝓑~(vartheta)"),
    f_base_full, total_frames
  )
  
  # Vertical segment grows then pauses
  vert_g <- seq(0, 1, length.out = grow_frames)
  vert_grow <- tibble(
    x    = sigma2,
    y    = 0,
    xend = sigma2,
    yend = vert_g * s_bar,
    frame = seq.int(f_vert_start, f_vert_full)
  )
  vert_pause <- rep_frames(
    tibble(x = sigma2, y = 0, xend = sigma2, yend = s_bar),
    f_vert_full + 1, total_frames
  )
  vert_seg <- bind_rows(vert_grow, vert_pause)
  
  vert_lab <- rep_frames(
    tibble(x = sigma2 + 0.015, y = s_bar/2 - 0.17,
           lab = "𝓑~(vartheta) * 𝐼(vartheta)", angle = 90),
    f_vert_full, total_frames
  )
  
  # Triangle appears immediately at its start, stays on
  tri_df <- rep_frames(
    tibble(x = c(theta_hat, sigma2, sigma2),
           y = c(0,          0,      s_bar),
           id = 1),
    f_tri_start, total_frames
  )
  
  # Axes / limits
  xlim <- c(0.56, 1.25)
  ylim <- c(-4, 4)
  
  p <- ggplot() +
    geom_hline(data = hline_df, aes(yintercept = y), linetype = "dashed") +
    
    # Orange curve (all frames)
    geom_line(data = df_orange, aes(theta, score, group = 1),
              color = "#F18F00", linewidth = 1) +
    
    # Horizontal base: grows, then stays
    geom_segment(data = base_seg,
                 aes(x = x, y = y, xend = xend, yend = yend, group = frame),
                 color = "#80715D", linewidth = 0.9) +
    geom_text(data = base_lab,
              aes(x, y, label = lab), parse = TRUE, size = 4.2) +
    
    # Vertical side: grows, then stays
    geom_segment(data = vert_seg,
                 aes(x = x, y = y, xend = xend, yend = yend, group = frame),
                 color = "#80715D", linewidth = 0.9) +
    geom_text(data = vert_lab,
              aes(x, y, label = lab, angle = angle),
              parse = TRUE, size = 4.2, vjust = 0.5, hjust = 0.5) +
    
    # Triangle fill (appears, then stays) — ensure colour type is character
    geom_polygon(data = tri_df,
                 aes(x, y, group = interaction(id, frame)),
                 fill = "#80715D", alpha = 0.15, colour = NA_character_) +
    
    # Teal curve morph (orange stays visible underneath)
    geom_line(data = df_teal, aes(theta, score, group = frame),
              color = "#00A6AA", linewidth = 1) +
    
    scale_x_continuous(
      limits = xlim,
      breaks = c(theta_hat, sigma2),
      labels = c(expression(hat(vartheta), bar(vartheta))),
      name = NULL
    ) +
    coord_cartesian(ylim = ylim, expand = FALSE) +
    labs(y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12)) +
    
    transition_manual(frame)
  
  p
}

anim <- plot_adjusted_score_anim(
  intro_frames = 6,   # just orange curve
  grow_frames  = 18,  # slow draw
  pause_frames = 6,   # pause after each
  morph_frames = 24   # shift duration
)

# GIF
animate(
  anim, fps = 30, duration = 8,
  width = 1800, height = 900,
  renderer = gifski_renderer(), 
  device = 'ragg_png',
  start_pause = 5,
  end_pause = 120,
  res = 250
)
anim_save("figures/adjusted_score.gif", animation = last_animation())

# MP4 
# animate(anim, fps = 30, duration = 8,
#         width = 900, height = 450,
#         renderer = av_renderer("adjusted_score.mp4"),
#         start_pause = 15,
#         end_pause = 90)
