library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(latex2exp)

# parameters
J     <- 5
alpha <- c(-2.5, -1.3, .2, 1.3) 
eta   <- -1.5   


# make a color pallette
library(RColorBrewer)
colors_J <- brewer.pal(J, "Dark2")  # or "Set1", "Paired", "Accent"
colors_J





cumulative_prob <- c(plogis(alpha - eta), 1)
prob <- c(cumulative_prob[1], diff(cumulative_prob))

# S-shaped curve
curve_df <- tibble(x = seq(-8, 8, by = 0.001)) |>
  mutate(prob = plogis(x)) |>
  glimpse()

# lines
x_lines_df <- tibble(x0 = alpha - eta, x1 = alpha - eta, 
                     y0 = 0, y1 = cumulative_prob[1:(J - 1)]) |>
  glimpse()

y_lines_df <- tibble(x0 = min(curve_df$x), x1 = alpha - eta, 
                     y0 = cumulative_prob[1:(J - 1)], y1 = cumulative_prob[1:(J - 1)]) |>
  glimpse()

# labels
x_labs <- paste0(
  r"($\alpha_{)", 1:(J - 1), r"(} - \eta_i = )",
  scales::number(alpha - eta, accuracy = 0.01),
  r"($)"
)
x_labels_df <- tibble(
  x = alpha - eta,
  y = cumulative_prob[1:(J - 1)]/2,
  label = TeX(x_labs)
)
y_labs <- paste0(
  r"($\Pr(Y_i \leq )", 1:(J - 1), ")",
  r"(=logit^{-1}(\alpha_{)", 1:(J - 1),
  r"(} - \eta_i)=)",
  scales::number(cumulative_prob[1:(J - 1)], accuracy = 0.01),
  r"($)"
)
y_labels_df <- tibble(
  x = (min(curve_df$x) + (alpha - eta)) / 2,
  y = cumulative_prob[1:(J - 1)],
  label = TeX(y_labs), 
  region = as.character(1:(J-1))
)

# probability labels
probs_df <- tibble(x = min(curve_df$x), y = cumulative_prob)
prob_labs <- paste0(
  r"($\Pr(Y_i = )", 1:J, ") = ",
  r"(\Pr(Y_i \leq )", (1:J), ") - ",
  r"(\Pr(Y_i \leq )", 1:J - 1, ") = ", 
  scales::number(prob[1:J], accuracy = 0.01),
  r"($)"
)
prob_labs[1] <- paste0(
  r"($\Pr(Y_i = 1) = \Pr(Y_i \leq 1) = )", 
  scales::number(prob[1], accuracy = 0.01),
  r"($)"
)
prob_labs[J] <- paste0(
  r"($\Pr(Y_i = )", J, ") =", r"(1 - \Pr(Y_i \leq )", J - 1, ") =", 
  scales::number(prob[1], accuracy = 0.01),
  r"($)"
)
prob_labels_df <- tibble(
  x = min(curve_df$x),
  y = cumulative_prob - diff(c(0, cumulative_prob))/2,
  label = TeX(prob_labs), 
  region = as.character(1:(J))
)

# y polygons: use a for-loop to build the polygons one at a time
y_poly_list <- NULL
y_poly_list[[1]] <- curve_df |>
  filter(x <= alpha[1] - eta) |>
  mutate(to = cumulative_prob[1], 
         from = prob) |>
  mutate(region = as.character(1)) |>
  glimpse()
for (j in 2:(J - 1)) {
  y_poly_list[[j]] <- curve_df |>
    filter(x <= alpha[j] - eta) |>
    mutate(to = cumulative_prob[j], 
           from = ifelse(cumulative_prob[j-1] > prob, cumulative_prob[j - 1], prob)) |>
    mutate(region = as.character(j)) |>
    glimpse()  
}
y_poly_list[[J]] <- curve_df |>
  mutate(to = 1, 
         from = ifelse(cumulative_prob[J-1] > prob, cumulative_prob[J - 1], prob)) |>
  mutate(region = as.character(J)) |>
  glimpse()
y_poly_df <- bind_rows(y_poly_list) |>
  glimpse()

# x polygons: use a for-loop to build the polygons one at a time
x_poly_list <- NULL
x_poly_list[[1]] <- curve_df |>
  filter(x <= alpha[1] - eta) |>
  mutate(to = prob, 
         from = 0) |>
  mutate(region = as.character(1)) |>
  glimpse()
for (j in 2:(J - 1)) {
  x_poly_list[[j]] <- curve_df |>
    filter(x <= alpha[j] - eta, x > alpha[j - 1] - eta) |>
    mutate(to = prob, 
           from = 0) |>
    mutate(region = as.character(j)) |>
    glimpse()  
}
x_poly_list[[J]] <- curve_df |>
  filter(x > alpha[J - 1] - eta) |>
  mutate(to = prob, 
         from = 0) |>
  mutate(region = as.character(J)) |>
  glimpse()
x_poly_df <- bind_rows(x_poly_list) |>
  glimpse()

# probability blocks
width <- diff(range(curve_df$x))*0.05
block_list <- NULL
block_list[[1]] <- tibble(x = seq(min(curve_df$x) - width, min(curve_df$x), by = 0.01)) |>
  mutate(from = 0, to = cumulative_prob[1], region = "1") 
for (j in 2:(J - 1)) {
  block_list[[j]] <- tibble(x = seq(min(curve_df$x) - width, min(curve_df$x), by = 0.01)) |>
    mutate(from = cumulative_prob[j-1], to = cumulative_prob[j], region = as.character(j)) |>
    glimpse()  
}
block_list[[J]] <- tibble(x = seq(min(curve_df$x) - width, min(curve_df$x), by = 0.01)) |>
  mutate(from = cumulative_prob[J - 1], to = cumulative_prob[J], region = as.character(J))


block_df <- bind_rows(block_list)




ggplot() +
  geom_ribbon(data = block_df, aes(x = x, ymin = from, ymax = to, fill = region), alpha = 1.0, color = "black") +
  geom_segment(aes(x = min(curve_df$x), y = 0, yend = 1), color = "black") + 
  geom_segment(aes(x = min(curve_df$x) - width, y = 0, yend = 1), color = "black") + 
  geom_ribbon(data = x_poly_df, aes(x = x, ymin = from, ymax = to, fill = region), alpha = 0.35) +
  geom_ribbon(data = y_poly_df, aes(x = x, ymin = from, ymax = to, fill = region), alpha = 0.35) +
  
  geom_segment(data = x_lines_df, aes(x = x0, xend = x1, y = y0, yend = y1), linetype = "dotted") +
  geom_label(data = x_labels_df, aes(x = x, y = y, label = label), parse = TRUE) +
  geom_segment(data = y_lines_df, aes(x = x0, xend = x1, y = y0, yend = y1), linetype = "dotted") +
  geom_segment(data = y_labels_df, aes(x = x, xend = x, y = y, yend = 0, color = region), linewidth = 4) + 
  
  geom_label(data = y_labels_df, aes(x = x, y = y, label = label, color = region), parse = TRUE) +
  geom_line(data = curve_df, aes(x = x, y = prob)) +
  geom_point(data = x_lines_df, aes(x = x1, y = y1), shape = 21, fill = "white") +
  geom_point(data = probs_df, aes(x = x, y = y), pch = 19) +
  geom_label(data = prob_labels_df, aes(x = x - 2*width, y = y, label = label, color = region), parse = TRUE, hjust = 0) +
  labs(x = "Input to Inverse-Logit Function", y = "Probability") +
  scale_fill_manual(values = colors_J) + 
  scale_color_manual(values = colors_J) + 
  theme_minimal() 
