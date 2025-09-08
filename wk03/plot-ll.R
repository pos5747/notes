


library(tidyverse)
library(plotly)
library(hrbrthemes)
library(grid)  
library(latex2exp)

library(showtext)
font_add_google("Source Sans 3", family = "Source Sans 3")
showtext_auto() 

library(rayshader)



set.seed(1234)
y <- rbeta(1000, shape1 = 10, shape2 = 10)


compute_ll <- Vectorize(function(a, b) {
    sum(dbeta(y, shape1 = a, shape2 = b, log = TRUE))
  })
alpha <- seq(0.9, 25, length.out = 250)
beta  <- seq(0.9, 25, length.out = 250)
data <- crossing(alpha, beta) %>%
  mutate(log_lik = compute_ll(alpha, beta)) |>
  glimpse()

contour_lines_at <- quantile(data$log_lik, probs = c(.99, 0.97, .9, .8, .6, .4))

# find the max log-likelihood
max_point <- data %>% 
  slice_max(order_by = log_lik, n = 1)

# add it to the ggplot
p <- ggplot(data, aes(x = alpha, y = beta, z = log_lik)) + 
  coord_fixed(ratio = 1) + 
  geom_raster(aes(fill= log_lik)) + 
  geom_contour(breaks = contour_lines_at, color = "grey30") + 
  geom_point(data = max_point, aes(x = alpha, y = beta),
             color = "black", size = 3, shape = 19) +
  scale_fill_gradient2(high = "#e41a1c", low = "#377eb8", mid = "#f7f7f7", midpoint = -5000) +
  theme_ipsum(base_family = "Source Sans 3") + 
  theme(plot.background = element_rect(color = "white"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, angle = 0)) + 
  labs(x = expression(alpha), y = expression(beta),
       fill = "Log L",
       title = "Log-likelihood for beta model")

p


# --- build a rotatable 3D surface with matching colors & contour levels ----

# reshape to a wide matrix for plotly::add_surface()
Z <- data |>
  arrange(alpha, beta) |>
  select(alpha, beta, log_lik) |>
  tidyr::pivot_wider(names_from = beta, values_from = log_lik) |>
  select(-alpha) |>
  as.matrix()

x <- alpha                      # x-grid
y <- beta                       # y-grid
zmin <- min(data$log_lik)
zmax <- max(data$log_lik)
zmid <- -5000                   # same midpoint as your ggplot scale

# put the midpoint at the correct position in the colorscale (0..1)
midpos <- (zmid - zmin) / (zmax - zmin)
midpos <- min(max(midpos, 0), 1)  # clamp just in case

# custom three-stop colorscale to match scale_fill_gradient2()
cs <- list(
  list(0,      "#377eb8"),   # low
  list(midpos, "#f7f7f7"),   # mid
  list(1,      "#e41a1c")    # high
)

fig3d <- plot_ly() |>
  # surface with (approximate) on-surface contours for context while rotating
  add_surface(
    x = ~x,
    y = ~y,
    z = ~Z,
    colorscale = cs,
    cmin = zmin,
    cmax = zmax,
    showscale = TRUE,
    contours = list(
      z = list(
        show = TRUE,
        color = "rgba(60,60,60,0.6)",
        highlightcolor = "rgba(60,60,60,0.9)",
        project = list(z = TRUE),
        start = min(contour_lines_at),
        end   = max(contour_lines_at),
        size  = (max(contour_lines_at) - min(contour_lines_at)) / max(1, length(contour_lines_at) - 1)
      )
    )
  )

# overlay exact contour levels on the XY plane (matching your ggplot lines)
for (lev in contour_lines_at) {
  fig3d <- fig3d |>
    add_contour(
      x = x, y = y, z = Z,
      contours = list(
        start = lev, end = lev, size = 1e-9,  # draw only this level
        coloring = "lines", showlabels = FALSE
      ),
      line = list(color = "grey30", width = 1),
      showscale = FALSE,
      hoverinfo = "skip"
    )
}

# add the MLE point as a 3D marker
fig3d <- fig3d |>
  add_markers(
    x = max_point$alpha,
    y = max_point$beta,
    z = max_point$log_lik,
    marker = list(size = 4, color = "black", symbol = "circle"),
    name = "MLE",
    hovertemplate = "α=%{x:.3f}<br>β=%{y:.3f}<br>log L=%{z:.1f}<extra>MLE</extra>"
  ) |>
  layout(
    title = list(text = "Log-likelihood for beta model"),
    scene = list(
      xaxis = list(title = "α"),
      yaxis = list(title = "β"),
      zaxis = list(title = "log L"),
      aspectmode = "cube",
      camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8))
    ),
    font = list(family = "Source Sans 3")
  )

fig3d


