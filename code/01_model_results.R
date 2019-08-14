### Setup ####

source(here::here("code/_dependencies.R"))

# values of n to consider
ns <- c(5, 7, 9)
# ns_linetypes <- c("solid", "dashed", "dotted")
ns_linetypes <- c("solid", "solid", "solid")
ns_colors <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
# http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3

# range and granularity of p's to consider
ps <- seq(from = 0, to = 1, by = .001)



### Functions ####

# Calculate the probability that the majority of reasons point to the correct alternative
# given p and the number of subdecisions n;
# this calculation is identical to the standard Condorcet Jury Theorem
# (see, e.g., Grofman et al. 1983, eq. 1, p. 264)
fnc_prob_maj_correct <- function(p, n) {
  
  fnc_prob_maj_correct1 <- function(p, n) {
    if ((n %% 2) == 0)
      stop("n needs to be odd!")
    
    h <- seq(from = (n + 1) / 2,
             to = n,
             by = 1)
    # h: sizes of all possible correct majorities
    
    sum(choose(n, h) * (p ^ h) * (1 - p) ^ (n - h))
  }
  
  mapply(fnc_prob_maj_correct1, p, n)
}


fnc_conf <- function(p, n) {
  
  fnc_conf1 <- function(p, n) {
    if ((n %% 2) == 0)
      stop("n needs to be odd!")
    
    s <- 0:n
    # s: all possible cue tallies
    
    sum(choose(n, s) * (p ^ s) * (1 - p) ^ (n - s) * (1 - sqrt((s/n)*(1 - s/n))))
  }
  
  mapply(fnc_conf1, p, n)
}


### Orthogonally vary variables and calculate measures ####

dat <- expand.grid(ps, ns) %>%
  as_tibble %>%
  rename(p = Var1, n = Var2) %>%
  mutate(
    P = fnc_prob_maj_correct(p = p, n = n),
    C = fnc_conf(p = p, n = n),
    I = 2 * P * (1 - P)
  )



### Theming ####

theme_set(theme_bw()) +
  theme_update(
    plot.title = element_text(face = "plain", size = 18, hjust = .5),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "plain", hjust = .5, size = 10),
    legend.background = element_blank()
  )
line_size <- .6



### Figure 1A ####
fig_I_P <- dat %>%
  ggplot(aes(P, I)) +
  coord_fixed(ratio = 2) +
  scale_x_continuous("Proportion correct (P)", limits = c(0, 1)) +
  scale_y_continuous("Inconsistency (I)",
                     limits = c(0 - 0.05, 0.5),
                     breaks = seq(0, .5, by = .1)) +
  annotate(
    "segment",
    x = 0.5,
    xend = 0.5,
    y = 0,
    yend = 0.5,
    linetype = "dotted",
    alpha = .25
  ) +
  geom_line(size = line_size, color = ns_colors[1]) +
  annotate(
    "text",
    0,
    0,
    label = "wicked\nagreement",
    hjust = -0.1,
    vjust = 1.25,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    1,
    0,
    label = "kind\nagreement",
    hjust = 1.1,
    vjust = 1.25,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    0.5,
    0,
    label = "ambiguous\ndisagreement",
    hjust = 0.5,
    vjust = 1.25,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  )
fig_I_P
# note: the relationship between P and I does not depend on n
# because n is only involved when going from p to P
#
# dat %>%
#   ggplot(aes(p, 1 - I, linetype = as.factor(n))) +
#   geom_line()


### Figure 1B ####
fig_C_P <- dat %>%
  ggplot(aes(P, C, color = as.factor(n))) +
  coord_fixed(ratio = 2) +
  scale_x_continuous("Proportion correct (P)", limits = c(0, 1)) +
  scale_y_continuous("Confidence", limits = c(0.5 - 0.025, 1)) +
  #scale_linetype_manual("n", values = ns_linetypes) +
  scale_color_manual("n", values = ns_colors) +
  annotate(
    "segment",
    x = 0.5,
    xend = 0.5,
    y = 0.5,
    yend = 1,
    linetype = "dotted",
    alpha = .25
  ) +
  geom_line(size = line_size) +
  theme(legend.position = c(0.65, .7)) +
  annotate(
    "text",
    0,
    0.5,
    label = "wicked\nagreement",
    hjust = -0.1,
    vjust = .75,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    0.5,
    0.5,
    label = "ambiguous\ndisagreement",
    hjust = 0.5,
    vjust = .75,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    1,
    0.5,
    label = "kind\nagreement",
    hjust = 1.1,
    vjust = 0.75,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  )
fig_C_P


### Figure 1C ####
fig_I_C <- dat %>%
  ggplot(aes(C, I, color = as.factor(n))) +
  coord_fixed(ratio = 1) +
  scale_x_continuous("Confidence (C)", limits = c(0.5, 1)) +
  scale_y_continuous("Inconsistency (I)", limits = c(0, 0.5)) +
  scale_color_manual("n", values = ns_colors) +
  geom_line(size = line_size) +
  theme(legend.position = c(0.75, .7))
fig_I_C



### Combine panels ####
fig_SCM <- plot_grid(
  fig_I_P,
  fig_C_P,
  fig_I_C,
  nrow = 1,
  labels = c("A", "B", "C"),
  scale = .95
)

save_plot(
  here("output/figures/fig_SCM.pdf"),
  fig_SCM,
  base_aspect_ratio = 3,
  base_height = 3
)
