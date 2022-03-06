### Setup ####
source(here::here("code/_dependencies.R"))


### Load data ####
trials <- read_rds(here("output/data/trials.rds"))
case_infos <- read_rds(here("output/data/case_infos.rds"))
study_infos <- read_rds(here("output/data/study_infos.rds"))


### Theming ####
theme_set(theme_bw()) +
  theme_update(
    plot.title = element_text(face = "bold", size = 18, hjust = .5),
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
point_size <- 1.5



title_breast <- filter(study_infos, study == "breast") %>%
  pull(study_title) %>% 
  as.character
color_breast <- filter(study_infos, study == "breast") %>%
  pull(study_color)

title_back <- filter(study_infos, study == "back") %>%
  pull(study_title) %>% 
  as.character
color_back <- filter(study_infos, study == "back") %>%
  pull(study_color)



### Functions ####

# get the posterior distribution of the predicted 
# population values for kind vs. wicked cases
# for first and second diagnoses, as well as the
# maximum-confidence-slating (MCS) diagnosis
fnc_get_M7_posterior <- function(m) {

  # conditions
  cells <- tribble(
    ~ decision, ~ type,
    "mcs", "kind",
    "first", "kind",
    "second", "kind",
    "mcs", "wicked",
    "first", "wicked",
    "second", "wicked",
  )
  
  # get the model's predictions for the different conditions
  # note: we only consider the uncertainty for the population/fixed
  # effects (i.e., we do not incorporate uncertainty in the group-level/
  # random effects; `re_formula = NA`)
  posterior <- m %>%
    fitted(newdata = cells,
           re_formula = NA,
           summary = FALSE)
  
  # create variable names for the posterior distribution matrix
  cells <- cells %>%
    mutate(cell = str_c(decision, "_", type))
  
  # name the variables of the posterior distribution matrix
  colnames(posterior) <- pull(cells, cell)
  
  # add a sample id
  posterior <- as.data.frame(posterior) %>%
    as_tibble() %>%
    mutate(i = row_number())
  
  # calculate the posterior distributions for the four comparisons
  posterior <- posterior %>%
    mutate(
      mcs_minus_first_kind = mcs_kind - first_kind,
      mcs_minus_second_kind = mcs_kind - second_kind,
      mcs_minus_first_wicked = mcs_wicked - first_wicked,
      mcs_minus_second_wicked = mcs_wicked - second_wicked
    )
  
  return(posterior)
}

# custom smoothing geom
my_smooth <- function(color, span = 2, ...)
  stat_smooth(
    geom = "smooth",
    alpha = 0,
    size = 1,
    color = color,
    method = "loess",
    formula = "y ~ x",
    span = span,
    ...
  )




### Figure `fig_empirical`: Main empirical results ####

fig_empirical_I_P <- case_infos %>%
  filter(study == "breast") %>%
  ggplot(aes(pc1, I)) +
  scale_x_continuous("Proportion correct (P)") +
  scale_y_continuous("Inconsistency (I)") +
  coord_cartesian(xlim = c(0, 1.05),
                  ylim = c(-0.05, max(case_infos$I) + 0.05)) +
  geom_vline(xintercept = 0.5,
             linetype = "dotted",
             alpha = .25)

fig_breast_I_P <- fig_empirical_I_P +
  ggtitle(title_breast) +
  my_smooth(color = color_breast,
            mapping = aes(x = pc2),
            linetype = "dashed") +
  my_smooth(color = color_breast) +
  geom_point(color = color_breast,
             size = point_size,
             alpha = .25) +
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
fig_breast_I_P

fig_back_I_P <- fig_empirical_I_P %+%
  filter(case_infos, study == "back") +
  ggtitle(title_back) +
  my_smooth(color = color_back,
            mapping = aes(x = pc2),
            linetype = "dashed") +
  my_smooth(color = color_back) +
  geom_point(
    aes(x = pc1j, y = Ij),
    color = color_back,
    size = point_size,
    alpha = .15
  ) +
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
fig_back_I_P


fig_breast_C_P_y_annotate <-
  min(filter(case_infos, study == "breast")$C1) - 0.1

fig_breast_C_P <- fig_empirical_I_P +
  aes(y = C1) +
  scale_y_continuous("Mean confidence (C)") +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(
      min(filter(case_infos, study == "breast")$C1) - 0.4,
      max(filter(case_infos, study == "breast")$C1) + 0.2
    )) +
  my_smooth(color = color_breast,
            mapping = aes(x = pc2, y = C2),
            linetype = "dashed") +
  my_smooth(color = color_breast) +
  geom_point(color = color_breast,
             size = point_size,
             alpha = .25) +
  annotate(
    "text",
    0,
    fig_breast_C_P_y_annotate,
    label = "wicked\nagreement",
    hjust = -0.1,
    vjust = 1.5,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    1,
    fig_breast_C_P_y_annotate,
    label = "kind\nagreement",
    hjust = 1.1,
    vjust = 1.5,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    0.5,
    fig_breast_C_P_y_annotate,
    label = "ambiguous\ndisagreement",
    hjust = 0.5,
    vjust = 1.5,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  )


fig_back_C_P_y_annotate <- min(filter(case_infos, study == "back")$C1)
fig_back_C_P <- fig_empirical_I_P %+%
  filter(case_infos, study == "back") +
  aes(y = C1) +
  scale_y_continuous("Mean confidence (C)") +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(1, 2)) +
  my_smooth(color = color_back,
            mapping = aes(x = pc2, y = C2),
            linetype = "dashed") +
  my_smooth(color = color_back) +
  geom_point(
    aes(y = C1j),
    color = color_back,
    size = point_size,
    alpha = .15
  ) +
  annotate(
    "text",
    0,
    fig_back_C_P_y_annotate,
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
    fig_back_C_P_y_annotate,
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
    fig_back_C_P_y_annotate,
    label = "ambiguous\ndisagreement",
    hjust = 0.5,
    vjust = 1.25,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9
  )


fig_empirical_I_C <- case_infos %>%
  filter(study == "breast") %>%
  ggplot(aes(C1, I)) +
  scale_x_continuous("Mean confidence (C)") +
  scale_y_continuous("Inconsistency (I)") +
  coord_cartesian(ylim = c(-0.05, max(case_infos$I) + 0.05))

fig_breast_I_C <- fig_empirical_I_C +
  my_smooth(color = color_breast,
            mapping = aes(x = C2),
            linetype = "dashed") +
  my_smooth(color = color_breast) +
  geom_point(color = color_breast,
             size = point_size,
             alpha = .25)
fig_breast_I_C

fig_back_I_C <- fig_empirical_I_C %+%
  filter(case_infos, study == "back") +
  my_smooth(color = color_back,
            mapping = aes(x = C2),
            linetype = "dashed") +
  my_smooth(color = color_back) +
  geom_point(
    aes(x = C1j, y = Ij),
    color = color_back,
    size = point_size,
    alpha = .15
  )
fig_back_I_C



# combine panels
fig_empirical <- plot_grid(
  fig_breast_I_P,
  fig_back_I_P,
  fig_breast_C_P,
  fig_back_C_P,
  fig_breast_I_C,
  fig_back_I_C,
  ncol = 2,
  labels = c("a", "b", "c", "d", "e", "f"),
  scale = .9
)


# save plot
save_plot(
  here("output/figures/fig_empirical.pdf"),
  fig_empirical,
  base_aspect_ratio = 2 / 3,
  base_height = 10
)

save_plot(
  here("output/figures/fig_empirical.jpg"),
  fig_empirical,
  base_aspect_ratio = 2 / 3,
  base_height = 10
)



### Figure `fig_empirical_pc`: Accuracy 1st vs. 2nd diagnoses ####

fig_empirical_pc <- case_infos %>%
  filter(study == "breast") %>%
  ggplot(aes(pc1, pc2)) +
  scale_x_continuous("P 1st diagnosis") +
  scale_y_continuous("P 2nd diagnosis") +
  coord_fixed(ratio = 1,
              xlim = c(0, 1),
              ylim = c(0, 1)) +
  geom_vline(xintercept = 0.5,
             size = line_size / 2,
             linetype = "dotted") +
  geom_hline(yintercept = 0.5,
             size = line_size / 2,
             linetype = "dotted") +
  geom_abline(intercept = 0,
              slope = 1,
              size = line_size / 2,
              linetype = "dotted")

fig_breast_pc <- fig_empirical_pc +
  ggtitle(title_breast) +
  geom_density_2d(alpha = .25, color = color_breast) +
  geom_point(color = color_breast, alpha = .25)


fig_back_pc <- fig_empirical_pc %+%
  filter(case_infos, study == "back") +
  ggtitle(title_back) +
  geom_density_2d(alpha = .25, color = color_back) +
  geom_point(
    aes(pc1j, pc2j),
    color = color_back,
    alpha = .15
  )


# combine panels
fig_empirical_pc <- plot_grid(
  fig_breast_pc,
  fig_back_pc,
  ncol = 2,
  labels = c("a", "b"),
  scale = .95
)

# save plot
save_plot(
  here("output/figures/fig_empirical_pc.pdf"),
  fig_empirical_pc,
  base_aspect_ratio = 2,
  base_height = 10 / 3
)

save_plot(
  here("output/figures/fig_empirical_pc.jpg"),
  fig_empirical_pc,
  base_aspect_ratio = 2,
  base_height = 10 / 3
)



### Figure `fig_mcs_m7`: MCS vs. 1st and 2nd diagnoses ####
set.seed(12345)
case_infos_mcs <- case_infos %>%
  # filter out ambiguous cases and
  # cases where the experts were always consistent
  # (because those latter cases cannot be plotted
  # since they have, by definition, `NA` for the measures
  # calculated on only the trials were experts were inconsistent
  # across the two diagnoses)
  filter(type != "ambiguous", I != 0) %>%
  select(study, picture, type, i_n, i_MCS, i_pc1, i_pc2) %>%
  mutate(
    type = fct_drop(type),
    i_MCS_impr_pc1 = i_MCS - i_pc1,
    i_MCS_impr_pc2 = i_MCS - i_pc2
  ) %>%
  gather(key = ref, value = impr, -(study:i_pc2)) %>%
  arrange(study, picture, ref, i_n) %>%
  mutate(
    ref = factor(
      ref,
      c("i_MCS_impr_pc1", "i_MCS_impr_pc2"),
      c("vs 1st", "vs 2nd")
    ),
    study = ordered(study,
                    c("breast", "back"),
                    c(title_breast, title_back)),
    typej = as.numeric(type) + runif(
      n = nrow(.),
      min = -.05,
      max = +.05
    )
  ) %>% 
  assert(not_na, everything())

fig_mcs <- case_infos_mcs %>% 
  ggplot(aes(x = type, y = impr, fill = study)) +
  scale_y_continuous("Improvement confidence rule") +
  scale_x_discrete("Case type") +
  scale_size("Observations", range = c(.5, 2)) +
  scale_color_manual(values = c(color_breast, color_back)) +
  scale_fill_manual(values = c(color_breast, color_back)) +
  
  guides(fill = "none", color = "none") +
  geom_hline(yintercept = 0,
             size = line_size,
             linetype = "dotted") +
  geom_point(aes(typej, color = study, size = i_n),
             alpha = 0.375) +
  geom_boxplot(
    aes(weight = i_n),
    width = .2,
    varwidth = TRUE,
    outlier.shape = NA,
    alpha = 0.5,
    position = position_nudge(x = .225, y = 0)
  ) +
  facet_grid(ref ~ study) +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text.y = element_text(face = "plain"))
fig_mcs


# load Model 7
m7_breast_MCS_type <-
  readRDS(here("output/models/m7_breast_MCS_type.rds"))
m7_back_MCS_type <-
  readRDS(here("output/models/m7_back_MCS_type.rds"))

# get posterior predictions
m7_breast_MCS_type_posterior <-
  fnc_get_M7_posterior(m7_breast_MCS_type)
m7_back_MCS_type_posterior <-
  fnc_get_M7_posterior(m7_back_MCS_type)

fnc_get_M7_posterior_stats <- function(posterior, study) {
  posterior %>%
    select(i, contains("minus")) %>%
    gather(cell, prob, -i) %>%
    group_by(cell) %>%
    do(median_qi(.$prob)) %>%
    rename(impr = y) %>%
    separate(cell, into = c(NA, NA, "ref", "type"), sep = "_") %>%
    mutate(study = study)
}

dat_M7 <- list(
  fnc_get_M7_posterior_stats(m7_breast_MCS_type_posterior, "breast"),
  fnc_get_M7_posterior_stats(m7_back_MCS_type_posterior, "back")
) %>%
  bind_rows() %>%
  mutate(
    study = ordered(study, c("breast", "back"), c(title_breast, title_back)),
    type = ordered(type, c("wicked", "kind")),
    ref = factor(ref, c("first", "second"), c("vs 1st", "vs 2nd"))
  )

fig_mcs_m7 <- fig_mcs +
  geom_point(size = 1,
             data = dat_M7,
             position = position_nudge(-0.2, y = 0)) +
  geom_linerange(aes(ymin = ymin, ymax = ymax),
                 data = dat_M7,
                 position = position_nudge(x = -0.2, y = 0))


save_plot(
  here("output/figures/fig_mcs_m7.pdf"),
  fig_mcs_m7,
  base_width = 6,
  base_height = 4.5
)

save_plot(
  here("output/figures/fig_mcs_m7.jpg"),
  fig_mcs_m7,
  base_width = 6,
  base_height = 4.5
)

dat_M7 %>%
  knitr::kable(digits = 2)




### Figure `fig_conf_experts`: Confidence diagnoses 1 vs. diagnoses 2 ####


# prepare data
dat_mcs_rnd <- trials %>%
  select(study, doctor, picture, corr1, corr2, mcs) %>%
  mutate(corr_rnd_expect = (corr1 + corr2) / 2)

dat_mcs_rnd_experts_n_inconsistent <- dat_mcs_rnd %>% 
  group_by(study, doctor) %>%
  summarize(n_inconsistent = sum(corr1 != corr2), n_cases = n()) %>% 
  mutate(prop_inconsistent = n_inconsistent/n_cases)

# create expert data frame
dat_mcs_rnd_experts <- dat_mcs_rnd %>%
  filter(corr1 != corr2) %>%
  group_by(study, doctor) %>%
  summarize(
    pc_mcs = mean(mcs),
    pc_rnd_expect = mean(corr_rnd_expect),
  ) %>%
  mutate(impr_mcs_rnd = pc_mcs - pc_rnd_expect) %>% 
  left_join(dat_mcs_rnd_experts_n_inconsistent)



# how often do experts change their diagnoses?
dat_mcs_rnd_experts %>%
  group_by(study) %>%
  skim(pc_mcs:prop_inconsistent)


# how does confidence change from first to second diagnoses?
dat_conf_experts <- trials %>%
  select(study, doctor, conf1, conf2) %>%
  group_by(study, doctor) %>%
  summarize(
    n = n(),
    conf1_mean = mean(conf1),
    conf1_se = sd(conf1) / sqrt(n - 1),
    conf2_mean = mean(conf2),
    conf2_se = sd(conf2) / sqrt(n - 1)
  ) %>%
  mutate(conf1_minus_conf = conf1_mean - conf2_mean)

dat_conf_experts %>% 
  group_by(study) %>%
  skim(conf1_mean, conf2_mean, conf1_minus_conf) %>% 
  arrange(study)

fig_conf_experts_base <- dat_conf_experts %>%
  filter(study == "breast") %>%
  ggplot(aes(conf1_mean, conf2_mean)) +
  scale_x_continuous("Mean 1st confidence") +
  scale_y_continuous("Mean 2nd confidence") +
  coord_fixed(ratio = 1,
              xlim = c(1, 5),
              ylim = c(1, 5)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dotted",
    size = line_size / 2
  )
fig_conf_experts_breast <- fig_conf_experts_base +
  geom_density_2d(alpha = .25, color = color_breast) +
  geom_point(alpha = .5, color = color_breast) +
  ggtitle(title_breast)
fig_conf_experts_breast

fig_conf_experts_back <- fig_conf_experts_breast %+%
  filter(dat_conf_experts, study == "back") +
  coord_fixed(ratio = 1,
              xlim = c(1, 2),
              ylim = c(1, 2)) +
  geom_density_2d(alpha = .25, color = color_back) +
  geom_linerange(aes(ymin = conf2_mean - 2 * conf2_se,
                     ymax = conf2_mean + 2 * conf2_se),
                 color = color_back, alpha = .5) +
  geom_linerange(aes(xmin = conf1_mean - 2 * conf1_se,
                     xmax = conf1_mean + 2 * conf1_se),
                 color = color_back, alpha = .5) +
  geom_point(alpha = 1, color = color_back) +
  ggtitle(title_back)
fig_conf_experts_back

fig_conf_experts <- plot_grid(
  fig_conf_experts_breast,
  fig_conf_experts_back,
  ncol = 2,
  labels = c("a", "b"),
  align = "hv",
  scale = .95
)
fig_conf_experts


save_plot(
  here("output/figures/fig_conf_experts.pdf"),
  fig_conf_experts,
  base_aspect_ratio = 2,
  base_height = 10 / 3
)

save_plot(
  here("output/figures/fig_conf_experts.jpg"),
  fig_conf_experts,
  base_aspect_ratio = 2,
  base_height = 10 / 3
)




### Figure `fig_mcs_rnd_experts`: MCS improvement over random choice ####


# Bayesian t-test
breast_ttest <- ttestBF(
  x = filter(dat_mcs_rnd_experts, study == "breast")$impr_mcs_rnd,
  rscale = "ultrawide",
  posterior = TRUE,
  iterations = 10 ^ 4
)
#breast_ttest
head(breast_ttest)
breast_ttest_quantiles <-
  quantile(breast_ttest[, "mu"], c(.025, .5, .975))
breast_ttest_quantiles

quantile(breast_ttest[, "delta"], c(.025, .5, .975))


back_ttest <- ttestBF(
  x = filter(dat_mcs_rnd_experts, study == "back")$impr_mcs_rnd,
  rscale = "ultrawide",
  posterior = TRUE,
  iterations = 10 ^ 4
)
#back_ttest
head(back_ttest)
back_ttest_quantiles <-
  quantile(back_ttest[, "mu"], c(.025, .5, .975))
back_ttest_quantiles

quantile(back_ttest[, "delta"], c(.025, .5, .975))


# consolidating the t-test statistics
dat_stats_ttest <- list(breast_ttest_quantiles,
                        back_ttest_quantiles) %>%
  bind_rows %>%
  mutate(study_title = factor(c("Mammograms", "Lumbosacral spine")))



# create figure
fig_mcs_rnd_experts <- dat_mcs_rnd_experts %>%
  left_join(study_infos) %>%
  ggplot(aes(study_title, impr_mcs_rnd, fill = study_title)) +
  xlab("") +
  scale_y_continuous("Improvement confidence rule\n(vs. random choosing)",
                     breaks = pretty_breaks(10)) +
  coord_fixed(ratio = 2) +
  scale_color_manual(values = c(color_breast, color_back)) +
  scale_fill_manual(values = c(color_breast, color_back)) +
  scale_size("Observations",
             range = c(1, 4)) +
  guides(fill = "none", color = "none") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(
    aes(color = study_title, size = n_inconsistent),
    position = position_jitter(
      width = 0.03,
      height = 0,
      seed = 12345
    ),
    alpha = .25
  ) +
  geom_boxplot(
    aes(weight = n_inconsistent),
    width = .1,
    varwidth = TRUE,
    outlier.shape = NA,
    alpha = 0.5,
    position = position_nudge(x = .175, y = 0)
  ) +
  geom_pointrange(
    aes(y = `50%`, ymin = `2.5%`, ymax = `97.5%`),
    position = position_nudge(x = -.175, y = 0),
    data = dat_stats_ttest
  )
fig_mcs_rnd_experts


save_plot(
  here("output/figures/fig_mcs_rnd_experts.pdf"),
  fig_mcs_rnd_experts,
  base_width = 5.25,
  base_height = 3.5
)

save_plot(
  here("output/figures/fig_mcs_rnd_experts.jpg"),
  fig_mcs_rnd_experts,
  base_width = 5.25,
  base_height = 3.5
)
