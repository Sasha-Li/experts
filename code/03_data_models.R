### Setup ####
source(here::here("code/_dependencies.R"))

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# total number of MCMC iterations (incl. burn-in)
iter <- 6000

# number of warmup MCMC iterations (aka burn-in)
warmup <- 2000

# thinning
thin <- 4



### Load data ####
trials <- read_rds(here("output/data/trials.rds"))





### Functions ####
fnc_save_posterior_samples <- function(m) {
  # find the names of the population-level variables for model `m`
  m_vars_pop <- str_subset(get_variables(m), START %R% or("b_", "sd_", "sigma"))
  
  # save model variable as string
  m_str <- deparse(substitute(m))
  
  # extract posterior samples and save them to disk
  posterior_samples(m, m_vars_pop) %>% 
    write_csv(here(glue("output/models/{m_str}_MCMC_samples.csv")))
}



#### Consistency ####

# base model
# to compare the relative size of the expert vs. case group-level effects
message("m1_breast_inconsistency")
m1_breast_inconsistency <- brm(
  inconsistent ~ 1 + (1 | doctor) + (1 | picture),
  data = filter(trials, study == "breast"),
  family = bernoulli(),
  file = here("output/models/m1_breast_inconsistency"),
  save_model = here("output/models/m1_breast_inconsistency.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m1_breast_inconsistency)

message("m1_back_inconsistency")
m1_back_inconsistency <- update(
  m1_breast_inconsistency,
  newdata = filter(trials, study == "back"),
  file = here("output/models/m1_back_inconsistency"),
  save_model = here("output/models/m1_back_inconsistency.stan"),
)
fnc_save_posterior_samples(m1_back_inconsistency)


# the target model
message("m2_breast_inconsistency_by_P_poly2")
m2_breast_inconsistency_by_P_poly2 <- brm(
  inconsistent ~ 1 + poly(pc1 - 0.5, 2) + (1 | doctor) + (1 | picture),
  data = filter(trials, study == "breast"),
  family = bernoulli(),
  file = here("output/models/m2_breast_inconsistency_by_P_poly2"),
  save_model = here("output/models/m2_breast_inconsistency_by_P_poly2.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m2_breast_inconsistency_by_P_poly2)

message("m2_back_inconsistency_by_P_poly2")
m2_back_inconsistency_by_P_poly2 <-
  update(
    m2_breast_inconsistency_by_P_poly2,
    newdata = filter(trials, study == "back"),
    file = here("output/models/m2_back_inconsistency_by_P_poly2"),
    save_model = here("output/models/m2_back_inconsistency_by_P_poly2.stan")
  )
fnc_save_posterior_samples(m2_back_inconsistency_by_P_poly2)



### Confidence ####

# base model
# to compare the relative size of the expert vs. case group-level effects
message("m3_breast_confidence")
m3_breast_confidence <- brm(
  conf1 ~ 1 + (1 | doctor) + (1 | picture),
  data = filter(trials, study == "breast"),
  file = here("output/models/m3_breast_confidence"),
  save_model = here("output/models/m3_breast_confidence.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m3_breast_confidence)

message("m3_back_confidence")
m3_back_confidence <-
  update(
    m3_breast_confidence,
    newdata = filter(trials, study == "back"),
    file = here("output/models/m3_back_confidence"),
    save_model = here("output/models/m3_back_confidence.stan")
  )
fnc_save_posterior_samples(m3_back_confidence)
# alternative: model this as Bernoulli process
# (because confidence has only two levels in the back data set)?


# the target model
message("m4_breast_confidence_by_P_poly2")
m4_breast_confidence_by_P_poly2 <- brm(
  conf1 ~ 1 + poly(pc1 - 0.5, 2) + (1 | doctor) + (1 | picture),
  data = filter(trials, study == "breast"),
  file = here("output/models/m4_breast_confidence_by_P_poly2"),
  save_model = here("output/models/m4_breast_confidence_by_P_poly2.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m4_breast_confidence_by_P_poly2)


message("m4_back_confidence_by_P_poly2")
m4_back_confidence_by_P_poly2 <-
  update(
    m4_breast_confidence_by_P_poly2,
    newdata = filter(trials, study == "back"),
    file = here("output/models/m4_back_confidence_by_P_poly2"),
    save_model = here("output/models/m4_back_confidence_by_P_poly2.stan")
  )
fnc_save_posterior_samples(m4_back_confidence_by_P_poly2)
# alternative: model this as Bernoulli process
# (because confidence has only two levels in the back data set)?



### Confidence--Consistency ####

message("m5_breast_inconsistency_by_confidence")
m5_breast_inconsistency_by_confidence <- brm(
  inconsistent ~ 1 + I(conf1 - 1) + (1 | doctor) + (1 | picture),
  data = filter(trials, study == "breast"),
  family = bernoulli(),
  file = here("output/models/m5_breast_inconsistency_by_confidence"),
  save_model = here("output/models/m5_breast_inconsistency_by_confidence.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m5_breast_inconsistency_by_confidence)

message("m5_back_inconsistency_by_confidence")
m5_back_inconsistency_by_confidence <-
  update(
    m5_breast_inconsistency_by_confidence,
    newdata = filter(trials, study == "back"),
    file = here("output/models/m5_back_inconsistency_by_confidence"),
    save_model = here("output/models/m5_back_inconsistency_by_confidence.stan")
  )
fnc_save_posterior_samples(m5_back_inconsistency_by_confidence)



### MCS ####

# only considering inconsistent trials!

trials_mcs <- trials %>%
  filter(dec1 != dec2) %>%
  select(study,
         doctor,
         picture,
         type,
         first = corr1,
         second = corr2,
         mcs) %>%
  gather(key = decision,
         value = correct,
         -study,
         -doctor,
         -picture,
         -type) %>%
  arrange(study, doctor, picture) %>%
  # recode `decision` and `type`
  mutate(
    decision = factor(decision, c("mcs", "first", "second")),
    type = factor(type, ordered = FALSE) %>% fct_relevel("kind")
  ) %>% 
  assert(not_na, everything())


message("m6_breast_MCS")
m6_breast_MCS <- brm(
  correct ~ 1 + decision + (1 | doctor) + (1 | picture),
  data = filter(trials_mcs, study == "breast"),
  family = bernoulli(),
  file = here("output/models/m6_breast_MCS"),
  save_model = here("output/models/m6_breast_MCS.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m6_breast_MCS)


message("m6_back_MCS")
m6_back_MCS <-
  update(
    m6_breast_MCS,
    newdata = filter(trials_mcs, study == "back"),
    file = here("output/models/m6_back_MCS"),
    save_model = here("output/models/m6_back_MCS.stan")
  )
fnc_save_posterior_samples(m6_back_MCS)


# interaction with type of environment
#
# only use clear cut cases
# (i.e., PC1 > 0.6 or PC1 < 0.4)
trials_mcs_kind_wicked <- trials_mcs %>% 
  filter(type != "ambiguous") %>% 
  mutate(type = fct_drop(type))


message("m7_breast_MCS_type")
m7_breast_MCS_type <- brm(
  correct ~ 1 + decision * type + (1 | doctor) + (1 | picture),
  data = filter(trials_mcs_kind_wicked, study == "breast"),
  family = bernoulli(),
  file = here("output/models/m7_breast_MCS_type"),
  save_model = here("output/models/m7_breast_MCS_type.stan"),
  iter = iter,
  warmup = warmup,
  thin = thin,
  seed = 12345
)
fnc_save_posterior_samples(m7_breast_MCS_type)

message("m7_back_MCS_type")
m7_back_MCS_type <-
  update(
    m7_breast_MCS_type,
    newdata = filter(trials_mcs_kind_wicked, study == "back"),
    file = here("output/models/m7_back_MCS_type"),
    save_model = here("output/models/m7_back_MCS_type.stan"),
    # to avoid a sampling problem
    control = list(max_treedepth = 30, adapt_delta = .95)
  )
fnc_save_posterior_samples(m7_back_MCS_type)
