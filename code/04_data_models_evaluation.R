### Setup ####
source(here::here("code/_dependencies.R"))


### Find all models ####
models <- tibble(
  filename = list.files(path = here("output/models"),
                        pattern = ".rds"),
  model = str_extract(filename,
                      START %R% one_or_more(or(ALNUM, "_")))
)


### Do automated diagnostic checks ####

# Needs to be supplemented by manual checks!

fnc_model_infos <- function(filename) {
  path <- str_c(here("output/models/"), filename)
  m <- read_rds(path)
  
  pop_params <- str_subset(get_variables(m), 
                           START %R% or("b_", "sd_", "sigma"))
  
  tibble(
    param = names(rhat(m)),
    rhat = rhat(m),
    neff_ratio = neff_ratio(m)
  ) %>%
    mutate(param_type = ifelse(
      param %in% pop_params,
      "population-level parameters",
      "group-level parameters"))
}
# note: this checks only the population-level parameters
# and the hyperparameters of the group-level distributions

# retrieve infos from each model
models <- models %>%
  mutate(checks = map(filename, fnc_model_infos)) %>% 
  unnest


### Check models ####
checks <- models %>% 
  group_by(model, param_type) %>% 
  summarize(
    rhat_median = median(rhat),
    rhat_worst = max(rhat),
    rhat_not_ok_prop = mean(rhat >= 1.01),
    neff_ratio_median = median(neff_ratio),
    neff_ratio_worst = min(neff_ratio),
    neff_not_ok_prop = mean(neff_ratio < .1)
  ) %>% ungroup


### Assert that the models are OK ####
checks %>%
  assert(in_set(0),
         contains("not_ok"),
         success_fun = success_logical,
         error_fun = warn_report)

### Export summary ####
checks %>% 
  kable(digits = 2) %>% 
  write_lines(here("output/models/_MCMC_checks.md"))