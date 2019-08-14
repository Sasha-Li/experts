### Setup ####
source(here::here("code/_dependencies.R"))


### Load and standardize raw data ####
back <- read_csv(here("data/back.csv"),
                 col_types = "iiiiiiiiic")

back <- back %>%
  select(
    study = domain,
    doctor,
    picture,
    ground_truth = abnormality,
    dec1 = assess1,
    conf1 = confidence1,
    dec2 = assess2,
    conf2 = confidence2
  ) %>%
  assert(not_na, everything()) %>% 
  verify(unique(.$study) == "back") %>%
  assert(in_set(1:13), doctor) %>%
  verify(length(unique(.$doctor)) == 13) %>%
  assert(in_set(1:300), picture) %>%
  verify(length(unique(.$picture)) == 300) %>%
  assert(in_set(1:2), conf1, conf2)

# check for mammography dataset
# if it does not exist, use the back pain dataset 
# so that the rest of the code runs
if (file.exists(here("data/mammography.xlsx"))) {
  breast <- read_excel(
    here("data/mammography.xlsx"),
    col_types = c(
      "skip",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "skip",
      "skip",
      "numeric",
      "numeric",
      "numeric",
      "skip",
      "skip",
      "text"
    )
  )
  
  breast <- breast %>%
    select(
      study = domain,
      doctor,
      picture,
      ground_truth = cancer,
      dec1 = assess1,
      conf1,
      dec2 = assess2,
      conf2
    ) %>%
    assert(not_na, everything()) %>% 
    verify(unique(.$study) == "breast") %>%
    verify(length(unique(.$doctor)) == 102) %>%
    verify(length(unique(.$picture)) == 58) %>%
    assert(in_set(1:5), conf1, conf2)
  
  breast_title <- "Mammograms"
  
} else {
  breast <- back %>%
    mutate(study = "breast")
  
  breast_title <- "Not mammograms!"
}


### Prepare study information ####
study_infos <-
  tibble(
    study = factor(
      x = c("breast", "back"),
      levels = c("breast", "back")
    ),
    study_color = RColorBrewer::brewer.pal(n = 3, name = "Dark2")[1:2],
    study_title = factor(
      x = c(breast_title, "Lumbosacral spine"),
      levels = c(breast_title, "Lumbosacral spine")
    )
  )



### Row bind the two studies and add variables ####
set.seed(12345)
trials <- bind_rows(breast, back) %>%
  mutate(
    study = factor(study, c("breast", "back")),
    corr1 = ifelse(dec1 == ground_truth, 1, 0),
    corr2 = ifelse(dec2 == ground_truth, 1, 0),
    consistent = ifelse(dec1 == dec2, 1, 0),
    inconsistent = ifelse(dec1 != dec2, 1, 0),
    consistency = ifelse(dec1 == dec2, "consistent", "inconsistent"),
    conf_same = ifelse(conf1 == conf2, "conf same", "conf different")
  ) %>%
  rowwise() %>%
  mutate(corr_rnd = sample(c(corr1, corr2), size = 1)) %>%
  ungroup() %>%
  mutate(mcs = case_when(conf1 > conf2 ~ corr1,
                         conf1 < conf2 ~ corr2,
                         TRUE ~ corr_rnd))


### Calculate information for each case ####
case_infos <- trials %>%
  group_by(study, picture) %>%
  summarize(
    pc1 = mean(corr1),
    pc2 = mean(corr2),
    I = 1 - mean(consistent),
    C1 = mean(conf1),
    C2 = mean(conf2),
    n = n()
  ) %>%
  verify(nrow(.) ==
           length(unique(back$picture))
         + length(unique(breast$picture))) %>%
  ungroup %>%
  mutate(
    pc12 = (pc1 + pc2) / 2,
    type = case_when(pc1 > .6 ~ "kind",
                     pc1 < .4 ~ "wicked",
                     TRUE ~ "ambiguous"),
    type = ordered(type,
                   c("wicked", "ambiguous", "kind"))
  ) %>%
  left_join(study_infos) %>%
  verify(nrow(.) ==
           length(unique(back$picture))
         + length(unique(breast$picture)))

### Add reproducible jittering offset ####

# For the back dataset we plot jittered points to avoid
# overplotting. Unfortunately, the randomness used to 
# jitter points in `ggplot` cannot be made reproducible 
# by setting a seed (using `set.seed()`). Therefore, we 
# explicitly create variants of four variables 
# (`pc1`, `pc2`, `I`, and `C1`) where we add a tiny amount 
# of uniformly distributed noise (using `runif()`)
# to the original values. Importantly, those jittered 
# values are only used for plotting points. For everything
# else (boxplots, LOESS smooth lines, ...) use the original
# values.
set.seed(12345)
case_infos <- case_infos %>%
  mutate(
    pc1j = pc1 + runif(
      n = nrow(.),
      min = -.01,
      max = +.01
    ),
    pc2j = pc2 + runif(
      n = nrow(.),
      min = -.01,
      max = +.01
    ),
    Ij = I + runif(
      n = nrow(.),
      min = -.01,
      max = +.01
    ),
    C1j = C1 + runif(
      n = nrow(.),
      min = -.01,
      max = +.01
    )
  )


### Calculate information for each case for inconsistent trials ####
case_infos_inconsistent <- trials %>%
  filter(dec1 != dec2) %>%
  group_by(study, picture) %>%
  summarize(
    i_pc1 = mean(corr1),
    i_pc2 = mean(corr2),
    i_MCS = mean(mcs),
    i_C1 = mean(conf1),
    i_C2 = mean(conf2),
    i_n = n()
  )
            
### Combine case information
case_infos <- case_infos %>%
  left_join(case_infos_inconsistent, by = c("study", "picture")) %>% 
  verify(nrow(.) == nrow(case_infos))

# Cases for which the experts were always consistent
# cannot, by design, have any values for the variables starting
# with `i_` because they are only calculated on inconsistent trials
# for a case (see the code above that defines the data frame 
# `case_infos_inconsistent`). Here we check in the full data frame
# (`case_infos`) that those cases indeed have `NA`s in the `i_`
# variables. This is simply sanity checking the merging of the two
# data frames.
filter(case_infos, I == 0) %>% 
  select(starts_with("i_")) %>% 
  assert(is.na, everything(), success_fun = success_logical)


### Add case and study information to trial data ####
trials <- trials %>%
  left_join(case_infos) %>%
  left_join(study_infos) %>% 
  verify(nrow(.) == nrow(trials))


### Save data to disk ####
write_rds(trials, here("output/data/trials.rds"))
write_rds(case_infos, here("output/data/case_infos.rds"))
write_rds(study_infos, here("output/data/study_infos.rds"))
