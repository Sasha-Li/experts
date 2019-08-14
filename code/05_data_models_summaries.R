### Setup ####
source(here::here("code/_dependencies.R"))

### Function to calculate posterior summary of a model ####
fnc_posterior_summary <- function(m) {
  m <- read_rds(here(glue("output/models/{m}.rds")))
  
  mdl_post_summ <-
    posterior_summary(m,
                      robust = TRUE,
                      pars = parnames(m))
  
  pars <- rownames(mdl_post_summ)
  
  mdl_post_summ <- mdl_post_summ %>%
    as_tibble() %>%
    mutate(var = pars) %>%
    rename(p = Estimate,
           l = 3,
           u = 4) %>%
    select(var, p, l, u) %>%
    data.frame(stringsAsFactors = FALSE)
  
  rownames(mdl_post_summ) <- pars
  mdl_post_summ <- mdl_post_summ[, -1]
  return(mdl_post_summ)
}


### Parse the `.tex.rsp` file #####

rfile(
  file = here("code/tbl_models.tex.rsp"),
  output = here("output/tables/tbl_models.tex"),
  postprocess = FALSE,
  verbose = FALSE
)
