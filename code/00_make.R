### Overview ####
#
# Use this file to run the whole analysis pipeline.
#
# WARNING: The regression models 
# in `03_data_models.R` take some time to run
# 
# Alternatively, the numbered scripts can be run individually,
# but note the dependencies.

### Save session information to disk ####
#
source(here::here("code/_dependencies.R"))
write_lines(capture.output(sessionInfo()),
            here("output/_sessionInfo.txt"))
# `_dependencies.R` loads all R packages needed for the whole
# analysis pipeline


### 01_model_results.R ####
#
# `01_model_results.R` visualizes the predictions from the
# self-consistency model (SCM) and saves the figure (Figure 1) 
# to disk (into `output/figures`).
source(here("code/01_model_results.R"))


### 02_make_data.R ####
#
# `02_make_data.R` prepares the empirical data from the two studies
# (located in `/data`) and saves the derived data frames to disk
# (into `output/data`).
#
# IMPORTANT: We are not allowed to make the breast cancer mammography
# dataset available as part of this repository (see manuscript
# for more information on the dataset).
# Because of this, this script checks for the existence of the
# mammography dataset. If it does not exist (as in this public 
# version of the repository), the script will duplicate the back pain 
# dataset and use it as a surrogate for the mammography dataset so 
# that the rest of the code still runs.
source(here("code/02_make_data.R"))


### 03_data_models.R ####
#
# `03_data_models.R` runs the 14 regression models
# (see supplementary materials for details) and
# saves them to disk (into `output/models`):
# 
# - the Stan model as a text file (`.stan`)
# - the R data structure of the `brmsfit` (`.rds`)
# 
# IMPORTANT:
# - This script depends on `02_make_data.R` being run before.
#
# - Running the 14 regression models can, depending on the hardware,
#   take some time, but presumably less than 1/2 h
#   given a moderately powerful setup with multiple cores
#
# - Note that `brms::brm` will look for the model .rds-file 
#   (see the `file` argument) and if it finds a file, 
#   it will load this already computed model
#   (i.e., it will not run again!);
#   if the model should be re-run,
#   one needs to delete the respective model file(s)
source(here("code/03_data_models.R"))


### 04_data_models_evaluation.R ####
#
# `04_data_models_evaluation.R` loads all models and 
# automatically checks their R-hat and ratio of 
# effective MCMC sample size relative to the actual
# number of samples. If those MCMC diagnostics are not
# appropriate, the script will stop with an error.
# 
# Since the models are run with a fixed random seed,
# their MCMC samples should be perfectly reproducible.
# Since the checks were OK when the models were run for 
# the analyses reported in the manuscript, they should 
# also be OK if the models are run later by somebody else.
# 
# IMPORTANT:
# This script depends on `03_data_models.R` being run before.
source(here("code/04_data_models_evaluation.R"))


### 05_data_models_summaries.R ####
#
# `05_data_models_summaries.R` uses the RSP markup language
# (see the `R.rsp` R package) to dynamically populate the 
# LaTeX skeleton (`code/tbl_models.tex.rsp`) for the 
# supplementary Table 1 with the summaries of the posterior 
# distributions of the regression models and saves the 
# rendered LaTeX file to disk (`output/tables/tbl_models.tex`).
#
# IMPORTANT:
# This script depends on `03_data_models.R` being run before.
source(here("code/05_data_models_summaries.R"))


### 06_data_figures.R ####
#
# `06_data_figures.R` creates the figures for the two empirical
# studies and saves them to disk (into `output/figures`).
#
# IMPORTANT:
# This script depends on `02_make_data.R` and `03_data_models.R`
# being run before.
source(here("code/06_data_figures.R"))
