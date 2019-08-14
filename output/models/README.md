This folder contains the regression models:

- the Stan model as a text file (`.stan`)
- the R data structure of the `brmsfit` (`.rds`)
- a summary of the MCMC diagnostic checks (`_MCMC_checks.md`)

The Stan model files are tracked by git to ensure that changes in the models or priors possibly introduced in later versions of `brms` can be detected. The `.rds` files are not tracked.
