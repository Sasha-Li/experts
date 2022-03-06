# How experts' own inconsistency relates to their confidence and between-expert disagreement

This repository contains the code and part of the data to reproduce all analyses reported in the manuscript "How experts' own inconsistency relates to their confidence and between-expert disagreement".

## `/`

Next to this README file, there are two additional files in the top-level folder of this repository.

- `experts.Rproj`: RStudio project file
- `.here`: indicates to the top-level folder of this repository (used by the R function `here::here()`)


## `/code`

This folder contains the R scripts necessary to run the analysis pipeline. See `code/00_make.R` for an overview over the different scripts.


## `/data`

IMPORTANT: We are not allowed to make the breast cancer mammography dataset available as part of this repository (see manuscript for more information on the dataset). Because of this, our analysis pipeline checks for the existence of the mammography dataset. If it does not exist (as in this public version of the repository), the script will duplicate the back pain dataset and use it as a surrogate for the mammography dataset so that the rest of the code still runs.


## `/output`

This folder contains outputs of the analysis pipeline.
