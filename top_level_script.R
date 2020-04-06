##############################################
## Scripts to run dengue model for MdD Peru ##
##############################################

#### The code in this script with the data provided in the github repo will recreate the
#### predictions for "present" shown here: xxxx
#### Some code modifications will be needed for other rasters/problems, but the code is hopefully
#### clear enought to make those changes minimally painful

## Setup scripts and parameters
source("packages.R")
source("../ggplot_theme.R")
linkfun <- make.link("logit")
  # Duplicate data with some noise (a trial that mostly didn't work). Best to keep this false
sim_data <- FALSE
  # Fine or coarse predictions? (pulls in different exported csv from QGIS)
pred_scale    <- "coarse" # coarse
  # prediction for present or future? (only data for "present" available on github)
pred_time     <- "fut"   # pres, fut
  # prediction area
pred_area     <- "PM"     # PM, TAH
  # prediction scenario
pred_scenario <- "rea"    # now, peor, sost, rea

## Load data, exported as .csv from raster samples made in QGIS
source("load_data.R")

## Clean the data for mosquito sampling and set up predictors for the stan model
source("setup_mosquito_abund.R")

## Clean the data for dengue cases and set up predictors for the stan model
source("setup_dengue_counts.R")

## Clean the data for the out-of-sample predictions (here around Puerto Maldonado) and set up predictors for the stan model
source("setup_regular_points.R")

## Fit mosquito abundance stan model
source("fit_mosquito_abund.R")

## Export estimates from the mosquito abundance stan model
source("export_mosquito_abund.R")

## Export estimates from the mosquito abundance stan model
source("fit_dengue_counts.R")

## Eport estimates from the dengue stan model
source("export_dengue_counts.R")
