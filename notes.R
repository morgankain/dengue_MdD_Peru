#####
## January 23: Plan to set this project down for a while. Here are the notes required to pick this back up when the time comes:
#####

## Current status is as follows:

## (A) Have a series of models in: 
  ## -- "~/Documents/Research/Stanford/Work/Mosquito Abundance Model/stan_models"
  ## -- "~/Documents/Research/Stanford/Work/Dengue/stan_models"
 ##     that fit more or less complicated count models to the dengue cases. None of them fit particularly well because the data is so bad.
  ## I tried to simulate some data (quite naively) but it turns out that didn't work very well because I didn't perturb the predictos
  ## in any sensible way.
 ## I really do want to fit a model that allows for observed count to be some function of the true count given an observational process
  ## that under or over! predicts the count because of how the coutns were taken (people going to the hospital and stating where 
  ## they are from, which may not be where they got infected)
  
## (B.1) The current pipeline is quite click-heavy, but relies on Chris's mosquito sampling, Chris's mosquito predictions, and
  ## all of his layers for habitat and temperature. 
  ## I am still a bit unclear on what predictos should go into differnet pieces of each model
  ## for a full

#####################################################################################################################

## (B.2) The exact workflow is as follows:
  ## i) Open QGIS and export points of sampled rasters for where you want to predict
  ## ii) Load this in given the code below. The models will predict for the mosquito sampling locations and the dengue locations
   ## and predict for these brought in layers.
    ## -- right now Chris's layers still have a number of issues that make predicting into the future quite rediculous
  ## BUT: iii) What I think is a reasonable thing to predict for is the theta that predicts if a poisson or negative binomial is 
   ## sampled from over every pixel, and the lambda prediction for the poisson or negative binomial for population centers which
   ## predicts dengue cases in these locations. Imagine predicting for the present and then for each future scenario
  ## iv) export the results around lines 600 back into QGIS for maps
   
## (C.1) currently the first pass exported layers reside in:
      ## /Dengue/map_export
      ## /Mapping
    ## -- /Mapping also contains a bunch of new layers for future predictions that more or less don't work
     ## !! It is these layers that could be switched once Chris gets back to me to make better future predictions

## Note: Pipeline is a bit click-heavy but smooth for regular points for the future, not quite as
 ## smooth for predicting future for the pop centers but working on it.
  ## For now bring this in as pop_buffer, save it as a separate object and use that object as regular points
    
## (C.2) A visual model summary is in "dengue_model_first_pass"
  ## -- this is incomplete, but can be organized and updated for a talk or for Peru

## (C.3) A currently saved workspace is in dengue/dengue_scr_workspaces

#####################################################################################################################

## (D) Some issues
  ## -- Chris's future mosquito layer has a hole in the center of Puerto Maldonado
  ## -- Chris's present and future population has some NA values. I have a bit of code below to switch these NAs for nearby values 
   ## as a placeholder but that obviously isn't great
  ## -- Chris's PEOR scenario LST and LC have weird scambled pixels
 ## -- I am just using raster point samples, but would probably prefer buffer summaries

#####
## Still so much progress to be made here:
#####

## 1a) Are these even the models we would want to be fitting?
  ## A) e.g. what is the exact interpretation for such a dengue count model?
  ## B) If so, what predictors do we want in the 0 part and in the count part?
  ## C) Really do want better estimates for population so I can use population as offset and population density as a predictor
  ## D) The JAGS model at the bottom is the one that I actually want to fit, but it seems like it may require a lot of data
    ## I think the possibility of allowing higher or lower counts than what is observed is pretty interesting given the method
    ## of collecting the data. Wonder if we could get there with simulated data... (e.g. seems like a lot is needed to separate
    ## the observational from the risk process)

## 1b) Definitely also need to double check all of the scaling that is going on here of the predictors...

## 2) How best to proceed with simulated data given:
  ## A) That we only have one data point for each place. How do we go from that to reasonable time series?
  ## B) How do we perturb the predictors?

## 3) How do we estimate counts for a landscape in a statistical way? Seems hard...
  ## A) That is, how do we sample rasters to make reasonable/sensible predictions?

#####
## Quick pipeline steps here:
#####

## A) Fit a model to mosquito counts that uses Chris's sampling and Chris's predicted suitability as 
 ## an informative prior. In this model predict counts for the locations where we have dengue counts AND
  ## locations around Puerto Maldonado where the scenarios are built, that is:

## Model 1: Mosquito count model:
 ## Adjusted to predict counts for the locations of the dengue cases (predictions 1.1) AND 
  ## for the Puerto Maldonado scenario region (PM) (predictions 1.2)

## A.extra) If desired: simulate new data for dengue counts to get the models to fit better (see simulate_data.R)
# sim_data <- FALSE

## B) Fit a model for dengue cases that uses the predicted mosquito counts (1.1 above) along with other predictors
 ## including forest cover, population, temperature etc. 

## Model 2: Dengue count model fit using 1.1 above:
 ## 2.1 Adjusted to predict dengue cases around PM for the future scenarios (regular points need to be taken from these maps)

## C) Produce maps of these scenarios as well as a summary of the rectangle... (not part of this script)