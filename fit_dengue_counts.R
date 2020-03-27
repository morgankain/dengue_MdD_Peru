#####
## Fit Degnue counts using the mosauito abundance predictions and other predictions for Dengue cases
## Use buffer region summary values instead of point samples from the raster layer
#####

which.predictors.d <- c("pop.d", "aedes_mean", "land_cov1_mean", "tempmean_mean", "tempvar_mean")

stan.predictors.d  <- dengue_cases[,
  c(
    grep(which.predictors.d[1], colnames(dengue_cases))
# , grep(which.predictors.d[2], colnames(dengue_cases))
  , grep(which.predictors.d[3], colnames(dengue_cases))
  , grep(which.predictors.d[4], colnames(dengue_cases))
  , grep(which.predictors.d[5], colnames(dengue_cases))
  )
  ]

stan.response.d    <- dengue_cases$cases

#####
## Model without observational process, but zero-inflated
## Some issues here in that I didn't subset by raw measure of pop > denuge counts, so offset is a bit funny in that it 
## is fundamentally a density... A place to make progress in future iterations if I get more data
## Goal here with the zero inflated model is to model true 0 as a function of suitability, and then counts (with 0 possible) as 
## a function of pop, mosq AND the observational process? A bit funny, yes, but what I felt like I could do with 1 year of data
#####

stan.predictors.o  <- dengue_cases[ , 
  c(
    grep("dist_to_road", colnames(dengue_cases))
  , grep("dist_to_health", colnames(dengue_cases))
  )
]

stan.predictors.o   <- sweep(stan.predictors.o, 1, rep(1000, 2), FUN = "/")
stan.predictors.o.m <- apply(stan.predictors.o, 2, FUN = function (z) mean(z))
stan.predictors.o.v <- apply(stan.predictors.o, 2, FUN = function (z) sd(z))

stan.predictors.o   <- apply(stan.predictors.o, 2, FUN = function (z) (z - mean(z)) / sd(z))

#####
## Second step of bringing in data for regularly spaced points aroung Puerto Maldonado. 
#####

reg_points.x      <- as.matrix(reg_points %>% dplyr::select(
  land1
, land3
, pop
, tempmean
, tempvar
  ))

reg_points.x[, 3] <- log(reg_points.x[, 3])

## A little precarious. Relies on perfect matching of order. Should probably have a check to make sure of a match before running 
reg_points.x      <- t(apply(reg_points.x, 1, FUN = function (z) (z - dengue_cases.s.m) / dengue_cases.s.v))

reg_points.o      <- as.matrix(reg_points %>% dplyr::select(dist_to_r, dist_to_h))
reg_points.o      <- t(apply(reg_points.o, 1, FUN = function (z) (z - stan.predictors.o.m) / stan.predictors.o.m))

######
## Fit model for Dengue cases and predict number of cases for regularly sampled points to estimate for in the region
## around Puerto Maldonado. Do this for the maps for each future scenario
######

if (!sim_data) {
  
## Idea here would be to have year as a random effect instead of trying to fit time series in any way. Treating year as a random effect
 ## will allow estimates to be interpreted as dengue risk on average 
  
## Extra note to myself that the models with "random" attached are models in which I hoped to
  ## add a random effect for year from simulated data (or real data if I ever get it)

stan.data.d <- list(
  N             = nrow(stan.predictors.d)
, K_den         = ncol(stan.predictors.d[, -1])
, K_obs         = ncol(stan.predictors.o)
, K_mos_theta   = 1
, K_mos_lambda  = 1
, x             = stan.predictors.d[, -1]
, x_obs         = stan.predictors.o
, z_theta       = as.matrix(stan.pred_mos.d[, 1])
, z_lambda      = as.matrix(stan.pred_mos.d[, 2])
, y             = stan.response.d
, offset        = stan.predictors.d[, 1]
## Add in the data for predictions here for future dates under different scenarios. 
 ## Adding all at the same time will get cumbersome and slow, so just carefully rerun the script with different
   ## regular points for the various scenarios
, N_pred        = nrow(reg_points.x)
## Not so clean to do this manually, but need the same predictors from this matrix. Has different names...
, x_pred        = reg_points.x[, c(1, 4, 5)]
, x_obs_pred    = reg_points.o
## !! A little weird to scale these maybe....
, z_theta_pred  = as.matrix(stan.pred_mos.PM[, 1])
, z_lambda_pred = as.matrix(stan.pred_mos.PM[, 2])
, offset_pred   = reg_points.x[, 1]
  )

} else {
  
###### !!
## Not updated because sim_data should be FALSE. Something to come back to potentially
###### !!
  
stan.data.d <- list(
  N             = nrow(stan.predictors.d)
, K_den         = ncol(stan.predictors.d[, -1])
, K_obs         = ncol(stan.predictors.o)
, K_mos_theta   = 1
, K_mos_lambda  = 1
, x             = stan.predictors.d[, -1]
, x_obs         = stan.predictors.o
## Repeat the predicted values 20x, once for each year for each location
, z_theta       = as.matrix(rep(stan.pred_mos.d[, 1], n_years))
, z_lambda      = as.matrix(rep(stan.pred_mos.d[, 2], n_years))
, y             = stan.response.d
, offset        = stan.predictors.d[, 1]
, loc_id        = as.numeric(as.factor(dengue_cases$OBJECTID))
, N_loc         = length(unique(as.numeric(as.factor(dengue_cases$OBJECTID))))
# , year        = dengue_cases$year
## Add in the data for predictions here for future dates under different scenarios. 
, N_pred        = nrow(reg_points.x)
   ## using land1, tempmean, and tempvar for now
, x_pred        = reg_points.x[, c(1, 3, 4, 5)]
, x_obs_pred    = reg_points.o
   ## !! A little weird to scale these maybe....
, z_theta_pred  = as.matrix(stan.pred_mos.PM[, 1])
, z_lambda_pred = as.matrix(stan.pred_mos.PM[, 2])
, offset_pred   = reg_points.x[, 1]
  )  

}

## MCMC settings
ni <- 2000
nt <- 1
nb <- 1000
nc <- 3

###### !!
## Moving forward will want to fit a negative binomial model, but with the current data that model
## is too hard to fit so step back and fit a zero-inflated poisson. This most likely will be under-estimating
## downtown MdD, but for now I cant really hope to do better 
###### !!

if (!file.exists("../stan_output/stan.fit.d.Rds")) {

stan.fit.d <- stan(
  file    = "../stan_models/dengue_counts_zero_inf_pois_opt2.stan"
, data    = stan.data.d
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.85
  , max_treedepth = 10
  )
  ## for option 1 (see export_mosquito_abund and export_dengue_counts for details) could also 
   ## save the predictions of interest to save space, model gets too big otherwise.
    ## This actually turns out to be extremely slow, so instead try predicting after (opt 2)
# , include = TRUE
# , pars    = c("theta_pred", "lambda_log_pred")
  )

saveRDS(stan.fit.d, "../stan_output/stan.fit.d.Rds")

} else {
  
stan.fit.d <- readRDS("../stan_output/stan.fit.d.Rds")
  
}

## Pleasant way to debug quickly
# launch_shinystan(stan.fit.d)
