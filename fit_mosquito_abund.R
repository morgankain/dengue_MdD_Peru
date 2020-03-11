#####
## Fit the mosquito abundance model and predict counts for out-of-sample locations (being the locations for the Dengue counts)
#####

######### !! Some notes: 

  ## 1) Depending on the desired model to fit, the data needed could vary (for example if generated quantities block is used or not)
    ## There are two options here for how to do out-of-sample predictions for this stan model (see _opt1 vs _opt2 extensions in the 
    ## named stan models):
      ## i) The more principled way that allows for propegation of error, which is to:
        ## Bring the predictors into the stan model and predict in the generated quantities block which
         ## will return an entire posterior for each prediction
      ## i) Predict without uncertainty, which can be done outside of stan. 
         ## This is fast and is much more efficient in that it can be run with far more locations to predict for
         ## (option 1 runs can run into memory issues because predicting for a large region produces a very large stan model)
         ## When (if) I receive better data it will be best to continue with option 1, but it isn't worth it for now.
           ## That being said, I don't like option 2 for a few reasons: 
             ## A) Should really be the _number predicted_ given the distribution, but that blows up so just use the estimated parameter for now
             ## B) Doesn't store uncertainty in the predictions for use in the next model. In reality these models
              ## should be combined into one large stan model, but I was having a hard time fitting that model so I stepped back
              ## and am going to fit this until I get better data
  
  ## 2) Naming conventions:
    ## Model brings in two sets of predictors for out-of-sample prediction:
      ## One set for fitting dengue count model (_d)
      ## One set for predicting for regular points (_PM)

  ## 3) The model that is fit here is relatively simple, but could be more complicated with additional predictors 
    ## from stan.predictors.m included in the model

  ## 4) This model potentially runs into the classic regression extrapolation problem: many of the mosquito sampling locations
     ## have a smaller population than a decent portion of the out-of-sample prediction locations, leading to very large mosquito 
     ## count estimates for some locations.

######### !! 

stan.data.m <- list(
  N              = nrow(stan.predictors.m)
, K_lambda       = 1
, theta_sigma    = 1
, pop            = stan.predictors.m[, which.pop]
, ae_pri         = linkfun$linkfun(1 - (mosq_buffer$aedes_pred / 100))
, y              = stan.response.m
, N_pred_d       = nrow(dengue_cases[dengue_cases$year == 1, ])
, N_pred_PM      = length(reg_points.x[, grep("pop", dimnames(reg_points.x)[[2]])])
, x_pred_d       = dengue_cases[dengue_cases$year == 1, ]$pop.m
, ae_pri_pred_d  = linkfun$linkfun(1 - (dengue_cases[dengue_cases$year == 1, ]$aedes_mean / 100))
, x_pred_PM      = reg_points.x.d
, ae_pri_pred_PM = linkfun$linkfun(1 - (reg_points.z / 100))[, 1]
  )

## MCMC settings
ni <- 2000
nt <- 1
nb <- 1000
nc <- 3

## Set up so that the output gets stored and the stan model doesn't need to be rerun every time
if (!file.exists("stan_output/stan.fit.m.Rds")) {

stan.fit.m <- stan(
  file    = "stan_models/mosquito_abund_model_only_lambda_error_for_dengue_nb_opt2.stan"
, data    = stan.data.m
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.9
  ))

saveRDS(stan.fit.m, "stan_output/stan.fit.m.Rds")

} else {
  
stan.fit.m <- readRDS("stan_output/stan.fit.m.Rds")
  
}

## Pleasant way to debug quickly
# launch_shinystan(stan.fit.m)

