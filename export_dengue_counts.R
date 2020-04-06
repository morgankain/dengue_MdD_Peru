###### !!
## Two options here as well. For details see fit_mosquito_abund.R, the principles are the same
###### !!

#####  
## Option 1: Pull estimates directly out of the stan model
#####

#stan.fit.d.mean_est <- data.frame(
#  lwr = summary(stan.fit.d)[[1]][grep("y_sim", dimnames(summary(stan.fit.d)[[1]])[[1]]), 4] 
#, est = summary(stan.fit.d)[[1]][grep("y_sim", dimnames(summary(stan.fit.d)[[1]])[[1]]), 6]
#, upr = summary(stan.fit.d)[[1]][grep("y_sim", dimnames(summary(stan.fit.d)[[1]])[[1]]), 8]
#)

## Predictions for regular spaced points
#stan.fit.d.zero_est <- data.frame(
#  lwr = summary(stan.fit.d)[[1]][grep("theta_pred", dimnames(summary(stan.fit.d)[[1]])[[1]]), 4] 
#, est = summary(stan.fit.d)[[1]][grep("theta_pred", dimnames(summary(stan.fit.d)[[1]])[[1]]), 6]
#, upr = summary(stan.fit.d)[[1]][grep("theta_pred", dimnames(summary(stan.fit.d)[[1]])[[1]]), 8]
#)

## Predictions for the population centers
#stan.fit.d.zero_est_buffer <- data.frame(
#  lwr = summary(stan.fit.d)[[1]][grep("theta", dimnames(summary(stan.fit.d)[[1]])[[1]]), 4] 
#, est = summary(stan.fit.d)[[1]][grep("theta", dimnames(summary(stan.fit.d)[[1]])[[1]]), 6]
#, upr = summary(stan.fit.d)[[1]][grep("theta", dimnames(summary(stan.fit.d)[[1]])[[1]]), 8]
#)
#stan.fit.d.zero_est_buffer <- stan.fit.d.zero_est_buffer[6:247, ]

#####  
## Option 2: Pull out coefficients and calculate parameter estimates outside of stan
#####

## !! Slight difference here between mosquito and dengue estimates are that I actually do want
 ## CI on the predictions, so estimating outside of stan is definitely more of a pain...
  ## and will involve pulling out all the samples to preserve vcov and then summarizing. Fine
   ## but easier to instead adjust the stan model so that only the estimates are returned so that it isn't
    ## too large of an object (see fit_dengue_counts.R for the extra few parameters for the call to stan)

## For option 1 these would be in the stan model, for option 2 they are calculated outside of stan

## Eeek, not dynamic at all... Hope to move to opt 1 anyway...
 ## To do it this way to get CI on the predictions need all the samples
export.name <- paste("../map_export/", "dengue_"
  , paste(pred_area, pred_scale, pred_time, pred_scenario, sep = "_")
  , sep = "")

## If export to the raster has already happened don't do this slow step...
#if (!file.exists(paste(export.name, ".Rds", sep = ""))) {

stan.pred_den.samps  <- data.frame(
  alpha_theta     = stan.fit.d@sim$samples[[1]]$alpha_theta
, beta_theta1     = stan.fit.d@sim$samples[[1]]$'beta_theta[1]'
, beta_theta2     = stan.fit.d@sim$samples[[1]]$'beta_theta[2]'
, beta_theta3     = stan.fit.d@sim$samples[[1]]$'beta_theta[3]'
, beta_theta_mos  = stan.fit.d@sim$samples[[1]]$'beta_theta_mos[1]'
, alpha_lambda    = stan.fit.d@sim$samples[[1]]$alpha_lambda
, beta_lambda1    = stan.fit.d@sim$samples[[1]]$'beta_lambda[1]'
, beta_lambda2    = stan.fit.d@sim$samples[[1]]$'beta_lambda[2]'
, beta_lambda3    = stan.fit.d@sim$samples[[1]]$'beta_lambda[3]'
, beta_lambda_mos = stan.fit.d@sim$samples[[1]]$'beta_lambda_mos[1]'
, beta_obs1       = stan.fit.d@sim$samples[[1]]$'beta_obs[1]'
, beta_obs2       = stan.fit.d@sim$samples[[1]]$'beta_obs[2]'
)

## Will want to convert this to apply, but this is easy...
stan.fit.d.theta_est <- data.frame(
  lwr     = numeric(nrow(stan.data.d$x_pred))
, lwr_nrw = numeric(nrow(stan.data.d$x_pred))
, est     = numeric(nrow(stan.data.d$x_pred))
, upr     = numeric(nrow(stan.data.d$x_pred))
, upr_nrw = numeric(nrow(stan.data.d$x_pred))
)

stan.fit.d.lambda_est <- data.frame(
  lwr     = numeric(nrow(stan.data.d$x_pred))
, lwr_nrw = numeric(nrow(stan.data.d$x_pred))
, est     = numeric(nrow(stan.data.d$x_pred))
, upr_nrw = numeric(nrow(stan.data.d$x_pred))
, upr     = numeric(nrow(stan.data.d$x_pred))
)

stan.fit.d.zero_est <- data.frame(
  lwr     = numeric(nrow(stan.data.d$x_pred))
, lwr_nrw = numeric(nrow(stan.data.d$x_pred))
, est     = numeric(nrow(stan.data.d$x_pred))
, upr     = numeric(nrow(stan.data.d$x_pred))
, upr_nrw = numeric(nrow(stan.data.d$x_pred))
)

## Damn slow...
for (i in 1:nrow(stan.data.d$x_pred)) {
  
## probability of drawing from a count distribution
 theta_pred  <- 1 - 
  linkfun$linkinv(
  stan.pred_den.samps$alpha_theta + 
  stan.data.d$x_pred[i, 1] * stan.pred_den.samps$beta_theta1 +
  stan.data.d$x_pred[i, 2] * stan.pred_den.samps$beta_theta2 +
  stan.data.d$x_pred[i, 3] * stan.pred_den.samps$beta_theta3 +
  stan.data.d$z_theta_pred[i, 1] * stan.pred_den.samps$beta_theta_mos
  ) 
 
## mean of the count distribution
 lambda_pred <- 
  exp((stan.pred_den.samps$alpha_lambda + 
  stan.data.d$x_pred[i, 1] * stan.pred_den.samps$beta_lambda1 +
  stan.data.d$x_pred[i, 2] * stan.pred_den.samps$beta_lambda2 +
  stan.data.d$x_pred[i, 3] * stan.pred_den.samps$beta_lambda3 +
  stan.data.d$z_theta_pred[i, 1] * stan.pred_den.samps$beta_lambda_mos +
  stan.data.d$x_obs_pred[i, 1] * stan.pred_den.samps$beta_obs1 + 
  stan.data.d$x_obs_pred[i, 2] * stan.pred_den.samps$beta_obs2 + 
  stan.data.d$offset_pred[i])) 

## Probability of getting a zero = 1 - probability of drawing from the count distribution + probability of draqing a zero | draw from a count
prob_0_pred <- 1 - (  ## 1 - 
  ## total probability of zero = 
  (1 - theta_pred) +                 ## probability of a structural 0 
  theta_pred * dpois(0, lambda_pred) ## probability of drawing a zero from the count distribution
)

theta_pred  <- theta_pred  %>% quantile(c(0.05, 0.25, 0.50, 0.75, 0.95))
lambda_pred <- lambda_pred %>% quantile(c(0.05, 0.25, 0.50, 0.75, 0.95))
prob_0_pred <- prob_0_pred %>% quantile(c(0.05, 0.25, 0.50, 0.75, 0.95))
 
stan.fit.d.theta_est[i, ]  <- theta_pred
stan.fit.d.lambda_est[i, ] <- lambda_pred
stan.fit.d.zero_est[i, ]   <- prob_0_pred

if (((i / 1000) %% 1) == 0) {
  print(i / nrow(stan.data.d$x_pred))
}

}

names(stan.fit.d.theta_est)  <- paste(names(stan.fit.d.theta_est), "theta", sep = "_")
names(stan.fit.d.lambda_est) <- paste(names(stan.fit.d.lambda_est), "lambda", sep = "_")
names(stan.fit.d.zero_est)   <- paste(names(stan.fit.d.zero_est), "zero", sep = "_")

#####  
## Regardless of the option, take the predictions and export to the map
#####

### Want to export these estimates onto a map:
reg_points.export  <- cbind(reg_points, stan.fit.d.theta_est)
reg_points.export  <- cbind(reg_points.export, stan.fit.d.lambda_est)
reg_points.export  <- cbind(reg_points.export, stan.fit.d.zero_est)

saveRDS(reg_points.export, paste(export.name, ".Rds", sep = ""))

reg_points.export <- reg_points.export %>% filter(pop < 30)
# reg_points.export <- reg_points.export  %>% mutate(pred_diff = est_theta - est_zero)

raster_out <- rasterFromXYZ(reg_points.export[, c('lat', 'lon', 'est_theta')])
writeRaster(raster_out
  , paste(export.name, "_theta", ".tif", sep = "")
  , format = "GTiff")

#} else {
#  
#raster_out <- raster(paste(export.name, ".tif", sep = ""))
#
#}

