#####  
## Option 1: Pull estimates directly out of the stan model (see commnets in "fit_mosquito_abund.R")
#####

# stan.pred_mos.d  <- cbind(1 - summary(stan.fit.m)$summary[grep("theta_pred_d", rownames(summary(stan.fit.m)$summary)), 6]
#   , summary(stan.fit.m)$summary[grep("lambda_pred_log_d", rownames(summary(stan.fit.m)$summary)), 6])

# stan.pred_mos.PM <- cbind(1 - summary(stan.fit.m)$summary[grep("theta_pred_PM", rownames(summary(stan.fit.m)$summary)), 6]
#   , summary(stan.fit.m)$summary[grep("lambda_pred_log_PM", rownames(summary(stan.fit.m)$summary)), 6])

#####  
## Option 2: Pull out coefficients and calculate parameter estimates outside of stan (see commnets in "fit_mosquito_abund.R")
#####

## For option 1 these would be in the stan model, for option 2 they are calculated outside of stan

## theta_pred_d  = inv_logit(ae_pri_pred_d + theta_bias);
## theta_pred_PM = inv_logit(ae_pri_pred_PM + theta_bias);

## lambda_pred_log_d  = alpha_lambda + x_pred_d * beta_lambda[1];
## lambda_pred_log_PM = alpha_lambda + x_pred_PM * beta_lambda[1];

## This code will have to change a bit if the model predictors expand 
stan.pred_mos.coef  <- data.frame(
  theta_bias   = summary(stan.fit.m)$summary[grep("theta_bias", rownames(summary(stan.fit.m)$summary)), 6]
, alpha_lambda = summary(stan.fit.m)$summary[grep("alpha_lambda", rownames(summary(stan.fit.m)$summary)), 6]
, beta_lambda  = summary(stan.fit.m)$summary[grep("beta_lambda", rownames(summary(stan.fit.m)$summary)), 6]
  )

stan.pred_mos.d  <- data.frame(
  1 - linkfun$linkinv(stan.pred_mos.coef$theta_bias + stan.data.m$ae_pri_pred_d)
, stan.pred_mos.coef$alpha_lambda + stan.data.m$x_pred_d
) %>% as.matrix()
dimnames(stan.pred_mos.d)[[2]] <- NULL
  
stan.pred_mos.PM  <- data.frame(
  1 - linkfun$linkinv(stan.pred_mos.coef$theta_bias + stan.data.m$ae_pri_pred_PM)
, stan.pred_mos.coef$alpha_lambda + stan.data.m$x_pred_PM
) %>% as.matrix()
dimnames(stan.pred_mos.PM)[[2]] <- NULL

stan.pred_mos.d.m <- apply(stan.pred_mos.d, 2, FUN = function (z) mean(z))
stan.pred_mos.d.v <- apply(stan.pred_mos.d, 2, FUN = function (z) sd(z))
stan.pred_mos.d   <- apply(stan.pred_mos.d, 2, FUN = function (z) (z - mean(z)) / sd(z))

stan.pred_mos.PM.m <- apply(stan.pred_mos.PM, 2, FUN = function (z) mean(z))
stan.pred_mos.PM.v <- apply(stan.pred_mos.PM, 2, FUN = function (z) sd(z))
stan.pred_mos.PM   <- apply(stan.pred_mos.PM, 2, FUN = function (z) (z - mean(z)) / sd(z))

