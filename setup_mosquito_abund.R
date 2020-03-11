#####
## Mosquito abundance model setup
#####

mosq_samples    <- mosq_samples %>% filter(country == "PE")

## Select relevant columns for analysis
mosq_samples    <- mosq_samples %>% dplyr::select(NAME, aedes, non_aedes, total, landuse
  , aedes_mean
  , pop_mean
  , land_cov1_mean, land_cov2_mean, land_cov3_mean
  , tempmean_mean, tempvar_mean)
names(mosq_samples)[c(6:7)]   <- c("aedes_pred", "pop")

############# !*! 
## Informed Chris about this: there are a few values above 100 and below 0, which could indicate
## a pathology in his model
############# !*! 
mosq_samples$aedes_pred[mosq_samples$aedes_pred > 100] <- 99.99

## rename (residual from older script)
mosq_buffer     <- mosq_samples

## Put together data for the stan model
which.predictors.m <- c("land_cov1_mean", "land_cov3_mean", "pop", "tempmean_mean", "tempvar_mean")

stan.predictors.m  <- mosq_buffer[,
  c(
    grep(which.predictors.m[1], colnames(mosq_buffer))
  , grep(which.predictors.m[2], colnames(mosq_buffer))
  , grep(which.predictors.m[3], colnames(mosq_buffer))
  , grep(which.predictors.m[4], colnames(mosq_buffer))
  , grep(which.predictors.m[5], colnames(mosq_buffer))
  )
  ]

## Use log pop instead of pop as a predictor 
which.pop                      <- grep("pop", colnames(stan.predictors.m))
stan.predictors.m[, which.pop] <- log(stan.predictors.m[, which.pop])
stan.predictors.m              <- as.matrix(stan.predictors.m)

## Also store the mean and var of each predictor for out of sample predictions
stan.predictors.m.mean <- apply(stan.predictors.m, 2, FUN = function (z) mean(z))
stan.predictors.m.sd   <- apply(stan.predictors.m, 2, FUN = function (z) sd(z))

## try and scale each x predictor
stan.predictors.m <- apply(stan.predictors.m, 2, FUN = function (z) (z - mean(z)) / sd(z))
stan.response.m   <- mosq_buffer$aedes

