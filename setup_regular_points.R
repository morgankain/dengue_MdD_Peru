#####
## First step of bringing in data for regularly spaced points aroung Puerto Maldonado. 
## Need to do this up front, because predictions are needed for mosquito counts for all of these locations in addition 
## to predicting mosquito counts for the dengue case locations for fitting the dengue count model
#####

## Make sure the exported .csv samples the rasters in this order! change this line if the samples are in differnet orders
names(reg_points)        <- c("id", "lat", "lon", "aedes", "land1", "land2", "land3", "tempmean", "tempvar", "tempskew", "pop")
names(reg_points_dist_h) <- c("id", "out", "dist_to_h")
names(reg_points_dist_r) <- c("id", "out", "dist_to_r")
reg_points               <- left_join(reg_points, reg_points_dist_h[, -2], "id")
reg_points               <- left_join(reg_points, reg_points_dist_r[, -2], "id")
reg_points               <- transform(reg_points
  , dist_to_h = dist_to_h / 1000
  , dist_to_r = dist_to_r / 1000
)
## First scale for fitting in the mosquito count model (also take care of building the right object with all of
 ## the predictors for the Dengue count model up front)
reg_points.x <- as.matrix(reg_points %>% dplyr::select(land1, land3, pop, tempmean, tempvar))

## NAs are rivers in this specific model. Would be ok to leave them as NA, but for now have them take on the
## value in the adjacent pixel in order to get a smooth map and because stan will complain
## Do some simple local autocorrelation == 1
check_na <- which(is.na(reg_points.x[, 3]))
for (i in seq_along(check_na)) {
  nearby_not_na  <- reg_points.x[seq(check_na[i] - 10, check_na[i] + 10), 3]
  closest_not_na <- (which(!is.na(nearby_not_na)) - 11) 
  reg_points.x[check_na[i], 3] <- nearby_not_na[which(abs(closest_not_na) == min(abs(closest_not_na)))[1]]
}

## Only pop needed here for mosquito model (for now), so just pull that out, save the rest of the predictors for later down
 ## Recall, used log(pop) earlier to get stan.predictors.m.mean[3]
reg_points.x.d <- (log(reg_points.x[, 3]) - stan.predictors.m.mean[3]) / stan.predictors.m.sd[3]
  
reg_points.z <- as.matrix(reg_points %>% dplyr::select(aedes))

check_na <- which(is.na(reg_points.z[, 1]))
if (length(check_na) > 0) {
for (i in seq_along(check_na)) {
  nearby_not_na  <- reg_points.z[seq(check_na[i] - 10, check_na[i] + 10), 1]
  closest_not_na <- (which(!is.na(nearby_not_na)) - 11) 
  reg_points.z[check_na[i], 1] <- nearby_not_na[which(abs(closest_not_na) == min(abs(closest_not_na)))[1]]
}
}

