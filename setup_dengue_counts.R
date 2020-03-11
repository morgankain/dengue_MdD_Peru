#####
## Denuge counts setup 
#####

## First load the data, and add dengue cases to each pop by finding the matching pop center where
 ## dengue cases were taken. Requires some matching (nearest neighbor in QGIS). 72 of 87 locations have
  ## the same named population center and most are within 1.5km

## Why pop buffer entries are duplicated I have no idea, came that way
pop_buffer   <- pop_buffer %>% filter(!is.na(X))

pop_buffer   <- transform(pop_buffer, NOMBRE_COM = as.character(NOMBRE_COM))
dengue_cases <- transform(dengue_cases, NOMBRE = as.character(NOMBRE))

names(pop_buffer)[2]   <- "pop_name"
names(dengue_cases)[3] <- "dengue_name"

############# !*! 
## This is potentially dangerous, becuase the closest hospital for some of these places
## Is outside of MdD, so we don't have data on those places. That is, infections may be recorded 
## outside of MdD so we see a 0 recorded
## Hopefully even with this caveat these 0s will be helpful for predicting suitability vs observation
############# !*!

dist_mat_to_cases  <- dist_mat_to_cases[-which(duplicated(dist_mat_to_cases)), ]
dist_mat_to_health <- dist_mat_to_health[-which(duplicated(dist_mat_to_health)), ]
dist_mat_to_health <- dist_mat_to_health %>% group_by(InputID) %>% summarize(Distance = min(Distance))
dist_mat_to_road   <- dist_mat_to_road[-which(duplicated(dist_mat_to_road)), ]
dist_mat_to_road   <- dist_mat_to_road %>% group_by(InputID) %>% summarize(Distance = min(Distance))

names(dist_mat_to_cases)   <- c("dengue_name", "pop_name", "dist")
names(dist_mat_to_health)  <- c("pop_name", "dist")
names(dist_mat_to_road)    <- c("pop_name", "dist")

dengue_cases <- left_join(dengue_cases, dist_mat_to_cases, by = "dengue_name")
dengue_cases <- dengue_cases %>% dplyr::select(pop_name, dist, CASOS_DENG)
names(dengue_cases)[3] <- c("cases")

## Leaves behind NA for the locations with 0 cases, but the distance isn't actually used from this point on, the goal up to
 ## this point was just to pull the locations with dengue to a defined population location
pop_buffer <- left_join(pop_buffer, dengue_cases, "pop_name")

pop_buffer[is.na(pop_buffer$cases), ]$cases <- 0

## rename to match the code written from the other script
dengue_cases  <- pop_buffer
## Remove the one row with NA... 
dengue_cases  <- dengue_cases[-which(is.na(dengue_cases$aedes_mean)), ]

## Add in the distance from these places to hospitals and roads
dengue_cases <- left_join(dengue_cases, dist_mat_to_road, "pop_name")
dengue_cases <- left_join(dengue_cases, dist_mat_to_health, "pop_name")

## rename
names(dengue_cases)[apply(matrix(c("dist.x", "dist.y", "dist")), 1, FUN = function(x) which(names(dengue_cases) == x))] <- 
  c("dist_to_case", "dist_to_road", "dist_to_health")

## Sticking with the theme of using log(pop) take the log of pop here as well
dengue_cases <- transform(dengue_cases, pop_mean = log(pop_mean))

######
## Step A.extra: For proof of concept simulate new data? Mostly a failed experiment.
## Best to set to FALSE, but still run these lines because it modifies data structure
######
set.seed(10001)
n_years <- 10
source("simulate_data.R")

## grab the columns for the predictors for the mosquito abundance model; which.predictors.m
dengue_cases.s.m     <- apply(dengue_cases[
  , c("land_cov1_mean", "land_cov3_mean"
    , "pop_mean", "tempmean_mean", "tempvar_mean")], 2, FUN = function (z) mean(z))
dengue_cases.s.v     <- apply(dengue_cases[
  , c("land_cov1_mean", "land_cov3_mean"
    , "pop_mean", "tempmean_mean", "tempvar_mean")], 2, FUN = function (z) sd(z))
dengue_cases.s       <- apply(dengue_cases[
  , c("land_cov1_mean", "land_cov3_mean"
    , "pop_mean", "tempmean_mean", "tempvar_mean")], 2, FUN = function (z) (z - mean(z)) / sd(z))

## Scale the population by the mean and sd from the mosquito model to get correct estimates
 ## Note that most of these places are less populated than the places used for the mosquito sampling
dengue_cases.pop.m   <- (dengue_cases$pop_mean - stan.predictors.m.mean[which.pop]) / stan.predictors.m.sd[which.pop]

dengue_cases         <- cbind(dengue_cases[, c("OBJECTID", "pop_name", "aedes_mean", "cases", "dist_to_road", "dist_to_health", "year")]
  ,  dengue_cases.s, dengue_cases.pop.m)

## name the population columns by raw pop for dengue cases and mosquito abundance scaled population
names(dengue_cases)[c(
  which(names(dengue_cases) == "pop_mean"),
  which(names(dengue_cases) == "dengue_cases.pop.m"))] <- c("pop.d", "pop.m")

