## pointers to where all of the layers reside

## mosquito sampling and raster samples at these locations
mosq_samples       <- read.csv("../dengue_zero_cases_stan_data/new/mosq_buffer.csv")

## raster means using buffer region around pop centers
pop_buffer         <- read.csv("../dengue_zero_cases_stan_data/new/pop_buffer.csv")
names(pop_buffer)  <- c("OBJECTID", "NOMBRE_COM", "Tipo", "POINT_X", "POINT_Y", "Niveles" 
  , "X", "Y", "aedes_mean", "land_cov1_mean", "land_cov2_mean", "land_cov3_mean"
  , "tempmean_mean", "tempvar_mean", "pop_mean" 
)

## dengue case info
dengue_cases       <- read.csv("../dengue_zero_cases_stan_data/dengue_cases.csv")

## distances 
dist_mat_to_cases  <- read.csv("../dengue_zero_cases_stan_data/distances/cases_to_pop_centers.csv")

## pop center distances to roads and hospitals
dist_mat_to_road   <- read.csv("../dengue_zero_cases_stan_data/distances/cases_to_roads.csv")
dist_mat_to_health <- read.csv("../dengue_zero_cases_stan_data/distances/cases_to_health.csv")

## fine or coarse depending on the predictions being generated
reg_points_name    <- paste("../dengue_zero_cases_stan_data/final/"
  , paste(pred_area, pred_scale, pred_time, pred_scenario, sep = "_")
  , ".csv", sep = "")
reg_points         <- read.csv(reg_points_name)

reg_points_name    <- paste("../dengue_zero_cases_stan_data/final/"
  , pred_area
  , "_to_health.csv", sep = "")
reg_points_dist_h  <- read.csv(reg_points_name)

reg_points_name    <- paste("../dengue_zero_cases_stan_data/final/"
  , pred_area
  , "_to_roads.csv", sep = "")
reg_points_dist_r  <- read.csv(reg_points_name)
