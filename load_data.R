## pointers to where all of the layers reside

## mosquito sampling and raster samples at these locations
mosq_samples       <- read.csv("data/mosq_buffer.csv")

## raster means using buffer region around pop centers
pop_buffer         <- read.csv("data/pop_buffer.csv")

## dengue case info
dengue_cases       <- read.csv("data/dengue_cases.csv")

## distances 
dist_mat_to_cases  <- read.csv("data/cases_to_pop_centers.csv")

## pop center distances to roads and hospitals
dist_mat_to_road   <- read.csv("data/cases_to_roads.csv")
dist_mat_to_health <- read.csv("data/cases_to_health.csv")

## fine or coarse depending on the predictions being generated
reg_points_name    <- paste("data/", pred_scale, "_pres_all.csv", sep = "")
reg_points         <- read.csv(reg_points_name)
reg_points_name    <- paste("data/", pred_scale, "_to_health.csv", sep = "")
reg_points_dist_h  <- read.csv(reg_points_name)
reg_points_name    <- paste("data/", pred_scale, "_to_roads.csv", sep = "")
reg_points_dist_r  <- read.csv(reg_points_name)

