################################################################################
### Extremelt ugly scr for converting predictions into result figures.       ###
### Apologize that this cant just be run through... but the idea here is to: ###                                          
################################################################################
 ## 1) create a long form dataset that stacks the results for each scenario
 ## 2) create a wide dataset that cbinds CI for all of the scenarios
 ## 3) from this wide dataset pull out the pixels where we expect dengue to increase or decrease

#### Figures to export from here
 ## 1) Dengue risk in the present with numbers of cases on top
 ## 2) 3 layer raster with raw predicted medians
 ## 3) 3 layer raster with 95% CI overlap that also shows predicted population sizes
 ## 4) Table with risk, including # of people

names(stan.fit.d.zero_est.sost) <- paste(
  names(stan.fit.d.zero_est.sost), "sost", sep = "_"
)

names(stan.fit.d.zero_est.sost) <- names(stan.fit.d.zero_est)

names(stan.fit.d.zero_est.sost)[c(4, 5)] <- names(stan.fit.d.zero_est.sost)[c(5, 4)]

reg_points.export.all.wide <- cbind(
  id  = reg_points.export.pres$id
, lat = reg_points.export.pres$lat
, lon = reg_points.export.pres$lon
, stan.fit.d.zero_est.pres
, stan.fit.d.zero_est.rea
, stan.fit.d.zero_est.sost
, stan.fit.d.zero_est.peor
  )


### Stack of present prediction with population density
raster.pres      <- rasterFromXYZ(reg_points.export.pres[, c('lat', 'lon', 'est_theta')])
raster.pres.pop  <- rasterFromXYZ(reg_points.export.pres[, c('lat', 'lon', 'pop')])
raster.brick     <- brick(raster.pres, raster.pres.pop)
export.name      <- paste("../map_export/", pred_time, "/", "dengue_", pred_scale, "_pres_with_pop.tif", sep = "")
bf               <- writeRaster(raster.brick, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

### Stack of all future predictions
raster.rea   <- rasterFromXYZ(reg_points.export.rea[, c('lat', 'lon', 'est_theta')])
raster.sost  <- rasterFromXYZ(reg_points.export.sost[, c('lat', 'lon', 'est_theta')])
raster.peor  <- rasterFromXYZ(reg_points.export.peor[, c('lat', 'lon', 'est_theta')])
raster.brick <- brick(raster.rea, raster.peor, raster.sost)

export.name <- paste("../map_export/", pred_time, "/", "dengue_", pred_scale, "_fut_all.tif", sep = "")
bf          <- writeRaster(raster.brick, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

# saveRDS(reg_points.export.all.wide, "reg_points.export.all.wide.Rds")

### Check which pixels are greater or less than the present for each future scenario 
reg_points.export.all.wide.pres <- reg_points.export.all.wide %>%
  mutate(
    fut_lower  = ifelse(lwr_theta_sost < upr_theta_pres, -1, 0)
  , fut_higher = ifelse(upr_theta_sost > lwr_theta_pres, 1, 0)
    ) %>% 
  mutate(
    fut_diff   = ifelse(fut_lower == -1, -1, 0)
  ) %>% 
  mutate(
    fut_diff   = ifelse(fut_higher == 1, 1, fut_diff)
  ) %>% dplyr::select(
    id, lat, lon, fut_higher, fut_lower, fut_diff
  ) %>% 
  left_join(., reg_points.export.pres[, c(2, 3, 11)])

names(reg_points.export.all.wide.sost)[7] <- c("pop_sost")

reg_points.export.all.wide.sost <- reg_points.export.all.wide.sost %>% mutate(pop_sost = ifelse(fut_diff != 0, pop_sost, 0))

raster.sost.diff       <- rasterFromXYZ(reg_points.export.all.wide.sost[, c('lat', 'lon', 'fut_diff')])
raster.sost.diff.pop   <- rasterFromXYZ(reg_points.export.all.wide.sost[, c('lat', 'lon', 'pop_sost')])
raster.brick.diff      <- brick(raster.sost.diff, raster.sost.diff.pop)
export.name            <- paste("../map_export/", pred_time, "/", "dengue_", pred_scale, "_fut_sost_diff.tif", sep = "")
bf                     <- writeRaster(raster.brick.diff, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

### All together
export.name <- paste("../map_export/", pred_time, "/", "dengue_", pred_scale, "_fut_all_diff.tif", sep = "")

raster.rea.diff   <- rasterFromXYZ(reg_points.export.all.wide.rea[, c('lat', 'lon', 'fut_diff')])
raster.sost.diff  <- rasterFromXYZ(reg_points.export.all.wide.sost[, c('lat', 'lon', 'fut_diff')])
raster.peor.diff  <- rasterFromXYZ(reg_points.export.all.wide.peor[, c('lat', 'lon', 'fut_diff')])
raster.brick.diff <- brick(raster.rea.diff, raster.peor.diff, raster.sost.diff)

bf <- writeRaster(raster.brick.diff, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

### Also, want the actual risk in the pixels with non-overlap of 95% CI for Peor overlapped
 ### With predictions for the present
reg_points.export.all.wide.peor.t <- reg_points.export.all.wide.peor.t %>%
  mutate(est_theta_peor = ifelse(fut_higher == 1, est_theta_peor, 0))

raster.pres.pcomp  <- rasterFromXYZ(reg_points.export.all.wide.peor.t[, c('lat', 'lon', 'est_theta_pres')])
raster.peor.pcomp  <- rasterFromXYZ(reg_points.export.all.wide.peor.t[, c('lat', 'lon', 'est_theta_peor')])
raster.brick.pcomp <- brick(raster.pres.pcomp, raster.peor.pcomp)
export.name        <- paste("../map_export/", pred_time, "/", "dengue_", pred_scale, "_pres_peor_comp.tif", sep = "")
bf                 <- writeRaster(raster.brick.pcomp, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)



## Determine area where we expect dengue to be increasing, decreasing and avg weighted dengue risk
summary_change <- data.frame(
  scenario                     = c("sost", "rea", "peor")
, dengue_risk_area_expansion   = c(
 sum(reg_points.export.all.wide.sost$fut_higher) * (250 * 250 / 1000 / 1000)
, sum(reg_points.export.all.wide.rea$fut_higher)  * (250 * 250 / 1000 / 1000)
, sum(reg_points.export.all.wide.peor$fut_higher) * (250 * 250 / 1000 / 1000)
)
, dengue_risk_area_reduction   = c(
  sum(reg_points.export.all.wide.sost$fut_lower) * (250 * 250 / 1000 / 1000) * -1 
, sum(reg_points.export.all.wide.rea$fut_lower)  * (250 * 250 / 1000 / 1000) * -1
, sum(reg_points.export.all.wide.peor$fut_lower) * (250 * 250 / 1000 / 1000) * -1
)
, people_experiencing_higher_risk = c(
  sum(reg_points.export.all.wide.sost$fut_higher * (250 * 250 / (100 * 100)) *
      reg_points.export.all.wide.sost$pop_sost) - 
  sum(reg_points.export.all.wide.sost$fut_higher * (250 * 250 / (100 * 100)) *
      reg_points.export.all.wide.pres$pop)
, sum(reg_points.export.all.wide.rea$fut_higher  * (250 * 250 / (100 * 100)) * 
    reg_points.export.all.wide.rea$pop_rea) - 
  sum(reg_points.export.all.wide.rea$fut_higher  * (250 * 250 / (100 * 100)) * 
    reg_points.export.all.wide.pres$pop)
, sum(reg_points.export.all.wide.peor$fut_higher * (250 * 250 / (100 * 100)) * 
    reg_points.export.all.wide.peor$pop_peor) - 
  sum(reg_points.export.all.wide.peor$fut_higher * (250 * 250 / (100 * 100)) * 
    reg_points.export.all.wide.pres$pop)
)
)

write.table(summary_change, file = "summary_change.csv")

reg_points.export.peor  <- cbind(
  reg_points.peor
, stan.fit.d.zero_est.peor
  ) %>% mutate(scenario = "peor")

reg_points.export.all <- rbind(
  reg_points.export.pres
, reg_points.export.rea
, reg_points.export.sost
, reg_points.export.peor
)

# saveRDS(reg_points.export.all, "reg_points.export.all.Rds")


check_vals <- data.frame(
  covariate = c(
    rep("land1", nrow(reg_points.pres) * 2)
  , rep("pop", nrow(reg_points.pres) * 2)
  , rep("aedes", nrow(reg_points.pres) * 2)
  , rep("den", nrow(reg_points.pres) * 2)
  )

, model = c(
  c(rep("pres", nrow(reg_points.pres)), rep("peor", nrow(reg_points.pres)))
, c(rep("pres", nrow(reg_points.pres)), rep("peor", nrow(reg_points.pres)))
, c(rep("pres", nrow(reg_points.pres)), rep("peor", nrow(reg_points.pres)))
, c(rep("pres", nrow(reg_points.pres)), rep("peor", nrow(reg_points.pres)))
)
  
, value     = c(
  reg_points.pres$land1, reg_points.peor$land1
, reg_points.pres$pop, reg_points.peor$pop
, reg_points.pres$aedes, reg_points.peor$aedes
, stan.fit.d.zero_est.pres$est_theta_pres, stan.fit.d.zero_est.peor$est_theta_peor
)

)

check_vals %>% 
  group_by(model, covariate) %>%
  summarize(mean(value))




