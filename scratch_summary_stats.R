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

reg_points.export.peor <- readRDS("../map_export/dengue_TAH_coarse_fut_peor.Rds")

## Only needed for PM because of odd out-of boundary sampling
# reg_points.export.pres <- reg_points.export.pres %>% filter(pop < 30)
raster_out             <- rasterFromXYZ(reg_points.export.peor[, c('lat', 'lon', 'est_theta')])

export.name            <- "../map_export/dengue_TAH_coarse_peor_fut"

writeRaster(raster_out
  , paste(export.name, "_theta", ".tif", sep = "")
  , format = "GTiff")

## Just pull out theta est
reg_points.export.peor.est <- reg_points.export.peor %>% dplyr::select(lwr_theta, lwr_nrw_theta, est_theta, upr_theta, upr_nrw_theta)

## upr_theta and upr_nrw_theta named backwards
names(reg_points.export.rea.est)[c(4, 5)] <- names(reg_points.export.rea.est)[c(5, 4)]

## add name of the scenario
names(reg_points.export.pres.est) <- paste(names(reg_points.export.pres.est), "pres", sep = "_")

## Combine
reg_points.export.all.wide <- cbind(
  id  = reg_points.export.pres$id
, lat = reg_points.export.pres$lat
, lon = reg_points.export.pres$lon
, reg_points.export.pres.est
, reg_points.export.rea.est
, reg_points.export.sost.est
, reg_points.export.peor.est)

saveRDS(reg_points.export.all.wide, "reg_points.export.all.wide.TAH.Rds")

### Stack of present prediction with population density
raster.pres      <- rasterFromXYZ(reg_points.export.pres[, c('lat', 'lon', 'est_theta')])
raster.pres.pop  <- rasterFromXYZ(reg_points.export.pres[, c('lat', 'lon', 'pop')])
raster.brick     <- brick(raster.pres, raster.pres.pop)
export.name      <- paste("../map_export/", "dengue_", pred_scale, "_pres_with_pop.tif", sep = "")
bf               <- writeRaster(raster.brick, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

### Stack of all future predictions
raster.rea   <- rasterFromXYZ(reg_points.export.rea[, c('lat', 'lon', 'est_theta')])
raster.sost  <- rasterFromXYZ(reg_points.export.sost[, c('lat', 'lon', 'est_theta')])
raster.peor  <- rasterFromXYZ(reg_points.export.peor[, c('lat', 'lon', 'est_theta')])
raster.brick <- brick(raster.rea, raster.peor, raster.sost)

export.name <- paste("../map_export/", "dengue_", "TAH_", pred_scale, "_fut_all.tif", sep = "")
bf          <- writeRaster(raster.brick, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

### Check which pixels are greater or less than the present for each future scenario 
reg_points.export.all.wide.pres <- reg_points.export.all.wide %>%
  mutate(
    fut_lower  = ifelse(upr_theta_rea < lwr_theta_pres, -1, 0)
  , fut_higher = ifelse(lwr_theta_rea > upr_theta_pres, 1, 0)
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

names(reg_points.export.all.wide.pres)[7] <- c("pop_rea")

## Bit of a hacky way to just keep pop in the areas where fut_diff is != 0 (for calculations lower down and visualizations)
reg_points.export.all.wide.rea <- reg_points.export.all.wide.rea %>% mutate(pop_rea = ifelse(fut_diff != 0, pop_rea, 0))

raster.rea.diff       <- rasterFromXYZ(reg_points.export.all.wide.rea[, c('lat', 'lon', 'fut_diff')])
raster.rea.diff.pop   <- rasterFromXYZ(reg_points.export.all.wide.rea[, c('lat', 'lon', 'pop_rea')])
raster.brick.diff     <- brick(raster.rea.diff, raster.rea.diff.pop)
export.name           <- paste("../map_export/", "dengue_", "TAH_", pred_scale, "_fut_rea_diff.tif", sep = "")
bf                    <- writeRaster(raster.brick.diff, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

## All together
export.name <- paste("../map_export/", "dengue_", "TAH_", pred_scale, "_fut_all_diff.tif", sep = "")

raster.rea.diff   <- rasterFromXYZ(reg_points.export.all.wide.rea[, c('lat', 'lon', 'fut_diff')])
raster.sost.diff  <- rasterFromXYZ(reg_points.export.all.wide.sost[, c('lat', 'lon', 'fut_diff')])
raster.peor.diff  <- rasterFromXYZ(reg_points.export.all.wide.peor[, c('lat', 'lon', 'fut_diff')])
raster.brick.diff <- brick(raster.rea.diff, raster.peor.diff, raster.sost.diff)

bf <- writeRaster(raster.brick.diff, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

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

### Also, want the actual risk in the pixels with non-overlap of 95% CI for peor overlapped with predictions for the present
reg_points.export.all.wide.rea.t <- reg_points.export.all.wide %>%
  mutate(
    fut_lower  = ifelse(upr_theta_rea < lwr_theta_pres, -1, 0)
  , fut_higher = ifelse(lwr_theta_rea > upr_theta_pres, 1, 0)
    ) %>% 
  mutate(
    fut_diff   = ifelse(fut_lower == -1, -1, 0)
  ) %>% 
  mutate(
    fut_diff   = ifelse(fut_higher == 1, 1, fut_diff)
  )

reg_points.export.all.wide.peor.t <- reg_points.export.all.wide.peor.t %>%
  mutate(est_theta_peor = ifelse(fut_higher == 1, est_theta_peor, 0))

raster.pres.pcomp  <- rasterFromXYZ(reg_points.export.all.wide.peor.t[, c('lat', 'lon', 'est_theta_pres')])
raster.peor.pcomp  <- rasterFromXYZ(reg_points.export.all.wide.peor.t[, c('lat', 'lon', 'est_theta_peor')])
raster.brick.pcomp <- brick(raster.pres.pcomp, raster.peor.pcomp)
export.name        <- paste("../map_export/", "dengue_", "TAH_", pred_scale, "_pres_peor_comp.tif", sep = "")
bf                 <- writeRaster(raster.brick.pcomp, filename = export.name, options = "INTERLEAVE=BAND", overwrite = TRUE)

## Some scr
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
