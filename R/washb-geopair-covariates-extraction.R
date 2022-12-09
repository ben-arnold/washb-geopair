#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - WashB covariates
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code extracts covariates at cluster and village level for WashB

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("cluster-level-covariates/R", "0-config.R"))

#-------------------------------------------------------------------------------

###################
### Import data ###
###################
# BGD_Admin
BGD_Adm <- raster::getData("GADM",
                           country = "BGD",
                           level = 1,
                           path = here("data/untouched/country-admin"))

# WashB clusters GPS
washB <- readRDS("data/untouched/cluster-gps/bangl_analysis_gps.rds")

# WashB Jade village-level climatic variables
washb_bangladesh_consecutive_months_of_water <- readRDS("data/untouched/jade-covariates/washb-bangladesh-consecutive-months-of-water.rds")
washb_bangladesh_months_of_water <- readRDS("data/untouched/jade-covariates/washb-bangladesh-months_of_water.rds")
washb_bangladesh_surface_water <- readRDS("data/untouched/jade-covariates/washb-bangladesh-surface-water.rds")
washb_bangladesh_water_history <- readRDS("data/untouched/jade-covariates/washb-bangladesh-water-history.rds")

# Flood (created from global flood database in flood-bangladesh project)
flood_area_percent <- readRDS(here("data/untouched/flood", "flood_area_percent"))

# Accessibility to cities (malaria atlas project)
accessibility_to_cities_2015_min <- raster(here("data/untouched/2015_accessibility_to_cities_v1/2015_accessibility_to_cities_v1.0.tif"))

accessibility_to_cities_2015_min_mask <- mask(crop(accessibility_to_cities_2015_min, BGD_Adm), BGD_Adm)
plot(accessibility_to_cities_2015_min_mask)
#-------------------------------------------------------------------------------

####################
### Process data ###
####################
## aggregate data at block level
washB_block <- (washB
                %>% group_by(block)
                %>% summarise(lon = mean(lon, na.rm = T),
                              lat = mean(lat, na.rm = T))
                )

## Extract flood and accesibility from raster
washB_block$accessibility_to_cities_2015_min <- extract(x = accessibility_to_cities_2015, y = st_as_sf(washB_block, coords = c("lon", "lat")))
washB_block$flood_area_percent <- extract(x = flood_area_percent, y = st_as_sf(washB_block, coords = c("lon", "lat")))

## Extract jade climatic covariates from neighrest neighboor
# Summarize Jade's variables at village level (i.e remove temporal variations)
washb_bangladesh_water_covariates <- (washb_bangladesh_surface_water
                                      %>% left_join(washb_bangladesh_water_history
                                                    %>% dplyr::select(qgpslong, qgpslat, distance_yearly_hist, distance_monthly_hist)
                                                    %>% distinct(qgpslong, qgpslat, .keep_all = T))
                                      %>% dplyr::select(-distance_from_source)
                                      )

# Identify nearest village to each block centroid
nearest_village_to_block_centroid <- st_nearest_feature(x = st_as_sf(washB_block, coords = c("lon", "lat")),
                                                        y = st_as_sf(washb_bangladesh_water_covariates, coords = c("qgpslong", "qgpslat")))

# Append covariates to block level data
washB_block <- cbind(washB_block,
                     washb_bangladesh_water_covariates[nearest_village_to_block_centroid,])


####################
#-------------------------------------------------------------------------------

# Save dataset
saveRDS(washB_block, file = here("data/final", "washB_block"))
readr::write_csv(washB_block, file = here("data/final", "washB_block.csv"))

#-------------------------------------------------------------------------------
