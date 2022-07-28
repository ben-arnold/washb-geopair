#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - WashB covariates
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code extracts distance to dakha for WashB clusters
# follows blog spot in https://medium.com/@abertozz/mapping-travel-times-with-malariaatlas-and-friction-surfaces-f4960f584f08

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

# WashB block level covariates
washB_block <- readRDS(file = here("data/final", "washB_block"))

# Dakha coordinates
dakha_gps <- data.frame(lon = 90.40744, lat = 23.7104)
dakha_gps_sp <- SpatialPoints(dakha_gps)

 #######################
### Data extraction ###
#######################
BGD_Adm.shp <- BGD_Adm[BGD_Adm@data$NAME_1 %in% c("Dhaka"),]

friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = BGD_Adm.shp)
malariaAtlas::autoplot_MAPraster(friction)
plot(BGD_Adm.shp)
points(washB_block[,c("lon","lat")])
points(dakha_gps)

T <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T) 

access.raster <- gdistance::accCost(T.GC, dakha_gps_sp)
malariaAtlas::autoplot_MAPraster(access.raster)

# Extract distance to closest health facility at each site
washB_block$Distance_Minute_To_Dakha <- extract(x = access.raster, y = washB_block[,c("lon","lat")])

washB_block_With_Distance_To_Dakha <- washB_block

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(washB_block_With_Distance_To_Dakha, file = here("data/final", "washB_block_With_Distance_To_Dakha"))
readr::write_csv(washB_block_With_Distance_To_Dakha, file = here("data/final", "washB_block_With_Distance_To_Dakha.csv"))

#-------------------------------------------------------------------------------

