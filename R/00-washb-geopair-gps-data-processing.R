#----------------------------------
# Geographic pair matching in large-scale cluster randomized trials
#
# washb-geopair-gps-data-processing.R
#
# process the GPS data for the
# trials
#----------------------------------


#----------------------------------
# preamble
#----------------------------------
library(here)

# source configuration file
source(here("R","washb-geopair-Config.R"))

# file path to the Box data directory
Box_data_directory <- "~/Library/CloudStorage/Box-Box/washb-geopair/data/"

#----------------------------------
# Bangladesh GPS data
# includes lon/lat data and
# a bridge dataset to link
# public IDs with original IDs
#----------------------------------

# Baseline compound GPS coordinates
d_gps <- read_dta(file=paste0(Box_data_directory,"untouched/6. WASHB_Baseline_gps.dta"))

# public-private ID link
d_ids <- read_csv(file=paste0(Box_data_directory,"untouched/public-ids.csv")) %>%
  mutate(dataid = str_pad(dataid, 5, pad = "0"))

# cluster level treatment assignments
d_tr <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-tr-public.csv"))


#----------------------------------
# Load the Bangladesh administrative 
# boundaries down to level 3
# country/district/sub-district (upazila)
# 
# downloaded from: https://data.humdata.org/dataset/cod-ab-bgd
#----------------------------------
admin_b <- st_read(dsn = paste0(Box_data_directory,"geodata/bgd_adm_bbs_20201113_SHP"),layer = "bgd_admbnda_adm3_bbs_20201113")

#----------------------------------
# join public ids to the GPS 
# coordinates
#----------------------------------
d <- d_gps %>%
  left_join(d_ids,by="dataid") %>%
  dplyr::select(dataid = dataid_r, clusterid = clusterid_r, block=block_r, lon = qgpslong, lat = qgpslat)

#----------------------------------
# merge on treatment labels
#----------------------------------
d2 <- d %>%
  left_join(d_tr, by =c("block","clusterid")) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH")))

#----------------------------------
# compute median centroids for
# each block, clusterid, and treatment group
#----------------------------------
dcl <- d2 %>%
  group_by(block,clusterid,tr) %>%
  summarize(lon = median(lon),
            lat = median(lat), 
            .groups = "keep")

#----------------------------------
# join the cluster centroids to
# Bangladesh administrative data
# for use later in analyses
#----------------------------------
# convert points to sf object
# for a spatial join
dcl_sf <- dcl  %>% 
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(4326)

# join to administrative polygons
# subset to relevant labels
dcl2 <- st_join(dcl_sf,admin_b, join = st_intersects) %>%
  dplyr::select(block, clusterid, tr, geometry,ADM0_PCODE,ADM0_EN,ADM1_PCODE,ADM1_EN,ADM2_PCODE,ADM2_EN,ADM3_PCODE,ADM3_EN)

# convert back to a regular data frame
coords_b <- st_coordinates(dcl2)
dcl3 <- as.data.frame(dcl2) %>%
  dplyr::select(-geometry) %>%
  mutate(lon=coords_b[,1],
         lat=coords_b[,2])

#----------------------------------
# for a public dataset,
# shift the study clusters out
# to the middle of the indian ocean
# using a secret offset of lon/lat
#----------------------------------

dclpub <- dcl %>%
  mutate(lon = lon - 10.21412414,
         lat = lat - 35.32452342)

#----------------------------------
# view shifted points
#----------------------------------

# pointpal <- colorNumeric(
#   palette = "viridis",
#   domain=c(0,max(dclpub$block)),
#   na.color = "transparent",
# )
# 
trpal <- colorFactor(cbpal, dclpub$tr)
xlon <- mean(dclpub$lon)
ylat <- mean(dclpub$lat)
map_points <- leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.Positron",options=providerTileOptions(opacity=1)) %>%
  setView(lng = xlon, lat = ylat, zoom = 2) %>%
  addCircleMarkers(data = dclpub,
                   lng = ~ lon, lat = ~ lat,
                   # color = ~pointpal(block), 
                   color = ~trpal(tr),
                   weight=1,fillOpacity = 0.8,
                   radius=4
  ) 

map_points

#----------------------------------
# create a distance matrix between
# control clusters
#----------------------------------

# compute median centroids for
# all control clusters in each block
# then shift them into the Indian ocean 
# (same offset as above)
dcontrol_centroids <- d2 %>%
  filter(tr=="Control") %>%
  group_by(block) %>%
  summarize(lon = median(lon),
            lat = median(lat), 
            .groups = "keep") %>%
  mutate(lon = lon - 10.21412414,
         lat = lat - 35.32452342)

dclpubsf_control <- dcontrol_centroids %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(4326) %>%
  arrange(block)
  
ddist <- st_distance(x = dclpubsf_control, y = dclpubsf_control)

ddist2 <- data.frame(ddist)
colnames(ddist2) <- paste0("block_",dclpubsf_control$block)
ddist3 <- ddist2 %>%
  mutate(block = dclpubsf_control$block) %>%
  dplyr::select(block, everything())


#----------------------------------
# save analysis files
#----------------------------------
write_rds(dcl3,file=paste0(Box_data_directory,"final/bangl_analysis_gps.rds"))
write_rds(dclpub,file=paste0(Box_data_directory,"final/bangl_analysis_gps_public.rds"))
write_rds(ddist3,file=paste0(Box_data_directory,"final/bangl_analysis_block_dists.rds"))


#----------------------------------
# Kenya GPS data
# includes lon/lat data and
# a bridge dataset to link
# public IDs with original IDs
#----------------------------------


# Baseline compound GPS coordinates
d_gps <- read_csv(file=paste0(Box_data_directory,"untouched/washk_gps_jep_cleaned_manual_changes_4.1.18_encrypted.csv")) %>%
  mutate(clusterid = as.character(clusterid),
         hhid = as.character(hhid))

# public-private ID link
d_ids <- read_dta(file=paste0(Box_data_directory,"untouched/washk_id_link_ca20201215.dta"))  %>%
  dplyr::select(clusterid, compoundid, hhid, clusteridr2, compoundidr2,hhidr2) %>%
  distinct()

# anthropometry data with treatment labels
d_tr <- read_csv(file=paste0(Box_data_directory,"untouched/washb-kenya-endline-anthro-public.csv")) %>%
  dplyr::select(clusteridr2=clusterid,block,tr) %>%
  mutate(clusteridr2 = as.character(clusteridr2)) %>%
  mutate(tr = factor(tr, levels = c("Passive Control","Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))) %>%
  distinct()

# merge GPS to public IDs and treatment labels
d <- d_gps %>%
  left_join(d_ids, by=c("clusterid","hhid")) %>%
  group_by(hhid) %>%
  filter(row_number() == 1) %>%
  filter(!is.na(clusteridr2)) %>%
  left_join(d_tr,by = "clusteridr2") %>%
  ungroup() %>%
  dplyr::select(block,clusterid=clusteridr2,hhidr2,tr,lat=GPS_lat_bl,lon=GPS_long_bl)

#----------------------------------
# Load the Kenya administrative 
# boundaries down to level 2
# country/county/sub-county
# downloaded from : https://data.humdata.org/dataset/cod-ab-ken
#----------------------------------
admin_k <- st_read(dsn = paste0(Box_data_directory,"geodata/ken_adm_iebc_20191031_shp"),layer = "ken_admbnda_adm2_iebc_20191031")


#----------------------------------
# compute median centroids for
# each block,clusterid and treatment group
#----------------------------------
dcl <- d %>%
  group_by(block,clusterid,tr) %>%
  summarize(lon = median(lon),
            lat = median(lat), 
            .groups = "keep")

#----------------------------------
# join the cluster centroids to
# Bangladesh administrative data
# for use later in analyses
#----------------------------------

# convert points to sf object
# for a spatial join
dcl_sf <- dcl  %>% 
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(4326)

# this is a patch: there is one feature in the Kenya admin
# data with invalid spherical geometry, triggering this error
# with the st_join() function below:
# Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
#   Evaluation error: Found 1 feature with invalid spherical geometry.
# this reverts sf to not enforce this
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data/68481205#68481205
sf::sf_use_s2(FALSE)

# join to administrative polygons
# subset to relevant labels
dcl2 <- st_join(dcl_sf,admin_k, join = st_intersects) %>%
  dplyr::select(block, clusterid, tr, geometry,ADM0_PCODE,ADM0_EN,ADM1_PCODE,ADM1_EN,ADM2_PCODE,ADM2_EN)

# convert back to a regular data frame
coords_k <- st_coordinates(dcl2)
dcl3 <- as.data.frame(dcl2) %>%
  dplyr::select(-geometry) %>%
  mutate(lon=coords_k[,1],
         lat=coords_k[,2])


#----------------------------------
# for public dataset:
# shift the study clusters out
# to the middle of the indian ocean
# using a secret offset of lon/lat
#----------------------------------
dclpub <- dcl %>%
  mutate(lon = lon + 25.21412414,
         lat = lat + 10.12312123)

#----------------------------------
# view shifted points
#----------------------------------
trpal <- colorFactor(cbpal, dclpub$tr)
xlon <- mean(dclpub$lon)
ylat <- mean(dclpub$lat)
map_points_kenya <- leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.Positron",options=providerTileOptions(opacity=1)) %>%
  setView(lng = xlon, lat = ylat, zoom = 2) %>%
  addCircleMarkers(data = dclpub,
                   lng = ~ lon, lat = ~ lat,
                   # color = ~pointpal(block), 
                   color = ~trpal(tr),
                   weight=1,fillOpacity = 0.8,
                   radius=4
  ) 

map_points_kenya


#----------------------------------
# create a distance matrix between
# control clusters
#----------------------------------

# compute median centroids for
# all control clusters in each block
# then shift them into the Indian ocean 
# (same offset as above)
dcontrol_centroids <- d %>%
  filter(tr=="Control") %>%
  group_by(block) %>%
  summarize(lon = median(lon),
            lat = median(lat), 
            .groups = "keep") %>%
  mutate(lon = lon + 25.21412414,
         lat = lat + 10.12312123)

dclpubsf_control <- dcontrol_centroids %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(4326) %>%
  arrange(block)

ddist <- st_distance(x = dclpubsf_control, y = dclpubsf_control)

ddist2 <- data.frame(ddist)
colnames(ddist2) <- paste0("block_",dclpubsf_control$block)
ddist3 <- ddist2 %>%
  mutate(block = dclpubsf_control$block) %>%
  dplyr::select(block, everything())


#----------------------------------
# save analysis files
#----------------------------------
write_rds(dcl3,file=paste0(Box_data_directory,"final/kenya_analysis_gps.rds"))
write_rds(dclpub,file=paste0(Box_data_directory,"final/kenya_analysis_gps_public.rds"))
write_rds(ddist3,file=paste0(Box_data_directory,"final/kenya_analysis_block_dists.rds"))


