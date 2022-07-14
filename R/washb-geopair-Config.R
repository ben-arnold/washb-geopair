
#----------------------------
# washb-pairmatching-Config.R
#
# Relative efficiency of pair
# matching in the WASH Benefits
# trials
#
# Configuration file
#----------------------------


#----------------------------
# load packages
#----------------------------

library(here)
library(tidyverse)
library(lubridate)
library(haven)

#----------------------------
# display packages
#----------------------------
library(table1)
library(kableExtra)
library(gridExtra)
library(ggplot2)

#----------------------------
# stats packages
#----------------------------
library(sandwich)
library(lmtest)
library(rptR)
library(experiment)

#----------------------------
# spatial packages
#----------------------------
library(sp)
library(sf)
library(geosphere)
library(ggspatial)
library(automap)
library(leaflet)
library(rcartocolor)
library(raster)
library(malariaAtlas)
library(gdistance)

#----------------------------
# parallel computing packages
#----------------------------
library(foreach)
library(doParallel)
registerDoParallel(detectCores() - 1)


#----------------------------
# custom color pallettes
#----------------------------
library(viridis)

# safe color blind palette
# http://jfly.iam.u-tokyo.ac.jp/color/
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# Reference: Bang Wong, Nature Methods 2011: https://www.nature.com/articles/nmeth.1618
cbpal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# NY Times rainbow palettes from the Upshot
# https://www.nytimes.com/interactive/2020/03/21/upshot/coronavirus-deaths-by-country.html
nytpal <- c("#510000", "#AC112D", "#EC6D47", "#F2A058", "#F7D269", "#839772", "#325D8A")

# https://www.nytimes.com/2020/04/23/us/coronavirus-early-outbreaks-cities.html
nytpal2 <- c("#7486AD", "#77658E","#A1493B","#C17B3F", "#E6CA83")


