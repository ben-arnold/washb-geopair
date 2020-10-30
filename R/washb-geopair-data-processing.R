#----------------------------------
# washb-pairmatching-data-processing.R
#
# process the public datasets into
# final analysis files
#----------------------------------


#----------------------------------
# preamble
#----------------------------------
library(here)

# source configuration file
source(here("R","washb-geopair-Config.R"))


#----------------------------------
# Bangladesh public data
#----------------------------------

# cluster level treatment assignments
d_tr <- read_csv(here("data","washb-bangladesh-tr-public.csv"))

# child anthropometry measured at
# follow-up visits 1 and 2
d_anth <- read_csv(here("data","washb-bangladesh-anthro-public.csv"))

# child diarrhea measured at 
# baseline and at 
# follow-up visits 1 and 2
d_diar <- read_csv(here("data","washb-bangladesh-diar-public.csv"))

# child protozoan infection measured
# at follow-up visit 2
d_prot <- read_csv(here("data","washb-bangladesh-protozoa-public.csv"))

#----------------------------------
# merge on treatment assignments
#
# filter to years 1+2 (diarrhea)
# and year 2 (anthropometry)
#
# filter to the control and 
# nutrition-containing intervention
# arms
# 
# limit to relevant variables
#----------------------------------
d_anth2 <- d_anth %>%
  select(dataid,childid,tchild,clusterid,svy,waz,laz,whz,hcz) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy==2) %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(-tr,-svy) %>%
  rename(tr=tr2)
  
d_diar2 <- d_diar %>%
  select(dataid,childid,tchild,clusterid,svy,diar7d) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy > 0) %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(-tr,-svy) %>%
  rename(tr=tr2)

d_prot2 <- d_prot %>%
  select(dataid,clusterid,personid,block,tr,posgi) %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(-tr) %>%
  rename(tr=tr2)


#----------------------------------
# save analysis files
#----------------------------------
write_rds(d_anth2,path = here("data","bangl_analysis_anthro.rds"))
write_rds(d_diar2,path = here("data","bangl_analysis_diar.rds"))
write_rds(d_prot2,path = here("data","bangl_analysis_parasite.rds"))



