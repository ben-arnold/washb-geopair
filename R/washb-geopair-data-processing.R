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

# child helminth infection measured
# at follow-up visit 2
d_sth <- read_csv(here("data","washb-bangladesh-sth-public.csv"))

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
  select(dataid,clusterid,personid,block,tr,giar=posgi, delta_prot) %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  # exclude missing values
  filter(delta_prot == "Observed") %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(-tr,-delta_prot) %>%
  rename(tr=tr2)

d_sth2 <- d_sth %>%
  select(dataid,clusterid,personid,block,tr,al,tt,hw,sth) %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  # exclude missing values
  filter(!is.na(al)) %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(-tr) %>%
  rename(tr=tr2)

# merge giardia and STH datasets
# there were 3 children with giardia measures but no STH measures,
# and 137 children with STH measrues but no giardia measures
d_parasite <- d_sth2 %>%
  full_join(d_prot2,by=c("dataid","clusterid","personid","block","tr"))

#----------------------------------
# save analysis files
#----------------------------------
write_rds(d_anth2,path = here("data","bangl_analysis_anthro.rds"))
write_rds(d_diar2,path = here("data","bangl_analysis_diar.rds"))
write_rds(d_parasite,path = here("data","bangl_analysis_parasite.rds"))


#----------------------------------
# Kenya data
#----------------------------------

# child anthropometry measured at visit 2
dk_anth <- read_csv(here("data","washb-kenya-endline-anthro-public.csv")) 


# child diarrhea measured at baseline
# and at visits 1 and 2
dk_diar <- read_csv(here("data","washb-kenya-diar-public.csv"))


# child giardia and STH infection
# measured at visit 2
# note: this file is not as well
# documented as other data
# use the replication script as
# documentation: https://osf.io/fpxms/
dk_para <- haven::read_dta(here("data","parasites_kenya_public_ca20171215.dta"))






#----------------------------------
# filter to the control and 
# nutrition-containing intervention
# arms
# 
# limit to relevant variables
#----------------------------------
dk_anth2 <- dk_anth %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(clusterid, compoundid, hhid, childid, block, tr=tr2, targetchild, haz, waz, whz, hcz, haz_who, waz_who, whz_who)

dk_diar2 <- dk_diar %>%
  filter(time > 0) %>%
  filter(tr %in% c("Control","Nutrition","Nutrition + WSH")) %>%
  mutate(tr2 = ifelse(tr == "Control","Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(clusterid, compoundid, hhid, childid, block, tr=tr2, targetchild, diarr7)


dk_para2 <- dk_para %>%
  # restrict to control or nutrition arms
  # 1 = control, 6 = N, 7 = N+WSH
  filter(tr %in% c(1,6,7)) %>%
  mutate(tr2 = ifelse(tr == 1,"Control","Nutrition"),
         tr2 = factor(tr2)) %>%
  select(-tr) %>%
  select(childidr2, hhidr2, block,tr=tr2, giar = giardia_yn, al = ascaris_yn, tt = trichuris_yn, hw = hook_yn, sth = sth_yn)

#----------------------------------
# save analysis files
#----------------------------------
write_rds(dk_anth2,path = here("data","kenya_analysis_anthro.rds"))
write_rds(dk_diar2,path = here("data","kenya_analysis_diar.rds"))
write_rds(dk_para2,path = here("data","kenya_analysis_parasite.rds"))

