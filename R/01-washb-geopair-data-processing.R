#----------------------------------
# Geographic pair matching in large-scale cluster randomized trials
#
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

# file path to the Box data directory
Box_data_directory <- "~/Library/CloudStorage/Box-Box/washb-geopair/data/"

#----------------------------------
# Bangladesh public data
#----------------------------------

# cluster level treatment assignments
d_tr <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-tr-public.csv"))

# child anthropometry measured at
# follow-up visits 1 and 2
d_anth <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-anthro-public.csv"))


# child development data
# at follow-up visit 2
# Extended Ages and Stages Questionnaire (EASQ)
# and MacArthur-Bates Communicative Development Inventory (CDI)
d_easq <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-easq-year2.csv"))
d_cdi  <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-cdi-year2.csv"))

# child diarrhea measured at 
# baseline and at 
# follow-up visits 1 and 2
d_diar <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-diar-public.csv"))

# child protozoan infection measured
# at follow-up visit 2
d_prot <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-protozoa-public.csv"))

# child helminth infection measured
# at follow-up visit 2
d_sth <- read_csv(file=paste0(Box_data_directory,"untouched/washb-bangladesh-sth-public.csv"))

#----------------------------------
# merge on treatment assignments
#
# filter to years 1+2 (diarrhea)
# and year 2 (anthropometry)
# 
# limit to relevant variables
#----------------------------------
d_anth2 <- d_anth %>%
  dplyr::select(dataid,childid,clusterid,svy,agey,sex,waz,laz,whz,hcz) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy==2) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  dplyr::select(-svy) %>%
  dplyr::select(block,clusterid,tr,dataid,childid,agey,sex,waz,laz,whz,hcz)
  

d_easq2 <- d_easq %>%
  dplyr::select(dataid,childid,clusterid,block,tchild,arm, sex, z_easq_com = z_com, z_easq_motor = z_motor, z_easq_pers = z_personal, z_easq_total = z_combined) %>%
  mutate(tr = case_when(
    arm == 1 ~ "Sanitation",
    arm == 2 ~ "Handwashing",
    arm == 3 ~ "Water",
    arm == 4 ~ "Nutrition",
    arm == 5 ~ "WSH",
    arm == 6 ~ "Nutrition + WSH",
    arm == 7 ~ "Control"
  ), 
  tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  dplyr::select(-arm)
  

d_cdi2 <- d_cdi %>%
  dplyr::select(dataid,childid,clusterid,block,tchild, arm, agey=ageyears, sex,  z_cdi_comp = z_endline_CDI_understand, z_cdi_expr = z_endline_CDI_say) %>%
  mutate(tr = case_when(
    arm == 1 ~ "Sanitation",
    arm == 2 ~ "Handwashing",
    arm == 3 ~ "Water",
    arm == 4 ~ "Nutrition",
    arm == 5 ~ "WSH",
    arm == 6 ~ "Nutrition + WSH",
    arm == 7 ~ "Control"
  ), 
  tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  dplyr::select(-arm)
  

d_chd2 <- full_join(d_easq2, d_cdi2, by = c("dataid","childid","clusterid","block","tchild","tr","sex")) %>%
  dplyr::select(dataid,childid,clusterid,tchild,block,tr,agey,sex,everything())

## child development data has not previosly been made public
## replace internal IDs with public IDs
d_pubids <- read_csv(here(Box_data_directory,"untouched/public-ids.csv"))
d_chd3 <- left_join(d_chd2, d_pubids, join_by(block,clusterid,dataid)) %>%
  mutate(sex = ifelse(sex==0,"female","male")) %>%
  dplyr::select(-block,-clusterid,-dataid) %>%
  dplyr::select(block=block_r,clusterid = clusterid_r, tr, dataid = dataid_r,childid,tchild,agey,sex,everything()) 


d_diar2 <- d_diar %>%
  dplyr::select(dataid,childid,tchild,clusterid,agey=ageyrs,sex,svy,diar7d) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy > 0) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  dplyr::select(block,clusterid,tr,dataid,childid,tchild,svy,agey,sex,diar7d)
  

d_prot2 <- d_prot %>%
  dplyr::select(dataid,clusterid,personid,block,tr,sex, giar=posgi, delta_prot) %>%
  # exclude missing values
  filter(delta_prot == "Observed") %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  )

d_sth2 <- d_sth %>%
  dplyr::select(dataid,clusterid,personid,block,tr,agey,sex,al,tt,hw,sth) %>%
  # exclude missing values
  filter(!is.na(al)) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  )

# merge giardia and STH datasets
# there were 5 children with giardia measures but no STH measures,
# and 293 children with STH measures but no giardia measures
# these 5 children w/ giardia but not STH will have missing age
# in the composite dataset
d_parasite <- d_sth2 %>%
  full_join(d_prot2,by=c("dataid","clusterid","personid","block","tr","sex")) %>%
  dplyr::select(block,clusterid,tr,dataid,personid,agey,sex,al,tt,hw,sth,giar)


#----------------------------------
# save analysis files
#----------------------------------
skimr::skim(d_anth2)
write_rds(d_anth2,file = paste0(Box_data_directory,"final/bangl_analysis_anthro.rds"))
write_csv(d_anth2,file = paste0(Box_data_directory,"final/bangl_analysis_anthro.csv"))

skimr::skim(d_chd3)
write_rds(d_chd3, file = paste0(Box_data_directory,"final/bangl_analysis_chdev.rds"))
write_csv(d_chd3, file = paste0(Box_data_directory,"final/bangl_analysis_chdev.csv"))

skimr::skim(d_diar2)
write_rds(d_diar2,file = paste0(Box_data_directory,"final/bangl_analysis_diar.rds"))
write_csv(d_diar2,file = paste0(Box_data_directory,"final/bangl_analysis_diar.csv"))

skimr::skim(d_parasite)
write_rds(d_parasite,file = paste0(Box_data_directory,"final/bangl_analysis_parasite.rds"))
write_csv(d_parasite,file = paste0(Box_data_directory,"final/bangl_analysis_parasite.csv"))


#----------------------------------
# Kenya data
#----------------------------------

# child anthropometry measured at visit 2
dk_anth <- read_csv(file=paste0(Box_data_directory,"untouched/washb-kenya-endline-anthro-public.csv")) 

# child development measures
# at visit 2
dk_chd <- read_csv(file=paste0(Box_data_directory,"untouched/washk_dev_public_20180201.csv"))


# child diarrhea measured at baseline
# and at visits 1 and 2
dk_diar <- read_csv(file=paste0(Box_data_directory,"untouched/washb-kenya-diar-public.csv"))


# child giardia and STH infection
# measured at visit 2
# note: this file is not as well
# documented as other data
# use the replication script as
# documentation: https://osf.io/fpxms/
dk_para <- haven::read_dta(file=paste0(Box_data_directory,"untouched/parasites_kenya_public_ca20230105.dta"))

#----------------------------------
# limit to relevant variables
#----------------------------------
dk_anth2 <- dk_anth %>%
  # drop children who were not in the birth cohort
  filter(targetchild==1) %>%
  dplyr::select(block, clusterid, tr, compoundid, hhid, childid, agey, sex,  laz=haz, waz, whz, hcz)

dk_chd2 <- dk_chd %>%
  # filter to children who had child development measurements
  filter(!is.na(comtotz)) %>%
  dplyr::select(block, clusteridr2,  tr,  hhidr2, childidr2, agey=childage_dev, sex, z_easq_com = comtotz, z_easq_motor = mottotz, z_easq_pers = pstotz, z_easq_total = globaltotz) %>%
  mutate(agey=agey/365.25,
         sex = ifelse(sex==1,"Male","Female")
         )

dk_diar2 <- dk_diar %>%
  # drop baseline
  filter(time > 0) %>%
  dplyr::select(block, clusterid, tr, compoundid, hhid, childid, targetchild, time, agey, sex, diar7d=diarr7) 

dk_para2 <- dk_para %>%
  mutate(tr2 = case_when(
    tr == 1 ~ "Control",
    tr == 2 ~ "Water",
    tr == 3 ~ "Sanitation",
    tr == 4 ~ "Handwashing",
    tr == 5 ~ "WSH",
    tr == 6 ~ "Nutrition",
    tr == 7 ~ "Nutrition + WSH"
  ),
  tr2 = factor(tr2, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
         ) %>%
  dplyr::select(block, clusteridr2, tr=tr2, hhidr2, childidr2, targetchild=target_child, agey=childage_sth, sex, giar = giardia_yn, al = ascaris_yn, tt = trichuris_yn, hw = hook_yn, sth = sth_yn) %>%
  mutate(agey=agey/365.25,
         sex = ifelse(sex==1,"Male","Female")
         )

#----------------------------------
# save analysis files
#----------------------------------
skimr::skim(dk_anth2)
write_rds(dk_anth2,file = paste0(Box_data_directory,"final/kenya_analysis_anthro.rds"))
write_csv(dk_anth2,file = paste0(Box_data_directory,"final/kenya_analysis_anthro.csv"))

skimr::skim(dk_chd2)
write_rds(dk_chd2, file = paste0(Box_data_directory,"final/kenya_analysis_chdev.rds"))
write_csv(dk_chd2, file = paste0(Box_data_directory,"final/kenya_analysis_chdev.csv"))

skimr::skim(dk_diar2)
write_rds(dk_diar2,file = paste0(Box_data_directory,"final/kenya_analysis_diar.rds"))
write_csv(dk_diar2,file = paste0(Box_data_directory,"final/kenya_analysis_diar.csv"))

skimr::skim(dk_para2)
write_rds(dk_para2,file = paste0(Box_data_directory,"final/kenya_analysis_parasite.rds"))
write_csv(dk_para2,file = paste0(Box_data_directory,"final/kenya_analysis_parasite.csv"))

#----------------------------------
# Session info
#----------------------------------
sessionInfo()
