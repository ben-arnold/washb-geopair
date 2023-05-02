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
  dplyr::select(dataid,childid,tchild,clusterid,svy,agey,waz,laz,whz,hcz) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy==2) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  dplyr::select(-svy)
  

d_easq2 <- d_easq %>%
  dplyr::select(dataid,childid,clusterid,block,tchild,arm, z_easq_com = z_com, z_easq_motor = z_motor, z_easq_pers = z_personal, z_easq_total = z_combined) %>%
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
  dplyr::select(dataid,childid,clusterid,block,tchild, agey=ageyears, arm, z_cdi_comp = z_endline_CDI_understand, z_cdi_expr = z_endline_CDI_say) %>%
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
  

d_chd2 <- full_join(d_easq2, d_cdi2, by = c("dataid","childid","clusterid","block","tchild","tr")) %>%
  dplyr::select(dataid,childid,clusterid,tchild,block,tr,everything())

d_diar2 <- d_diar %>%
  dplyr::select(dataid,childid,tchild,clusterid,agey=ageyrs,svy,diar7d) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy > 0) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  dplyr::select(-svy)
  

d_prot2 <- d_prot %>%
  dplyr::select(dataid,clusterid,personid,block,tr,giar=posgi, delta_prot) %>%
  # exclude missing values
  filter(delta_prot == "Observed") %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  )

d_sth2 <- d_sth %>%
  dplyr::select(dataid,clusterid,personid,block,tr,agey,al,tt,hw,sth) %>%
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
  full_join(d_prot2,by=c("dataid","clusterid","personid","block","tr"))

#----------------------------------
# save analysis files
#----------------------------------
write_rds(d_anth2,file = paste0(Box_data_directory,"final/bangl_analysis_anthro.rds"))
write_csv(d_anth2,file = paste0(Box_data_directory,"final/bangl_analysis_anthro.csv"))

write_rds(d_chd2, file = paste0(Box_data_directory,"final/bangl_analysis_chdev.rds"))
write_csv(d_chd2, file = paste0(Box_data_directory,"final/bangl_analysis_chdev.csv"))

write_rds(d_diar2,file = paste0(Box_data_directory,"final/bangl_analysis_diar.rds"))
write_csv(d_diar2,file = paste0(Box_data_directory,"final/bangl_analysis_diar.csv"))

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
dk_para <- haven::read_dta(file=paste0(Box_data_directory,"untouched/parasites_kenya_public_ca20201202.dta"))

#----------------------------------
# filter to the control and 
# nutrition-containing intervention
# arms
# 
# limit to relevant variables
#----------------------------------
dk_anth2 <- dk_anth %>%
  # drop children who were not in the birth cohort
  filter(targetchild==1) %>%
  dplyr::select(clusterid, compoundid, hhid, childid, block, tr, targetchild, agey, laz=haz, waz, whz, hcz, haz_who, waz_who, whz_who)

dk_chd2 <- dk_chd %>%
  dplyr::select(childidr2, hhidr2, clusteridr2, block, tr, childage_dev, z_easq_com = comtotz, z_easq_motor = mottotz, z_easq_pers = pstotz, z_easq_total = globaltotz) %>%
  mutate(agey=childage_dev/365.25) %>%
  dplyr::select(-childage_dev)

dk_diar2 <- dk_diar %>%
  # drop baseline
  filter(time > 0) %>%
  dplyr::select(clusterid, compoundid, hhid, childid, block, tr, targetchild, agey, diar7d=diarr7) 

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
  dplyr::select(childidr2, hhidr2, clusteridr2, block,tr=tr2, giar = giardia_yn, al = ascaris_yn, tt = trichuris_yn, hw = hook_yn, sth = sth_yn)

#----------------------------------
# save analysis files
#----------------------------------
write_rds(dk_anth2,file = paste0(Box_data_directory,"final/kenya_analysis_anthro.rds"))
write_csv(dk_anth2,file = paste0(Box_data_directory,"final/kenya_analysis_anthro.csv"))

write_rds(dk_chd2, file = paste0(Box_data_directory,"final/kenya_analysis_chdev.rds"))
write_csv(dk_chd2, file = paste0(Box_data_directory,"final/kenya_analysis_chdev.csv"))

write_rds(dk_diar2,file = paste0(Box_data_directory,"final/kenya_analysis_diar.rds"))
write_csv(dk_diar2,file = paste0(Box_data_directory,"final/kenya_analysis_diar.csv"))

write_rds(dk_para2,file = paste0(Box_data_directory,"final/kenya_analysis_parasite.rds"))
write_csv(dk_para2,file = paste0(Box_data_directory,"final/kenya_analysis_parasite.csv"))

#----------------------------------
# Session info
#----------------------------------
sessionInfo()
