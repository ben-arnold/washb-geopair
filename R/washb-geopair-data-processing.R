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


# child development data
# at follow-up visit 2
# Extended Ages and Stages Questionnaire (EASQ)
# and MacArthur-Bates Communicative Development Inventory (CDI)
d_easq <- read_csv(here("data","washb-bangladesh-easq-year2.csv"))
d_cdi  <- read_csv(here("data","washb-bangladesh-cdi-year2.csv"))

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
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  select(-svy)
  

d_easq2 <- d_easq %>%
  select(dataid,childid,clusterid,block,tchild,arm, z_easq_com = z_com, z_easq_motor = z_motor, z_easq_pers = z_personal, z_easq_total = z_combined) %>%
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
  select(-arm)
  

d_cdi2 <- d_cdi %>%
  select(dataid,childid,clusterid,block,tchild,arm, z_cdi_comp = z_endline_CDI_understand, z_cdi_expr = z_endline_CDI_say) %>%
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
  select(-arm)
  

d_chd2 <- full_join(d_easq2, d_cdi2, by = c("dataid","childid","clusterid","block","tchild","tr")) %>%
  select(dataid,childid,clusterid,tchild,block,tr,everything())

d_diar2 <- d_diar %>%
  select(dataid,childid,tchild,clusterid,svy,diar7d) %>%
  left_join(d_tr, by = "clusterid") %>%
  filter(svy > 0) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  ) %>%
  select(-svy)
  

d_prot2 <- d_prot %>%
  select(dataid,clusterid,personid,block,tr,giar=posgi, delta_prot) %>%
  # exclude missing values
  filter(delta_prot == "Observed") %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  )

d_sth2 <- d_sth %>%
  select(dataid,clusterid,personid,block,tr,al,tt,hw,sth) %>%
  # exclude missing values
  filter(!is.na(al)) %>%
  mutate(tr = factor(tr, levels = c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  )

# merge giardia and STH datasets
# there were 3 children with giardia measures but no STH measures,
# and 137 children with STH measrues but no giardia measures
d_parasite <- d_sth2 %>%
  full_join(d_prot2,by=c("dataid","clusterid","personid","block","tr"))

#----------------------------------
# save analysis files
#----------------------------------
write_rds(d_anth2,path = here("data","bangl_analysis_anthro.rds"))
write_rds(d_chd2,path = here("data","bangl_analysis_chdev.rds"))
write_rds(d_diar2,path = here("data","bangl_analysis_diar.rds"))
write_rds(d_parasite,path = here("data","bangl_analysis_parasite.rds"))


#----------------------------------
# Kenya data
#----------------------------------

# child anthropometry measured at visit 2
dk_anth <- read_csv(here("data","washb-kenya-endline-anthro-public.csv")) 

# child development measures
# at visit 2
dk_chd <- read_csv(here("data","washk_dev_public_20180201.csv"))


# child diarrhea measured at baseline
# and at visits 1 and 2
dk_diar <- read_csv(here("data","washb-kenya-diar-public.csv"))


# child giardia and STH infection
# measured at visit 2
# note: this file is not as well
# documented as other data
# use the replication script as
# documentation: https://osf.io/fpxms/
dk_para <- haven::read_dta(here("data","parasites_kenya_public_ca20201202.dta"))


#----------------------------------
# filter to the control and 
# nutrition-containing intervention
# arms
# 
# limit to relevant variables
#----------------------------------
dk_anth2 <- dk_anth %>%
  select(clusterid, compoundid, hhid, childid, block, tr, targetchild, laz=haz, waz, whz, hcz, haz_who, waz_who, whz_who)

dk_chd2 <- dk_chd %>%
  select(childidr2, hhidr2, clusteridr2, block, tr, z_easq_com = comtotz, z_easq_motor = mottotz, z_easq_pers = pstotz, z_easq_total = globaltotz)


dk_diar2 <- dk_diar %>%
  filter(time > 0) %>%
  select(clusterid, compoundid, hhid, childid, block, tr, targetchild, diar7d=diarr7) %>%
  mutate(childidr2 = (childid+3252)*10,
         clusteridr2 = (clusterid+3252)*10)


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
  select(childidr2, hhidr2, clusteridr2, block,tr=tr2, giar = giardia_yn, al = ascaris_yn, tt = trichuris_yn, hw = hook_yn, sth = sth_yn)

#----------------------------------
# save analysis files
#----------------------------------
write_rds(dk_anth2,path = here("data","kenya_analysis_anthro.rds"))
write_rds(dk_chd2,path = here("data","kenya_analysis_chdev.rds"))
write_rds(dk_diar2,path = here("data","kenya_analysis_diar.rds"))
write_rds(dk_para2,path = here("data","kenya_analysis_parasite.rds"))

