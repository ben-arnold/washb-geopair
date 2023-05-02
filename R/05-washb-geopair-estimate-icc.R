#----------------------------
# Geographic pair matching in large-scale cluster randomized trials
# 
# washb-geopair-estimate-icc.R
#
# Estimate intra-cluster correlation
# for each outcome
# 
# using mixed models with
# random effects for geographically
# matched pair. Include only
# control clusters
#----------------------------

#----------------------------
# Preamble
#----------------------------
library(here)
here()
# source the config file
source(here("R","washb-geopair-Config.R"))

#----------------------------
# Box data directory
#----------------------------
Box_data_directory <- "~/Library/CloudStorage/Box-Box/washb-geopair/data/"

#----------------------------
# Set the seed and 
# the number of bootstrap
# replicates
#----------------------------
set.seed(91742)
nbootreps <- 1000

#-------------------------------
# Bangladesh Estimates
#-------------------------------

#-------------------------------
# load the formatted analysis
# data created by
# washb-geopair-data-processing.R
#-------------------------------

# anthropometry data
danth <- read_rds(here(Box_data_directory,"final/bangl_analysis_anthro.rds")) %>%
  mutate(blockf = factor(block))

# child development data
dchdev <- read_rds(here(Box_data_directory,"final/bangl_analysis_chdev.rds")) %>%
  mutate(blockf = factor(block))

# diarrhea data
ddiar <- read_rds(here(Box_data_directory,"final/bangl_analysis_diar.rds")) %>%
  mutate(blockf = factor(block))

# giardia data
dpara <- read_rds(here(Box_data_directory,"final/bangl_analysis_parasite.rds")) %>%
  mutate(blockf = factor(block))

#-------------------------------
# Estimate outcome ICC within 
# each pair among the 
# control arm observations
#-------------------------------

#-------------------------------
# Block-level ICC
# anthropometry outcomes
#-------------------------------
danth_control <- danth %>%
  filter(tr == "Control")
icc_laz <- rptGaussian(laz ~ 1 + (1|blockf), grname = "blockf", data = danth_control, nboot = nbootreps)
icc_waz <- rptGaussian(waz ~ 1 + (1|blockf), grname = "blockf", data = danth_control, nboot = nbootreps)
icc_whz <- rptGaussian(whz ~ 1 + (1|blockf), grname = "blockf", data = danth_control, nboot = nbootreps)
icc_hcz <- rptGaussian(hcz ~ 1 + (1|blockf), grname = "blockf", data = danth_control, nboot = nbootreps)

#-------------------------------
# Block-level ICC
# Child development outcomes
#-------------------------------
dchdev_control <- dchdev %>%
  filter(tr == "Control")
icc_easqcom <- rptGaussian(z_easq_com ~ 1 + (1|blockf), grname = "blockf", data = dchdev_control, nboot = nbootreps)
icc_easqmot <- rptGaussian(z_easq_motor ~ 1 + (1|blockf), grname = "blockf", data = dchdev_control, nboot = nbootreps)
icc_easqps <- rptGaussian(z_easq_pers ~ 1 + (1|blockf), grname = "blockf", data = dchdev_control, nboot = nbootreps)
icc_cdicomp <- rptGaussian(z_cdi_comp ~ 1 + (1|blockf), grname = "blockf", data = dchdev_control, nboot = nbootreps)
icc_cdiexpr <- rptGaussian(z_cdi_expr ~ 1 + (1|blockf), grname = "blockf", data = dchdev_control, nboot = nbootreps)

#-------------------------------
# Block-level ICC
# Diarrhea
#-------------------------------
ddiar_control <- ddiar %>%
  filter(tr == "Control")
icc_diar <- rptBinary(diar7d ~ 1 + (1|blockf), grname = "blockf", data=ddiar_control, nboot = nbootreps)

#-------------------------------
# Block-level ICC
# Giardia, Ascaris, Trichuris, Hookworm
#-------------------------------
dpara_control <- dpara %>%
  filter(tr == "Control")
icc_giar <- rptBinary(giar ~ 1 + (1|blockf), grname = "blockf", data=dpara_control, nboot = nbootreps)
icc_al   <- rptBinary(al ~ 1 + (1|blockf), grname = "blockf", data=dpara_control, nboot = nbootreps)
icc_tt   <- rptBinary(tt ~ 1 + (1|blockf), grname = "blockf", data=dpara_control, nboot = nbootreps)
icc_hw   <- rptBinary(hw ~ 1 + (1|blockf), grname = "blockf", data=dpara_control, nboot = nbootreps)


#-------------------------------
# summarize the results 
# in a table
#-------------------------------
iccs <- unlist( c(icc_laz$R,icc_waz$R, icc_whz$R, icc_hcz$R, 
                  icc_easqcom$R, icc_easqmot$R, icc_easqps$R,icc_cdicomp$R,icc_cdiexpr$R,
                  icc_diar$R[1,1], icc_giar$R[1,1], icc_al$R[1,1], icc_tt$R[1,1], icc_hw$R[1,1]))
icc_lb <- unlist(c(icc_laz$CI_emp[1],icc_waz$CI_emp[1],icc_whz$CI_emp[1],icc_hcz$CI_emp[1],
                   icc_easqcom$CI_emp[1],icc_easqmot$CI_emp[1],icc_easqps$CI_emp[1],icc_cdicomp$CI_emp[1],icc_cdiexpr$CI_emp[1],
                   icc_diar$CI_emp[[1]][1], icc_giar$CI_emp[[1]][1], icc_al$CI_emp[[1]][1], icc_tt$CI_emp[[1]][1], icc_hw$CI_emp[[1]][1]))
icc_ub <- unlist(c(icc_laz$CI_emp[2],icc_waz$CI_emp[2],icc_whz$CI_emp[2],icc_hcz$CI_emp[2],
                    icc_easqcom$CI_emp[2],icc_easqmot$CI_emp[2],icc_easqps$CI_emp[2],icc_cdicomp$CI_emp[2],icc_cdiexpr$CI_emp[2],
                   icc_diar$CI_emp[[1]][2], icc_giar$CI_emp[[1]][2], icc_al$CI_emp[[1]][2], icc_tt$CI_emp[[1]][2], icc_hw$CI_emp[[1]][2]))
icc_tab <- data.frame(
  outcome = c("length-for-age z","weight-for-age z","weight-for-height z","head circumference z",
              "EASQ communication z","EASQ gross motor z","EASQ personal-social z","CDI comprehension z","CDI expression z",
              "Diarrhea","Giardia sp.","Ascaris sp.","Trichuris sp.","Hookworm"),
  icc = iccs,
  icc_min95 = icc_lb,
  icc_max95 = icc_ub
) %>%
  mutate(icc_print = paste0(sprintf("%1.2f",icc)," (",sprintf("%1.2f",icc_min95),", ",sprintf("%1.2f",icc_max95),")"))



#-------------------------------
# Kenya
#-------------------------------

#-------------------------------
# load the formatted analysis
# data created by
# washb-geopair-data-processing.R
#
# limit to the 72 complete blocks
# used in the present analyses
#-------------------------------

# anthropometry data
dkanth <- read_rds(here(Box_data_directory,"final/kenya_analysis_anthro.rds")) %>%
  mutate(blockf = factor(block)) %>%
  filter(!block %in% c(11,13,20,23,30,33,37,40,49,51,54,56,65,72,80,84,85))

# child development data
dkchdev <- read_rds(here(Box_data_directory,"final/kenya_analysis_chdev.rds")) %>%
  mutate(blockf = factor(block)) %>%
  filter(!block %in% c(11,13,20,23,30,33,37,40,49,51,54,56,65,72,80,84,85))


# diarrhea data
dkdiar <- read_rds(here(Box_data_directory,"final/kenya_analysis_diar.rds")) %>%
  mutate(blockf = factor(block)) %>%
  filter(!block %in% c(11,13,20,23,30,33,37,40,49,51,54,56,65,72,80,84,85))

# giardia data
dkpara <- read_rds(here(Box_data_directory,"final/kenya_analysis_parasite.rds")) %>%
  mutate(blockf = factor(block)) %>%
  filter(!block %in% c(11,13,20,23,30,33,37,40,49,51,54,56,65,72,80,84,85))

#-------------------------------
# Estimate outcome ICC within 
# each pair among the 
# control arm observations
#-------------------------------

#-------------------------------
# Block-level ICC
# anthropometry outcomes
#-------------------------------
dkanth_control <- dkanth %>%
  filter(tr == "Control")
kicc_laz <- rptGaussian(laz ~ 1 + (1|blockf), grname = "blockf", data = dkanth_control, nboot = nbootreps)
kicc_waz <- rptGaussian(waz ~ 1 + (1|blockf), grname = "blockf", data = dkanth_control, nboot = nbootreps)
kicc_whz <- rptGaussian(whz ~ 1 + (1|blockf), grname = "blockf", data = dkanth_control, nboot = nbootreps)
kicc_hcz <- rptGaussian(hcz ~ 1 + (1|blockf), grname = "blockf", data = dkanth_control, nboot = nbootreps)

#-------------------------------
# Block-level ICC
# Child development outcomes
#-------------------------------
dkchdev_control <- dkchdev %>%
  filter(tr == "Control")
kicc_easqcom <- rptGaussian(z_easq_com ~ 1 + (1|blockf), grname = "blockf", data = dkchdev_control, nboot = nbootreps)
kicc_easqmot <- rptGaussian(z_easq_motor ~ 1 + (1|blockf), grname = "blockf", data = dkchdev_control, nboot = nbootreps)
kicc_easqps <- rptGaussian(z_easq_pers ~ 1 + (1|blockf), grname = "blockf", data = dkchdev_control, nboot = nbootreps)


#-------------------------------
# Block-level ICC
# Diarrhea
#-------------------------------
dkdiar_control <- dkdiar %>%
  filter(tr == "Control")
kicc_diar <- rptBinary(diar7d ~ 1 + (1|blockf), grname = "blockf", data=dkdiar_control, nboot = nbootreps)

#-------------------------------
# Block-level ICC
# Giardia, Ascaris, Hookworm
# no analysis for Trichuris in Kenya (too rare)
#-------------------------------
dkpara_control <- dkpara %>%
  filter(tr == "Control")
kicc_giar <- rptBinary(giar ~ 1 + (1|blockf), grname = "blockf", data=dkpara_control, nboot = nbootreps)
kicc_al   <- rptBinary(al ~ 1 + (1|blockf), grname = "blockf", data=dkpara_control, nboot = nbootreps)
kicc_hw   <- rptBinary(hw ~ 1 + (1|blockf), grname = "blockf", data=dkpara_control, nboot = nbootreps)

#-------------------------------
# summarize the results 
# in a table
#-------------------------------
kiccs <- unlist( c(kicc_laz$R,kicc_waz$R, kicc_whz$R, kicc_hcz$R, 
                   kicc_easqcom$R, kicc_easqmot$R, kicc_easqps$R,
                   kicc_diar$R[1,1], kicc_giar$R[1,1], kicc_al$R[1,1], kicc_hw$R[1,1]))
kicc_lb <- unlist(c(kicc_laz$CI_emp[1],kicc_waz$CI_emp[1],kicc_whz$CI_emp[1],kicc_hcz$CI_emp[1],
                    kicc_easqcom$CI_emp[1],kicc_easqmot$CI_emp[1],kicc_easqps$CI_emp[1],
                    kicc_diar$CI_emp[[1]][1], kicc_giar$CI_emp[[1]][1], kicc_al$CI_emp[[1]][1], kicc_hw$CI_emp[[1]][1]))
kicc_ub <- unlist(c(kicc_laz$CI_emp[2],kicc_waz$CI_emp[2],kicc_whz$CI_emp[2],kicc_hcz$CI_emp[2],
                    kicc_easqcom$CI_emp[2],kicc_easqmot$CI_emp[2],kicc_easqps$CI_emp[2],
                    kicc_diar$CI_emp[[1]][2], kicc_giar$CI_emp[[1]][2], kicc_al$CI_emp[[1]][2], kicc_hw$CI_emp[[1]][2]))
kicc_tab <- data.frame(
  outcome = c("length-for-age z","weight-for-age z","weight-for-height z","head circumference z",
              "EASQ communication z","EASQ gross motor z","EASQ personal-social z",
              "Diarrhea","Giardia sp.","Ascaris sp.","Hookworm"),
  icc = kiccs,
  icc_min95 = kicc_lb,
  icc_max95 = kicc_ub
) %>%
  mutate(icc_print = paste0(sprintf("%1.2f",icc)," (",sprintf("%1.2f",icc_min95),", ",sprintf("%1.2f",icc_max95),")"))


#-------------------------------
# Save estimates for re-use
# in tables or figures
#-------------------------------
icc_tab_all <- icc_tab %>%
  mutate(country = "Bangladesh") %>%
  bind_rows(kicc_tab) %>%
  mutate(country = ifelse(is.na(country),"Kenya",country)) %>%
  dplyr::select(country, everything())

write_csv(icc_tab_all, file = here("data","washb-geopair-icc-estimates.csv") )
write_rds(icc_tab_all, file = here("data","washb-geopair-icc-estimates.rds") )


print(icc_tab_all)


# Session Info
sessionInfo()



