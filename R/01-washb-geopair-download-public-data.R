#----------------------------------
# Geographic pair matching in large-scale cluster randomized trials
#
# washb-geopair-download-public-data.R
#
# Download .rds versions of 
# public datasets from osf.io 
# and save them in the local working repository:
# ~/washb-geopair/data
#
# data are available here: https://osf.io/jmtpz/
#----------------------------------


#----------------------------------
# preamble
#----------------------------------
library(here)

# source configuration file
source(here("R","washb-geopair-Config.R"))

#----------------------------------
# Bangladesh datasets
#----------------------------------

# anthropometry dataset
# https://osf.io/n72y9
bang_anth <- osf_retrieve_file("n72y9") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)

# diarrhea dataset
# https://osf.io/r93qv
bang_diar <- osf_retrieve_file("r93qv") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)

# child development dataset
# https://osf.io/2rje9
bang_chd <- osf_retrieve_file("2rje9") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)

# parasite dataset
# https://osf.io/yu784
bang_para <- osf_retrieve_file("yu784") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)


#----------------------------------
# Kenya datasets
#----------------------------------

# anthropometry dataset
# https://osf.io/hv8ud
kenya_anth <- osf_retrieve_file("hv8ud") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)

# diarrhea dataset
# https://osf.io/wr4ys
kenya_diar <- osf_retrieve_file("wr4ys") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)

# child development dataset
# https://osf.io/qvgsu
kenya_chd <- osf_retrieve_file("qvgsu") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)

# parasite dataset
# https://osf.io/6vrpn
kenya_para <- osf_retrieve_file("6vrpn") %>%
  osf_download(path=here("data"), conflicts = "overwrite", progress = TRUE)


#----------------------------------
# Session info
#----------------------------------
sessionInfo()

