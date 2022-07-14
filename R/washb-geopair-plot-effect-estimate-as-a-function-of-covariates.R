#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - WashB covariates
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots block-level estimates as a function of spatial cov

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("cluster-level-covariates/R", "0-config.R"))

#-------------------------------------------------------------------------------

###################
### Import data ###
###################
# WashB block level covariates
washB_block <- readRDS(file = here("data/final", "washB_block"))

# WashB block level effect estimate
washb_pair_level_summary_estimates <- readRDS(file = here("data/untouched/effect-estimate", "washb_pair_level_summary_estimates.rds"))


#### Merge
data <- (washb_pair_level_summary_estimates %>%
           filter(country == "Bangladesh") %>%
           left_join(washB_block %>% mutate(block = as.character(block)))
         )


#### Plot
data_plot <- (data %>%
                filter(outcome_lab %in% c("length-for-age z", "weight-for-age z", "weight-for-height z")) %>%
                # filter(flood_area_percent < 0.15) %>%
                # ggplot(aes(x = flood_area_percent, y = diff)) +
                # ggplot(aes(x = distance_yearly_hist, y = diff)) +
                ggplot(aes(x = accessibility_to_cities_2015_min, y = diff)) +
                geom_point(aes(col = flood_area_percent > 0)) +
                # geom_point() +
                geom_smooth(method = "lm") +
                facet_wrap(~outcome_lab)
              )

data_plot

####################
#-------------------------------------------------------------------------------

# Save dataset
pdf(here("cluster-level-covariates/output/figures", "washBblock-plot.pdf"))
data_plot
dev.off()


#-------------------------------------------------------------------------------
