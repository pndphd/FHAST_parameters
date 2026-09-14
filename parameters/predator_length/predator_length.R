################################################################################
# This script fits a log normal distribution to the length data of predators
################################################################################

##### Options ##################################################################
input_folder = "parameters/predator_length/inputs"
output_folder = "parameters/predator_length/outputs"

# Processed files are in output folder
input_file = "fishbio_length_data.csv"
# input_file = "michel_2018_length_data.csv"
################################################################################

##### Load libraries and functions #############################################
source("general_scripts/load_libraries.R")

source(file = here("parameters", "predator_length", "predator_length_functions.R"))

##### Main Work ################################################################
# Fit the data and find the parameters
pred_dists = read.csv(here(output_folder, input_file)) %>% 
  group_nest(species) %>%
  # fit exponential distributions to the length data per species
  mutate(fit = map(data, ~ fitdist(.$length_cm, distr = "lnorm")), 
         # select the parameter from the distributions
         meanlog = map_dbl(fit, ~ get_dist_param(., param = 1)), 
         # select the parameter from the distributions 
    sdlog = map_dbl(fit, ~ get_dist_param(., param = 2))) %>% 
  select(-c(data, fit)) %>% 
  pivot_longer(cols = meanlog:sdlog, 
               names_to = "term", 
               values_to = "estimate")

write.csv(pred_dists,
          here(output_folder, "predator_length_params.csv"),
          row.names = FALSE)

################################################################################
# END
################################################################################



