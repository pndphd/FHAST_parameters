################################################################################
# This script preps the data for fitting
################################################################################

##### Options ##################################################################
# Input folder
input_folder = "parameters/predator_length/inputs"
output_folder = "parameters/predator_length/outputs"

# Predator length minimum in cm
pred_length_min = 20
################################################################################

##### Load libraries and functions #############################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
# Format the data
dataset = read.csv(here(input_folder, "michel_2018_lenght_data.csv" )) %>%
  mutate(count = round(count*10, 0)) %>%
  uncount(count) %>%
  mutate(species = "bass",
         length_cm = length_mm/10) %>% 
  select(species, length_cm) %>% 
  write.csv(here(output_folder, "michel_2018_length_data.csv"), row.names = FALSE)

################################################################################
# END
################################################################################



