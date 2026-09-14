################################################################################
# This script preps fish bio data for fitting
################################################################################

##### Options ##################################################################
# Input folder
input_folder = "parameters/predator_length/inputs"
output_folder = "parameters/predator_length/outputs"

# Predators of interest
poi = c("Bass",
        "Pikeminnow",
        "Sacramento Pikeminnow",
        "Spotted Bass",
        "Largemouth Bass",
        "Smallmouth Bass")

# Predator length minimum in cm
pred_length_min = 20
################################################################################

##### Load libraries and functions #############################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
# Make a list of parameters
data_list = as.list(here(input_folder, list.files(input_folder, pattern = "sac_length_*")))
name_codes = read.csv(here(input_folder, "species_code_list.csv"))

# Format the data
pred_dists = map_dfr(data_list, ~ read.csv(.x) %>%
                       mutate(across(starts_with("TL"), ~.x/10, .names = "length_cm")) %>% 
                       select(Species, length_cm)) %>% 
  left_join(name_codes, by = c("Species" = "code")) %>% 
  mutate(Species = ifelse(is.na(species), Species, species)) %>% 
  select(-species) %>%
  filter(Species %in% poi,
         length_cm > pred_length_min) %>% 
  mutate(Species = ifelse(str_detect(Species, "Bass"), "bass", "pikeminnow")) %>% 
  drop_na() %>% 
  rename_all(.funs = tolower) %>% 
  write.csv(here(output_folder, "fishbio_length_data.csv"), row.names = FALSE)

################################################################################
# END
################################################################################



