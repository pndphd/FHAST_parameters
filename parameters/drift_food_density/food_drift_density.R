################################################################################
# This script Calculate density of drift food
################################################################################

##### Options ##################################################################
input_folder = "parameters/drift_food_density/inputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
# Read in the mass per item data
mass_per_item = read.csv(here(input_folder, "drift_food_mass_per_item.csv")) %>%
  select(number_per_m3, dryBiomass_g_per_m3) %>%
  mutate(massConv = dryBiomass_g_per_m3/number_per_m3) %>%
  summarize(massConv = mean(massConv)) %>%
  .$massConv

# Read in the wet to dry biomass conversion
wet_to_dry = read.csv(here(input_folder, "drift_food_wet_to_dry_bm.csv")) %>%
  select(ratio) %>%
  summarize(ratio = mean(ratio)) %>%
  .$ratio

# Now calculate drift in g/m^3
drift_food_con = read.csv(here(input_folder, "drift_food_concentration.csv")) %>%
  select(number_per_m3) %>%
  # Calculate density and convert from m^3 
  mutate(wetBiomass = number_per_m3*mass_per_item/wet_to_dry) %>%
  summarise(wetBiomass = mean(wetBiomass, na.rm = TRUE)) %>%
  .$wetBiomass

message(">>>>> Drift food density is ", drift_food_con, " g/m^3")

################################################################################
# END
################################################################################
