################################################################################
# This script calculates the density of benthic food
################################################################################

##### Options ##################################################################
input_folder = "parameters/benthic_food_density_and_energy_density/inputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
# Read in the mass per item data
mass_per_item = read.csv(here(input_folder,
                              "benthic_food_mass_per_item.csv")) %>% 
  select(species, weight_per_ind_g)
digestibility = read.csv(here(input_folder,
                              "benthic_food_digestibility.csv")) %>% 
  select(species, digestibility)
energy_density = read.csv(here(input_folder,
                               "benthic_food_energy_density.csv")) %>% 
  select(species, j_per_g)
makeup = read.csv(here(input_folder,
                       "benthic_food_makeup.csv")) %>% 
  select(fraction, species)
bmi_density = read.csv(here(input_folder,
                            "benthic_food_concentration.csv")) %>% 
  pull(per_m_sq)

data = mass_per_item %>% 
  left_join(digestibility, by = "species") %>% 
  left_join(energy_density, by = "species") %>%
  left_join(makeup, by = "species") %>% 
  summarise(digestibility = weighted.mean(digestibility, fraction),
            j_per_g = weighted.mean(j_per_g, fraction),
            weight_per_ind_g = weighted.mean(weight_per_ind_g, fraction)) %>% 
  mutate(total_food = weight_per_ind_g * bmi_density,
         avaiable_energy = digestibility * j_per_g) 
  
benthic_food_con = data$total_food
benthic_food_energy_density = data$avaiable_energy

message(">>>>> Benthic food density is ", benthic_food_con, " g/m^2")
message(">>>>> Benthic food energy density is ",
        benthic_food_energy_density,
        " J/g")

################################################################################
# END
################################################################################