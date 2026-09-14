################################################################################
# This script calculates the energy density of the agent fish
################################################################################

##### Options ##################################################################

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = here("parameters", "fish_energy_density", "inputs", "fish_energy_density.csv")) 

##### Analysis #################################################################
energy_density = data %>% 
  group_by(year, author, journal, species, life_stage, article, units) %>% 
  summarize(value = mean(value)) %>% 
  group_by(species) %>% 
  summarize(value = mean(value)) 

#### Print results #############################################################
walk(seq(1, nrow(energy_density)), function(i){
  message(">>>>> The energy density of ", energy_density$species[i], " is: ",
          energy_density$value[i], " J/g wet mass")
})

################################################################################
# END
################################################################################
