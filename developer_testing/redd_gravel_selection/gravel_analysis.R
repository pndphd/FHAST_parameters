################################################################################
# This script
################################################################################

##### Options ##################################################################

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################

################################################################################
# END
################################################################################
# Using: Parsley MJ, Beckman LG, McCabe GT. 1993. Spawning and Rearing 
# Habitat Use by White Sturgeons in the Columbia River Downstream from
# McNary Dam. Transactions of the American Fisheries Society 122:217–227.
grain_sizes = list(fines = 2, gravel = 64, cobble = 250)

##### Load Files #####
data = read.csv(file = here("parameters", "redd_gravel_data.csv")) 

redd_data = data %>% 
  filter(lifestage == "redd",
         concern == "selection",
         measure == "D50") 

redd_plot = ggplot(data = redd_data, aes(x = value, fill = species)) +
  theme_classic()+
  geom_histogram(position = "dodge", binwidth = 10)+
  geom_vline(xintercept = grain_sizes$gravel, color = "orange")
print(redd_plot)

sturgeon_spawn =  data %>% 
  filter(lifestage == "eggs/larvae",
         concern == "selection",
         species == "green sturgeon" | species == "white sturgeon") 

spawn_plot = ggplot(data = sturgeon_spawn, aes(x = value, fill = measure)) +
  theme_classic()+
  geom_histogram(position = "dodge", binwidth = 10)
  # geom_vline(xintercept = grain_sizes$gravel, color = "orange")
print(spawn_plot)

