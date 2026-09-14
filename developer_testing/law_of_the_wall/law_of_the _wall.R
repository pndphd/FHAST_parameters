################################################################################
# This script calculates the velocity benifit form being close to the bottom
# this code produces no new parameters
################################################################################

##### Options ##################################################################
# from Wikepidia
# https://en.wikipedia.org/wiki/Law_of_the_wall
# sheer velocity between 5 and 10% of free stream velocity
fraction = 0.07
# The von Kármán constant
kappa = 0.41
# the average diameter of the 84th largest percentile of the grains of the bed material
D84 = 0.05
#output folder
output_folder = "parameters/law_of_the_wall/outputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
data = data.frame(expand.grid(height = seq(0.01, .2, .01),
                              free_velocity = seq(1,5,0.5))) %>% 
  # Green sturgeon are ~ 20cm tall so maybe use 10 cm
  mutate(velocity = fraction*free_velocity/kappa*log((height/D84)*(30/3.5)))

plot = ggplot(data = data, aes(y = height,
                               x = velocity,
                               color = free_velocity,
                               group = free_velocity)) +
  theme_classic(base_size = 20) +
  geom_path(linewidth = 1) +
  scale_color_viridis_c(name = "Free Velocity\n(m/s)")+
  labs(y = "Height (m)", x = "Velocity (m/s)")

print(plot)

ggsave(here(output_folder,
            "law_of_the_wall.png"),
            device = "png")

################################################################################
# END
################################################################################


