################################################################################
# How much does cover affect experienced velocity
################################################################################

##### Options ##################################################################
input_folder = "parameters/velocity_shelter_effect/inputs"
output_folder = "parameters/velocity_shelter_effect/outputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = here(input_folder, "velocity_shelter_effect.csv")) 

##### Main Work ################################################################
# Calculate the shelter factor provided by cover
velocity_shelter = data %>% 
  group_by(author) %>% 
  summarize(reduction = mean(reduction)) %>% 
  ungroup() %>% 
  summarize(reduction = mean(reduction)) %>% 
  .$reduction
 
# Calculate the velocity experienced
velocity_exp = 1 - velocity_shelter

##### Print oputputs ###########################################################
message(">>>>> The fraction of velocity experienced is: ",
        velocity_exp)

write.csv(data.frame(parameter = "franction velocity experienced",
                           value_c = velocity_exp),
                file = here(output_folder, "velocity_shelter_effect.csv"),
                row.names = FALSE)

################################################################################
# END
################################################################################