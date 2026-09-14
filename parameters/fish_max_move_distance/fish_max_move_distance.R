################################################################################
# This script fits the max move distance of a fish
################################################################################

##### Options ##################################################################
input_folder = "parameters/fish_max_move_distance/inputs"
output_folder = "parameters/fish_max_move_distance/outputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = here(input_folder, "fish_max_move_distance.csv")) 


##### Analysis #################################################################
# Format data
moveData = data %>%
  filter(!is.na(year)) %>%
  mutate(ln_size = log(size_cm),
         ln_distance = log(distance_m))

# Make a linear model
moveModel = lm(moveData$ln_distance ~ moveData$ln_size,
               data=moveData)

# Calculate the parameters
fishMoveDistParamA = exp(moveModel$coefficients[1])
fishMoveDistParamB = moveModel$coefficients[2]


##### Plot #####################################################################
# Make ploting data
plotMoveData = data.frame(length = c(seq(0.5,100,0.5))) %>%
  mutate(linearModel = fishMoveDistParamA*length^fishMoveDistParamB)

# make the plot
plotName = ggplot(plotMoveData, aes(x = length)) +
  # scale_y_log10() +
  theme_classic(base_size = 25) +
  theme(legend.title = element_blank())+
  labs(y = "Max Distance (m)", x = "Length (cm)") +
  geom_point(data = moveData, aes(x = size_cm, y = distance_m),
             size = 5, shape = 1, stroke = 1.5) +
  geom_path(aes(y = linearModel),linewidth = 0.5)
print(plotName)

print(plotName)

ggsave(filename = here(output_folder,"fish_max_move_distance.png"),
       plot = plotName,
       device = "png")

##### Print parameters #########################################################
message(">>>>> The intercept and slope values are: ",
        fishMoveDistParamA,
        " and ",
        fishMoveDistParamB)

write.csv(data.frame(parameter = c("intercept", "slope"),
                     value_c = c(fishMoveDistParamA, fishMoveDistParamB)),
          file = here(output_folder, "fish_max_move_distance.csv"),
          row.names = FALSE)

################################################################################
# END
################################################################################