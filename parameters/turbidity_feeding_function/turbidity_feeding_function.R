################################################################################
# This script fits the turbidity feeding function for drift feeders
################################################################################

##### Options ##################################################################
# Input and output folder
output_folder = "parameters/turbidity_feeding_function/outputs"
input_folder = "parameters/turbidity_feeding_function/inputs"

# Set the max turbidity for analysis
max_turb = 250
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = here(input_folder, "turbidity_feeding_function_data.csv")) 

##### Chinook Analysis #########################################################
# Load the data
chinook_data = data %>%
  # Filter only the ones with Chinook on Plankton
  filter(species == "Chinook",
         prey == "Plankton") %>%
  # subtract off the first non zero turbidity value
  mutate(scaledDistance = reactionDistance_cm/max(reactionDistance_cm))

# get the minimum non zero turbidity that doesn't affect distance, this is the threshold
# it doesn't affect reaction distance if a case with 0 turbidity has worse reaction 
chinook_threshold = min(chinook_data$turbidity_NTU[(which(chinook_data$turbidity_NTU>0))])

# Get the data to fit the fishTurbidExp
chinook_fit_data = chinook_data %>%
  filter(turbidity_NTU>chinook_threshold)

chinook_model = nlsLM(scaledDistance ~ A +
                          (1-A)*exp(B*(turbidity_NTU-chinook_threshold)),
                                 data = chinook_fit_data,
                                 start = list(A = 0.2, B = -0.1))
chinook_min = chinook_model$m$getPars()["A"]
chinook_exp = chinook_model$m$getPars()["B"]

# Made data for a graph
chinook_data_w_fit = data.frame(turbidity_NTU = seq(0,max(chinook_fit_data$turbidity_NTU),1)) %>%
  mutate(predict = ifelse(turbidity_NTU > chinook_threshold,
                          predict(chinook_model,
                                  type = "response",
                                  newdata = .),
                          1)) %>%
  arrange(turbidity_NTU)

# Make a plot
plotName = ggplot(chinook_data, aes(x = turbidity_NTU)) +
  theme_classic(base_size = 30) +
  labs(y = "Turb. Function", x = "Turbidity (NTU)") +
  geom_point(aes(y = scaledDistance), shape = 1, size = 5, stroke = 1.5)+
  geom_path(data = chinook_data_w_fit, aes(y = predict), linewidth = 0.7)+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
print(plotName)

ggsave(filename = here(output_folder,"chinook_turbidity_feeding_function.png"),
       plot = plotName,
       device = "png")

##### Trout Analysis ###########################################################

# Load the data
trout_data = data %>%
  # Filter only the ones with trout on Plankton
  filter(species == "Brook Trout") %>%
  # subtract off the first non zero turbidity value
  mutate(scaledDistance = reactionDistance_cm/max(reactionDistance_cm))

# get the minimum non zero turbidity that doesn't affect distance, this is the threshold
# it doesn't affect reaction distance if a case with 0 turbidity has worse reaction 
trout_threshold = min(trout_data$turbidity_NTU[(which(trout_data$turbidity_NTU>0))])

# Get the data to fit the fishTurbidExp
trout_fit_data = trout_data %>%
  filter(turbidity_NTU>trout_threshold)

trout_model <- nlsLM(scaledDistance ~ A +
                         (1-A)*exp(B*(turbidity_NTU-trout_threshold)),
                       data = trout_fit_data,
                       start = list(A = 0.2, B = -0.1))
trout_min = trout_model$m$getPars()["A"]
trout_exp = trout_model$m$getPars()["B"]

# Made data for a graph
trout_data_w_fit = data.frame(turbidity_NTU = seq(0,max(trout_fit_data$turbidity_NTU),1)) %>%
  mutate(predict = ifelse(turbidity_NTU > trout_threshold,
                          predict(trout_model,
                                  type = "response",
                                  newdata = .),
                          1)) %>%
  arrange(turbidity_NTU)

# Make a plot
plotName = ggplot(trout_data, aes(x = turbidity_NTU)) +
  theme_classic(base_size = 30) +
  labs(y = "Turb. Function", x = "Turbidity (NTU)") +
  geom_point(aes(y = scaledDistance), shape = 1, size = 5, stroke = 1.5)+
  geom_path(data = trout_data_w_fit, aes(y = predict), linewidth = 0.7)
print(plotName)

ggsave(filename = here(output_folder,"trout_turbidity_feeding_function.png"),
       plot = plotName,
       device = "png")

##### Write the data ###########################################################
parameters = data.frame(species = c(rep("Chinook", 3),
                                    rep("steelhead", 3)),
                        parameter = rep(c("threshold", "minimum", "exp"), 2),
                        value = c(chinook_threshold, chinook_min, chinook_exp,
                                  trout_threshold, trout_min, trout_exp))
write.csv(parameters,
          file = here(output_folder,"trout_turbidity_feeding_parameters.csv"),
          row.names = FALSE)
################################################################################
# END
################################################################################