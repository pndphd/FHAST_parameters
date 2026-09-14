################################################################################
# This script fits the temperature risk values for salmonids
################################################################################

##### Options ##################################################################
input_file_path = "./parameters/fish_temperature_risk/inputs/steelhead_temp_risk_data.csv"
species = "steelhead"
output_folder = "./parameters/fish_temperature_risk/outputs"

# Minimum temp fish should be acclimated at 
min_acclimation_temperature = 10
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(input_file_path) %>% 
  filter(life_stage == "juvenile",
         acclimation_temp_C > min_acclimation_temperature) %>% 
  select(target_temp_degrees_C,
         daily_survival_prob) %>% 
  rename(temperature = target_temp_degrees_C,
         survival = daily_survival_prob) %>% 
  na.omit() 

# do a logistic fit
SurvivalModel = glm(survival ~ temperature,
                    family = quasibinomial(logit),
                    data = data)

# add in predictions for plotting
conSurvivalDataWFitData = data.frame(survival = 1,
                                     temperature = seq(min(data$temperature),
                                                       max(data$temperature),
                                                       length.out = 100)) %>%
  mutate(predict = predict.glm(object = SurvivalModel, newdata = ., type = "response")) %>%
  arrange(temperature)


# solve for FHAST Parameters
mortFishCondition1C = -(log(1/0.1-1)+SurvivalModel[[1]][1])/SurvivalModel[[1]][2]
mortFishCondition9C = -(log(1/0.9-1)+SurvivalModel[[1]][1])/SurvivalModel[[1]][2]

##### Plot #####################################################################
plotName = ggplot(data, aes(x = temperature)) +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank())+
  labs(y = "Survival Probablity", x = expression("Temperature ("*~degree*C*")")) +
  geom_path(data = conSurvivalDataWFitData, aes(y = predict, x = temperature),
            color = "black", linewidth = 0.5) +
  geom_point(aes(y = survival),
             size = 3, shape = 1, alpha = 0.5, stroke = 1) 

print(plotName)

ggsave(filename = here(output_folder, paste0(species, "_risk_plot.png")),
       height = 4,
       width = 6,
       plot = plotName,
       device = "png")

##### Print oputputs ###########################################################
message(">>>>> The 10 and 90 values are: ",
        mortFishCondition1C,
        " and ",
        mortFishCondition9C,
        "degrees C")

write.csv(data.frame(parameter = c(10, 90),
                     value_c = c(mortFishCondition1C, mortFishCondition9C)),
          file = here(output_folder, paste0(species, "_risk_parameters.csv")),
          row.names = FALSE)
                      
                    
################################################################################
# END
################################################################################
