Basic################################################################################
# This script fits condition factors to mortality risk
################################################################################

##### Options ##################################################################
input_file_path = "./parameters/fish_condition_risk/inputs/fish_condition_risk_data.csv"
species = "chinook"
output_folder = "./parameters/fish_condition_risk/outputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = input_file_path) 

##### Analysis #################################################################
conSurvivalDataChinook = data %>%
  na.omit()%>% 
  filter(species == "Chinook")

conSurvivalDataGS = data %>%
  filter(species == "Siberian sturgeon")

# do a logistic fit
conSurvivalModelChinook = glm(dailySurvival ~ conFactor,
                              family=quasibinomial(logit),
                              data=conSurvivalDataChinook)

conSurvivalModelGS = glm(dailySurvival ~ conFactor,
                         family=quasibinomial(logit),
                         data=conSurvivalDataGS)

# add in predictions for plotting
conSurvivalDataWFitDataChinook = data.frame(dailySurvival = 1, conFactor = seq(0.7, 1, 0.01)) %>%
  mutate(predict = predict.glm(object = conSurvivalModelChinook, newdata = ., type = "response")) %>%
  arrange(conFactor)

# add in predictions for plotting
conSurvivalDataWFitDataGS = data.frame(dailySurvival = 1, conFactor = seq(0.7, 1, 0.01)) %>%
  mutate(predict = predict.glm(object = conSurvivalModelGS, newdata = ., type = "response")) %>%
  arrange(conFactor)

# solve for inSALMO Parameters
mortFishCondition1C = -(log(1/0.1-1)+conSurvivalModelChinook[[1]][1])/conSurvivalModelChinook[[1]][2]
mortFishCondition9C = -(log(1/0.9-1)+conSurvivalModelChinook[[1]][1])/conSurvivalModelChinook[[1]][2]

mortFishCondition1GS = -(log(1/0.1-1)+conSurvivalModelGS[[1]][1])/conSurvivalModelGS[[1]][2]
mortFishCondition9GS = -(log(1/0.9-1)+conSurvivalModelGS[[1]][1])/conSurvivalModelGS[[1]][2]

##### Plot green sturgeon ######################################################
plotName = ggplot(conSurvivalDataGS, aes(x = conFactor)) +
  theme_classic(base_size = 25) +
  theme(legend.title = element_blank())+
  labs(y = "Survival", x = "Condition Factor") +
  geom_path(data = conSurvivalDataWFitDataGS, aes(y = predict, x = conFactor),
            color = "black", linewidth = 0.5) +
  coord_cartesian(ylim = c(0.97,1.0), xlim = c(0.7,1))+
  geom_point(aes(y = dailySurvival),
             size = 5, shape = 1 , stroke = 1.5) 

print(plotName)

ggsave(filename = here(output_folder,"gs_mort_con.png"),
       plot = plotName,
       device = "png")

##### Plot chinook #############################################################
plotName = ggplot(conSurvivalDataChinook, aes(x = conFactor)) +
  theme_classic(base_size = 25) +
  theme(legend.title = element_blank())+
  labs(y = "Survival", x = "Condition Factor") +
  geom_path(data = conSurvivalDataWFitDataChinook, aes(y = predict, x = conFactor),
            color = "black", linewidth = 0.5) +
  coord_cartesian(ylim = c(.97,1.0), xlim = c(0.7,1))+
  geom_point(aes(y = dailySurvival),
             size = 5, shape = 1 , stroke = 1.5) 

print(plotName)

ggsave(filename = here(output_folder,"chinook_mort_con.png"),
       plot = plotName,
       device = "png")

##### Print outputs ############################################################
##### Print oputputs ###########################################################
message(">>>>> The 10 and 90 values for Chinook are: ",
        mortFishCondition1C,
        " and ",
        mortFishCondition9C)
message(">>>>> The 10 and 90 values for green sturgeon are: ",
        mortFishCondition1GS,
        " and ",
        mortFishCondition9GS)

write.csv(data.frame(parameter = c(10, 90),
                     value_c = c(mortFishCondition1C, mortFishCondition9C)),
          file = here(output_folder, paste0(species, "Chinook_risk_parameters.csv")),
          row.names = FALSE)

write.csv(data.frame(parameter = c(10, 90),
                     value_c = c(mortFishCondition1GS, mortFishCondition9GS)),
          file = here(output_folder, paste0(species, "green_sturgeon_risk_parameters.csv")),
          row.names = FALSE)

################################################################################
# END
################################################################################
