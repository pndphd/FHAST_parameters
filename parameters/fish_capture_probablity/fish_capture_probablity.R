################################################################################
# This script calculates the capture probablity of drift food vs velocity
################################################################################

##### Options ##################################################################

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = here("parameters",
                            "fish_capture_probablity",
                            "inputs",
                            "fish_capture_probablity.csv")) 

##### Analysis #################################################################
# load the data
captureData = data %>%
  mutate(ratio_V_per_Max = velocity_cm_per_s/maxSwimSpeed_cm_per_s)

# do a logistic fit
captureModel = glm(captureData$prob ~ captureData$ratio_V_per_Max,
                   #family=binomial(logit),
                   family=quasibinomial(logit),
                   data=captureData)

# add in predictions for plotting
captureWFitData = captureData %>%
  mutate(predict = predict.glm(captureModel, type = "response")) %>%
  arrange(ratio_V_per_Max)

# solve for inSALMO Parameters
fishCaptureParam1 = -(log(1/0.1-1)+captureModel[[1]][1])/captureModel[[1]][2]
fishCaptureParam9 = -(log(1/0.9-1)+captureModel[[1]][1])/captureModel[[1]][2]

##### Make a plot ##############################################################
plotName = ggplot(captureWFitData, aes(x = ratio_V_per_Max))+ 
  theme_classic(base_size = 30) +
  labs(y = "Capture Prob.", x = "Velocity/Ucrit") +
  geom_point(aes(y = prob), shape = 1, size = 5, stroke = 1.5) +
  geom_path(aes(y = predict), linewidth = 0.7)
print(plotName)

ggsave(filename = here("parameters",
                       "fish_capture_probablity",
                       "outputs",
                       "fish_capture_probablity.png"),
       plot = plotName,
       device = "png")

################################################################################
# END
################################################################################
