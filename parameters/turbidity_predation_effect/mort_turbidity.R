################################################################################
# This script calculates the survival benifit from turbidity
################################################################################

##### Options ##################################################################
input_file = "parameters/turbidity_predation_effect/inputs/mort_fish_by_mort.csv"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
predLData = read.csv(file = input_file) %>%
  filter(note != "outlier",
         !is.na(turb_NTU)) %>%
  mutate(dailySurvival = NA,
         dailySurvival = ifelse(units == "survival", measure^(1/time_days), dailySurvival),
         dailySurvival = ifelse(units == "daily survival", measure, dailySurvival),
         dailySurvival = ifelse(units == "relative vlun.", 1-measure, dailySurvival),
         dailySurvival = dailySurvival-min(dailySurvival),
         dailySurvival = dailySurvival/max(dailySurvival))

# do a logistic fit
predLModel = glm(predLData$dailySurvival ~ predLData$turb_NTU,
                 family=quasibinomial(logit),
                 data=predLData)

# add in predictions for plotting
predLWFitData = predLData %>%
  mutate(predict = predict.glm(predLModel, type = "response")) %>%
  arrange(turb_NTU)

# solve for 90-10 arameters
# convert form m to cm
mortFishAqPredL1 = -(log(1/0.1-1)+predLModel[[1]][1])/predLModel[[1]][2]
mortFishAqPredL9 = -(log(1/0.9-1)+predLModel[[1]][1])/predLModel[[1]][2]

##### Plot #####################################################################

plotName = ggplot(predLWFitData, aes(x = turb_NTU)) +
  theme_classic(base_size = 30) +
  labs(y = "Daily Survival", x = "Turbidity (NTU)") +
  geom_point(aes(y = dailySurvival),shape = 1, size = 5, stroke = 1.5) +
  geom_path(aes(y = predict), color = "black", linewidth = 0.7)
print(plotName)

ggsave(filename = here("parameters",
                       "turbidity_predation_effect",
                       "outputs",
                       "turbidity_predation_effect.png"),
       plot = plotName,
       device = "png")

##### Print out the results ####################################################
message(">>>>> The 10% and 90% benifit are: ",
        round(mortFishAqPredL1, 2),
        " and ",
        round(mortFishAqPredL9, 2),
        " NTUs")

################################################################################
# END
################################################################################