################################################################################
# This script calculates the reaction distance of drift feeders
################################################################################

##### Options ##################################################################

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
data = read.csv(file = here("parameters",
                            "fish_reaction_distance",
                            "inputs",
                            "fish_reaction_distance.csv")) 

##### Analysis #################################################################
# Load in the fecundity data
detectionData = data
# Fit a lm 
detectionModel = lm(reaction_distance_m ~ fLength_cm,
                    data=detectionData)

# Calculate the inslamo parameters
fishDetectDistA = detectionModel$coefficients[1]
fishDetectDistB = detectionModel$coefficients[2]

# Made data for a graph
detectionDataWFit = data.frame(fLength_cm = seq(0, max(detectionData$fLength_cm),0.1)) %>%
  mutate(predict = predict(detectionModel,
                           type = "response",
                           newdata = .)) %>%
  arrange(fLength_cm)

##### Make a plot ##############################################################
plotName = ggplot(detectionDataWFit, aes(x = fLength_cm)) +
  theme_classic(base_size = 30) +
  theme(legend.position = c(0.8,0.2)) +
  labs(y = "Reaction Dist. (m)", x = "Length (cm)") +
  geom_jitter(data = detectionData,
              aes(y = reaction_distance_m, color = species),
              shape = 1,
              size = 5,
              stroke = 0.8,
              width = 1)+
  geom_path(aes(y = predict), linewidth = 0.7) +
  scale_color_manual(values = cbPalette, name = "Species") +
  scale_y_continuous(limits = c(0,NA))
print(plotName)


ggsave(filename = here("parameters",
                       "fish_reaction_distance",
                       "outputs",
                       "fish_reaction_distance.png"),
       plot = plotName,
       device = "png",
       height = 8,
       width = 8)

##### Write data ###############################################################
fishDetectDistA = detectionModel$coefficients[1]
fishDetectDistB = detectionModel$coefficients[2]

output = data.frame(parameter = c("A", "B"),
                    values = c(fishDetectDistA, fishDetectDistB))

write.csv(output,
          file = here("parameters",
                       "fish_reaction_distance",
                       "outputs",
                       "fish_reaction_distance.csv"),
          row.names = FALSE)

################################################################################
# END
################################################################################





