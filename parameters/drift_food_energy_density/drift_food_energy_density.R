################################################################################
# This script calculates energy density of drift food
################################################################################

##### Options ##################################################################
input_folder = "parameters/drift_food_energy_density/inputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
# load the data and do calculations to get the energy density of food
drift_food_energy_density = read.csv(here(input_folder, "drift_food_energy_density.csv")) %>%
  select(year, author, scaledWeightOfSample, digestability, energyDensity_j_per_gWetMass) %>%
  # calculate the scaled weight of each digeatability measure
  mutate(digestableMatterPer = scaledWeightOfSample * digestability,
         wetMassPer = scaledWeightOfSample * energyDensity_j_per_gWetMass) %>%
  group_by(year, author) %>%
  # get the totals for each study
  summarize(digestableMatter = sum(digestableMatterPer),
            wetMass = sum(wetMassPer),
            weight = sum(scaledWeightOfSample)) %>%
  ungroup() %>%
  # divide by the weights to get the averages from each study
  mutate(digestableMatterSacled = digestableMatter/weight,
         wetMassScaled = wetMass/weight) %>%
  # get averages of all studies
  summarise(digestableMatterTot = mean(digestableMatterSacled),
            wetMassTot = mean(wetMassScaled)) %>%
  # Convert to usuable energy
  mutate(energyDensity = digestableMatterTot * wetMassTot) %>%
  .$energyDensity

message(">>>>> The energy density of drift food is: ", drift_food_energy_density, " J/g wet mass")

################################################################################
# END
################################################################################