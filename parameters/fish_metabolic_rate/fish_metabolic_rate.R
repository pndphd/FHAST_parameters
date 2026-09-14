################################################################################
# This script calculates the metabolic parameters for the fish
# 
# Part 1: The first section of this code loads in the required packages and
# the desired dataset. Data is read from an excel file (respiration_data.xlsx),
# which is divided into sheets for each species. Just paste the directory 
# location of "respiration_data.xlsx" into setwd(). Data is then checked data 
# flags and cleaned if necessary. Specify whether to include swim speed
# and/or tempe in the MR parameter estimation (recommended) using the variables
# "toggle_temp" and "toggle_swimspeed" (TRUE = toggle on = variable included)
################################################################################

##### Options ##################################################################
# Select a species
# OPTIONS:
# "chinook"
# "green_sturgeon" 
# "steelhead"
# "other_sturgeon"
# "all_data"
species = "steelhead"

# Select a lifestage
# OPTIONS:
# "egg"
# "endogenous feeders"
# "exogenous feeders and small juveniles"
# "large juveniles and adults"
# "juvenile"
# "adult"
# "all ages"
life_stage_name = "all ages"

# Select a feeding mode
# OPTIONS:
# "exogenous"
# "endogenous"
feeding_mode = "exogenous"

# Do you want to include swim speed and/or temperature in WI model formula? 
toggle_swimspeed = TRUE
toggle_temp = TRUE

# Select which model will be selected from each dredge analysis for plotting, 
# downstream analysis etc.
# ONLY ONE OF THESE CAN BE SET TO T 
# If you want the simplest model within 2 delta AICc from the top model
# set toggle_simple_mod = T
# If you want to select the top model (default)
# set toggle_top_mod to T
# If you want to manually select the model from the dredge table
# NOTE if you select manually, you will have to specify the row number of the
# model in question later on just ctrl + F for "toggle_manual_model_select" to
# find where this occurs, as the models have to be fit first
# set toggle_manual_model_select = T
toggle_manual_model_select = FALSE
toggle_top_mod = TRUE
toggle_simple_mod = FALSE

# Assign prefix for file outputs. Default is just the species name.
todays_date = species

# Do you want to save output tables and figures? T = yes
save_outputs = TRUE

# Set output/input folder
output_folder = "parameters/fish_metabolic_rate/outputs"
input_folder = "parameters/fish_metabolic_rate/inputs/select"

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load and clean the data ##################################################
# Load data
resp_data = read.csv(here(input_folder,
                          paste0("fish_metabolic_rate_",
                                 species,
                                 ".csv")))

# Remove big fish for which we have no swim speed data: 
# Dredge analysis requires there be no missing values
# Filter if endogenous
if (feeding_mode == "exogenous") {
  resp_data = resp_data %>%
    filter(!is.na(swimspeed_cm_per_s),
           exogenous_or_endogenous == "exogenous")
}
if (feeding_mode  == "endogenous") {
  resp_data = resp_data %>%
    filter(exogenous_or_endogenous == "endogenous")
}


# Filter by life stage
if (life_stage_name == "juvenile") {
  resp_data = resp_data %>% 
    filter(life_stage == "juvenile")
}
if (life_stage_name == "adult") {
  resp_data = resp_data %>% 
    filter(life_stage == "adult")
}

##### Initial Calcvulations ####################################################
# Filter for data quality
# Sub-setting for quality control and for desired feeding mode
# -data flag of 1 = good data
# -data flag of 2 = somewhat questionable or less desirable data,
#                   i.e. fish were cannulated
# -data flag of 3 = don't use for MR parameter calculation! 
#                   Data has effects of an experimental treatment or is suspect.
resp_data = resp_data %>% 
  filter(data_quality == 1,
         life_stage != "egg",
         life_stage != "embryo",
         !is.na(mass_g),
         !is.na(mr_mgo2_per_h)) %>% 
  # Convert swim speed to m/s
  mutate(swimspeed_m_per_s = swimspeed_cm_per_s/100,
         # Covering daily oxygen consumption to joules of energy used per day
         resp_j_per_day = mr_mgo2_per_h * 24 * 19.3 * 22.4 / 32,
         mr_go2_per_day = mr_mgo2_per_h * 24/1000,
         # Concatenate author, year and journal for each study
         study = paste(author, year, journal, sep = ", "),
         # Round mass
         mass_g = round(mass_g, 2),
         # Get specific respiration rate
         resp_j_per_day_per_g = resp_j_per_day / mass_g,
         # Some transforms for fitting
         sqrt_swimspeed_m_per_s = sqrt(swimspeed_m_per_s),
         log_temperature_c = log(temperature_c),
         log_mass_g = log(mass_g))

##### Flag smolts or the like ##################################################
# This chunk adds a new column for metabolic_category_specific, which here is
# the smolting status of Chinook/Steelhead and seawater tolerance for green
# sturgeon. Smolting status is designated based on a likely minimum smolting
# mass for the two salmonids (data from Notch et al. 2020 and Peven et al. 1994
# for steelhead and Chinook salmon, respectively). Green sturgeon have a minimum
# age of seawater tolerance around 134 days post hatch (Allen et al. 2011)
resp_data = resp_data %>% 
  mutate(metabolic_category_specific = case_when(
    # All Species
    (life_stage != "egg" &
       life_stage != "embryo" &
       exogenous_or_endogenous == "endogenous") ~
      "larvae (endg.)",
    (life_stage != "egg" | life_stage != "embryo") ~
      "eggs",
    # Green sturgeon 
    (species == "Acipenser medirostris" &
       age_dph < 134 &
       exogenous_or_endogenous == "exogenous") ~
      "SW intolerant (< 134 dph)",
    (species == "Acipenser medirostris" &
       age_dph >= 134 &
       exogenous_or_endogenous == "exogenous") ~
      "SW tolerant (> 134 dph)",
    # Steelhead
    (species == "Oncorhynchus mykiss" &
       mass_g < 29 &
       exogenous_or_endogenous == "exogenous") ~
      "pre-smolt (< 29g)",
    (species == "Oncorhynchus mykiss" &
       mass_g >= 29 &
       exogenous_or_endogenous == "exogenous") ~
      "pre-smolt (> 29g)",
    (species == "Oncorhynchus mykiss" &
       life_stage == "adult" &
       mass_g >= 29) ~
      "adult",
    # Chinook
    (species == "Oncorhynchus mykiss" &
       mass_g < 7.2 &
       exogenous_or_endogenous == "exogenous") ~
      "pre-smolt (< 7.2g)",
    (species == "Oncorhynchus mykiss" &
       mass_g >= 7.2 &
       exogenous_or_endogenous == "exogenous") ~
      "pre-smolt (> 7.2g)",
    (species == "Oncorhynchus mykiss" &
       life_stage == "adult" &
       mass_g >= 7.2) ~
       "adult"))

# Make all possible data transformations and use model.sel() for sorting by AIC:
transform_mod_global = glm(log(resp_j_per_day) ~ log(mass_g) * temperature_c +
                             swimspeed_m_per_s,
                           data = resp_data,
                           na.action = "na.fail")

transform_mod1 = glm(log(resp_j_per_day) ~ log(mass_g) * log(temperature_c) + 
                       swimspeed_m_per_s,  
                     data = resp_data,
                     na.action = "na.fail")

transform_mod2 = glm(log(resp_j_per_day) ~ log(mass_g) * log(temperature_c) +
                       sqrt(swimspeed_m_per_s) , 
                     data = resp_data,
                     na.action = "na.fail")

transform_mod3 = glm(log(resp_j_per_day) ~ log(mass_g) * temperature_c + 
                       sqrt(swimspeed_m_per_s) , 
                     data = resp_data,
                     na.action = "na.fail")

transform_mod4 = glm(log(resp_j_per_day) ~ log(mass_g) + log(temperature_c) + 
                       sqrt(swimspeed_m_per_s) , 
                     data = resp_data,
                     na.action = "na.fail")

transform_modNULL = glm(log(resp_j_per_day) ~ 1 ,  data = resp_data,
                        na.action = "na.fail")

# Rank transformation models by AIC and save output 
transform_model_list = list(transform_mod_global,
                            transform_mod1, 
                            transform_mod2,
                            transform_mod3,
                            transform_mod4,
                            transform_modNULL)

transform_model_table = model.sel(transform_model_list)
best_transform = get.models(transform_model_table, subset = 1)[[1]]

##### Save the output ##########################################################

write.csv(summary(best_transform)$coefficient,
          file = here(output_folder,
                      paste0(species,
                             ("_metabolic_rate_parameters.csv"))))
  
print(summary(best_transform)$coefficient)
################################################################################
# END
################################################################################
