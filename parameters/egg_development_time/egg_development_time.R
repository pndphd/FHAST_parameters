################################################################################
# Fit models of development time to data to compare emergence time
################################################################################

##### Inputs ###################################################################
# Parameters from Beacham and Murray 1990
a_beacham = 33000
b_beacham = -2.04
c_beacham = -7.58

# Parameters from Martin et al. 2016 
a_zueg_martin = 0.001044
b_zueg_martin = 0.00056

# Parameters from Zeug et al. 2012 (converted for f)
a_zueg = 0.001044
b_zueg = 0.00056

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Make the data ############################################################
development_df = data.frame(temperature = seq(5,25, 0.1)) %>% 
  mutate(dev_beacham = (1/(a_beacham*(temperature-c_beacham)^b_beacham))^-1,
         dev_zueg = (a_zueg * temperature + b_zueg)^-1,
         dev_zueg_martin = (a_zueg_martin * temperature + b_zueg_martin)^-1) %>% 
  pivot_longer(cols = c(starts_with("dev_")),
               names_to = "model",
               values_to = "days")

##### Plot #####################################################################
ggplot(data = development_df,
       aes(x = temperature,
           y = days,
           color = model)) +
  theme_classic(base_size = 16) +
  geom_path(size = 3)
  

