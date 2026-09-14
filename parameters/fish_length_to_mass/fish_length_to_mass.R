################################################################################
# This script calculates the relation between lenght and mass of fish
################################################################################

##### Options ##################################################################
input_folder = "parameters/fish_length_to_mass/inputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data_raw = read.csv(file = here(input_folder, "fish_length_to_mass_raw.csv")) 
data_par = read.csv(file = here(input_folder, "fish_length_to_mass_parms.csv")) 

##### Functions #################################################################
get_parameters = function(df){
  fishRawWtData = df %>%
    mutate(ln_length = log(length_cm),
           ln_weight = log(weight_g)) %>%
    group_by(species) %>%
    do(weightFit = tidy(lm(ln_weight ~ ln_length, data = .))) %>%
    unnest(weightFit) %>%
    select(term, estimate, species) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    rename(intercept = "(Intercept)",
           slope = ln_length) 
}

##### Analysis #################################################################
# Turn the raw data in to aprameters to average
fishRawWtData = get_parameters(data_raw)

# Read in already calculated parameters
fish_lenght_to_weight = data_par %>%
  select(intercept, slope, species) %>%
  # transform intercept
  mutate(intercept = log(intercept)) %>% 
  bind_rows(fishRawWtData) %>%
  group_by(species) %>% 
  summarise_all(mean) %>% 
  mutate(intercept = exp(intercept))

##### Plots ####################################################################
#Function to make plots
make_plots = function(this_species, df){

  df_filtered = df %>%
    filter(species == this_species)

  params = fish_lenght_to_weight %>% 
    filter(species == this_species)

  plot_data = data.frame(length_cm = seq(from = min(df_filtered$length_cm),
                                         to = max(df_filtered$length_cm),
                                         length.out = 100)) %>%
    mutate(weight_g = (params$intercept)*length_cm^(params$slope))

  plot_name = ggplot(df_filtered) +
    theme_classic(base_size = 25) +
    theme(legend.title = element_blank())+
    labs(y = "Mass (g)", x = "Length (cm)") +
    geom_path(data = plot_data, aes(y = weight_g, x = length_cm),
              color = "black", linewidth = 0.5) +
    geom_point(aes(y = weight_g, x = length_cm), size = 5, shape =1, alpha = 0.5)

  print(plot_name)
}

walk(.x = unique(data_raw$species), ~make_plots(.x, data_raw))

##### Print Outputs ############################################################
walk(seq(1, nrow(fish_lenght_to_weight)), function(i){
  message(">>>>> The parameters of ", fish_lenght_to_weight$species[i], " are: ",
          fish_lenght_to_weight$intercept[i], " intercept and ", fish_lenght_to_weight$slope[i], " slope.")
})
################################################################################
# END
################################################################################

