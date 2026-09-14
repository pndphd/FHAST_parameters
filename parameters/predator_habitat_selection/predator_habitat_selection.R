################################################################################
# Predator distribution based on PA data
################################################################################

##### Libraries ################################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
pred_data = read.csv(file = here("parameters",
                                 "predator_habitat_selection",
                                 "inputs",
                                 "predator_pa_data.csv")) 

##### Functions ################################################################
# Update the cover names 
update_cover_names <- function(var) {
  dplyr::case_when(
    {{ var }} == "VERY LOW" ~ "S",
    {{ var }} == "LOW" ~ "M",
    {{ var }} == "MEDIUM" ~ "M",
    {{ var }} == "HIGH" ~ "H",
    {{ var }} == "VERY HIGH" ~ "H",
    {{ var }} == "HEAVY" ~ "H",
    {{ var }} == "NOW" ~ "S",
    {{ var }} == "HIGH" ~ "H",
    {{ var }} == "D" ~ "H",
    {{ var }} == "NONE" ~ "A",
    TRUE ~ {{ var }})
}

# Change catagorical cover to numeric values
convert_cover_to_val <- function(var) {
  dplyr::case_when(
    {{ var }} == "A" ~ 0,
    {{ var }} == "S" ~ 0.055,
    {{ var }} == "M" ~ 0.3,
    TRUE ~ 0.6,)
}

##### Clean Data ###############################################################
pred_data = pred_data %>%
  # for more consistent naming
  rename_all(~ tolower(.)) %>% 
  rename_all(~ str_replace_all(., "\\s+", "_")) %>% 
  # make data in cover columns consistent and convert to proportions
  mutate(across(emergvegdensity:emergwmdensity,toupper)) %>%
  mutate(across(emergvegdensity:emergwmdensity, .fns = ~ update_cover_names(.x))) %>% 
  filter(!emergwmdensity == "1") %>% 
  # convert cover to values
  mutate(across(emergvegdensity:emergwmdensity,.fns = ~ convert_cover_to_val(.x))) %>% 
  # update shade
  mutate(shade = toupper(shade),
         shade = case_when(
           shade == "ABSENT" ~ "A",
           shade == "PRESENT" ~ "P",
           shade == "C" ~ "P",
           TRUE ~ shade),
         shade = if_else(shade == "P", 1, 0)) %>% 
  # add mean depth and velocity
  mutate(mean_depth = rowMeans(across(depth5:depth15)),
         mean_vel = rowMeans(across(vel5:vel15))) %>% 
  # drop distance-dependant depth and velocity
  select(-c(depth5:depth15, vel5:vel15)) %>% 
  # update substrate
  mutate(substrate = substrate10,
         substrate = case_when(
           substrate == "G" ~ "R",
           substrate == "R/G" ~ "R",
           substrate == "R/M" ~ "R",
           substrate == "M/R" ~ "R",
           substrate == "M/G" ~ "M",
           substrate == "M/V" ~ "M",
           TRUE ~ substrate),
         substrate = dplyr::if_else(substrate == "R", 1, 0)) %>%
  select(-c(substrate5:substrate15)) %>% 
  # set pred counts to presence/absence
  mutate(across(bass:sasq, .fns = ~ dplyr::if_else(.x > 0, 1, 0))) %>% 
  # create predator specific data sets
  pivot_longer(cols = c(bass, sasq), names_to = "species", values_to = "count") %>% 
  # set character data types as factors
  mutate_if(is.character, factor) %>% 
  # drop any NA's
  drop_na() %>% 
  # rename cols to be more compatible with our modeling terms
  rename(veg = emergvegdensity, # veg_cover
         wood = emergwmdensity, # wood_cover
         depth_ft = mean_depth,
         velocity_fps = mean_vel) %>% 
  # convert to metric
  mutate(across(depth_ft:velocity_fps, .fns = ~ .x / 3.28)) %>%
  rename(velocity = velocity_fps,
         depth = depth_ft) %>% 
  # rename sasq to pikeminnow for clarity
  mutate(species = if_else(species == "sasq", "pikeminnow", as.character(species))) %>% 
  # set shade and substrate as factor variables
  mutate(across(c(shade, substrate),
                as.factor))

##### Fit the GLM ##############################################################
# up sample to get same number of P/A
pred_data_max = pred_data %>%
  group_by(count) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == max(n))

pred_data_sampeled = pred_data %>%
  group_by(count) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == min(n)) %>%
  sample_n(NROW(pred_data_max), replace = TRUE) %>%
  bind_rows(pred_data_max) %>% 
  select(-n)


# Fit the data
species_names = data.frame(species = unique(pred_data_sampeled$species))
model_fit = map(species_names$species,
                   ~glm(count ~
                          depth +
                          velocity +
                          veg +
                          wood +
                          substrate +
                          shade,
                        family="binomial",
                        data=filter(pred_data_sampeled, species == .x)))

# Make the tabular output
table_output = map2_df(model_fit, species_names$species, ~data.frame(Parameter = names(.x$coefficients),
                                             Estimate = round(.x$coefficients, 3),
                                             species = .y) %>% 
                        mutate(Parameter = str_replace(Parameter,"_", " "))) %>% 
  mutate(Parameter = case_when(
    Parameter == "(Intercept)" ~ "int",
    Parameter == "shade1" ~ "shade",
    Parameter == "substrate1" ~ "substrate",
    .default = Parameter),
    model_type = "glm") 

# Write the table
write.csv(table_output, here("parameters",
                            "predator_habitat_selection",
                            "outputs",
                            "predator_distribution_parameters.csv"),
          row.names = FALSE)

##### Plots ####################################################################
# Basic plot of data
plot = ggplot(data = table_output,
              aes(x = Parameter,
                  y = Estimate,
                  color = model_type)) +
  theme_classic(base_size = 20) +
  geom_point(size = 3, shape = 1) +
facet_grid(rows = vars(species))
print(plot)

# Plot of reaction curves
variables = c("depth", "velocity", "veg", "wood", "substrate", "shade")
max_values = c(10, 4, 1.0, 1.0, 1.0, 1.0)
zero_data = data.frame(depth = 0,
                       velocity = 0,
                       veg = 0,
                       wood = 0,
                       substrate = 0,
                       shade = 0)

reaction_x_data = map2_df(variables, max_values, ~zero_data %>%
                            select(-all_of(.x)) %>%
                            bind_cols(data.frame(test = seq(0, .y, length.out = 100))) %>% 
                            rename(!!sym(.x) := test) %>%
                            mutate(shade = as.factor(round(shade, 0)),
                                   substrate = as.factor(round(substrate,0)),
                                   varaible = .x))
                            
reaction_data = map2_df(model_fit, species_names$species, ~reaction_x_data %>%
  mutate(predict = predict(.x, newdata = .) - .x$coefficients["(Intercept)"],
         predict = predict/max(abs(predict)),
         species = .y)) %>% 
  distinct()

reaction_plots = map(variables, ~ggplot(data = reaction_data %>% filter(varaible == .x),
                                      aes(x = !!sym(.x), y = predict, color = species, group = species)) +
                      theme_classic(base_size = 15) +
                      labs(y = "effect") +
                      theme(legend.position = "top") +
                      scale_color_manual(values = cbPalette, name = "Species") +
                      geom_path() +
                      scale_y_continuous(limits = c(-1,1)))

reaction_plot = wrap_plots(reaction_plots, guides = "collect", axes = "collect") &
  theme(legend.position = 'top')

ggsave(filename = here("parameters",
                       "predator_habitat_selection",
                       "outputs",
                       "predator_distribution_curves.png"),
       plot = reaction_plot,
       device = "png",
       height = 4,
       width = 8)
  
print(reaction_plot)

################################################################################
# END
################################################################################