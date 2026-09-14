################################################################################
# This script makes the logistic relation for haw predator activity increases
# with temperature
################################################################################

##### Options ##################################################################
# Data path
input_path = here("parameters",
                  "predator_temperature_effect",
                  "inputs",
                  "predator_temperature_effect_data.csv")
output_path = here("parameters",
                   "predator_temperature_effect",
                   "outputs")
################################################################################

##### Load libraries and Functions #############################################
source("general_scripts/load_libraries.R")

# Function to translate fit parameters to values used in inSALMO and FHAST
get_10_90_params = function(df, names){
  
  calc_x <- function(a, b, param){
    (log(1/param - 1) + a) / -b
  }
  
  params = c(0.1, 0.9)
  new_col_names <- map_chr(params, ~ glue("{names} {.x * 100}"))
  new_params <- map_dfc(params, ~ calc_x(df[,1] %>% pull(), df[,2] %>% pull(), .x)) %>% 
    setnames(old = c("...1", "...2"), new = new_col_names)
  if(grepl("_", names)) {
    new_params <- new_params %>% 
      rename_with(~str_replace_all(.x, " ", "_"))
  }
  return(new_params)
  
}

##### Main Work ################################################################
# Fit the model
scaled_data = read_csv(input_path) %>%
  # group by various factors so only data from the same experiments are changed
  group_by(author, year, journal, species, experiment) %>% 
  # convert to values relative to the max of each experiment
  mutate(unitless_y = y_value / max(y_value)) %>% 
  ungroup() %>%
  select(temperature_C, unitless_y) 
  
# Do the model fit 
scaled_data %>% 
  # nesting allows the glm function to be piped onto the dataframe
  nest(data = everything()) %>% 
  mutate(fit = map(.x = data,
                   .f = ~ glm(unitless_y ~ temperature_C,
                              family = quasibinomial(logit),
                              data = .)),
         tidy = map(fit, tidy)) %>% 
  unnest(tidy) %>% 
  select(term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>% 
  get_10_90_params("area_pred") %>% 
  write_csv(here(output_path, "predator_temperature_effect_parameters.csv"))

##### Make a plot ##############################################################
scaled_data %>%
  # nesting allows the glm function to piped onto the dataframe
  nest(data = everything()) %>% 
  mutate(fit = map(.x = data,
                   .f = ~ glm(unitless_y ~ temperature_C, family = quasibinomial(logit),
                              data = .)),
         augment = map(fit, augment, type.predict = "response")) %>%
  unnest(augment) %>%
  ggplot() +
  geom_point(aes(temperature_C, unitless_y), shape = 1, size = 3, stroke = 0.5) +
  geom_line(aes(temperature_C, .fitted),
            color = "black",
            linewidth = 1,
            show.legend = FALSE) +
  labs(x = "Temperature (°C)", y = "Relative Activity") +
  theme_classic(base_size = 25)

ggsave(here(output_path, "predator_temperature_effect.png"), 
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

################################################################################
# END
################################################################################
