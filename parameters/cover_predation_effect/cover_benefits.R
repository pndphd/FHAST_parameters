################################################################################
# This script makes the logistic equation converts distance to cove to survival
################################################################################

##### Options ##################################################################
cover_data_path = "parameters/cover_predation_effect/inputs/distance_to_cover_data.csv"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

# Function to translate fit parameters to values used in inSALMO and FHAST
get_10_90_params = function(df, names){
  df = as_tibble(df)
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
# Clean the data
clean_data = read_csv(cover_data_path) %>%
  # this section cleans and fits a glm to observed data of fish vs. distance to cover
  group_by(fish_size_mm) %>% 
  mutate(fraction = cumulative_fraction - lag(cumulative_fraction),
         fraction = ifelse(is.na(fraction), cumulative_fraction, fraction),
         unitless_y = fraction/max(fraction, na.rm = T)) %>%
  ungroup() %>%
  select(dis_to_cover_m, unitless_y) %>%
  # nesting allows the glm function to piped onto the dataframe
  nest(data = everything())

# Make the fit
clean_data %>% 
  # fit glm
  mutate(fit = map(.x = data,
                   .f = ~ glm(unitless_y ~ dis_to_cover_m,
                              family = quasibinomial(logit),
                              data = .))) %>%
  pull(fit) %>%
  .[[1]] %>% 
  tidy() %>% 
  mutate(term = str_replace(term, "[(]Intercept[)]", "intercept")) %>% 
  select(term, estimate) %>% 
  pivot_wider(names_from = "term", values_from = "estimate") %>% 
  get_10_90_params(names = "distance to cover") %>% 
  write_csv(here("parameters", "cover_predation_effect", "outputs", "cover_predation_effect.csv"))

# Make the plot
clean_data %>% 
# nesting allows the glm function to piped onto the dataframe
  mutate(fit = map(.x = data,
                   .f = ~glm(unitless_y ~ dis_to_cover_m,
                             family = quasibinomial(logit),
                             data = .)),
         augment = map(fit, augment, type.predict = "response")) %>% 
  unnest(augment)%>%
  ggplot() +
  geom_point(aes(dis_to_cover_m, unitless_y), shape = 1, size = 3, stroke = 0.5) +
  geom_line(aes(dis_to_cover_m, .fitted), color = "black", linewidth = 1, show.legend = FALSE) +
  labs(x = "Distance to Cover (m)", y = "Scaled Prop. of \n Observed Salmonids") +
  theme_classic(base_size = 25)

ggsave(here("parameters", "cover_predation_effect", "outputs", "cover_predation_effect.png"),
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

################################################################################
# END
################################################################################
