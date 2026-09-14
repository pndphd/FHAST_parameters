################################################################################
# This script
################################################################################

##### Options ##################################################################
# Where is the simulation data
data_path = "parameters/distance_to_cover_vs_area/outputs/cover_simulation_data.csv"

# What cell size is used in the simulation (in meters)
cell_size_m = 1
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main work ################################################################
# Make a model
dis_to_cover_model = read_csv(data_path) %>%
  # Mean_dis_w_0 is just one of the two simulations run
  # It includes all values with 0 dis to cover
  select(pct_cover, mean_dis_w_0) %>% 
  # Adjust cell dimensions
  mutate(dis_to_cover_m = mean_dis_w_0 * cell_size_m) %>% 
  # Use the nesting trick to run a model in pipline
  nest(data = dplyr::everything()) %>% 
  # Fit a polynomial: x^0.5 + x + x^1.5
  mutate(fit = map(.x = data,
                   .f = ~lm(dis_to_cover_m ~ sqrt(pct_cover) * pct_cover,
                            data = .))) %>%
  pull(fit) %>%
  .[[1]] %>%
  tidy() %>%
  mutate(term = str_replace(term, "[(]Intercept[)]", "intercept"),
         term = str_replace(term, "sqrt[(]pct_cover[)]", "sqrt_pct_cover"),
         term = str_replace(term, ":pct_cover", "^3")) %>%
  select(term, estimate) %>% 
  readr::write_csv(here::here("parameters",
                              "distance_to_cover_vs_area",
                              "outputs",
                              "distance_to_cover_vs_area_model.csv"))

##### Make a plot ##############################################################
# Make a plot
plot_name = ggplot(data = read_csv(data_path),
                   aes(x = pct_cover,
                       y = mean_dis_w_0)) +
  theme_classic(base_size = 25) +
  theme(legend.title = element_blank())+
  labs(y = "Fraction Cover", x = "Fraction Distance to Cover") +
  geom_point( size = 2, shape = 1, alpha = 0.25, stroke = 0.1) +
  geom_smooth(method = "lm",
              formula = y ~ sqrt(x) * x,
              color = "red",
              linewidth = 1)
print(plot_name)

# Save the plot
ggsave(filename = here("parameters",
                      "distance_to_cover_vs_area",
                      "outputs",
                      "distance_to_cover_vs_area.png"),
       plot = plot_name,
       device = "png")
################################################################################
# END
################################################################################
