# This script is to test energy balance

##### Environmental Variables #####
temperatures = 12 # C
depths = 0.5 # m
cover = 0 # Y/N
turbidities = seq(0, 20, 10) # NTU
food_energies = 1878  # j/g
food_densities = seq(0.0, 1, 0.01) # g/m^3 
velocities = seq(0.0, 1, 0.01) # m/s
lengths = seq(4, 8, 2) # cm
cover_fraction = 0.488
photoperiod = 13.6
adds = seq(0, 1, 0.01)

##### Load Functions and Libraries ######
library(here)
source(here("general_scripts", "load_libraries.R"))
source(here("testing", "energy_balance",
            "energy_balance_testing_functions.R"))

##### Load Comparison data #####
growth_lit_data = readRDS(here("testing",
                               "energy_balance",
                               "inputs",
                               "dudley_et_al_2022_lit_growth_data.RDS")) %>% 
  mutate(rate_g_days = (end_mass - start_mass)/days)
compare_data = density(growth_lit_data$rate_g_days, n = 2^12)

###### Parameters #####
fish_params = read.csv(here("testing",
                            "energy_balance",
                            "inputs",
                            "fish_params_test.csv")) %>% 
  rename(species_temp = species) %>% 
  pivot_longer(cols=c(-species_temp), names_to="specie")%>%
  pivot_wider(names_from=c(species_temp)) %>% 
  as.list()

##### Functions #####
calculate_energy = function(i, p){


data = expand.grid(velocity = velocities,
                   add = adds,
                   length = lengths,
                   temp = temperatures,
                   depth = depths,
                   cover = cover,
                   turb = turbidities,
                   food_energy = food_energies,
                   food_density = food_densities) %>% 
  mutate(species = p$specie[i],
         # Calculate fish mass form length
         fish_mass = p$length_mass_a[i] * length^p$length_mass_b[i],
         # Calculate the max temperature function for Cmax
         cmax_temp_value = calc_beta_sig(parm_A = p$cmax_c[i],
                                         parm_B = p$cmax_d[i],
                                         temp = temp),
         # Calculate Cmax
         cmax = p$cmax_a[i] * fish_mass^(1 + p$cmax_b[i]) *  cmax_temp_value,
         # Calculate the max temperature function for Ucrit
         max_swim_speed_temp = calc_beta_sig(parm_A = p$ucrit_c[i],
                                             parm_B = p$ucrit_d[i],
                                             temp = temp),
         # Calculate the max swim speed in BL/s and then convert to m/s
         max_swim_speed = (p$ucrit_a[i] / length + p$ucrit_b[i]) *
           max_swim_speed_temp * length/100,
         add_vel = ifelse(velocity< max_swim_speed, add * (max_swim_speed - velocity), 0),
         # Calculate the turbidity function
         turbidity_fun = ifelse(turb <= p$turbid_threshold[i], 1,
                                p$turbid_min[i] + (1 - p$turbid_mi[i]) *
                                  exp(p$turbid_exp[i] *
                                        (turb - p$turbid_threshold[i]))),
         # Account for cover
         swim_speed = ifelse(cover == 1, (velocity + add_vel) * cover_fraction, (velocity + add_vel)),
         # Calculate detection distance
         detection_dist = (p$react_dist_a[i] +
                             p$react_dist_b[i] * length) * turbidity_fun,
         fish_met_log_active = p$met_int[i] +
           p$met_lm[i] * log(fish_mass) +
           p$met_lt[i] * log(temp) +
           # velocity is in body lengths/sec
           p$met_v[i] * swim_speed +
           p$met_lm_lt[i] * log(fish_mass) * log(temp) +
           p$met_t[i] * temp +
           p$met_lm_t[i] * log(fish_mass) * temp +
           p$met_sqv[i] * sqrt(swim_speed),
         fish_met_log_passive = p$met_int[i] +
           p$met_lm[i] * log(fish_mass) +
           p$met_lt[i] * log(temp) +
           # velocity is in body lengths/sec
           p$met_v[i] * 0 +
           p$met_lm_lt[i] * log(fish_mass) * log(temp) +
           p$met_t[i] * temp +
           p$met_lm_t[i] * log(fish_mass) * temp +
           p$met_sqv[i] * sqrt(0),
         fish_met_j_per_day_active = exp(fish_met_log_active),
         fish_met_j_per_day_pass = exp(fish_met_log_passive),
         fish_met_j_per_day = (fish_met_j_per_day_active* photoperiod + fish_met_j_per_day_pass*(24-photoperiod))/24,
         # Put in NA's if the fish is above the critical swimming speed
         fish_met_j_per_day_ucrit = ifelse((velocity + add_vel) > max_swim_speed, NA, fish_met_j_per_day),
         capture_area = 2 * detection_dist * pmin(depth, detection_dist),
         capture_success = calc_logistic(parm_10 = p$capture_V1[i],
                                         parm_90 = p$capture_V9[i],
                                         value = (velocity + add_vel)/max_swim_speed),
         drift_eaten = capture_success * capture_area * food_density * (velocity + add_vel) * 86400 * photoperiod/24,
         drift_intake = pmin(drift_eaten, cmax),
         energy_intake = drift_intake * food_energy,
         net_energy = energy_intake - fish_met_j_per_day,
         daily_growth = net_energy / p$energy_density[i],
         percent_growth = daily_growth/fish_mass * 100)

}

# Format the data for plotting
fish_data = map_df(.x = seq(1,length(fish_params[[1]]),1),
                   ~calculate_energy(.x, fish_params)) %>% 
  filter(species == "chinook") %>% 
  group_by(velocity, length, turb, food_density) %>%
  filter(daily_growth == max(daily_growth, na.rm = T)) %>% 
  ungroup() 

# Raster plot of growth rate
energy_plot = ggplot(data = fish_data,
                     aes(x = food_density,
                         y = velocity,
                         z = daily_growth,
                         fill = daily_growth)) +
  theme_classic(base_size = 20) +
  theme(legend.key.width=unit(2, "cm"),
        strip.background = element_rect(color = "white",fill = "white", size = 0)) +
  geom_raster() +
  scale_fill_viridis(limits = c(-0.4, 0.5)) +
  facet_grid(length~turb) +
  labs(x = expression("Food Density" ~ (g/m^3)),
       y = "Velocity (m/s)",
       fill = "Growth\n(g/day)") +
  scale_y_continuous(sec.axis = sec_axis(~ . ,
                                         name = "Fish Length (cm)",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(sec.axis = sec_axis(~ . ,
                                         name = "Turbidity (NTU)",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = c(0.0, 0.5, 1.0))

print(energy_plot)

# Density plot of growth data from lit
density_plot = ggplot(data = data.frame(x = compare_data$x, y = compare_data$y),
                      aes(x = compare_data$x,
                          y = compare_data$y)) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dotted", size = 0.5),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.5)) +
  geom_segment(aes(xend = x, yend = 0, color = x)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Growth (g/day)") +
  scale_color_viridis_c(limits = c(-0.4, 0.5),
                        guide = NULL)

# pva_histogram_plot = ggplot(data = survival_raw,
#                             aes(x = survival)) +
#   theme_classic(base_size = parms$plot_font_size) +
#   theme(legend.position = c(0.8, 0.5),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank()) +
#   geom_density(size = 1, color = cbPalette[1], fill = cbPalette[1], alpha = 0.5) +
#   scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


print(density_plot)

combined_plot = energy_plot +
  density_plot +
  plot_layout(ncol = 1,
              guides = "collect",
              heights = c(2,1)) + 
  plot_annotation(tag_levels = "A") & 
  theme(legend.position = "bottom") 
  

print(combined_plot)
ggsave(here("testing", "energy_balance", "outputs", "alpha_test_energy.png"),
       combined_plot,
       height = 10,
       width = 8)


# met_plot = ggplot(data = fish_data %>%
#                     filter(species == 'steelhead'),
#                   aes(x = velocity,
#                       fill = fish_met_j_per_day/1000,
#                       z = fish_met_j_per_day/1000,
#                       y = length)) +
#   theme_classic()+
#   geom_raster()

# met_plot = ggplot(data = fish_data %>% 
#                     filter(species != "green_sturgeon"), 
#                   aes(x = velocity,
#                       y = percent_growth, 
#                       color = factor(species))) +
#   theme_classic(base_size = 20)+
#   scale_color_manual(values = cbPalette, name = "Species") +
#   coord_cartesian(ylim = c(-2,12), xlim = c(0, 1)) +
#   labs(x = "Velcoity (m/s)", y = "Percent Growth") +
#   geom_line(size = 2)



# print(met_plot)

# ggsave(here("outputs", "alpha_test_met_v_vel.png"), met_plot)
