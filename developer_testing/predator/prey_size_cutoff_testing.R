################################################################################
# This script
################################################################################

##### Options ##################################################################
# Size limits of predators (cm)
predator_size_limits = c(5, 100)

# the log normal distributions parameters
size_dist_parms_michel = c(3.35, 0.26)
size_dist_parms_fishbio = c(3.18, 0.28)

# Gape equations
# Large gape has the form y = exp(A+B*x)
large_gape_parms = c(1.31 , 0.0372)
# Small gape has the form y = exp(A+B*x^2)
small_gape_parms = c(2.34, 2.48E-4)

# Prey sized
prey_sizes = c(16.4, 16.9, 15.2, 15.3, 15.8, 21.7, 22.4, 22.8, 19.6, 20.4, 5.75, 5.68, 13.7)

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

log_normal_pdf = function(mu, sig, x){
  return(1/(x*sig*sqrt(2*pi))*exp(-(log(x)-mu)^2/(2*sig^2)))
}

##### Main Work ################################################################
data = data.frame(length_cm = seq(5, 100, length.out = 100)) %>% 
  mutate(michael_count = log_normal_pdf(mu = size_dist_parms_michel[1],
                                        sig = size_dist_parms_michel[2],
                                        x = length_cm),
         fishbio_count = log_normal_pdf(mu = size_dist_parms_fishbio[1],
                                        sig = size_dist_parms_fishbio[2],
                                        x = length_cm),
         michael_count_filter = ifelse(length_cm < 20, 0, michael_count),
         fishbio_count_filter = ifelse(length_cm < 20, 0, fishbio_count),
         michael_per = michael_count/sum(michael_count)*100,
         fishbio_per = fishbio_count/sum(fishbio_count)*100,
         michael_per_filter = michael_count_filter/sum(michael_count_filter)*100,
         fishbio_per_filter = fishbio_count_filter/sum(fishbio_count_filter)*100,
         michael_per_cume = 100 - cumsum(michael_per_filter),
         fishbio_per_cume = 100 - cumsum(fishbio_per_filter),
         large_gape = exp(large_gape_parms[1] + large_gape_parms[2] * length_cm),
         small_gape = exp(small_gape_parms[1] + small_gape_parms[2] * length_cm^2)) %>% 
  select(-michael_count, -fishbio_count)  


##### Plots ####################################################################
basic_plot = ggplot(data = data %>%
                pivot_longer(cols = -length_cm),
              aes(x = length_cm, y = value, color = name)) +
  theme_classic(base_size = 15) +
  geom_line() 
print(basic_plot)

percent_plot = ggplot(data = data %>% 
                        select(small_gape, michael_per_cume, fishbio_per_cume) %>% 
                        pivot_longer(cols = -small_gape),
                      aes(x = small_gape, y = value, color = name)) +
  theme_classic(base_size = 15) +
  theme(legend.position = "top") +
  geom_line() +
  scale_color_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  geom_vline( xintercept = prey_sizes) +
  coord_cartesian(xlim = c(10,25))
print(percent_plot)
  
filter_plot = ggplot(data = data %>% 
                        select(small_gape, michael_per_filter, fishbio_per_filter) %>% 
                        pivot_longer(cols = -small_gape),
                      aes(x = small_gape, y = value, color = name)) +
  theme_classic(base_size = 15) +
  theme(legend.position = "top") +
  geom_line() +
  scale_color_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  geom_vline( xintercept = prey_sizes) +
  coord_cartesian(xlim = c(10,25))
print(filter_plot)
         

################################################################################
# END
################################################################################
