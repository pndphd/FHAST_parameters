################################################################################
# This script calculates the territory size 
################################################################################

##### Options ##################################################################
input_folder = "parameters/fish_territory_size/inputs"
output_folder = "parameters/fish_territory_size/outputs"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = here(input_folder, "fish_territory_size.csv")) 

##### Functions ################################################################

##### Analysis #################################################################
# Read in already calculated parameters
territory_size = data %>%
  select(FL_cm, territory_square_m, n) %>%
  na.omit() %>% 
  mutate(ln_fl = log(FL_cm),
         ln_ter = log(territory_square_m))  
  
lm = lm(ln_ter ~ ln_fl, data = territory_size)

plot_data = data.frame(FL_cm = seq(min(territory_size$FL_cm),
                                   max(territory_size$FL_cm),
                                   lenght.out = 100)) %>% 
  mutate(territory_square_m = exp(lm$coefficients[[1]]) * FL_cm^lm$coefficients[[2]])

##### Plot chinook #############################################################
plotName = ggplot(territory_size,
                  aes(x = FL_cm,
                      y = territory_square_m)) +
  theme_classic(base_size = 25) +
  theme(legend.title = element_blank())+
  labs(y = expression("Territory (m"^2*")"), x = "Fork Length (cm)") +
  geom_point(size = 4, shape = 1 , stroke = 1, alpha = 0.4)+
  geom_path(data = plot_data, aes(x = FL_cm, y = territory_square_m),
          color = "black", linewidth = 0.5) 

print(plotName)

ggsave(filename = here(output_folder,"fish_territory_size.png"),
       plot = plotName,
       device = "png",
       height = 5,
       width = 6)

##### Print outputs ############################################################
message(">>>>> The intercept and slope values are: ",
        exp(lm$coefficients[[1]]),
        " and ",
        lm$coefficients[[2]])

write.csv(data.frame(parameter = c("intercept", "slope"),
                     value_c = c( exp(lm$coefficients[[1]]), lm$coefficients[[2]])),
          file = here(output_folder, "fish_territory_parameters.csv"),
          row.names = FALSE)

################################################################################
# END
################################################################################

