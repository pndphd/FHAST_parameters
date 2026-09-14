################################################################################
# This script
################################################################################

##### Options ##################################################################
input_file = "parameters/fish_cmax_length_and_temp/inputs/fish_cmax_length_and_temp_data.csv"
output_folder = "parameters/fish_cmax_length_and_temp/outputs/"
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = read.csv(file = input_file) 

##### Chinook Analysis #########################################################
chinook_data = data %>% 
  filter(Species == "Chinook") %>% 
  mutate(value = value/max(value)) %>% 
  arrange(temperature)

# Fitting a beta sigmoid
# Yin, X., Goudriaan, J., Lantinga, E.A., Vos, J., and Spiertz, H.J. 2003.
# A flexible sigmoid function of determinate growth. Ann. Bot. 91(3): 361–371.
# doi:10.1093/aob/mcg029.
chinook_fit <- nlsLM(value ~ (1+(A-temperature)/(A-B))*(temperature/A)^(A/(A-B)),
                     data = filter(chinook_data,note == "taken from data"),
                     start = list(A = 20, B = 15))
chinook_A_set = chinook_fit$m$getPars()["A"]
chinook_B_set = chinook_fit$m$getPars()["B"]

chinook_predict <- chinook_data %>%
  mutate(predict = predict(chinook_fit,
                              newdata = .))

plotName = ggplot(chinook_predict, aes(x = temperature)) +
  theme_classic(base_size = 25) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  scale_x_continuous(limits = c(0, 30)) +
  geom_path(aes(y = predict), color = "black", linewidth = 0.5) +
  geom_point(data = chinook_predict %>% 
               filter(note == "taken from data"),
             shape = 1,
             aes(y = value),
             size = 5) 
print(plotName)

ggsave(filename = here(output_folder,"chinook_temp_cmax.png"),
       plot = plotName,
       device = "png")

##### Steelhead Analysis #######################################################
steel_data = data %>% 
  filter(Species == "rainbow trout") %>% 
  mutate(value = value/max(value)) %>% 
  arrange(temperature)

steel_fit <- nlsLM(value ~ (1+(A-temperature)/(A-B))*(temperature/A)^(A/(A-B)),
                     data = steel_data,
                     start = list(A = 22, B = 15))
steel_A_set = steel_fit$m$getPars()["A"]
steel_B_set = steel_fit$m$getPars()["B"]

steel_predict <- data.frame(temperature = seq(0,35,1)) %>%
  mutate(predict = predict(steel_fit,
                           newdata = .))

plotName = ggplot(steel_predict, aes(x = temperature)) +
  theme_classic(base_size = 25) +
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  scale_x_continuous(limits = c(0, 30)) +
  geom_path(aes(y = predict), color = "black", linewidth = 0.5) +
  coord_cartesian(ylim = c(0,1.0))+
  geom_point(data = steel_data,
             aes(y = value, x = temperature),
             shape = 1,
             size = 5) 
print(plotName)

ggsave(filename = here(output_folder,"steelhead_temp_cmax.png"),
       plot = plotName,
       device = "png")

##### Green sturgeon analysis ##################################################
green_sturgeon_data = data %>% 
  filter(Species == "green sturgeon",
         units != "vpm") %>% 
  group_by(group) %>% 
  mutate(count = n(),
         value = ifelse(count > 1, value/max(value), value)) %>% 
  ungroup()

gs_fit <- nlsLM(value ~ 1*(1+(A-(temperature))/(A-B))*((temperature)/A)^(A/(A-B)),
                     data = green_sturgeon_data,
                     start = list(A = 17, B = 12))
gs_A_set = gs_fit$m$getPars()["A"]
gs_B_set = gs_fit$m$getPars()["B"]

gs_predict <- data.frame(temperature = seq(0,35,1)) %>%
  mutate(predict = predict(gs_fit,
                           newdata = .))

plotName = ggplot(green_sturgeon_data, aes(x = temperature)) +
  theme_classic(base_size = 25) +
  theme(legend.title = element_blank())+
  labs(y = "Fraction of Cmax", x = "Temperature (\u00B0C)") +
  scale_x_continuous(limits = c(0, 30)) +
  geom_path(data = gs_predict, aes(y = predict, x = temperature),
            color = "black", linewidth = 0.5) +
  coord_cartesian(ylim = c(0,1.0))+
  geom_point(aes(y = value, color = note),
             shape = 1,
             stroke = 1.5,
             size = 5) +
  scale_color_manual(values = cbPalette,
                     labels = c("CTM", "cmax"))

print(plotName)

ggsave(filename = here(output_folder,"green_sturgeon_temp_cmax.png"),
       plot = plotName,
       height = 5,
       width = 10,
       device = "png")

##### Save a CSV ###############################################################
parameters = data.frame(species = c("Chinook", "Chinook",
                                    "steelhead", "steelhead",
                                    "green sturgeon", "green sturgeon"),
                        parameter = rep(c("A", "B"), 3),
                        value = c(chinook_A_set, chinook_B_set,
                                  steel_A_set, steel_B_set,
                                  gs_A_set, gs_B_set))

################################################################################
# END
################################################################################

