################################################################################
# This script
################################################################################

##### Options ##################################################################
input_folder = "parameters/fish_ucrit/inputs"
output_folder = "parameters/fish_ucrit/outputs"

fish_species = c("salmonid", "sturgeon")
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data = map(fish_species,
           ~read.csv(file = here(input_folder, paste0(.x, "_ucrit.csv")))) %>% 
  setNames(fish_species)

##### Salmonid Temperature Analysis ############################################
chinook_temp_data = data$salmonid %>% 
  filter(data_quality == 1) %>% 
  group_by(species, author) %>% 
  mutate(count = n(),
         # need more then 2 for this to work
         value = ifelse(count > 2, 
                    swimspeed_BL/max(swimspeed_BL, na.rm = TRUE),
                    swimspeed_BL)) %>% 
  ungroup() %>% 
  arrange(temperature_C)

# Fitting a beta sigmoid
# Yin, X., Goudriaan, J., Lantinga, E.A., Vos, J., and Spiertz, H.J. 2003.
# A flexible sigmoid function of determinate growth. Ann. Bot. 91(3): 361–371.
# doi:10.1093/aob/mcg029.
chinook_fit = nlsLM(value ~ (1+(A-temperature_C)/(A-B))*(temperature_C/A)^(A/(A-B)),
                     data = chinook_temp_data,
                     start = list(A = 20, B = 15))
chinook_C_set = chinook_fit$m$getPars()["A"]
chinook_D_set = chinook_fit$m$getPars()["B"]

chinook_predict = data.frame(temperature_C = seq(0,35,1)) %>%
  mutate(predict = predict(chinook_fit,
                              newdata = .)) %>% 
  ungroup() %>% 
  arrange(temperature_C)

plotName = ggplot(chinook_predict,
                  aes(x = temperature_C)) +
  theme_classic(base_size = 25) +
  labs(y = "Fraction of Ucrit", x = "Temperature (\u00B0C)", color = "Source", shape = "Species") +
  scale_x_continuous(limits = c(0, 30)) +
  geom_point(data = chinook_temp_data,
             aes(y = value,
                 shape = common,
                 color = source),
             size = 5) +
  geom_line(aes(y = predict), color = "black", linewidth = 0.5) +
  scale_shape_manual(values = c(1, 2)) +
  coord_cartesian(ylim = c(0,1.1), xlim = c(8, 26)) +
  scale_color_manual(values = cbPalette)
print(plotName)

ggsave(filename = here(output_folder,"chinook_temp_ucrit.png"),
       plot = plotName,
       device = "png",
       height = 8,
       width = 10)

##### Green sturgeon temperature analysis ##################################################
green_sturgeon_data = data$sturgeon %>% 
  filter(author == "Rodgers" |
           author == "CTM") %>% 
  group_by(species, author) %>% 
  mutate(count = n(),
         value = ifelse(count > 1, 
                        swimspeed_BL/max(swimspeed_BL, na.rm = TRUE),
                        swimspeed_BL)) %>% 
  ungroup() %>% 
  arrange(temperature_C)

gs_fit <- nlsLM(value ~ 1*(1+(A-(temperature_C))/(A-B))*((temperature_C)/A)^(A/(A-B)),
                     data = green_sturgeon_data,
                     start = list(A = 17, B = 12))
gs_C_set = gs_fit$m$getPars()["A"]
gs_D_set = gs_fit$m$getPars()["B"]

gs_predict <- data.frame(temperature_C = seq(0,35,1)) %>%
  mutate(predict = predict(gs_fit,
                           newdata = .))

plotName = ggplot(green_sturgeon_data, aes(x = temperature_C)) +
  theme_classic(base_size = 25) +
  labs(y = "Fraction of Ucrit", x = "Temperature (\u00B0C)", color = "Source") +
  scale_x_continuous(limits = c(0, 30)) +
  coord_cartesian(ylim = c(0,1.0))+
  geom_point(aes(y = value, color = source),
             shape = 1,
             stroke = 1.5,
             size = 5) +
  geom_path(data = gs_predict, aes(y = predict, x = temperature_C),
            color = "black", linewidth = 0.5) +
  scale_color_manual(values = cbPalette,
                     labels = c("CTM", "cmax"))

print(plotName)

ggsave(filename = here(output_folder,"green_sturgeon_temp_ucrit.png"),
       plot = plotName,
       height = 5,
       width = 8,
       device = "png")

##### Salmonid length analysis ############################################
chinook_length_data = data$salmonid %>%
  filter(data_quality == 1) %>% 
  # Just use fish close to optimum T
  filter(temperature_C >= 17,
         temperature_C <= 20)
  # filter(temperature_C >= 11,
  #        temperature_C <= 22)

chinook_fit = nlsLM(swimspeed_BL ~ (A/TL_cm + B),
                    data = chinook_length_data,
                    start = list(A = 30, B = 1))
chinook_A_set = chinook_fit$m$getPars()["A"]
chinook_B_set = chinook_fit$m$getPars()["B"]

chinook_predict = data.frame(TL_cm = seq(0,40,1)) %>%
  mutate(predict = predict(chinook_fit,
                           newdata = .))

plotName = ggplot(chinook_length_data, aes(x = TL_cm)) +
  theme_classic(base_size = 25) +
  labs(y = "Ucrit (BL/s)", x = "Total Length (cm)", color = "Source") +
  geom_point(aes(y = swimspeed_BL, color = source, shape = common),
             stroke = .8,
             size = 5) +
  geom_path(data = chinook_predict, aes(y = predict, x = TL_cm),
            color = "black", linewidth = 0.5) +
  scale_color_manual(values = cbPalette)+
  scale_shape_manual(values = c(1, 2)) +
  coord_cartesian(ylim = c(2,9), xlim = c(0,35))

print(plotName)

ggsave(filename = here(output_folder,"chinook_length_ucrit.png"),
       plot = plotName,
       device = "png",
       height = 8,
       width = 10)

##### Green sturgeon length analysis ##################################################
green_sturgeon_data = data$sturgeon %>% 
  group_by(author) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  # Just use sturgeon close to optimum T
  filter(!(temperature_C < 15 & Sturgeon == "Green"),
         !(temperature_C >= 21),
         # just use large lake sturgeon
         !(Sturgeon == "Lake" & TL_cm < 80),
         count > 1) 

gs_fit <- nlsLM(swimspeed_BL ~ A/TL_cm + B,
                data = green_sturgeon_data,
                start = list(A = 30, B = 0.5),
                #weight by inverse count to prevent single studu domination 
                weight = 1/count)
gs_A_set = gs_fit$m$getPars()["A"]
gs_B_set = gs_fit$m$getPars()["B"]

gs_predict <- data.frame(TL_cm = seq(0,125,1)) %>%
  mutate(predict = predict(gs_fit,
                           newdata = .))

plotName = ggplot(green_sturgeon_data, aes(x = TL_cm)) +
  theme_classic(base_size = 25) +
  labs(y = "Ucrit (BL/s)", x = "Total Length (cm)", color = "Source") +
  coord_cartesian(ylim = c(0,9.0))+
  geom_point(aes(y = swimspeed_BL, color = source, shape = Sturgeon),
             stroke = .8,
             size = 5) +
  geom_path(data = gs_predict, aes(y = predict, x = TL_cm),
            color = "black", linewidth = 0.5) +
  scale_color_manual(values = cbPalette)+
  scale_shape_manual(values = c(1, 2))

print(plotName)

ggsave(filename = here(output_folder,"green_sturgeon_length_ucrit.png"),
       plot = plotName,
       height = 6,
       width = 9,
       device = "png")

##### Save a CSV ###############################################################
parameters = data.frame(species = c(rep("Salmonid", 4),rep("Sturgeon", 4)),
                        parameter = rep(c("A", "B", "C", "D"), 2),
                        value = c(chinook_A_set, chinook_B_set,
                                  chinook_C_set, chinook_D_set,
                                  gs_A_set, gs_B_set,
                                  gs_C_set, gs_D_set))

write.csv(x = parameters,
          file = here(output_folder,"ucrit_parameters.csv"))

################################################################################
# END
################################################################################

