################################################################################
# Analysis of cumulative catch vs photoperiod for chinook and steelhead
# both files have data for years 2017 to 2021
################################################################################

##### Options ##################################################################
input_folder = "parameters/fish_photoperiod_effect/inputs"
output_folder = "parameters/fish_photoperiod_effect/outputs"

species_names = c("spring_chinook", "steelhead", "winter_chinook")
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Load Files ###############################################################
data_temp = map(species_names, ~read.csv(file = here(input_folder, paste0(.x, "_photoperiod_effect.csv"))))

##### Analysis #################################################################
# Remove NAs, remove 0s, and add column for daylength (photoperiod)
# Add columns for proportion of fish caught out of total for the year
process_data = function(df){
  result = df %>% 
    drop_na() %>%
    filter(Real != 0)%>%
    mutate(Photoperiod = daylength(38,Julian_Date)) %>% 
    group_by(Year)%>%
    mutate(Total = max(Real)) %>%
    mutate(Proportion = Real / Total)%>%
    ungroup()
}

data = data_temp %>% 
  map(~process_data(.x))

# Do a logistic fit
models = data %>% 
  map(~glm(Proportion ~ 
             Photoperiod,
           family=quasibinomial(logit),
           data=.x))

# add in predictions for plotting
predictions = data %>% 
  map2(models, ~ mutate(.x, predict = predict.glm(.y, type = "response")) %>% 
         arrange(Photoperiod))

# solve for 10/90 Parameters
param_10 = map(models, ~{-(log(1/0.1-1)+.x[[1]][1])/.x[[1]][2]})
param_90 = map(models, ~{-(log(1/0.9-1)+.x[[1]][1])/.x[[1]][2]})

##### Plots ####################################################################
# Make a plot for Chinook winter
plots = predictions %>% 
  map(~ggplot(.x, aes(x = Photoperiod))+ 
        theme_classic(base_size = 22) +
        labs(y = "Probability in smolting window", x = "Daylength (hours)") +
        geom_point(aes(y = Proportion), shape = 1, size = 5, stroke = 1.5) +
        geom_path(aes(y = predict), linewidth = 0.7))

walk(plots, ~print(.x))

walk2(plots, species_names, ~ggsave(filename = here(output_folder,
                                                    paste0(.y, "_photoperiod_effect.png")),
                                    plot = .x,
                                    device = "png"))

##### Print oputputs ###########################################################
walk2(param_10,
      param_90,
      ~message(">>>>> The 10 and 90% values are: ",
               .x,
               " and ",
               .y))

walk(seq(1, length(species_names)),
      ~write.csv(data.frame(parameter = c("10", "90"),
                            value_c = c(param_10[[.x]], param_90[[.x]])),
                 file = here(output_folder, paste0(species_names[.x],"_photoperiod_effect.csv")),
                 row.names = FALSE))

################################################################################
# END
################################################################################

