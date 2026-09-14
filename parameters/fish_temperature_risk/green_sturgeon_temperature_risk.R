################################################################################
# This is the Bayesian script to fit the logistic regression and get samples 
################################################################################

##### Options ##################################################################
input_file_path = "./parameters/fish_temperature_risk/inputs/green_sturgeon_temp_risk_data.csv"
species = "sturgeon"
################################################################################

##### Load libraries and functions #############################################
source("general_scripts/load_libraries.R")

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

##### Load Data ################################################################
data = read.csv(input_file_path) %>% 
  filter(life_stage == "juvenile") %>% 
  select(target_temp_degrees_C,
         daily_survival_prob) %>% 
  rename(temperature = target_temp_degrees_C,
         survival = daily_survival_prob) %>% 
  na.omit() 
  
##### Make model ###############################################################
modelstring = "  
model {
  for(i in 1 : nData) {
    y[i] ~ dbern( mu[i] )
    mu[i] <- 1/(1+exp(-( b0 + b1*x[i])))
  }
  b0 ~ dnorm( 0 , 1.0E-12 )
  b1 ~ dnorm( 0 , 1.0E-12 )
}" 

# Get initial estimates
initsList =  function() list(b0 = rnorm(1, 0 , 1.0E-12 ),
                             b1 = rnorm(1, 0 , 1.0E-12 ))

# Setup MCMC Chains
# put data in data list for jags
bayDataList = list(x = as.vector(data$temperature),
                   y = as.vector(data$survival),
                   nData = NROW(data))

# run chains
# list parameters to be monitored
parameters = c("b0", "b1") 
# Number of steps to tune the samplers
adaptSteps = 1000   
# Number of steps to burn-in the samplers
burnInSteps = 20000  
# Number of chains 
nChains = 3 
# Total number of steps to save
numSavedSteps = 10000      
# Don't thin any steps
thinSteps=10      
# Steps per chain
nPerChain = ceiling((numSavedSteps*thinSteps)) 
# Create, initialize, and adapt the model
jagsModel = jags.model(textConnection(modelstring),
                       data=bayDataList,
                       inits=initsList, 
                       n.chains=nChains,
                       n.adapt=adaptSteps)
# Burn-in
cat("Burning in the chain.../n")
update(jagsModel , n.iter=burnInSteps)
# The saved MCMC chain
cat("Sampling final MCMC chain.../n")
codaSamples = coda.samples(jagsModel , variable.names=parameters , 
                           n.iter=nPerChain , thin=thinSteps)
gelman.diag(codaSamples)
mcmcplot(codaSamples, parms = c("b0", "b1"))

# Convert coda-object codaSamples to matrix object for easier handling.
mcmcChain = as.matrix( codaSamples )
mcmcChainList = list()

# Extract chain values:
b0Sample = matrix( mcmcChain[, "b0" ] )
b1Sample = matrix( mcmcChain[, "b1" ] )

# Get the data into sets of parameters
sample_df = data.frame(cbind(b0Sample, b1Sample)) %>%
  rename(b0=X1, b1=X2) 

# Make them a list
sample = sample_df %>% 
  split(seq(nrow(.)))

# Get average parameters
sample_avg = sample_df %>% 
  summarise_all(mean) 

# Make data to calculate the sample lines
line_plot_x = expand.grid(x = seq(min(data$temperature),max(data$temperature), length.out = 100))

# Make a function to calculate all the lines
makeLines = function(Pars, n){
  output = line_plot_x %>% 
    mutate(prob = 1/(1+exp(-( Pars$b0 + Pars$b1*x))),
           n = n)
  return(output)
}

# Calculate the lines
plot_lines = length(sample)/250
Line_Data = sample(sample, plot_lines) %>% 
  future_map2_dfr(seq(1,plot_lines,1), ~makeLines(.x, .y))%>% 
  rename(length = x)

avg_line = makeLines(sample_avg, 1)

##### Plots ####################################################################
plotName = ggplot(Line_Data, aes(x = length, y = prob)) +
  theme_classic(base_size = 20) +
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))+
  labs(y = "Survival Probablity", x = expression("Temperature ("*~degree*C*")")) +
  # geom_path(aes(group = n, color = "Samples"), alpha = 0.2) +
  geom_path(data = avg_line, aes(x = x , y = prob), color = "black", linewidth = 1) +
  # scale_color_manual(name = NULL, values = c(Samples = cbPalette[3], Average = cbPalette[2]))+ 
  geom_point(data = data, aes(x=temperature, y=survival),
             size = 3, shape = 1, alpha = 0.5, stroke = 1)+
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
scale_x_continuous(breaks = seq(25, 32.5, 2.5))

print(plotName)

ggsave(paste0("parameters/fish_temperature_risk/outputs/", species, "_risk_plot.png"),
       plotName,
       height = 4,
       width = 6,
       device = "png")

##### Print the parameters #####################################################
parameters = sample_avg %>% 
  get_10_90_params(names = "temp risk")

message(">>>>> The 10 and 90 values are: ",
        parameters$`temp risk 10`,
        " and ",
        parameters$`temp risk 90`,
        "degrees C")

write.csv(data.frame(parameter = c(10, 90),
                     value_c = c(parameters$`temp risk 10`, parameters$`temp risk 90`)),
          file = here(output_folder, paste0(species, "_risk_parameters.csv")),
          row.names = FALSE)

################################################################################
# END
################################################################################
