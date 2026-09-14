################################################################################
# This simply loads libraries and some other defaults such as cb color scheme
################################################################################

##### Libraries ################################################################
# Tidyverse 
library(tidyverse)
# Here for file paths
library(here)
# Library to parallel map
library(furrr)
# shape file package
library(sf)
# This library is to fit the gamma distribution
library(fitdistrplus)
#linear regression fitting
library(lme4)
# This does the nls fitting
library(minpack.lm)
# jags for fitting probability of spawning with age
library(rjags)
# library to make the marginal plot
library(ggExtra)
# Library to combine plots
library(patchwork)
# Library for viridis color plate
library(viridis)
# MCMC post run analysis
library(mcmcplots)
# Tidy models package
library(broom)
library(glue)
library(data.table)
library(fitdistrplus)
# for model selection form dredge
library(MuMIn)
# for the daylength function
library(geosphere)

# Overwrite any files which may have been masked. Example:
# Make sure raster or MASS doesn't mask dplyr::select
select = dplyr::select

##### Color blind pallet #######################################################
cbPalette <- c("#999999",
               "#0072B2",
               "#D55E00",
               "#F0E442",
               "#56B4E9",
               "#E69F00",
               "#009E73",
               "#CC79A7")
                        
##### Set seed ################################################################# 
set.seed(42)

################################################################################
# END
################################################################################
