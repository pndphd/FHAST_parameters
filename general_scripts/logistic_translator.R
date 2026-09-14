################################################################################
# This script translates between standard lofistic slope and intercept to the 90
# 10 % values ised in inSALMO and in FHAST
################################################################################

##### Options ##################################################################
# One of the flowing values must have 2 values and one must be NA
# Enter a and b parameters or NA
a_and_b = NA # c(-2.9, 0.21)

# Enter 10 and 90 parameter or NA
v10_and_v90 = c(3.4, 24.8)
################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Main Work ################################################################
if(is.na(a_and_b[1])){
  b = log((0.9/0.1)^2)/(v10_and_v90[2]-v10_and_v90[1])
  a = log(0.1/0.9)-(b*v10_and_v90[1])
  
  # As in net logo FHAST code
  # C = log(0.1/(1-0.1))
  # D = log(0.9/(1-0.9))
  # B = (C - D)/(v10_and_v90[1]-v10_and_v90[2])
  # A =  C - (B * v10_and_v90[1])

  message(paste0(">>>> The A and B values are: ", a, ", ", b))
  
  check_10 = 1/(1+exp(-(a + b*v10_and_v90[1])))
  check_90 = 1/(1+exp(-(a + b*v10_and_v90[2])))
  
} else if(is.na(v10_and_v90[1])){
  value_10 = -(log(1/0.1-1) + a_and_b[1])/a_and_b[2]
  value_90 = -(log(1/0.9-1) + a_and_b[1])/a_and_b[2]
  
  message(paste0(">>>> The 0.1 and 0.9 values are: ", value_10, ", ", value_90))
                                                  
} else {
  stop("One of the options must be NA.")
}
################################################################################
# END
################################################################################

