# This script contains the functions for the energy_balance_testing.R script

# calculate the logistic function in the style of inSALMO
calc_logistic <- function(parm_10 = NULL,
                          parm_90 = NULL,
                          value = NULL){
  log_d = log(0.9/0.1)
  log_c = log(0.1/0.9)
  log_b = (log_c - log_d)/(parm_10 - parm_90)
  log_a = log_c - (log_b * parm_10)
  z = log_a + (log_b * value)
  s = exp(z)/(1 + exp(z))
  return(s)
}

# Calculate the beta sigmoid function
calc_beta_sig <- function(parm_A = NULL,
                          parm_B = NULL,
                          temp = NULL){
  
  s = (1 + (parm_A - temp)/(parm_A - parm_B)) *
    (temp/parm_A)^(parm_A / (parm_A - parm_B))
  
  return(s)
}