# This script will try to put bounds on how far a sturgeon can search for food

# Load the libraries
source("./scripts/load_libraries.R")

# from Kelly et al. 2007 
feeding_speed = 0.21 # m/s length of these was 105 cm 
feeding_speed_bl = 0.21/1.05
feeding_time = 12*3600 # s
bl = 0.15 # m

# calculation if it on a straight line assuming half 
# a body length scanning radius

straight_area = feeding_speed_bl * bl * feeding_time * bl
straight_side = sqrt(straight_area)

# for a random walk returning to the start where each step is a body length
n = feeding_speed_bl * feeding_time


# range (unique # of points visited) of 2d random walk
# Jiao, J. 2014. The range of two dimensional simple random walk.
# University of Pennsylvania.
rw_area = pi*n/log(n)*(bl)^2
rw_side = sqrt(rw_area)
