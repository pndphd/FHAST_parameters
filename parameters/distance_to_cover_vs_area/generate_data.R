################################################################################
# This script generates the data for the distance to cover vs cover area model
################################################################################

##### Options ##################################################################
# Size of each cell; value isn't really important
cell_size = 1 
# Minimum number of sides a polygon can have; going below 3 is not recommended.
min_n_poly_sides = 3 
# Max number of sides a polygon can have; feel free to play around with it;
# Min and max can be the same if you like
max_n_poly_sides = 10 
# Maximum number of polygons that will be generated in any one cell
max_n_poly = 4 
# "fish" are just the random points generated in a cell and used to measure
# distances to the polygons
num_fish = 10000 
max_area_multiplier = 3

# What is the output path (which will be the input path for the next step)
output_path = here('parameters/distance_to_cover_vs_area/outputs')

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")
source(here('parameters/distance_to_cover_vs_area/functions/distance_to_cover_vs_area_functions.R'))
plan(multisession)

##### Main Work ################################################################
# Put the parameters in a list
cover_sim_params = list(cell_size = cell_size,
                        min_n_poly_sides = min_n_poly_sides,
                        max_n_poly_sides = max_n_poly_sides,
                        max_n_poly = max_n_poly,
                        num_fish = num_fish,
                        max_area_multiplier = max_area_multiplier)

# The simulation takes a couple minutes, so saving the data is helpful
if(!file.exists(here(output_path, "cover_simulation_data.csv"))){
  write_csv(get_cover_vs_dis(cover_sim_params), here(output_path, "cover_simulation_data.csv"))
}

# create a polygon out of a specified number of shapes (n_poly) and a maximum possible area (max_area); 
# for reference, the cell size is 1
poly <- create_polygon(cover_sim_params, max_area = .3, n_poly =  4)

# generate a number of random points within the cell
pts <- generate_fish_locs(cover_sim_params) %>% sample_n(30)

##### Plots ####################################################################
# Make an example plot
example_plot = ggplot() +
  geom_sf(data = poly,
          fill = cbPalette[2], 
          color = cbPalette[5], 
          alpha = 1) +
  geom_point(data = pts, 
             mapping = aes(x = x, y = y)) +
  theme_classic(base_size = 25) +
  coord_sf(xlim = c(0, 1),
           ylim = c(0, 1)) + 
  labs(x = 'Cell Cidth',
       y = 'Cell Height') 

print(example_plot)

# Save the plot
ggsave(plot = example_plot,
       filename = here(output_path, 'example_polygon.png'),                 
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)


################################################################################
# END
################################################################################
