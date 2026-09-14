################################################################################
# This script converts the raw flood maps to FHAST cover files
################################################################################

##### Options ##################################################################
input_folder = "parameters/cover_shape_file/inputs"
output_folder = "parameters/cover_shape_file/outputs"

# Minimum height (in meters) for a shape to be counted as canopy
min_canopy_height = 3

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Functions ################################################################
# removes columns not needed
trim_data_cols <- function(df) {
  result = df %>% 
    select(any_of(c("CV_Group",
                    "HT_CODE_",
                    "HT_CODE",
                    "PER_HARDWO",
                    "PER_CONIFE",
                    "PER_TREE",
                    "PER_SHRUB",
                    "HERB_CODE",
                    "PER_TOTAL",
                    "geometry"))) %>%
    rename(any_of(c("HT_CODE" = "HT_CODE_")))
  return(result)
}

# joins a vector of strings using the "|" character
make_word_filter <- function(list) {
  paste(list, collapse = "|")
}

# calculates the mean height depending on the numbers extracted from a string;
# e.g., "1 - 5m" would return the mean of 1 and 5
calc_height <- function(nums_as_string) {
  nums <- as.numeric(nums_as_string)
  
  if (length(nums) > 1) {
    return(mean(nums))
  } else if (nums <= 1) {
    return(nums / 2)
  } else {
    return(nums)
  }
}

# uses map_dbl to return all height values from a list of strings
calc_all_heights <- function(height_list) {
  height_list <- replace_na(height_list, "0")
  nums_as_strings <- str_extract_all(height_list, "\\(?[0-9,.]+\\)?")
  map_dbl(.x = nums_as_strings, .f = calc_height)
}

##### Load the data ############################################################
# Make filters
wood_words = c("wood", "forest")
wood_filter = make_word_filter(wood_words)
gravel_sand_words = c("rock", "boulder", "cliff")
gravel_sand_filter = make_word_filter(gravel_sand_words)
rock_words = c("rock", "boulder", "cliff")
rock_filter = make_word_filter(rock_words)

# Read in the river data
data_river = read_sf(here(input_folder, "river_cover_shape_file.shp"))%>% 
  trim_data_cols()

# makes a table of the short Group names and their associated longer ones
# the central valley data set uses descriptive names in the CV_Group column
# these names are for sorting, but the delta data set uses only a 3 letter code
# this table is used to add the longer names to the delta data
name_conversion_table = data_river %>%
    as_tibble() %>%
    mutate(short_name = str_extract(CV_Group, "[A-Z]{1,3}")) %>%
    distinct(CV_Group, short_name) %>%
    select(CV_Group, short_name)

# Make the base map
base_data = read_sf(here(input_folder, "delta_cover_shape_file.shp")) %>% 
  st_transform(crs = st_crs(data_river)) %>% 
  # Uses the table from make_name_conversion_table to update the names in the delta data
  # Add longer group names
  left_join(name_conversion_table,
            by = join_by(GroupName == short_name)) %>% 
  trim_data_cols() %>% 
  bind_rows(data_river) %>% 
  # Make column names in a dataframe lowercase so they are more consistently named
  # Cols to lowercase
  rename_with(tolower) %>% 
  # Remove mine polygons 
  filter(!grepl("mines", cv_group)) %>%
  # make the CV_Group column values lowercase for easier filtering
  # cv group categories to lowercase 
  mutate(cv_group = tolower(cv_group)) %>% 
  # percents to proprotions
  mutate(across(.cols = c(per_hardwo:per_shrub, per_total),
                .fns = ~replace_na(.x, 0) / 100)) %>% 
  # some values of cover were above 100% for some reason but should be 0
  # set cover ceiling
  mutate(across(.cols = c(per_hardwo:per_shrub, per_total),
                .fns = ~ data.table::fifelse(.x >= 1, 0, .x))) %>% 
  # rescaling proportion cover so that it's 0 to 1
  # rescale proportions 
  mutate(across(.cols = c(per_hardwo:per_shrub, per_total),
                .fns = ~ .x / max(per_total))) %>% 
  # add height veg wood
  mutate(height = calc_all_heights(ht_code),
         veg = per_total - per_tree,
         wood = per_tree) %>% 
  # Add_class_names
  # Add a generic name for each polygon
  mutate(class = fcase(grepl(wood_filter, cv_group), "t_wood",
                       grepl(gravel_sand_filter, cv_group), "gravel",
                       grepl(rock_filter, cv_group), "rock",
                       grepl("urban", cv_group), "urban",
                       grepl("wat: water", cv_group), "water",
                       default = "t_veg")) %>% 
  # Add default substrate values
  # Add substrate values; most will be NA as they are unknown
  # Can (and should) be updated by the user
  # Some substrate data is included to some degree in the shape file
  mutate(fine = ifelse(class == "gravel", 0.5, NA_real_),
         gravel = ifelse(class == "gravel", 0.5, NA_real_),
         cobble = NA_real_, rock = ifelse(class == "rock", 1, NA_real_)) %>%
  # Select output columns
  select(class, height:rock) %>% 
  # Reset urban and water values
  mutate(across(.cols = height:rock,
                ~ ifelse(class == "water" | class == "urban", NA_real_, .x)))

# Subset into 2 sepetate fiels
ground_cover = base_data %>% 
  select(-height)
canopy_cover = base_data %>% 
  select(height, wood) %>%
  filter(height >= min_canopy_height)
  
##### Save the fiels ###########################################################
st_write(ground_cover,
         here(output_folder, "cover_shape_file.shp"),
         delete_layer = TRUE)
st_write(canopy_cover, 
         here(output_folder, "canopy_shape_file.shp"),
         delete_layer = TRUE)

################################################################################
# END
################################################################################