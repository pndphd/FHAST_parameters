
# cleans up the data labels as it reads in csvs ---------------------------

read_in_data <- function(path) {
  file <- readr::read_csv(path)
  if ("TL" %in% colnames(file)) {
    file <- file %>% dplyr::rename(., TL_mm = TL)
  } else {
    file
  }
  file %>%
    dplyr::rename(length_mm = TL_mm) %>%
    dplyr::select(Species, length_mm)
}

# reads in multiple csvs while cleaning them ------------------------------

get_data <- function(params) {
  data <- purrr::map_dfr(params$csvs, read_in_data)
  return(data)
}



# applies the recode_name function to all fish ----------------------------
# some fish are labeled with a name, and others with a code, this changes all species labels into species names



# puts all bass and all pikeminnow labels together ------------------------

lump_fish_names <- function(data) {
  data %>%
    dplyr::mutate(Species = dplyr::case_when(
      stringr::str_detect(Species, "Bass") ~ "bass", # change all fish with "bass" in the name to simply "bass"
      TRUE ~ "pikeminnow" # everything else is labeled as a pikeminnow
    ))
}

# filters data to return only pred species of interest --------------------

subset_pred_species <- function(data, params) {
  params$preds %>%
    purrr::map_dfr(~ dplyr::filter(data, Species == .x))
}

# returns only preds above a certain length -------------------------------

subset_pred_length <- function(data, threshold = 150) {
  data %>%
    dplyr::filter(length_mm >= threshold) %>%
    dplyr::arrange(length_mm)
}

# cumulative percent function ---------------------------------------------

cumpct <- function(x) {
  cumpct <- cumsum(x) / sum(x)
  return(cumpct)
}

# bins and calculates cum. pct. for lengths -------------------------------

prep_data <- function(params) {
  pred_data <- get_pred_data(params)
  binned <- pred_data %>% dplyr::count(length_mm)
  w_pcts <- binned %>%
    dplyr::mutate(
      proportion_of_total = n / sum(n),
      cumulative_proportion = cumpct(n)
    ) %>%
    dplyr::rename(count = n)

  return(w_pcts)
}

# function to calculate gape-limited prey size for a given predator size (based on species)

prey_conv <- function(a = 4.64, B = 2.48e-6, pred_L) {
  exp(a + B * pred_L^2)
}

# an angle value used to calculate the reaction distance of predators

angle_calc <- function(length) {
  0.0167 * exp(9.14 - 2.4 * log(length) + 0.229 * log(length)^2)
}

# gets the parameter estimate for a fit exponential distribution

get_dist_param <- function(fit, param = 1) {
  fit$estimate[param]
}

# gets the max observed length of a predator species

get_max_length <- function(data) {
  max(data$length_mm)
}

# save subset of the data as a csv file

save_output <- function(data, species_name) {
  
  
  pred_data <- data %>% dplyr::filter(species == species_name)
  filename <- paste0(as.character(species_name), "_data.csv")
  pred_proj_path <- here::here("scripts", "R", "predation")
  output_path <- here::here(pred_proj_path, "output", filename)
  readr::write_csv(pred_data, output_path)
}

# make a unique csv per species

save_all_outputs <- function(data) {
  species_names <- data %>%
    dplyr::distinct(species) %>%
    dplyr::pull(species)
  purrr::walk(species_names, save_output, data = data)
}
