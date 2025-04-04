#----------------------------------------------------------#
#
#   Phylogenetic assembly of angiosperms through space and time in Europe and
#                     North America in the Holocene
#
# Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                             2025
#
#----------------------------------------------------------#
#----------------------------------------------------------#
# Import data from HOPE database, filter samples based on age criteria ----
#----------------------------------------------------------#

#--------------------------------------------------------#
# 1. Source configuration ----
#--------------------------------------------------------#

source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load input data ----
#--------------------------------------------------------#
# Note: This path will differ as per users!
# This data is also available at "https://zenodo.org/records/11369243"

data_input <-
  readr::read_rds(
    paste0(data_storage_path,
           "Data/Processed/Data_filtered/data_filtered_2022-05-23.rds")
    )$data %>% 
  dplyr::filter(region == "North America" | region == "Europe") %>% 
  dplyr::select(
    dataset_id,
    long,
    lat,
    region,
    levels,
    n_sample_counts,
    age_min,
    age_max,
    raw_counts,
    counts_harmonised,
    pollen_percentage,
    ecozone_koppen_5,
    ecozone_koppen_15
    ) %>% 
  dplyr::filter(!age_min > 1000) %>% 
  dplyr::filter(!age_max < 8000) 


data_input_age_filtered <- 
  data_input %>% 
  dplyr::mutate(
    levels_age_filtered = purrr::map(levels,
                                     ~.x %>% 
                                       dplyr::select(sample_id, 
                                                     depth, 
                                                     age, 
                                                     upper,
                                                     lower) %>% 
                                       dplyr::filter(!age > 12000)
                                     ),
    counts_age_filtered = purrr::map2(.x = counts_harmonised,
                                     .y = levels_age_filtered,
                                     .f = ~ .x %>% 
                                       dplyr::filter(sample_id %in% .y$sample_id)
                                     ),
    region = ifelse(region == "North America",
                    "North_America",
                    region)
    )
  

write_rds(data_input_age_filtered,
          file = "Inputs/Data/data_input_age_filtered_100624.rds",
          compress = "gz")  


