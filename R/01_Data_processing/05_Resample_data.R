#----------------------------------------------------------#
#
#   Phylogenetic assembly of angiosperms through space and time in Europe and
#                     North America in the Holocene
#
#    Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                              2025
#
#----------------------------------------------------------#

#-----------------------------------#
# Randomly sample sequences along latitudinal and longitudinal gradient ----
#-----------------------------------#

#--------------------------------------------------------#
# Source configuration ----
#--------------------------------------------------------#

source("R/00_Config_file.R")


#--------------------------------------------------------#
# Load combined data of PD and predictors ----
#--------------------------------------------------------#
pd_predictors_combined <- 
  readr::read_rds("Inputs/Data/data_pd_predictors_combined_050924.rds") 


#--------------------------------------------------------#
# Filter samples based on age criteria and randomly sample datasets ----
#--------------------------------------------------------#

# Age-criteria: 
#  only select samples with min. age < 1000 yr BP and max. age > 10000 yr BP

# Lat-long criteria: 
#  Randomly sample 3 datasets from each cell of 2 degree lat-long

filtered_data <- 
  pd_predictors_combined %>% 
  dplyr::mutate(filtered_data = 
                  purrr::map(.x = data,
                             .f = ~ {
                               set.seed(2567)
                               
                               .x %>% 
                                 dplyr::group_by(dataset_id, lat, long) %>% 
                                 tidyr::nest() %>% 
                                 ungroup() %>% 
                                 dplyr::mutate(
                                   min_age = purrr::map_dbl(data, ~ min(.x$age)),
                                   max_age = purrr::map_dbl(data, ~ max(.x$age))
                                   ) %>%                
                                 dplyr::filter(!min_age > 1000) %>% 
                                 dplyr::filter(!max_age < 10000) %>%    
                                 dplyr::arrange(lat) %>% 
                                 dplyr::mutate(lat_bin = ceiling(lat/2),
                                               lat_bin = lat_bin*2) %>% 
                                 dplyr::arrange(long) %>% 
                                 dplyr::mutate(long_bin = ceiling(long/2),
                                               long_bin = long_bin*2) %>% 
                                 dplyr::group_by(lat_bin, long_bin) %>% 
                                 
                                 dplyr::slice_sample(n = 3) %>% # 151 Eu, 314 NA
                                 dplyr::ungroup() %>% 
                                 tidyr::unnest(data) %>% 
                                 dplyr::select(-c(min_age, 
                                                  max_age, 
                                                  lat_bin,
                                                  long_bin))
                               }
                             ) 
                )

readr::write_rds(filtered_data,
                 file = "Inputs/Data/data_pd_predictors_combined_filtered_141124.rds",
                 compress = "gz")
