#----------------------------------------------------------#
#
#   Phylogenetic assembly of angiosperms through space and time in Europe and
#                     North America in the Holocene
#
#    Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                                2025
#
#----------------------------------------------------------#

#-----------------------------------#
# Combine predictor variables (climate and human density (SPD)) with the estimates ----
# NOTE: if was used from the HOPE database published at 
#  https://zenodo.org/records/11369243
# Citation:  "Felde et al. Climate outweighs human effects on vegetation properties 
#  during the early-to-mid Holocene".  DOI: 10.21203/rs.3.rs-4692574/v1
#-----------------------------------#

#--------------------------------------------------------#
# 1. Source configuration ----
#--------------------------------------------------------#

source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load PD, climate, summed probability distribution (SPD) data ----
#--------------------------------------------------------#

climate_data <- 
  readr::read_rds("Inputs/Data/Climate/data_climate-2024-01-23.rds") %>% 
  tidyr::unnest(climate_data) %>% 
  dplyr::select(-time_id)

data_alpha_pd <- 
  readr::read_rds("Inputs/Data/data_alpha_pd_220724.rds")

#--------------------------------------------------------#
# Filter data based on lat-long range ----
#--------------------------------------------------------#

# Europe
# lat: whole, long: 6E - max long

# North America
# lat: whole, long: -130E - max long

data_alpha_pd_eu <- 
  data_alpha_pd %>% 
  dplyr::filter(region == "Europe") %>% 
  dplyr::mutate(data = purrr::map(data, 
                                  ~ .x %>% 
                                    dplyr::mutate_at("dataset_id", as.character) %>% # to remove factor levels of unfiltered data
                                    
                                    # Filter data by lat/long criteria
                                    dplyr::filter(long > 6)
                                    )
                )

data_alpha_pd_na <- 
  data_alpha_pd %>% 
  dplyr::filter(region == "North_America") %>% 
  dplyr::mutate(data = purrr::map(data, 
                                  ~ .x %>% 
                                    dplyr::mutate_at("dataset_id", as.character) %>% # to remove factor levels of unfiltered data
                                    
                                    # Filter data by lat/long criteria
                                    dplyr::filter(long > -130)
                                  )
                )

data_alpha_pd_filtered <- 
  dplyr::bind_rows(data_alpha_pd_eu, data_alpha_pd_na) %>% 
  # Aggregate data at 100 bin because climate data is at 100-yr bins
  dplyr::mutate(data = purrr::map(data,
                                  ~ .x %>% 
                                    arrange(age) %>%
                                    dplyr::mutate(age = ceiling(age / 100)) %>%
                                    dplyr::mutate(age = age * 100) 
                                  )
                ) %>% 
  tidyr::unnest(data) 


#--------------------------------------------------------#
# 3. Extract anthropogenic indicator taxa ----
#--------------------------------------------------------#

# John's suggestion
# For Europe: Plantago lanceolata, Cerealia including Secale
# For North America: Ambrosia type, Cerealia including Zea

phylodiversity_estimated <- 
  readr::read_rds("Inputs/Data/Phylodiversity_estimated_260624.rds") 

filtered_raw_taxa <- 
  data_alpha_pd_filtered %>% 
  dplyr::group_by(region, vars, dataset_id) %>% 
  tidyr::nest() %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(pd_final = data) %>% 
  dplyr::inner_join(phylodiversity_estimated,
                    by = c("dataset_id", "region")) %>% 
  dplyr::mutate(raw_counts_filtered = 
                  purrr::map2(.x = raw_counts,
                              .y = pd_final,
                              .f = ~ {
                                df <- 
                                  .x %>% 
                                  as_tibble() %>% 
                                  dplyr::mutate_at("sample_id", as.character)  
                               pf <- 
                                 .y %>% 
                                 as_tibble() %>% 
                                 dplyr::mutate_at("sample_id", as.character)
                               cf <-
                                 df %>%
                                 dplyr::filter(sample_id %in% unique(pf$sample_id)) %>% 
                                 as_tibble()
                               
                               return(cf)
                              }
                              )
                ) 


anthropogenic_indicators <- 
  filtered_raw_taxa %>% 
  dplyr::mutate(indicator_taxa = 
                  purrr::pmap(.l = list(id = dataset_id,
                                        region = region,
                                        counts = raw_counts_filtered),
                              .f = get_anthropogenic_indicators
                              )
                ) %>% 
  dplyr::select(region, vars, dataset_id, indicator_taxa) %>% 
  tidyr::unnest(indicator_taxa)

nas <- 
  anthropogenic_indicators %>% 
  dplyr::mutate(nas = ifelse(is.na(anth_ind_taxa), TRUE, FALSE)) %>% 
  dplyr::filter(nas == TRUE) %>% 
  dplyr::group_by(dataset_id) %>% 
  tidyr::nest() 

# !!!two datasets have NAs for anthropogenic taxa, hence NAs for all samples!!!

# Europe, ses_mpd, dataset_id = 42720, 812 samples (406 samples each for ses_MPD and ses_MNTD)
# Problem = "mercurialis_annua_type" present in the dataset was not in the harmonisation table

# North_America, ses_mpd, dataset_id = 46730, 42 samples (406 samples each for ses_MPD and ses_MNTD)
# Problem = "agrostis", "araceae_undiff", "chimaphila", "poa", "thalictrum_type" present in the dataset was not in the harmonisation table

# Solution = These taxa added and harmonised in the harmonisation tables!

#--------------------------------------------------------#
# 4. Combine predictors with the diversity estimates ----
#--------------------------------------------------------#
pd_predictors_combined <- 
  data_alpha_pd_filtered %>% 
  dplyr::inner_join(anthropogenic_indicators,
                    by = c("region", "vars", "dataset_id", "sample_id")
                    ) %>% 
  dplyr::inner_join(climate_data, 
                    by = c("dataset_id", "age")) %>% 
 tidyr::nest(data = everything(), .by = c(region, vars))


#--------------------------------------------------------#
# 4. Save the combined data ----
#--------------------------------------------------------#
readr::write_rds(pd_predictors_combined,
                 file = "Inputs/Data/data_pd_predictors_combined_050924.rds",
                 compress = "gz")

