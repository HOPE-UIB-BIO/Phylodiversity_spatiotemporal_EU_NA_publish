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
# Estimate phylogenetic dispersion (MPD, MNTD, and their beta diversity measures)
#-----------------------------------#

#--------------------------------------------------------#
# Source configuration ----
#--------------------------------------------------------#

source("R/00_Config_file.R")

data_input_for_phylodiversity <- 
  readr::read_rds("Inputs/Data/data_input_for_phylodiversity_140624.rds")


phylodiversity_estimated <-
  data_input_for_phylodiversity %>% 
  
    dplyr::mutate(
    phylogenetic_diversity_mpd =
      purrr::pmap(
        .l = list(
          dataset_id, # ..1
          region, # ..2
          counts_apg_harmonised # ..3
          ),
        .f = ~ get_phylogenetic_diversity(
          dataset_id = ..1,
          region = ..2,
          counts = ..3,
          type = "mpd",
          null.model = "phylogeny.pool", 
          abundance.weighted = TRUE,
          runs = 999
          )
        ),
    
    phylogenetic_diversity_mntd =
      purrr::pmap(
        .l = list(
          dataset_id, # ..1
          region, # ..2
          counts_apg_harmonised # ..3
          ),
        .f = ~ get_phylogenetic_diversity(
          dataset_id = ..1,
          region = ..2,
          counts = ..3,
          type = "mntd",
          null.model = "phylogeny.pool", 
          abundance.weighted = TRUE,
          runs = 999
          )
      )
    ) 

#-----------------------------------#
# Save the outputs ----
#-----------------------------------#
readr::write_rds(
  phylodiversity_estimated,
  file = "Inputs/Data/Phylodiversity_estimated_260624.rds",
  compress = "gz")
#-----------------------------------#
# Save the coincise PD outputs for main analyses ----
#-----------------------------------#
data_phylodispersion <- 
  phylodiversity_estimated %>% 
  plyr::mutate(
    pd_alpha_combined = purrr::pmap(
      .l = list(
        levels_age_filtered, # ..1
        phylogenetic_diversity_mpd, # ..2
        phylogenetic_diversity_mntd # ..3
        ),
      .f = ~ {
        ..1 %>%
          dplyr::select(
            sample_id, 
            age, 
            upper, 
            lower
          ) %>%
          dplyr::inner_join(..2 %>%   
                              dplyr::select(
                                sample_id, 
                                mpd.obs.z
                              ),
                            by = "sample_id") %>%
          dplyr::inner_join(..3 %>%     
                              dplyr::select(
                                sample_id, 
                                mntd.obs.z
                              ),
                            by = "sample_id") %>% 
          dplyr::rename(
            ses_mpd = mpd.obs.z,
            ses_mntd = mntd.obs.z
            ) %>%
          return()
        }
      )
    )
  

data_alpha_pd <- 
  data_phylodispersion %>% 
  dplyr::select(
    dataset_id,
    long,
    lat,
    region,
    ecozone_koppen_15,
    pd_alpha_combined
    ) %>%
  tidyr::nest(data = -region) %>% 
  dplyr::mutate(data = purrr::map(data,
                                  ~.x %>% 
                                    dplyr::mutate_at("dataset_id", as_factor) %>% 
                                    tidyr::unnest(pd_alpha_combined) %>%
                                    dplyr::filter(age > 0) %>% 
                                    dplyr::mutate(
                                      age_uncertainty_index = 
                                        mean(
                                          abs(lower - upper)
                                          ) / abs(lower - upper)
                                      ) %>% 
                                    tidyr::pivot_longer(
                                      ses_mpd:ses_mntd, 
                                      names_to = "vars",
                                      values_to = "estimate"
                                      ) %>% 
                                    dplyr::select(
                                      dataset_id,
                                      sample_id,
                                      lat, 
                                      long,
                                      age, 
                                      age_uncertainty_index,
                                      vars, 
                                      estimate
                                      ) %>% 
                                    tidyr::nest(data = -vars)
                                  )
                ) %>% 
  tidyr::unnest(data)
                                  

readr::write_rds(
  data_alpha_pd,
  file = "Inputs/Data/data_alpha_pd_220724.rds",
  compress = "gz"
  )
