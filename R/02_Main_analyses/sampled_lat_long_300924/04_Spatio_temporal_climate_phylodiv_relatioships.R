#----------------------------------------------------------#
#
#   Phylogenetic assembly of angiosperms through space and time in Europe and
#                     North America in the Holocene
#
#    Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                             2025
#
#----------------------------------------------------------#

# Concordance of spatio-temporal variation in PD and climate ----

#--------------------------------------------------------#
# 1. Load configuration file ----
#--------------------------------------------------------#
source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load the fitted GAM models of PD ----
#--------------------------------------------------------#

gam_pd_latitudinal_temporal_predicted <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_latitudinal_temporal_predicted_262224.rds")
    ) %>% 
  dplyr::select(region, 
                vars, 
                filtered_data, 
                mod_latitudinal_periodwise)

gam_pd_longitudinal_temporal_predicted <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_longitudinal_temporal_predicted_262224.rds")
    ) %>% 
  dplyr::select(region, 
                vars, 
                mod_longitudinal_periodwise)

data_gam_pd <- 
  dplyr::inner_join(
    gam_pd_latitudinal_temporal_predicted,
    gam_pd_longitudinal_temporal_predicted,
    by = c("region", "vars"))


data_climate <- 
  data_gam_pd %>%
  dplyr::slice(c(1, 3)) %>% # climate data for 'ses_mpd' in both continent is similar to that for 'ses_mntd'
  dplyr::select(-c(mod_latitudinal_periodwise, mod_longitudinal_periodwise)) %>% 
  dplyr::mutate(
    data_climate =
      purrr::map(
        filtered_data,
        ~ .x %>%
          dplyr::mutate_at("dataset_id", as.character) %>%
          dplyr::arrange(age) %>%
          dplyr::mutate(period = ceiling(age / 1000)) %>%
          dplyr::mutate(period = period * 1000) %>%
          dplyr::mutate_at("period", as.factor) %>%
          dplyr::mutate_at("dataset_id", as.factor) %>%
          dplyr::select(
            dataset_id,
            lat,
            long,
            age,
            period,
            temp_cold,
            prec_summer,
            prec_win,
            anth_ind_taxa) %>%
          tidyr::gather(
            c(temp_cold, prec_summer, prec_win, anth_ind_taxa),
            key = "vars",
            value = "estimate") %>%
          tidyr::nest(.by = vars)
        )
    ) %>%
  dplyr::select(-vars) %>%
  tidyr::unnest(data_climate)

#--------------------------------------------------------#
# 3. Fit period-wise latitudinal and longitudinal models of climate ----
#--------------------------------------------------------#
gam_climate_periodwise <-
  data_climate %>% 
  dplyr::mutate(
    gam_models_latitudinal_climate =
      purrr::pmap(
        .l = list(region, # ..1
                  vars, # ..2
                  data # ..3
                  ), 
        .f = ~ {
          
          message(msg = paste0(..1, "_", ..2))
          
          set.seed(2468)
          
          mod_climate_latitudinal <- 
            mgcv::gam(estimate ~  s(lat, k = -1) + 
                        
                        s(period, 
                          k = nlevels(
                            unique(..3$period)), 
                          bs = 'fs') +
                        
                        s(lat, 
                          by = period, 
                          bs = 'fs',
                          k = -1,
                          m = 1) +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(..3$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = ..3,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          return(mod_climate_latitudinal)
          
        }
      ),
    gam_models_longitudinal_climate =
      purrr::pmap(
        .l = list(region, # ..1
                  vars, # ..2
                  data # ..3
                  ), 
        .f = ~ {
          
          message(msg = paste0(..1, "_", ..2))
          
          set.seed(2468)
          
          mod_climate_longitudinal <- 
            mgcv::gam(estimate ~  s(long, k = -1) + 
                        
                        s(period, 
                          k = nlevels(
                            unique(..3$period)), 
                          bs = 'fs') +
                        
                        s(long, 
                          by = period, 
                          bs = 'fs',
                          k = -1,
                          m = 1) +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(..3$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = ..3,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          return(mod_climate_longitudinal)
          }
        )
    )

readr::write_rds(gam_climate_periodwise,
          file = paste0("Outputs/Data/sampled_lat_long_151124/",
                        "gam_climate_periodwise_291124.rds"),
          compress = "gz")

#--------------------------------------------------------#
# 4. Test of differences in slopes of models of different periods ----
#--------------------------------------------------------#

#--------------------------------#
# A. Period-wise (per 1000 yr) difference slopes of latitudinal/longitudinal ---- 
#  patterns of PD ----
#--------------------------------#
gam_temporal_smooth_difference_pd <-
  data_gam_pd %>%
  dplyr::mutate(
    dat = purrr::map(
      filtered_data,
      ~ .x %>%
        dplyr::mutate_at("dataset_id", as.character) %>%
        dplyr::arrange(age) %>%
        dplyr::mutate(period = ceiling(age / 1000)) %>%
        dplyr::mutate(period = period * 1000) %>%
        dplyr::mutate_at("period", as.factor) %>%
        dplyr::mutate_at("dataset_id", as.factor)),
    
    temporal_variation_latitudinal_pd =
      purrr::map2(
        .x = dat,
        .y = mod_latitudinal_periodwise,
        .f = ~ {
          new_data <- 
            with(
              .x,
              base::expand.grid(
                lat = seq(min(lat), max(lat), by = 0.5),
                period = seq(1000,12000, by = 1000) %>% 
                  as_factor())
              )
          
          set.seed(2468)
          
          diff_pattern_lat <-
            gratia::difference_smooths(
              .y,
              select = "s(lat)",
              newdata = new_data,
              ci_level = 0.95,
              n = 500,
              method = "REML"
              )
          return(diff_pattern_lat)
        }
      ),
    
    temporal_variation_longitudinal_pd =
      purrr::map2(
        .x = dat,
        .y = mod_longitudinal_periodwise,
        .f = ~ {
         
          new_data_long <- 
            with(
              .x,
              base::expand.grid(
                long = seq(min(long), max(long), by = 0.5),
                period = seq(1000,12000, by = 1000) %>% 
                  as_factor())
              )
          
          set.seed(2468)
          
          diff_pattern_long <-
            gratia::difference_smooths(
              .y,
              select = "s(long)",
              newdata = new_data_long,
              ci_level = 0.95,
              n = 500,
              method = "REML"
              )
          return(diff_pattern_long)
          }
        )
    )

readr::write_rds(
  gam_temporal_smooth_difference_pd,
  file = paste0("Outputs/Data/sampled_lat_long_151124/",
                "gam_temporal_smooth_difference_pd_291124.rds"),
  compress = "gz")


#--------------------------------#
# B. Period-wise (per 1000 yr) difference slopes of latitudinal/longitudinal ---- 
#  patterns of climate ----
#--------------------------------#
gam_climate_periodwise <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_climate_periodwise_291124.rds"))

gam_temporal_smooth_difference_climate <-
  gam_climate_periodwise %>%
  dplyr::mutate(
    dat = purrr::map(
      filtered_data,
      ~ .x %>%
        dplyr::mutate_at("dataset_id", as.character) %>%
        dplyr::arrange(age) %>%
        dplyr::mutate(period = ceiling(age / 1000)) %>%
        dplyr::mutate(period = period * 1000) %>%
        dplyr::mutate_at("period", as.factor) %>%
        dplyr::mutate_at("dataset_id", as.factor)),
    
    temporal_variation_latitudinal_climate =
      purrr::map2(
        .x = dat,
        .y = gam_models_latitudinal_climate,
        .f = ~ {
          
          new_data_lat <-
            with(
              .x,
              base::expand.grid(
                lat = seq(min(lat), max(lat), by = 0.5),
                period = seq(1000, 12000, by = 1000) %>% 
                  as_factor())
              )
          
          set.seed(2468)
          
          diff_pattern_latitudinal <-
            gratia::difference_smooths(
              .y,
              select = "s(lat)",
              newdata = new_data_lat,
              ci_level = 0.95,
              n = 500,
              method = "REML")
          
          return(diff_pattern_latitudinal)
          }
        ),
    
    temporal_variation_longitudinal_climate = 
      purrr::map2(
        .x = dat,
        .y = gam_models_longitudinal_climate,
        .f = ~ {
          
          new_data_long <- 
            with(
              .x,
              base::expand.grid(
                long = seq(min(long), max(long), by = 0.5),
                period = seq(1000, 12000, by = 1000) %>% 
                  as_factor())
              )
      
      set.seed(2468)
      
      diff_pattern_longitudinal <-
        gratia::difference_smooths(
          .y,
          select = "s(long)",
          newdata = new_data_long,
          ci_level = 0.95,
          n = 500,
          method = "REML")
      return(diff_pattern_longitudinal)
    }
  )
  )

readr::write_rds(
  gam_temporal_smooth_difference_climate,
  file = paste0("Outputs/Data/sampled_lat_long_151124/",
                "gam_temporal_smooth_difference_climate_291124.rds"),
  compress = "gz")


#--------------------------------------------------------#
# 5. Test slope differences of spatio-temporal models ----
#--------------------------------------------------------#   
gam_temporal_smooth_difference_pd <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_temporal_smooth_difference_pd_291124.rds")
    )

gam_temporal_smooth_difference_climate <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_temporal_smooth_difference_climate_291124.rds")
  )

# Make species by samples dataframe of PD and climate variables

# A. Temporal variation in latitudinal patterns of PD and climate ----
pd_diff_latitudinal <- 
  gam_temporal_smooth_difference_pd %>% 
  dplyr::select(
    region,
    vars, 
    temporal_variation_latitudinal_pd) %>% 
  dplyr::mutate(
    diff_matrix_pd_latitudinal = 
      purrr::map(
        .x = temporal_variation_latitudinal_pd,
        .f = ~ {
          diff_matrix_init <- 
            .x %>%
            dplyr::select(level_1 = ".level_1",
                          level_2 = ".level_2",
                          diff = ".diff") %>%
            dplyr::group_by(level_1, level_2) %>%
            dplyr::summarise_all(list(mean)) %>%
            dplyr::ungroup() %>%
            
            # To make zero diagonal of the square matrix
            bind_rows(
              tibble::tibble(
              level_1 = seq(1000, 12000, by = 1000),
              level_2 = seq(1000, 12000, by = 1000),
              diff = 0) %>% 
                dplyr::mutate_at("level_1", as.character) %>%
                dplyr::mutate_at("level_2", as.character)
              )
          
          diff_matrix_init$level_1 <-
            factor(
              diff_matrix_init$level_1,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
          
          diff_matrix_init$level_2 <-
            factor(
              diff_matrix_init$level_2,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
          
          diff_matrix <-
            diff_matrix_init %>%
            tidyr::spread(key = level_2, value = diff) %>%
            column_to_rownames("level_1") 
                      
          # Copy upper triangle of the matrix to lower triangle to
          # make a square matrix, otherwise NAs in the lower half
          # produce error during ordination
          
          diff_matrix[lower.tri(diff_matrix)] <-
            diff_matrix[upper.tri(diff_matrix)]
          
          return(diff_matrix)
        }
      ),
    
    standardized_diff_matrix_pd_latitudinal = 
      purrr::map(diff_matrix_pd_latitudinal,
                 ~ vegan::decostand(.x, 
                                    method = "standardize")) # Standardize the matrix
    )


climate_diff_latitudinal <- 
  gam_temporal_smooth_difference_climate %>% 
  dplyr::select(
    region, 
    vars, 
    temporal_variation_latitudinal_climate) %>% 
  dplyr::mutate(
    diff_matrix_climate_latitudinal = 
      purrr::map(
        .x = temporal_variation_latitudinal_climate,
        .f = ~ {
          diff_matrix_climate_init <- 
            .x %>% 
            dplyr::select(
              level_1 = ".level_1",
              level_2 = ".level_2",
              diff = ".diff") %>%
            dplyr::group_by(level_1, level_2) %>%
            dplyr::summarise_all(list(mean)) %>%
            dplyr::ungroup() %>%
            
            # To make zero diagonal of the square matrix
            bind_rows(
              tibble::tibble(
                level_1 = seq(1000, 12000, by = 1000),
                level_2 = seq(1000, 12000, by = 1000),
                diff = 0) %>% 
                dplyr::mutate_at("level_1", as.character) %>%
                dplyr::mutate_at("level_2", as.character)
              )
          
          diff_matrix_climate_init$level_1 <-
            factor(
              diff_matrix_climate_init$level_1,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
          
          diff_matrix_climate_init$level_2 <-
            factor(
              diff_matrix_climate_init$level_2,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
          
          diff_matrix_climate <-
            diff_matrix_climate_init %>%
            tidyr::spread(key = level_2, value = diff) %>%
            column_to_rownames("level_1") 
          
          # Copy upper triangle of the matrix to lower triangle to
          # make a square matrix, otherwise NAs in the lower half
          # produce error during ordination
          
          diff_matrix_climate[lower.tri(diff_matrix_climate)] <-
            diff_matrix_climate[upper.tri(diff_matrix_climate)]
          
          return(diff_matrix_climate)
        }
      ),
    
    standardized_diff_matrix_climate_latitudinal = 
      purrr::map(diff_matrix_climate_latitudinal,
                 ~ vegan::decostand(.x, 
                                    method = "standardize")) # Standardize the matrix
    )


# B. Temporal variation in longitudinal patterns of PD and climate ----
pd_diff_longitudinal <- 
  gam_temporal_smooth_difference_pd %>% 
  dplyr::select(
    region, 
    vars, 
    temporal_variation_longitudinal_pd) %>% 
  dplyr::mutate(
    diff_matrix_pd_longitudinal = 
      purrr::map(
        .x = temporal_variation_longitudinal_pd,
        .f = ~ {
          diff_matrix_long_init <- 
            .x %>%
            dplyr::select(level_1 = ".level_1",
                          level_2 = ".level_2",
                          diff = ".diff") %>%
            dplyr::group_by(level_1, level_2) %>%
            dplyr::summarise_all(list(mean)) %>%
            dplyr::ungroup() %>%
            
            # To make zero diagonal of the square matrix
            bind_rows(
              tibble::tibble(
                level_1 = seq(1000, 12000, by = 1000),
                level_2 = seq(1000, 12000, by = 1000),
                diff = 0) %>% 
                dplyr::mutate_at("level_1", as.character) %>%
                dplyr::mutate_at("level_2", as.character)
            )
          
          diff_matrix_long_init$level_1 <-
            factor(
              diff_matrix_long_init$level_1,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
          
          diff_matrix_long_init$level_2 <-
            factor(
              diff_matrix_long_init$level_2,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
          
          diff_matrix_long <-
            diff_matrix_long_init %>%
            tidyr::spread(key = level_2, value = diff) %>%
            column_to_rownames("level_1") 
          
          diff_matrix_long[lower.tri(diff_matrix_long)] <-
            diff_matrix_long[upper.tri(diff_matrix_long)]
          
          return(diff_matrix_long)
        }
      ),
    
    standardized_diff_matrix_pd_longitudinal = 
      purrr::map(diff_matrix_pd_longitudinal,
                 ~ vegan::decostand(.x, 
                                    method = "standardize")) 
    )


climate_diff_longitudinal <- 
  gam_temporal_smooth_difference_climate %>% 
  dplyr::select(
    region, 
    vars, 
    temporal_variation_longitudinal_climate) %>% 
  dplyr::mutate(
    diff_matrix_climate_longitudinal =
      purrr::map(
        .x = temporal_variation_longitudinal_climate,
        .f = ~ {
          diff_matrix_climate_long_init <-
            .x %>%
            dplyr::select(level_1 = ".level_1",
                          level_2 = ".level_2",
                          diff = ".diff") %>%
            dplyr::group_by(level_1, level_2) %>%
            dplyr::summarise_all(list(mean)) %>%
            dplyr::ungroup() %>%
                        
            # To make zero diagonal of the square matrix
            bind_rows(
              tibble::tibble(
                level_1 = seq(1000, 12000, by = 1000),
                level_2 = seq(1000, 12000, by = 1000),
                diff = 0) %>%
                dplyr::mutate_at("level_1", as.character) %>%
                dplyr::mutate_at("level_2", as.character)
              )
          
          diff_matrix_climate_long_init$level_1 <-
            factor(
              diff_matrix_climate_long_init$level_1,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
                      
          diff_matrix_climate_long_init$level_2 <-
            factor(
              diff_matrix_climate_long_init$level_2,
              levels = c(
                "1000",
                "2000",
                "3000",
                "4000",
                "5000",
                "6000",
                "7000",
                "8000",
                "9000",
                "10000",
                "11000",
                "12000"))
                      
          diff_matrix_climate_long <-
            diff_matrix_climate_long_init %>%
            tidyr::spread(key = level_2, value = diff) %>%
            column_to_rownames("level_1")
          
          diff_matrix_climate_long[lower.tri(diff_matrix_climate_long)] <-
            diff_matrix_climate_long[upper.tri(diff_matrix_climate_long)]
          
          return(diff_matrix_climate_long)
          }
        ),
    
    standardized_diff_matrix_climate_longitudinal =
      purrr::map(diff_matrix_climate_longitudinal,
                 ~ vegan::decostand(.x, method = "standardize")) 
    )

#--------------------------------------------------------#
# 6. Procrustes test between the temporal variation in latitudinal pattern of ----
#  climate and that in latitudinal pattern of sesMPD/sesMNTD
#--------------------------------------------------------#

# Perform Procrustes test between PCA of latitudinal PD and each of climate variables

# A. Latitudinal PD vs individual climate, anthropogenic influence ----

pca_pd_latitudinal <- 
  pd_diff_latitudinal %>% 
  dplyr::mutate(
    pca_pd_diff_latitudinal = 
      purrr::map(diff_matrix_pd_latitudinal,
                 ~ vegan::rda(.x)),
    pca_standardized_pd_diff_latitudinal = 
      purrr::map(standardized_diff_matrix_pd_latitudinal,
                 ~ vegan::rda(.x))
    )

pca_climate_latitudinal <- 
  climate_diff_latitudinal %>% 
  dplyr::mutate(
    pca_climate_diff_latitudinal = 
      purrr::map(diff_matrix_climate_latitudinal,
                 ~ vegan::rda(.x)),
    pca_standardized_climate_diff_latitudinal = 
      purrr::map(standardized_diff_matrix_climate_latitudinal,
                 ~ vegan::rda(.x))
    )

combined_pd_predictors_latitudinal <- 
  pca_pd_latitudinal %>% 
  dplyr::select(
    region, 
    pd_metric = vars,
    pca_pd_diff_latitudinal,
    pca_standardized_pd_diff_latitudinal) %>% 
  dplyr::right_join(pca_climate_latitudinal %>% 
                      dplyr::select(
                        region,
                        pred_variable = vars,
                        pca_climate_diff_latitudinal,
                        pca_standardized_climate_diff_latitudinal),
                    by = "region") # Ignore warning, all's OK

# Test of significance of concordance between PD and individual predictor variable

procrustes_test_pd_vs_individual_pred_latitudinal <- 
  combined_pd_predictors_latitudinal %>% 
  dplyr::mutate(
    procrustes_individual = 
      purrr::map2(
        .x = pca_pd_diff_latitudinal,
        .y = pca_climate_diff_latitudinal,
        .f = ~ {
          set.seed(2330)
          res <- 
            vegan::protest(
            X = .x,
            Y = .y,
            scores = "sites",
            permutations = 999,
            choises = c(1, 2))
          return(res)
          }
        ),
    procrustes_individual_standardized_data = 
      purrr::map2(
        .x = pca_standardized_pd_diff_latitudinal,
        .y = pca_standardized_climate_diff_latitudinal,
        .f = ~ {
          set.seed(2330)
          res <- 
            vegan::protest(
              X = .x,
              Y = .y,
              scores = "sites",
              permutations = 999,
              choises = c(1, 2))
          return(res)
          }
        )
  )

readr::write_rds(
  procrustes_test_pd_vs_individual_pred_latitudinal,
  file = paste0("Outputs/Data/sampled_lat_long_151124/",
                "procrustes_test_pd_vs_individual_pred_latitudinal.rds"),
  compress = "gz"
  )

# Extract summary of Procrustes test

procrustes_test_pd_vs_individual_predictor_latitudinal <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "procrustes_test_pd_vs_individual_pred_latitudinal.rds")) 

procrustes_summary_pd_vs_individual_predictor_latitudinal <- 
  procrustes_test_pd_vs_individual_predictor_latitudinal %>% 
  dplyr::mutate(
    procrustes_summary = 
      purrr::pmap(
        .l = list(region, # ..1
                  pd_metric, # ..2
                  pred_variable, # ..3
                  procrustes_individual, # ..4
                  procrustes_individual_standardized_data # ..5
                  ),
        .f = ~ {
          summary_proc <- 
            tibble::tibble(
              "data_type" = "unstandardized",
              "region" = ..1,
              "spatial_gradient" = "latitude",
              "pd_metric" = ..2,
              "predictor_variable" = ..3,
              "Procrustes sum of squares" = summary(..4)$ss,
              "Procrustes root mean squared error" = summary(..4)$rmse,
              "Correlation in a symmetric Procrustes rotation" = ..4$t0,
              "Significance" = ..4$signif,
              Permutations = ..4$permutation) %>% 
            dplyr::bind_rows(
              tibble::tibble(
                "data_type" = "standardized",
                "region" = ..1,
                "spatial_gradient" = "latitude",
                "pd_metric" = ..2,
                "predictor_variable" = ..3,
                "Procrustes sum of squares" = summary(..5)$ss,
                "Procrustes root mean squared error" = summary(..5)$rmse,
                "Correlation in a symmetric Procrustes rotation" = ..5$t0,
                "Significance" = ..5$signif,
                Permutations = ..5$permutation)
              )
          return(summary_proc)
          }
        )
    ) %>% 
  dplyr::select(procrustes_summary) %>% 
  tidyr::unnest(procrustes_summary) 

readr::write_csv(
  procrustes_summary_pd_vs_individual_predictor_latitudinal,
  file = paste0(
    "Outputs/Table/sampled_lat_long_151124/",
    "procrustes_summary_pd_vs_individual_predictor_latitudinal_021224.csv"))


# B. Latitudinal PD vs. all climatic variables combined ----

# Combine all climatic variables in one matrix and standardize them

combined_climate_latitudinal_europe <- 
  climate_diff_latitudinal[1,]$diff_matrix_climate_latitudinal[[1]] %>% 
  tibble::rownames_to_column("samples") %>%
  dplyr::inner_join(
    climate_diff_latitudinal[2,]$diff_matrix_climate_latitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  dplyr::inner_join(
    climate_diff_latitudinal[3,]$diff_matrix_climate_latitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  tibble::column_to_rownames("samples") 

combined_climate_latitudinal_north_america <- 
  climate_diff_latitudinal[5,]$diff_matrix_climate_latitudinal[[1]] %>% 
  tibble::rownames_to_column("samples") %>%
  dplyr::inner_join(
    climate_diff_latitudinal[6,]$diff_matrix_climate_latitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  dplyr::inner_join(
    climate_diff_latitudinal[7,]$diff_matrix_climate_latitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  tibble::column_to_rownames("samples")   


# Do PCA of standardized matrix of combined climate and PD

pca_combined_climate_latitudinal_europe <- 
  vegan::rda(
    vegan::decostand(combined_climate_latitudinal_europe,
                     method = "standardize")
    )
pca_combined_climate_latitudinal_north_america <- 
  vegan::rda(
    vegan::decostand(combined_climate_latitudinal_north_america,
                     method = "standardize")
    )


# Test of significance of concordance between PD and combined climate

set.seed(2330)  
ses_mpd_vs_combined_climate_europe_latitudinal <- 
  vegan::protest(
    X = pca_pd_latitudinal[1,]$pca_standardized_pd_diff_latitudinal[[1]],
    Y = pca_combined_climate_latitudinal_europe,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.5738, 
# Correlation in a symmetric Procrustes rotation: 0.6529
# Procrustes root mean squared error: 0.2186654 
# Significance:  0.016, Number of permutations: 999

set.seed(2330)  
ses_mntd_vs_combined_climate_europe_latitudinal <- 
  vegan::protest(
    X = pca_pd_latitudinal[2,]$pca_standardized_pd_diff_latitudinal[[1]],
    Y = pca_combined_climate_latitudinal_europe,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.5579, 
# Correlation in a symmetric Procrustes rotation: 0.6649 
# Procrustes root mean squared error: 0.2156098 
# Significance:  0.012, Number of permutations: 999

set.seed(2330)  
ses_mpd_vs_combined_climate_north_america_latitudinal <- 
  vegan::protest(
    X = pca_pd_latitudinal[3,]$pca_standardized_pd_diff_latitudinal[[1]],
    Y = pca_combined_climate_latitudinal_north_america,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.4909, 
# Correlation in a symmetric Procrustes rotation: 0.7135 
# Procrustes root mean squared error: 0.2022 
# Significance:  0.002, Number of permutations: 999

set.seed(2330)  
ses_mntd_vs_combined_climate_north_america_latitudinal <- 
  vegan::protest(
    X = pca_pd_latitudinal[4,]$pca_standardized_pd_diff_latitudinal[[1]],
    Y = pca_combined_climate_latitudinal_north_america,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.4211, 
# Correlation in a symmetric Procrustes rotation: 0.7609 
# Procrustes root mean squared error: 0.1873 
# Significance:  0.001, Number of permutations: 999

procrustes_summary_combined_climate_latitudinal <- 
  tibble(
    "Region" = c(rep("Europe", 2), rep("North America", 2)),
    "Spatial gradient" = rep("Latitude", 4),
    "Matrix_1 (PCA)" = c("ses_MPD", 
                         "ses_MNTD", 
                         "ses_MPD", 
                         "ses_MNTD"), 
    
    "Matrix_2 (PCA)" = rep("temp_cold + prec_summer + prec_winter", 4),
    "Procrustes Sum of Squares (m12 squared)" = c(
      0.5738, 
      0.5579, 
      0.4909, 
      0.4211),
    
    "Correlation in a symmetric Procrustes rotation" = c(
      0.6529, 
      0.6649, 
      0.7135, 
      0.7609),
    
    "Procrustes root mean squared error" = c(
      0.2186, 
      0.2156, 
      0.2022, 
      0.1873),
    
    "Significance" = c(
      0.016, 
      0.012, 
      0.002, 
      0.001),
    
    "Number of permutations" = c(
      rep(999, 4))
  )

readr::write_csv(
  procrustes_summary_combined_climate_latitudinal,
  file = paste0(
    "Outputs/Table/sampled_lat_long_151124/",
    "Procrustes_test_PD_combined_climate_latitudinal_021224.csv")
  )


#--------------------------------------------------------#
# 7. Procrustes test between the temporal variation in longitudinal pattern of ----
#  climate and that in longitudinal pattern of sesMPD/sesMNTD
#--------------------------------------------------------#

# Perform Procrustes test between PCA of longitudinal PD and each of climate variables

# A. Longitudinal PD vs individual climate, anthropogenic influence ----

pca_pd_longitudinal <- 
  pd_diff_longitudinal %>% 
  dplyr::mutate(
    pca_pd_diff_longitudinal = 
      purrr::map(diff_matrix_pd_longitudinal,
                 ~ vegan::rda(.x)),
    pca_standardized_pd_diff_longitudinal = 
      purrr::map(standardized_diff_matrix_pd_longitudinal,
                 ~ vegan::rda(.x))
    )

pca_climate_longitudinal <- 
  climate_diff_longitudinal %>% 
  dplyr::mutate(
    pca_climate_diff_longitudinal = 
      purrr::map(diff_matrix_climate_longitudinal,
                 ~ vegan::rda(.x)),
    pca_standardized_climate_diff_longitudinal = 
      purrr::map(standardized_diff_matrix_climate_longitudinal,
                 ~ vegan::rda(.x))
    )

combined_pd_predictors_longitudinal <- 
  pca_pd_longitudinal %>% 
  dplyr::select(
    region, 
    pd_metric = vars,
    pca_pd_diff_longitudinal,
    pca_standardized_pd_diff_longitudinal) %>% 
  dplyr::right_join(pca_climate_longitudinal %>% 
                      dplyr::select(
                        region,
                        pred_variable = vars,
                        pca_climate_diff_longitudinal,
                        pca_standardized_climate_diff_longitudinal),
                    by = "region") # Ignore warning, all's OK


# Test of significance of concordance between PD and individual predictor variable

procrustes_test_pd_vs_individual_pred_longitudinal <- 
  combined_pd_predictors_longitudinal %>% 
  dplyr::mutate(
    procrustes_individual = 
      purrr::map2(
        .x = pca_pd_diff_longitudinal,
        .y = pca_climate_diff_longitudinal,
        .f = ~ {
          set.seed(2330)
          res <- 
            vegan::protest(
              X = .x,
              Y = .y,
              scores = "sites",
              permutations = 999,
              choises = c(1, 2))
          return(res)
        }
      ),
    procrustes_individual_standardized_data = 
      purrr::map2(
        .x = pca_standardized_pd_diff_longitudinal,
        .y = pca_standardized_climate_diff_longitudinal,
        .f = ~ {
          set.seed(2330)
          res <- 
            vegan::protest(
              X = .x,
              Y = .y,
              scores = "sites",
              permutations = 999,
              choises = c(1, 2))
          return(res)
        }
      )
    )

readr::write_rds(
  procrustes_test_pd_vs_individual_pred_longitudinal,
  file = paste0("Outputs/Data/sampled_lat_long_151124/",
                "procrustes_test_pd_vs_individual_pred_longitudinal.rds"),
  compress = "gz"
  )

# Extract summary of Procrustes test

procrustes_test_pd_vs_individual_predictor_longitudinal <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "procrustes_test_pd_vs_individual_pred_longitudinal.rds")) 

procrustes_summary_pd_vs_individual_predictor_longitudinal <- 
  procrustes_test_pd_vs_individual_predictor_longitudinal %>% 
  dplyr::mutate(
    procrustes_summary = 
      purrr::pmap(
        .l = list(region, # ..1
                  pd_metric, # ..2
                  pred_variable, # ..3
                  procrustes_individual, # ..4
                  procrustes_individual_standardized_data # ..5
        ),
        .f = ~ {
          summary_proc <- 
            tibble::tibble(
              "data_type" = "unstandardized",
              "region" = ..1,
              "spatial_gradient" = "longitude",
              "pd_metric" = ..2,
              "predictor_variable" = ..3,
              "Procrustes sum of squares" = summary(..4)$ss,
              "Procrustes root mean squared error" = summary(..4)$rmse,
              "Correlation in a symmetric Procrustes rotation" = ..4$t0,
              "Significance" = ..4$signif,
              Permutations = ..4$permutation) %>% 
            dplyr::bind_rows(
              tibble::tibble(
                "data_type" = "standardized",
                "region" = ..1,
                "spatial_gradient" = "longitude",
                "pd_metric" = ..2,
                "predictor_variable" = ..3,
                "Procrustes sum of squares" = summary(..5)$ss,
                "Procrustes root mean squared error" = summary(..5)$rmse,
                "Correlation in a symmetric Procrustes rotation" = ..5$t0,
                "Significance" = ..5$signif,
                Permutations = ..5$permutation)
            )
          return(summary_proc)
        }
      )
  ) %>% 
  dplyr::select(procrustes_summary) %>% 
  tidyr::unnest(procrustes_summary) 

readr::write_csv(
  procrustes_summary_pd_vs_individual_predictor_longitudinal,
  file = paste0(
    "Outputs/Table/sampled_lat_long_151124/",
    "procrustes_summary_pd_vs_individual_predictor_longitudinal_021224.csv"))


# B. Latitudinal PD vs. all climatic variables combined ----

# Combine all climatic variables in one matrix and standardize them
combined_climate_longitudinal_europe <- 
  climate_diff_longitudinal[1,]$diff_matrix_climate_longitudinal[[1]] %>% 
  tibble::rownames_to_column("samples") %>%
  dplyr::inner_join(
    climate_diff_longitudinal[2,]$diff_matrix_climate_longitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  dplyr::inner_join(
    climate_diff_longitudinal[3,]$diff_matrix_climate_longitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  tibble::column_to_rownames("samples") 

combined_climate_longitudinal_north_america <- 
  climate_diff_longitudinal[5,]$diff_matrix_climate_longitudinal[[1]] %>% 
  tibble::rownames_to_column("samples") %>%
  dplyr::inner_join(
    climate_diff_longitudinal[6,]$diff_matrix_climate_longitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  dplyr::inner_join(
    climate_diff_longitudinal[7,]$diff_matrix_climate_longitudinal[[1]] %>% 
      tibble::rownames_to_column("samples"), 
    by = "samples") %>% 
  tibble::column_to_rownames("samples")   


# Do PCA of standardized matrix of combined climate and PD

pca_combined_climate_longitudinal_europe <- 
  vegan::rda(
    vegan::decostand(combined_climate_longitudinal_europe,
                     method = "standardize")
    )
pca_combined_climate_longitudinal_north_america <- 
  vegan::rda(
    vegan::decostand(combined_climate_longitudinal_north_america,
                     method = "standardize")
    )


# Test of significance of concordance between PD and combined climate

set.seed(2330)  
ses_mpd_vs_combined_climate_europe_longitudinal <- 
  vegan::protest(
    X = pca_pd_longitudinal[1,]$pca_standardized_pd_diff_longitudinal[[1]],
    Y = pca_combined_climate_longitudinal_europe,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.7876, 
# Correlation in a symmetric Procrustes rotation: 0.4608
# Procrustes root mean squared error: 0.2561
# Significance: 0.188, Number of permutations: 999

set.seed(2330)  
ses_mntd_vs_combined_climate_europe_longitudinal <- 
  vegan::protest(
    X = pca_pd_longitudinal[2,]$pca_standardized_pd_diff_longitudinal[[1]],
    Y = pca_combined_climate_longitudinal_europe,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.9338, 
# Correlation in a symmetric Procrustes rotation: 0.2573 
# Procrustes root mean squared error: 0.2789
# Significance:  0.808, Number of permutations: 999

set.seed(2330)  
ses_mpd_vs_combined_climate_north_america_longitudinal <- 
  vegan::protest(
    X = pca_pd_longitudinal[3,]$pca_standardized_pd_diff_longitudinal[[1]],
    Y = pca_combined_climate_longitudinal_north_america,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.5298, 
# Correlation in a symmetric Procrustes rotation: 0.6857 
# Procrustes root mean squared error: 0.2101 
# Significance:  0.003, Number of permutations: 999

set.seed(2330)  
ses_mntd_vs_combined_climate_north_america_longitudinal <- 
  vegan::protest(
    X = pca_pd_longitudinal[4,]$pca_standardized_pd_diff_longitudinal[[1]],
    Y = pca_combined_climate_longitudinal_north_america,
    scores = "sites",
    permutations = 999,
    choises = c(1, 2))
# Procrustes Sum of Squares (m12 squared): 0.3523, 
# Correlation in a symmetric Procrustes rotation: 0.8048 
# Procrustes root mean squared error: 0.1713 
# Significance:  0.001, Number of permutations: 999

procrustes_summary_combined_climate_longitudinal <- 
  tibble(
    "Region" = c(rep("Europe", 2), rep("North America", 2)),
    "Spatial gradient" = rep("Longitude", 4),
    "Matrix_1 (PCA)" = c("ses_MPD", 
                         "ses_MNTD", 
                         "ses_MPD", 
                         "ses_MNTD"), 
    
    "Matrix_2 (PCA)" = rep("temp_cold + prec_summer + prec_winter", 4),
    "Procrustes Sum of Squares (m12 squared)" = c(
      0.7876, 
      0.9338,
      0.5298,
      0.3523),
    
    "Correlation in a symmetric Procrustes rotation" = c(
      0.4608, 
      0.2573,  
      0.6857,
      0.80),
    
    "Procrustes root mean squared error" = c(
      0.2561, 
      0.2789, 
      0.2101, 
      0.1713),
    
    "Significance" = c(
      0.188, 
      0.808, 
      0.003,
      0.001),
    
    "Number of permutations" = c(
      rep(999, 4))
    )

readr::write_csv(
  procrustes_summary_combined_climate_longitudinal,
  file = paste0(
    "Outputs/Table/sampled_lat_long_151124/",
    "Procrustes_test_PD_combined_climate_longitudinal_021224.csv")
  )

#-----------------------------------------------------------------------#

