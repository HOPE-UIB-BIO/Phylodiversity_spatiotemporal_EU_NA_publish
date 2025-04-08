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

#-----------------------------------#
#
# Fit the latitudinal/longitudinal-temporal pattern in PD ----
#
# NOTE: Output data files are too large for committing to Github!!!
#                          
#-----------------------------------#
#--------------------------------------------------------#
# 1. Source configuration ----
#--------------------------------------------------------#
source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load the source data ----
#--------------------------------------------------------#
data_pd_predictors_combined <- 
  readr::read_rds("Inputs/Data/data_pd_predictors_combined_filtered_141124.rds") 
# Europe 12250, NA 14898 samples

#eu_lat_long_range <- 
#  data_pd_predictors_combined[1,]$filtered_data[[1]] %>% 
#  dplyr::mutate(lat_range = max(lat)-min(lat),
#                long_range = max(long)-min(long)) %>% 
#  dplyr::select(lat_range, long_range) %>% 
#  distinct() # lat 33.5, long 27.7
  
#na_lat_long_range <- 
#  data_pd_predictors_combined[3,]$filtered_data[[1]] %>% 
#  dplyr::mutate(lat_range = max(lat)-min(lat),
#               long_range = max(long)-min(long)) %>% 
#  dplyr::select(lat_range, long_range) %>% 
#  distinct() # lat 46.3, long 77.2

#--------------------------------------------------------#
# 3. Develop possible alternative models to be compared ----
#--------------------------------------------------------#

# 1. Null model (intercept only model)
# 2. Latitude/longitude model
# 3. Latitude/longitude + time model

# 3.1 Latitudinal pattern PD ----

gam_pd_latitudinal_temporal <-
  data_pd_predictors_combined %>%
  dplyr::mutate(
    gam_models =
      purrr::pmap(
        .l = list(region, # ..1
                  vars, # ..2
                  filtered_data # ..3
                  ), 
        .f = ~ {
          
          dat <- 
            ..3 %>% 
            dplyr::mutate_at("dataset_id", as.character) %>% 
            dplyr::arrange(age) %>% 
            dplyr::mutate(period = ceiling(age / 1000)) %>%
            dplyr::mutate(period = period * 1000) %>%
            dplyr::mutate_at("period", as.factor) %>% 
            dplyr::mutate_at("dataset_id", as.factor)
          
          set.seed(2037)
          
          message(msg = paste(..1, "_", ..2, "null_model", sep = ""))
          
          mod0 <- 
            mgcv::gam(estimate ~ 1, 
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          message(msg = paste(..1, "_", ..2, "mod1", sep = ""))
          
          mod1 <- 
            mgcv::gam(estimate ~ s(lat, 
                                   k = -1,
                                   bs = 'tp') +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'), 
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          message(msg = paste(..1, "_", ..2, "mod2", sep = ""))
          
          mod2 <- 
            mgcv::gam(estimate ~ s(lat, 
                                   k = -1,
                                   bs = 'tp') +
                        s(age, 
                          k = 15, 
                          bs = 'tp') +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200)) 
          
          message(msg = paste(..1, "_", ..2, "mod3", sep = ""))
          
          mod3 <- 
            mgcv::gam(estimate ~ s(lat, 
                                   k = -1,
                                   bs = 'tp') +
                        s(age, 
                          k = -1, 
                          bs = 'tp') +
                        
                        t2(lat, age,
                           bs = c('tp', 'tp'),
                           k = c(30, 30)) + 
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200)) 
          
          message(msg = paste(..1, "_", ..2, "mod4", sep = ""))
          
          mod4 <- 
            mgcv::gam(estimate ~  s(lat, k = -1) + 
                        
                        s(period, 
                          k = nlevels(
                            unique(dat$period)), 
                          bs = 'fs') +
                        
                        s(lat, 
                          by = period, 
                          bs = 'fs',
                          k = -1,
                          m = 1) +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          list_models <- list(mod0 = mod0, 
                              mod1 = mod1, 
                              mod2 = mod2, 
                              mod3 = mod3, 
                              mod4 = mod4)
        
        return(list_models)
        
        }
      )
    )

readr::write_rds(gam_pd_latitudinal_temporal,
                 file = paste0("Outputs/Data/sampled_lat_long_151124/",
                               "gam_pd_latitudinal_temporal_221124.rds"),
                 compress = "gz")


# 3.2 Longitudinal pattern PD ----
gam_pd_longitudinal_temporal <-
  data_pd_predictors_combined %>%
  dplyr::mutate(
    gam_models =
      purrr::pmap(
        .l = list(region, # ..1
                  vars, # ..2
                  filtered_data # ..3
        ), 
        .f = ~ {
          
          dat <- 
            ..3 %>% 
            dplyr::mutate_at("dataset_id", as.character) %>% 
            dplyr::arrange(age) %>% 
            dplyr::mutate(period = ceiling(age / 1000)) %>%
            dplyr::mutate(period = period * 1000) %>%
            dplyr::mutate_at("period", as.factor) %>% 
            dplyr::mutate_at("dataset_id", as.factor)
          
          set.seed(2037)
          
          message(msg = paste(..1, "_", ..2, "null_model", sep = ""))
          
          mod0 <- 
            mgcv::gam(estimate ~ 1, 
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          message(msg = paste(..1, "_", ..2, "mod1", sep = ""))
          
          mod1 <- 
            mgcv::gam(estimate ~ s(long, 
                                   k = -1,
                                   bs = 'tp') +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'), 
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          message(msg = paste(..1, "_", ..2, "mod2", sep = ""))
          
          mod2 <- 
            mgcv::gam(estimate ~ s(long, 
                                   k = -1,
                                   bs = 'tp') +
                        s(age, 
                          k = 15, 
                          bs = 'tp') +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200)) 
          
          message(msg = paste(..1, "_", ..2, "mod3", sep = ""))
          
          mod3 <- 
            mgcv::gam(estimate ~ s(long, 
                                   k = -1,
                                   bs = 'tp') +
                        s(age, 
                          k = -1, 
                          bs = 'tp') +
                        
                        t2(long, age,
                           bs = c('tp', 'tp'),
                           k = c(30, 30)) + 
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200)) 
          
          message(msg = paste(..1, "_", ..2, "mod4", sep = ""))
          
          mod4 <- 
            mgcv::gam(estimate ~  s(long, k = -1) + 
                        
                        s(period, 
                          k = nlevels(
                            unique(dat$period)), 
                          bs = 'fs') +
                        
                        s(long, 
                          by = period, 
                          bs = 'fs',
                          k = -1,
                          m = 1) +
                        
                        s(dataset_id,
                          k = nlevels(
                            unique(dat$dataset_id))-1, 
                          bs = 're'),
                      
                      method = "REML",
                      family = "gaussian", 
                      data = dat,
                      control = mgcv::gam.control(trace = TRUE, maxit = 200))
          
          list_models <- list(mod0 = mod0, 
                              mod1 = mod1, 
                              mod2 = mod2, 
                              mod3 = mod3, 
                              mod4 = mod4)
          
          return(list_models)
          
        }
      )
  )


readr::write_rds(gam_pd_longitudinal_temporal,
                 file = paste0("Outputs/Data/sampled_lat_long_151124/",
                               "gam_pd_longitudinal_temporal_221124.rds"),
                 compress = "gz")

#--------------------------------------------------------#
# 4. Compare the GAM models (based on AIC, explained deviance and r2 adj.) ----
#--------------------------------------------------------#
gam_pd_latitudinal_temporal <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_latitudinal_temporal_221124.rds")) 

latitudinal_model_summary <- 
  gam_pd_latitudinal_temporal %>% 
  dplyr::mutate(model_summary = 
                  purrr::pmap(
                    .l = list(region, # ..1
                              vars, # ..2
                              gam_models # ..3
                              ),
                    .f = ~ {
                      mod0 <-
                        broom::glance(..3$mod0) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod0)$dev.expl,
                          model = paste("Null model"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod1 <-
                        broom::glance(..3$mod1) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod1)$dev.expl,
                          model = paste("latitude as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod2 <-
                        broom::glance(..3$mod2) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod2)$dev.expl,
                          model = paste("latitude+time as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod3 <-
                        broom::glance(..3$mod3) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod3)$dev.expl,
                          model = paste0("latitude+time+latitude:time",
                                         " as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod4 <-
                        broom::glance(..3$mod4) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod4)$dev.expl,
                          model = paste("latitude+factor(priod)+latitude by ",
                                        "priod as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      combined_mod <- 
                        dplyr::bind_rows(mod0, mod1, mod2, mod3, mod4)
                      
                      return(combined_mod)
                      
                      }
                    )
                ) %>% 
  dplyr::select(model_summary) %>% 
  tidyr::unnest(model_summary)

readr::write_csv(latitudinal_model_summary,
                 file = paste0("Outputs/Table/sampled_lat_long_151124/",
                               "latitudinal_model_summary.csv"))
  
  
gam_pd_longitudinal_temporal <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_longitudinal_temporal_221124.rds"))
  
  
longitudinal_model_summary <- 
  gam_pd_longitudinal_temporal %>% 
  dplyr::mutate(model_summary = 
                  purrr::pmap(
                    .l = list(region, # ..1
                              vars, # ..2
                              gam_models # ..3
                              ),
                    .f = ~ {
                      mod0 <-
                        broom::glance(..3$mod0) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod0)$dev.expl,
                          model = paste("Null model"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod1 <-
                        broom::glance(..3$mod1) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod1)$dev.expl,
                          model = paste("longitude as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod2 <-
                        broom::glance(..3$mod2) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod2)$dev.expl,
                          model = paste("longitude+time as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod3 <-
                        broom::glance(..3$mod3) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod3)$dev.expl,
                          model = paste0("longitude+time+longitude:time", 
                                         " as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      mod4 <-
                        broom::glance(..3$mod4) %>%
                        dplyr::mutate(
                          deviance_explained =
                            summary(..3$mod4)$dev.expl,
                          model = paste("longitude+factor(priod)+longitude by ", 
                                        "priod as predictor"),
                          region = paste(..1),
                          response_variable = paste(..2))
                      
                      combined_mod <-
                        dplyr::bind_rows(mod0, mod1, mod2, mod3, mod4)
                      
                      return(combined_mod)
                        
                      }
                    )
                  ) %>% 
    dplyr::select(model_summary) %>% 
    tidyr::unnest(model_summary)
  
  
readr::write_csv(longitudinal_model_summary,
                 file = paste0("Outputs/Table/sampled_lat_long_151124/",
                               "longitudinal_model_summary.csv"))
  
#--------------------------------------------------------#
# 5. Test of residuals ----
#--------------------------------------------------------#
data <- 
  gam_pd_latitudinal_temporal %>% 
  tidyr::unnest(gam_models)

models_name_list <-
  names(data$gam_models) %>% 
  as.list()
  
purrr::pmap(
  .l = list(data$region, # ..1,
            data$vars, # ..2,
            data$gam_models, # ..3
            models_name_list # ..4
            ),
  
  .f = ~ {
    
    tiff(
      filename =
        paste0(
          "Outputs/Figures/sampled_lat_long_151124/",
          "model_diagonostics/", 
          ..1,
          "/",
          "latitudinal/",
          ..2, "_", ..4, 
          ".tiff"),
      height = 15,
      width = 15,
      units = 'cm',
      res = 400,
      compress = "lzw")
    
    par(mfrow = c(2,2))
    mgcv::gam.check(..3, cex = 0.25)
    
    dev.off()
  }
)


data_long <- 
  gam_pd_longitudinal_temporal %>% 
  tidyr::unnest(gam_models)

models_name_list <-
  names(data_long$gam_models) %>% 
  as.list()


purrr::pmap(
  .l = list(data_long$region, # ..1,
            data_long$vars, # ..2,
            data_long$gam_models, # ..3
            models_name_list # ..4
  ),
  
  .f = ~ {
    
    tiff(
      filename =
        paste0(
          "Outputs/Figures/sampled_lat_long_151124/",
          "model_diagonostics/", 
          ..1,
          "/",
          "longitudinal/",
          ..2, "_", ..4, 
          ".tiff"),
      height = 15,
      width = 15,
      units = 'cm',
      res = 400,
      compress = "lzw")
    
    par(mfrow = c(2,2))
    mgcv::gam.check(..3, cex = 0.25)
    
    dev.off()
  }
)


# Test code ----

test <- gam_pd_latitudinal_temporal[1,]$gam_models[[1]]
models_name_list <-
  names(gam_pd_latitudinal_temporal$gam_models[[1]]) %>% 
  as.list()

purrr::map2(
  .x = test,
  .y = models_name_list,
  .f = function(.x, .y) {
    
    tiff(
      filename=
      paste0(
        "Outputs/Figures/sampled_lat_long_151124/",
        "model_diagonostics/",
        paste(.y), ".tiff"),
      height = 15,
      width = 15,
      units = 'cm',
      res = 400,
      compress = "lzw")
    
    par(mfrow = c(2,2))
    mgcv::gam.check(.x, cex = 0.25)
    
    dev.off()
    }
  )

tiff(
  filename = "Outputs/Figures/sampled_lat_long_151124/model_diagonostics/mod3_dharma.tiff",
  height = 15,
  width = 20,
  units = "cm",
  res = 400,
  compression = "lzw")
plot(DHARMa::simulateResiduals(mod3),
     cex = 0.5)
dev.off()


