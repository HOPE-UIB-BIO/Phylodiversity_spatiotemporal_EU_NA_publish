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
# Test temporal variation in latitudinal pattern of PD during the Holocene ----
#                          
#-----------------------------------#

# Fit GAM model of the latitudinal/longitudinal pattern of PD for every 1000 years

# Test the differences in slopes of models of different periods following 
# "https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/"

#--------------------------------------------------------#
# 1. Source configuration ----
#--------------------------------------------------------#
source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load the fitted GAM models ----
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


gam_temporal_lat_long <- 
  dplyr::inner_join(gam_pd_latitudinal_temporal_predicted,
                    gam_pd_longitudinal_temporal_predicted,
                    by = c("region", "vars"))

#--------------------------------------------------------#
# 3. Test of differences in slopes of models of different periods ----
#--------------------------------------------------------#
# Based on "https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/"

# 1. Make a new dataset to be predicted

# 2. Estimate the difference in the smooth term (slope) of the model between the groups
# Where the confidence interval excludes zero, we might infer significant differences 
# between a pairs of estimated smooths.

gam_temporal_smooths_difference <- 
  gam_temporal_lat_long %>% 
  dplyr::mutate(
    dat = purrr::map(filtered_data, 
                     ~ .x %>% 
                       dplyr::mutate_at("dataset_id", as.character) %>%
                       dplyr::arrange(age) %>%
                       dplyr::mutate(period = ceiling(age / 1000)) %>%
                       dplyr::mutate(period = period * 1000) %>%
                       dplyr::mutate_at("period", as.factor) %>%
                       dplyr::mutate_at("dataset_id", as.factor)),
    
    diff_smooths_lat_periodwise =
      purrr::map2(.x = dat,
                  .y = mod_latitudinal_periodwise, 
                  .f = ~ {
                    
                    new_data_latitudinal = 
                      with(.x,
                           base::expand.grid(
                             lat = seq(min(lat), max(lat), by = 0.5),
                             dataset_id = dataset_id[1],
                             period = seq(1000, 12000, by = 1000) %>% 
                               as_factor())
                           )
                    
                    res <- 
                      gratia::difference_smooths(
                        .y,
                        select = "s(lat)",
                        newdata = new_data_latitudinal,
                        ci_level = 0.95,
                        n = 500,
                        method = "REML") %>% 
                      dplyr::rename(level_1 = ".level_1",
                                    level_2 = ".level_2",
                                    diff = ".diff",
                                    se = ".se",
                                    lower_ci = ".lower_ci",
                                    upper_ci = ".upper_ci") %>%
                      
                      dplyr::mutate_at("level_1", as.factor) %>%
                      dplyr::mutate_at("level_2", as.factor) 
                                
                  # Set desired order of periods
                    res$level_1 <- 
                      factor(res$level_1,
                             levels = c("1000",
                                        "2000",
                                        "3000",
                                        "4000",
                                        "5000",
                                        "6000",
                                        "7000",
                                        "8000",
                                        "9000",
                                        "10000",
                                        "11000")
                             )
                    res$level_2 <- 
                      factor(res$level_2,
                             levels = c("2000",
                                        "3000", 
                                        "4000", 
                                        "5000", 
                                        "6000", 
                                        "7000", 
                                        "8000", 
                                        "9000", 
                                        "10000", 
                                        "11000", 
                                        "12000")
                             )
                    return(res)
                    } 
                  ),
    
    diff_smooths_long_periodwise =
      purrr::map2(.x = dat,
                  .y = mod_longitudinal_periodwise, 
                  .f = ~ {
                    new_data_longitudinal =
                      with(.x,
                           base::expand.grid(
                             long = seq(min(long), max(long), by = 0.5),
                             dataset_id = dataset_id[1],
                             age = mean(age),
                             period = seq(1000, 12000, by = 1000))
                           )
                    
                    res <- 
                      gratia::difference_smooths(
                        .y,
                        select = "s(long)",
                        newdata = new_data_longitudinal,
                        ci_level = 0.95,
                        n = 500,
                        method = "REML") %>% 
                      dplyr::rename(level_1 = ".level_1",
                                    level_2 = ".level_2",
                                    diff = ".diff",
                                    se = ".se",
                                    lower_ci = ".lower_ci",
                                    upper_ci = ".upper_ci") %>%
                      
                      dplyr::mutate_at("level_1", as.factor) %>%
                      dplyr::mutate_at("level_2", as.factor) 
                    
                    # Set desired order of periods
                    res$level_1 <- 
                      factor(res$level_1,
                             levels = c("1000",
                                        "2000",
                                        "3000",
                                        "4000",
                                        "5000",
                                        "6000",
                                        "7000",
                                        "8000",
                                        "9000",
                                        "10000",
                                        "11000")
                             )
                    res$level_2 <- 
                      factor(res$level_2,
                             levels = c("2000",
                                        "3000", 
                                        "4000", 
                                        "5000", 
                                        "6000", 
                                        "7000", 
                                        "8000", 
                                        "9000", 
                                        "10000", 
                                        "11000", 
                                        "12000")
                             )
                    return(res)
                    } 
                  )
    )


#--------------------------------------------------------#
# 4. Plot the outputs ----
#--------------------------------------------------------#

# A. Latitudinal patterns ---- 

latitudinal_peiodwise_smooth_difference <-
  purrr::pmap(
    .l = list(
      gam_temporal_smooths_difference$diff_smooths_lat_periodwise, # ..1
      gam_temporal_smooths_difference$vars, # ..2
      gam_temporal_smooths_difference$region # ..3
      ),
    .f = ~ {
    
      # Data for shaded bars at the points of significant change in smooths
      plus_pd <-
        ..1 %>%
        dplyr::filter(lower_ci > 0.1&
                        upper_ci > 0.1) %>%
        dplyr::arrange(lat) %>% 
        
        dplyr::mutate(lat_bin = ceiling(lat/5),
                      lat_bin = lat_bin*5) %>% 
        
        dplyr::group_by(level_1, level_2, lat_bin) %>%
        dplyr::summarise(max_lat = max(lat),
                         min_lat = min(lat),
                         range_lat = max_lat-min_lat,
                         .groups = "drop") %>%
        dplyr::ungroup() %>%
        dplyr::filter(range_lat > 2)
               
      minus_pd <-
        ..1 %>%
        dplyr::filter(lower_ci < -0.1&
                        upper_ci < -0.1) %>%
        dplyr::arrange(lat) %>% 
        
        dplyr::mutate(lat_bin = ceiling(lat/5),
                      lat_bin = lat_bin*5) %>% 
        
        dplyr::group_by(level_1, level_2, lat_bin) %>%
        dplyr::summarise(max_lat = max(lat),
                         min_lat = min(lat),
                         range_lat = max_lat-min_lat,
                         .groups = "drop") %>%
        dplyr::ungroup() %>%
        dplyr::filter(range_lat > 2)
      
      plot_smooth_diff <-
        ..1 %>% 
        ggplot2::ggplot(
          aes(x = lat,
              y = diff)) +
        
        ggplot2::geom_ribbon(
          aes(ymin = lower_ci,
              ymax = upper_ci),
          alpha = 0.15) +
        
        ggplot2::geom_line(
          color = "#0066CC",
          linewidth = 1) +
        
        lemon::facet_rep_grid(
          level_2 ~ level_1,
          scales = "free_y") +
        
        ggplot2::geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "red",
          linewidth = 0.75) +
        
        ggplot2::geom_rect(
          data = plus_pd,
          aes(
            xmin = min_lat,
            xmax = max_lat,
            ymin = -Inf,
            ymax = Inf,
            linewidth = 0.1),
          fill = "#E69F00",
          alpha = 0.4,
          inherit.aes = FALSE) +
        
        ggplot2::geom_rect(
          data = minus_pd,
          aes(
            xmin = min_lat,
            xmax = max_lat,
            ymin = -Inf,
            ymax = Inf,
            linewidth = 0.1),
          fill = "#E69F00",
          alpha = 0.4,
          inherit.aes = FALSE) +
        
        ggplot2::labs(
          x = expression(paste('Latitude ', (degree ~ N))),
          y = paste(
            "Difference in latitudinal trends of ", 
            ..2, 
            " for every 1000 years", 
            sep = ""),
          title = paste(..3)) +
        
        ggplot2::theme_classic() +
        
        ggplot2::theme(
          legend.position = "none",
          strip.text.x = element_text(size = 23),
          strip.text.y = element_text(size = 23),
          axis.text = element_text(color = color_common, size = 20),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = color_common, size = 30),
          plot.title = element_text(color = color_common, size = 30, hjust = 0.5),
          panel.border = element_blank(),
          axis.line = element_line(),
          panel.spacing = unit(-0.25, "lines"))
      }
    
    )
 
ggplot2::ggsave(latitudinal_peiodwise_smooth_difference[[1]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mpd_latitudinal_europe.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")

ggplot2::ggsave(latitudinal_peiodwise_smooth_difference[[2]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mntd_latitudinal_europe.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")

ggplot2::ggsave(latitudinal_peiodwise_smooth_difference[[3]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mpd_latitudinal_north_america.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")

ggplot2::ggsave(latitudinal_peiodwise_smooth_difference[[4]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mntd_latitudinal_north_america.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")


# B. Longitudinal patterns ---- 

longitudinal_peiodwise_smooth_difference <-
  purrr::pmap(
    .l = list(
      gam_temporal_smooths_difference$diff_smooths_long_periodwise, # ..1
      gam_temporal_smooths_difference$vars, # ..2
      gam_temporal_smooths_difference$region # ..3
      ),
    .f = ~ {
      
      # Data for shaded bars at the points of significant change in smooths
      plus_pd <-
        ..1 %>%
        dplyr::filter(lower_ci > 0.1&
                        upper_ci > 0.1) %>%
        dplyr::arrange(long) %>% 
        dplyr::mutate(long_bin = ceiling(long/5),
                      long_bin = long_bin*5) %>% 
        dplyr::group_by(level_1, level_2, long_bin) %>%
        dplyr::summarise(max_long = max(long),
                         min_long = min(long),
                         range_long = max_long-min_long,
                         .groups = "drop") %>%
        dplyr::ungroup() %>%
        dplyr::filter(range_long > 2)
      
      minus_pd <-
        ..1 %>%
        dplyr::filter(lower_ci < -0.1&
                        upper_ci < -0.1) %>%
        dplyr::arrange(long) %>% 
        dplyr::mutate(long_bin = ceiling(long/5),
                      long_bin = long_bin*5) %>% 
        dplyr::group_by(level_1, level_2, long_bin) %>%
        dplyr::summarise(max_long = max(long),
                         min_long = min(long),
                         range_long = max_long-min_long,
                         .groups = "drop") %>%
        dplyr::ungroup() %>%
        dplyr::filter(range_long > 2)
      
      plot_smooth_diff <-
        ..1 %>% 
        ggplot2::ggplot(
          aes(x = long,
              y = diff)) +
        
        ggplot2::geom_ribbon(
          aes(ymin = lower_ci,
              ymax = upper_ci),
          alpha = 0.15) +
        
        ggplot2::geom_line(
          color = "#0066CC",
          linewidth = 1) +
        
        lemon::facet_rep_grid(
          level_2 ~ level_1,
          scales = "free_y") +
        
        ggplot2::geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "red",
          linewidth = 0.75) +
        
        ggplot2::geom_rect(
          data = plus_pd,
          aes(
            xmin = min_long,
            xmax = max_long,
            ymin = -Inf,
            ymax = Inf,
            linewidth = 0.1),
          fill = "#E69F00",
          alpha = 0.4,
          inherit.aes = FALSE) +
        
        ggplot2::geom_rect(
          data = minus_pd,
          aes(
            xmin = min_long,
            xmax = max_long,
            ymin = -Inf,
            ymax = Inf,
            linewidth = 0.1),
          fill = "#E69F00",
          alpha = 0.4,
          inherit.aes = FALSE) +
        
        ggplot2::labs(
          x = expression(paste('Longitude ', (degree ~ E))),
          y = paste(
            "Difference in longitudinal trends of ", 
            ..2, 
            " for every 1000 years", 
            sep = ""),
          title = paste(..3)) +
        
        ggplot2::theme_classic() +
        
        ggplot2::theme(
          legend.position = "none",
          strip.text.x = element_text(size = 23),
          strip.text.y = element_text(size = 23),
          axis.text = element_text(color = color_common, size = 20),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = color_common, size = 30),
          plot.title = element_text(color = color_common, size = 30, hjust = 0.5),
          panel.border = element_blank(),
          axis.line = element_line(),
          panel.spacing = unit(-0.25, "lines"))
    }
    
  )

ggplot2::ggsave(longitudinal_peiodwise_smooth_difference[[1]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mpd_longitudinal_europe.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")

ggplot2::ggsave(longitudinal_peiodwise_smooth_difference[[2]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mntd_longitudinal_europe.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")

ggplot2::ggsave(longitudinal_peiodwise_smooth_difference[[3]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mpd_longitudinal_north_america.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")

ggplot2::ggsave(longitudinal_peiodwise_smooth_difference[[4]],
                filename = paste0(
                  "Outputs/Figures/sampled_lat_long_151124/",
                  "temporal_gam_smooth_difference/",
                  "ses_mntd_longitudinal_north_america.tiff"),
                dpi = 400,
                width = 40,
                height = 35,
                units = "cm",
                compress = "lzw")


