#----------------------------------------------------------#
#
# Spatio-temporal patterns of phylogenetic diversity in Europe and North America
#                           during the Holocene
#
#                            Kuber Bhatta
#                                2024
#
#----------------------------------------------------------#

# Plot predictor variable patterns during the Holocene ----

#--------------------------------------------------------#
# 1. Load configuration file ----
#--------------------------------------------------------#
source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load combined data of PD and predictors ----
#--------------------------------------------------------#
data_pd_predictors_combined <- 
  readr::read_rds("Inputs/Data/data_pd_predictors_combined_filtered_141124.rds") %>% 
  dplyr::select(-data)
# Europe 12250, NA 14898 samples

data_pd_predictors_combined <- 
  data_pd_predictors_combined[c(1,3),] %>% 
  dplyr::select(-vars)

# temp_annual, prec_annual,prec_summer, prec_win, temp_cold, anth_ind_taxa


#--------------------------------------------------------#
# 3. Prepare the data for plotting ----
#--------------------------------------------------------#
predictor_variables <- 
  data_pd_predictors_combined %>% 
  dplyr::mutate(matrix_lat = 
                  purrr::map(filtered_data,
                             ~ .x %>% 
                               dplyr::select(lat,
                                             age,
                                             temp_annual,
                                             prec_annual,
                                             prec_summer,
                                             prec_win,
                                             temp_cold,
                                             anth_ind_taxa) %>% 
                               dplyr::arrange(age) %>% 
                               dplyr::mutate(age = ceiling(age/500),
                                             age = age*500) %>% 
                               dplyr::arrange(lat) %>% 
                               dplyr::mutate(lat = ceiling(lat/2),
                                             lat = lat*2) %>% 
                               dplyr::group_by(lat, age) %>% 
                               dplyr::summarise_all(mean) %>% 
                               dplyr::ungroup()),
                matrix_long = 
                  purrr::map(filtered_data,
                             ~ .x %>% 
                               dplyr::select(long,
                                             age,
                                             temp_annual,
                                             prec_annual,
                                             prec_summer,
                                             prec_win,
                                             temp_cold,
                                             anth_ind_taxa) %>% 
                               dplyr::arrange(age) %>% 
                               dplyr::mutate(age = ceiling(age/500),
                                             age = age*500) %>% 
                               dplyr::arrange(long) %>% 
                               dplyr::mutate(long = ceiling(long/2),
                                             long = long*2) %>% 
                               dplyr::group_by(long, age) %>% 
                               dplyr::summarise_all(mean) %>% 
                               dplyr::ungroup())
                )


#--------------------------------------------------------#
# 4. Patterns of predictors ----
#--------------------------------------------------------#
# A. Latitudinal patterns ----

plot_lat_climate <- 
  purrr::map2(.x = predictor_variables$matrix_lat,
              .y = predictor_variables$region,
              .f = function(.x, .y){
               
               dat1 <- 
                 .x %>%
                 dplyr::group_by(lat) %>%
                 dplyr::summarise_all(mean) %>% 
                 dplyr::ungroup()
               
               dat2 <- 
                 .x %>%
                 dplyr::group_by(age) %>%
                 dplyr::summarise_all(mean) %>% 
                 dplyr::ungroup()
               
          # Annual mean temperature ----
               annual_temp_curve <-
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = lat,
                     y = temp_annual,
                     group = age,
                     colour = age)
                   ) +
                 
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::scale_color_gradient(
                   high = color_high_age,
                   low = color_low_age) +
                 ggplot2::theme_classic() +
                 ggplot2::geom_line(
                   aes(
                     x = lat,
                     y = temp_annual),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat1) +
                 
                 ggplot2::labs(
                   x = element_blank(),
                   y = 'ann_temp',
                   colour = "Time \n(cal yr BP)") +
                 ggplot2::theme(
                   axis.text.x = element_text(
                     color = color_common,
                     size = 18,
                     angle = 45,
                     hjust = 1),
                   axis.text.y = element_text(
                     color = color_common,
                     size = 18),
                   axis.title = element_text(
                     color = color_common,
                     size = 22),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               annual_temp_temporal <-
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = age,
                     y = temp_annual,
                     group = lat,
                     colour = lat)) +
                 ggplot2::scale_color_gradient(
                   high = color_high_lat,
                   low = color_low_lat) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::geom_line(
                   aes(x = age,
                       y = temp_annual),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat2) +
                 ggplot2::theme_classic() +
                 ggplot2::labs(
                   x = element_blank(),
                   y = element_blank(),
                   colour = expression(
                     paste(
                       'Lat ', (degree ~ N))
                     )
                   ) +
                 ggplot2::scale_x_continuous(
                   limits = c(0, 12000),
                   breaks = seq(0, 12000, 2000)) +
                 ggplot2::theme(
                   axis.title = element_text(
                     size = 22,
                     color = color_common),
                   axis.text = element_text(
                     size = 18,
                     color = color_common),
                   axis.text.x = element_text(
                     angle = 45,
                     hjust = 1),
                   legend.position = "right",
                   legend.background = element_rect(
                     fill = "transparent"),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )   
               
               final_annual_temp <- 
                 ggpubr::ggarrange(
                   annual_temp_curve,
                   annual_temp_temporal,
                   ncol = 2,
                   nrow = 1) +
                 ggplot2::theme(
                   plot.margin = margin(1, 0, 0, 0.1, "cm")) 
               
              
        # Minimum temperature of the coldest month ----
               temp_cold_curve <- 
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = lat,
                     y = temp_cold,
                     group = age,
                     colour = age)
                   ) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) + 
                 ggplot2::scale_color_gradient(
                   high = color_high_age,
                   low = color_low_age) +
                 ggplot2::theme_classic() +
                 ggplot2::geom_line(
                   aes(x = lat,
                       y = temp_cold),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat1) +
                 ggplot2::labs(
                   x = element_blank(),
                   y = 'temp_cold',
                   colour = "Time \n(cal yr BP)") + 
                 ggplot2::theme(
                   axis.text.x = element_text(
                     color = color_common,
                     size = 18,
                     angle = 45,
                     hjust = 1),
                   axis.text.y = element_text(
                     color = color_common,
                     size = 18),
                   axis.title = element_text(
                     color = color_common,
                     size = 22),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common))
               
               temp_cold_temporal <-
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = age,
                     y = temp_cold,
                     group = lat,
                     colour = lat)
                   ) +
                 ggplot2::scale_color_gradient(
                   high = color_high_lat,
                   low = color_low_lat) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::geom_line(
                   aes(
                     x = age,
                     y = temp_cold),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat2) +
                 ggplot2::theme_classic() +
                 ggplot2::labs(
                   x = element_blank(),
                   y = element_blank(),
                   colour = expression(
                     paste(
                       'Lat ', (degree ~ N)))
                   ) +
                 ggplot2::scale_x_continuous(
                   limits = c(0, 12000),
                   breaks = seq(0, 12000, 2000)) +
                 ggplot2::theme(
                   axis.title = element_text(
                     size = 22,
                     color = color_common),
                   axis.text = element_text(
                     size = 18,
                     color = color_common),
                   axis.text.x = element_text(
                     angle = 45,
                     hjust = 1),
                   legend.position = "right",
                   legend.background = element_rect(
                     fill = "transparent"),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )   
               
               final_temp_cold <- 
                 ggpubr::ggarrange(
                   temp_cold_curve,
                   temp_cold_temporal,
                   ncol = 2,
                   nrow = 1) +
                 ggplot2::theme(
                   plot.margin = margin(1, 0, 0, 0.1, "cm")) 
               
               
        # Annual precipitation ----
               annual_precip_curve <- 
                 .x %>% 
                 ggplot2::ggplot(
                   aes(
                     x = lat,
                     y = prec_annual,
                     group = age,
                     colour = age)
                   ) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) + 
                 ggplot2::scale_color_gradient(
                   high = color_high_age,
                   low = color_low_age) +
                 ggplot2::theme_classic() +
                 ggplot2::geom_line(
                   aes(
                     x = lat,
                     y = prec_annual),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat1) +
                 ggplot2::labs(
                   x = element_blank(),
                   y = 'ann_prec',
                   colour = "Time \n(cal yr BP)") + 
                 ggplot2::theme(
                   axis.text.x = element_text(
                     color = color_common,
                     size = 18,
                     angle = 45,
                     hjust = 1),
                   axis.text.y = element_text(
                     color = color_common,
                     size = 18),
                   axis.title = element_text(
                     color = color_common,
                     size = 22),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               annual_precip_temporal <-
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = age,
                     y = prec_annual,
                     group = lat,
                     colour = lat)
                   ) +
                 ggplot2::scale_color_gradient(
                   high = color_high_lat,
                   low = color_low_lat) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::geom_line(
                   aes(
                     x = age,
                     y = prec_annual),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat2) +
                 ggplot2::theme_classic() +
                 ggplot2::labs(
                   x = element_blank(),
                   y = element_blank(),
                   colour = expression(
                     paste(
                       'Lat ', (degree ~ N))
                     )
                   ) +
                 ggplot2::scale_x_continuous(
                   limits = c(0, 12000),
                   breaks = seq(0, 12000, 2000)) +
                 ggplot2::theme(
                   axis.title = element_text(
                     size = 22,
                     color = color_common),
                   axis.text = element_text(
                     size = 18,
                     color = color_common),
                   axis.text.x = element_text(
                     angle = 45,
                     hjust = 1),
                   legend.position = "right",
                   legend.background = element_rect(
                     fill = "transparent"),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common))   
               
               final_ann_prec <- 
                 ggpubr::ggarrange(
                   annual_precip_curve,
                   annual_precip_temporal,
                   ncol = 2,
                   nrow = 1) +
                 ggplot2::theme(
                   plot.margin = margin(1, 0, 0, 0.1, "cm")) 
           
               
        # Summer precipitation ----
               summer_precip_curve <- 
                 .x %>% 
                 ggplot2::ggplot(
                   aes(
                     x = lat,
                     y = prec_summer,
                     group = age,
                     colour = age)
                   ) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) + 
                 ggplot2::scale_color_gradient(
                   high = color_high_age,
                   low = color_low_age) +
                 ggplot2::theme_classic() +
                 ggplot2::geom_line(
                   aes(
                     x = lat,
                     y = prec_summer),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat1) +
                 ggplot2::labs(
                   x = element_blank(),
                   y = 'sum_prec',
                   colour = "Time \n(cal yr BP)") + 
                 ggplot2::theme(
                   axis.text.x = element_text(
                     color = color_common,
                     size = 18,
                     angle = 45,
                     hjust = 1),
                   axis.text.y = element_text(
                     color = color_common,
                     size = 18),
                   axis.title = element_text(
                     color = color_common,
                     size = 22),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               summer_precip_temporal <- 
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = age,
                     y = prec_summer,
                     group = lat,
                     colour = lat)
                   ) +
                 ggplot2::scale_color_gradient(
                   high = color_high_lat,
                   low = color_low_lat) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::geom_line(
                   aes(
                     x = age,
                     y = prec_summer),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat2) +
                 ggplot2::theme_classic() +
                 ggplot2::labs(
                   x = element_blank(),
                   y = element_blank(),
                   colour = expression(
                     paste(
                       'Lat ', (degree ~ N))
                     )
                   ) +
                 ggplot2::scale_x_continuous(
                   limits = c(0, 12000),
                   breaks = seq(0, 12000, 2000)
                   ) +
                 ggplot2::theme(
                   axis.title = element_text(
                     size = 22,
                     color = color_common),
                   axis.text = element_text(
                     size = 18,
                     color = color_common),
                   axis.text.x = element_text(
                     angle = 45,
                     hjust = 1),
                   legend.position = "right",
                   legend.background = element_rect(
                     fill = "transparent"),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )   
               
               final_prec_summer <- 
                 ggpubr::ggarrange(
                   summer_precip_curve,
                   summer_precip_temporal,
                   ncol = 2,
                   nrow = 1) +
                 ggplot2::theme(
                   plot.margin = margin(1, 0, 0, 0.1, "cm"))     
               
               
          # Winter precipitation ----
               winter_precip_curve <- 
                 .x %>% 
                 ggplot2::ggplot(
                   aes(
                     x = lat,
                     y = prec_win,
                     group = age,
                     colour = age)
                   ) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) + 
                 ggplot2::scale_color_gradient(
                   high = color_high_age,
                   low = color_low_age) +
                 ggplot2::theme_classic() + 
                 ggplot2::geom_line(
                   aes(
                     x = lat,
                     y = prec_win),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat1) +
                 ggplot2::labs(
                   x = element_blank(),
                   y = 'wint_prec',
                   colour = "Time \n(cal yr BP)") + 
                 ggplot2::theme(
                   axis.text.x = element_text(
                     color = color_common,
                     size = 18,
                     angle = 45,
                     hjust = 1),
                   axis.text.y = element_text(
                     color = color_common,
                     size = 18),
                   axis.title = element_text(
                     color = color_common,
                     size = 22),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               prec_winter_temporal <- 
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = age,
                     y = prec_win,
                     group = lat,
                     colour = lat)
                   ) +
                 ggplot2::scale_color_gradient(
                   high = color_high_lat,
                   low = color_low_lat) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::geom_line(
                   aes(
                     x = age,
                     y = prec_win),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat2) +
                 ggplot2::theme_classic() +
                 ggplot2::labs(
                   x = element_blank(),
                   y = element_blank(),
                   colour = expression(
                     paste(
                       'Lat ', (degree ~ N))
                     )
                   ) +
                 ggplot2::scale_x_continuous(
                   limits = c(0, 12000),
                   breaks = seq(0, 12000, 2000)) +
                 ggplot2::theme(
                   axis.title = element_text(
                     size = 22,
                     color = color_common),
                   axis.text = element_text(
                     size = 18,
                     color = color_common),
                   axis.text.x = element_text(
                     angle = 45,
                     hjust = 1),
                   legend.position = "right",
                   legend.background = element_rect(
                     fill = "transparent"),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               final_prec_winter <- 
                 ggpubr::ggarrange(
                   winter_precip_curve,
                   prec_winter_temporal,
                   ncol = 2,
                   nrow = 1) +
                 ggplot2::theme(
                   plot.margin = margin(1, 0, 0, 0.1, "cm"))  #t, r, b, l
               
               
          # Anthropogenic influence ----
               anthro_infl_curve <- 
                 .x %>% 
                 ggplot2::ggplot(
                   aes(
                     x = lat,
                     y = anth_ind_taxa,
                     group = age,
                     colour = age)
                   ) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) + 
                 ggplot2::scale_color_gradient(
                   high = color_high_age,
                   low = color_low_age) +
                 ggplot2::theme_classic() + 
                 ggplot2::geom_line(
                   aes(
                     x = lat,
                     y = anth_ind_taxa),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat1) +
                 ggplot2::labs(
                   x = expression(
                     paste(
                       'Latitude ', (degree ~ N))
                     ),
                   y = 'Anthr_infl',
                   colour = "Time \n(cal yr BP)") + 
                 ggplot2::theme(
                   axis.text.x = element_text(
                     color = color_common,
                     size = 18,
                     angle = 45,
                     hjust = 1),
                   axis.text.y = element_text(
                     color = color_common,
                     size = 18),
                   axis.title = element_text(
                     color = color_common,
                     size = 22),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               anthto_inf_temporal <- 
                 .x %>%
                 ggplot2::ggplot(
                   aes(
                     x = age,
                     y = anth_ind_taxa,
                     group = lat,
                     colour = lat)
                   ) +
                 ggplot2::scale_color_gradient(
                   high = color_high_lat,
                   low = color_low_lat) +
                 ggplot2::geom_line(
                   linewidth = 0.5,
                   alpha = 1) +
                 ggplot2::geom_line(
                   aes(
                     x = age,
                     y = anth_ind_taxa),
                   linewidth = 1.25,
                   colour = color_common,
                   data = dat2) +
                 ggplot2::theme_classic() +
                 ggplot2::labs(
                   x = "Time (cal yr BP)",
                   y = element_blank(),
                   colour = expression(
                     paste(
                       'Lat ', (degree ~ N))
                     )
                   ) +
                 ggplot2::scale_x_continuous(
                   limits = c(0, 12000),
                   breaks = seq(0, 12000, 2000)) +
                 ggplot2::theme(
                   axis.title = element_text(
                     size = 22,
                     color = color_common),
                   axis.text = element_text(
                     size = 18,
                     color = color_common),
                   axis.text.x = element_text(
                     angle = 45,
                     hjust = 1),
                   legend.position = "right",
                   legend.background = element_rect(
                     fill = "transparent"),
                   legend.title = element_text(
                     size = 14,
                     color = color_common),
                   legend.text = element_text(
                     size = 12,
                     color = color_common)
                   )
               
               final_anthro_inf <- 
                 ggpubr::ggarrange(
                   anthro_infl_curve,
                   anthto_inf_temporal,
                   ncol = 2,
                   nrow = 1) +
                 ggplot2::theme(
                   plot.margin = margin(1, 0, 0, 0.1, "cm"))  #t, r, b, l
               
              region <- as.character(.y) 
               
              final_figure <- 
                 ggpubr::annotate_figure(
                 ggpubr::ggarrange(
                   final_annual_temp,
                   final_temp_cold,
                   final_ann_prec,
                   final_prec_summer, 
                   final_prec_winter, 
                   final_anthro_inf,
                   nrow = 6,
                   labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                   font.label = list(size = 25),
                   hjust = -2,
                   vjust = 1),
                 top = ggpubr::text_grob(region, size = 28))
               
               return(final_figure)
               
               }
             )

#-----------------------------------------------#
# Save final climate trends ----
#-----------------------------------------------#
ggplot2::ggsave(
  plot_lat_climate[[1]],
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "europe_predictor_variable_trends_020225.tiff"),
  width = 25,
  height = 45,
  units = "cm",
  dpi = 400,
  compress = "lzw")


ggplot2::ggsave(
  plot_lat_climate[[2]],
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "north_america_predictor_variable_trends_020225.tiff"),
  width = 25,
  height = 45,
  units = "cm",
  dpi = 400,
  compress = "lzw")


# B. Longitudinal patterns ----

plot_long_climate <- 
  purrr::map2(.x = predictor_variables$matrix_long,
              .y = predictor_variables$region,
              .f = function(.x, .y){
                
                dat1 <- 
                  .x %>%
                  dplyr::group_by(long) %>%
                  dplyr::summarise_all(mean) %>% 
                  dplyr::ungroup()
                
                dat2 <- 
                  .x %>%
                  dplyr::group_by(age) %>%
                  dplyr::summarise_all(mean) %>% 
                  dplyr::ungroup()
                
                # Annual mean temperature ----
                annual_temp_curve <-
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = long,
                      y = temp_annual,
                      group = age,
                      colour = age)
                    ) +
                  
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::scale_color_gradient(
                    high = color_high_age,
                    low = color_low_age) +
                  ggplot2::theme_classic() +
                  ggplot2::geom_line(
                    aes(
                      x = long,
                      y = temp_annual),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat1) +
                  
                  ggplot2::labs(
                    x = element_blank(),
                    y = 'ann_temp',
                    colour = "Time \n(cal yr BP)") +
                  ggplot2::theme(
                    axis.text.x = element_text(
                      color = color_common,
                      size = 18,
                      angle = 45,
                      hjust = 1),
                    axis.text.y = element_text(
                      color = color_common,
                      size = 18),
                    axis.title = element_text(
                      color = color_common,
                      size = 22),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                annual_temp_temporal <-
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = age,
                      y = temp_annual,
                      group = long,
                      colour = long)) +
                  ggplot2::scale_color_gradient(
                    high = color_high_lat,
                    low = color_low_lat) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::geom_line(
                    aes(x = age,
                        y = temp_annual),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat2) +
                  ggplot2::theme_classic() +
                  ggplot2::labs(
                    x = element_blank(),
                    y = element_blank(),
                    colour = expression(
                      paste(
                        'Long ', (degree ~ E))
                      )
                    ) +
                  ggplot2::scale_x_continuous(
                    limits = c(0, 12000),
                    breaks = seq(0, 12000, 2000)) +
                  ggplot2::theme(
                    axis.title = element_text(
                      size = 22,
                      color = color_common),
                    axis.text = element_text(
                      size = 18,
                      color = color_common),
                    axis.text.x = element_text(
                      angle = 45,
                      hjust = 1),
                    legend.position = "right",
                    legend.background = element_rect(
                      fill = "transparent"),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )   
                
                final_annual_temp <- 
                  ggpubr::ggarrange(
                    annual_temp_curve,
                    annual_temp_temporal,
                    ncol = 2,
                    nrow = 1) +
                  ggplot2::theme(
                    plot.margin = margin(1, 0, 0, 0.1, "cm")) 
                
                
                # Minimum temperature of the coldest month ----
                temp_cold_curve <- 
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = long,
                      y = temp_cold,
                      group = age,
                      colour = age)
                    ) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) + 
                  ggplot2::scale_color_gradient(
                    high = color_high_age,
                    low = color_low_age) +
                  ggplot2::theme_classic() +
                  ggplot2::geom_line(
                    aes(x = long,
                        y = temp_cold),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat1) +
                  ggplot2::labs(
                    x = element_blank(),
                    y = 'temp_cold',
                    colour = "Time \n(cal yr BP)") + 
                  ggplot2::theme(
                    axis.text.x = element_text(
                      color = color_common,
                      size = 18,
                      angle = 45,
                      hjust = 1),
                    axis.text.y = element_text(
                      color = color_common,
                      size = 18),
                    axis.title = element_text(
                      color = color_common,
                      size = 22),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common))
                
                temp_cold_temporal <-
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = age,
                      y = temp_cold,
                      group = long,
                      colour = long)
                    ) +
                  ggplot2::scale_color_gradient(
                    high = color_high_lat,
                    low = color_low_lat) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::geom_line(
                    aes(
                      x = age,
                      y = temp_cold),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat2) +
                  ggplot2::theme_classic() +
                  ggplot2::labs(
                    x = element_blank(),
                    y = element_blank(),
                    colour = expression(
                      paste(
                        'Long ', (degree ~ E)))
                  ) +
                  ggplot2::scale_x_continuous(
                    limits = c(0, 12000),
                    breaks = seq(0, 12000, 2000)) +
                  ggplot2::theme(
                    axis.title = element_text(
                      size = 22,
                      color = color_common),
                    axis.text = element_text(
                      size = 18,
                      color = color_common),
                    axis.text.x = element_text(
                      angle = 45,
                      hjust = 1),
                    legend.position = "right",
                    legend.background = element_rect(
                      fill = "transparent"),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                  )   
                
                final_temp_cold <- 
                  ggpubr::ggarrange(
                    temp_cold_curve,
                    temp_cold_temporal,
                    ncol = 2,
                    nrow = 1) +
                  ggplot2::theme(
                    plot.margin = margin(1, 0, 0, 0.1, "cm")) 
                
                
                # Annual precipitation ----
                annual_precip_curve <- 
                  .x %>% 
                  ggplot2::ggplot(
                    aes(
                      x = long,
                      y = prec_annual,
                      group = age,
                      colour = age)
                    ) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) + 
                  ggplot2::scale_color_gradient(
                    high = color_high_age,
                    low = color_low_age) +
                  ggplot2::theme_classic() +
                  ggplot2::geom_line(
                    aes(
                      x = long,
                      y = prec_annual),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat1) +
                  ggplot2::labs(
                    x = element_blank(),
                    y = 'ann_prec',
                    colour = "Time \n(cal yr BP)") + 
                  ggplot2::theme(
                    axis.text.x = element_text(
                      color = color_common,
                      size = 18,
                      angle = 45,
                      hjust = 1),
                    axis.text.y = element_text(
                      color = color_common,
                      size = 18),
                    axis.title = element_text(
                      color = color_common,
                      size = 22),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                annual_precip_temporal <-
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = age,
                      y = prec_annual,
                      group = long,
                      colour = long)
                    ) +
                  ggplot2::scale_color_gradient(
                    high = color_high_lat,
                    low = color_low_lat) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::geom_line(
                    aes(
                      x = age,
                      y = prec_annual),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat2) +
                  ggplot2::theme_classic() +
                  ggplot2::labs(
                    x = element_blank(),
                    y = element_blank(),
                    colour = expression(
                      paste(
                        'Long ', (degree ~ E))
                      )
                    ) +
                  ggplot2::scale_x_continuous(
                    limits = c(0, 12000),
                    breaks = seq(0, 12000, 2000)) +
                  ggplot2::theme(
                    axis.title = element_text(
                      size = 22,
                      color = color_common),
                    axis.text = element_text(
                      size = 18,
                      color = color_common),
                    axis.text.x = element_text(
                      angle = 45,
                      hjust = 1),
                    legend.position = "right",
                    legend.background = element_rect(
                      fill = "transparent"),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common))   
                
                final_ann_prec <- 
                  ggpubr::ggarrange(
                    annual_precip_curve,
                    annual_precip_temporal,
                    ncol = 2,
                    nrow = 1) +
                  ggplot2::theme(
                    plot.margin = margin(1, 0, 0, 0.1, "cm")) 
                
                
                # Summer precipitation ----
                summer_precip_curve <- 
                  .x %>% 
                  ggplot2::ggplot(
                    aes(
                      x = long,
                      y = prec_summer,
                      group = age,
                      colour = age)
                    ) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) + 
                  ggplot2::scale_color_gradient(
                    high = color_high_age,
                    low = color_low_age) +
                  ggplot2::theme_classic() +
                  ggplot2::geom_line(
                    aes(
                      x = long,
                      y = prec_summer),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat1) +
                  ggplot2::labs(
                    x = element_blank(),
                    y = 'sum_prec',
                    colour = "Time \n(cal yr BP)") + 
                  ggplot2::theme(
                    axis.text.x = element_text(
                      color = color_common,
                      size = 18,
                      angle = 45,
                      hjust = 1),
                    axis.text.y = element_text(
                      color = color_common,
                      size = 18),
                    axis.title = element_text(
                      color = color_common,
                      size = 22),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                summer_precip_temporal <- 
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = age,
                      y = prec_summer,
                      group = long,
                      colour = long)
                    ) +
                  ggplot2::scale_color_gradient(
                    high = color_high_lat,
                    low = color_low_lat) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::geom_line(
                    aes(
                      x = age,
                      y = prec_summer),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat2) +
                  ggplot2::theme_classic() +
                  ggplot2::labs(
                    x = element_blank(),
                    y = element_blank(),
                    colour = expression(
                      paste(
                        'Long ', (degree ~ E))
                      )
                    ) +
                  ggplot2::scale_x_continuous(
                    limits = c(0, 12000),
                    breaks = seq(0, 12000, 2000)) +
                  ggplot2::theme(
                    axis.title = element_text(
                      size = 22,
                      color = color_common),
                    axis.text = element_text(
                      size = 18,
                      color = color_common),
                    axis.text.x = element_text(
                      angle = 45,
                      hjust = 1),
                    legend.position = "right",
                    legend.background = element_rect(
                      fill = "transparent"),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )   
                
                final_prec_summer <- 
                  ggpubr::ggarrange(
                    summer_precip_curve,
                    summer_precip_temporal,
                    ncol = 2,
                    nrow = 1) +
                  ggplot2::theme(
                    plot.margin = margin(1, 0, 0, 0.1, "cm"))     
                
                
                # Winter precipitation ----
                winter_precip_curve <- 
                  .x %>% 
                  ggplot2::ggplot(
                    aes(
                      x = long,
                      y = prec_win,
                      group = age,
                      colour = age)
                    ) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) + 
                  ggplot2::scale_color_gradient(
                    high = color_high_age,
                    low = color_low_age) +
                  ggplot2::theme_classic() + 
                  ggplot2::geom_line(
                    aes(
                      x = long,
                      y = prec_win),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat1) +
                  ggplot2::labs(
                    x = element_blank(),
                    y = 'wint_prec',
                    colour = "Time \n(cal yr BP)") + 
                  ggplot2::theme(
                    axis.text.x = element_text(
                      color = color_common,
                      size = 18,
                      angle = 45,
                      hjust = 1),
                    axis.text.y = element_text(
                      color = color_common,
                      size = 18),
                    axis.title = element_text(
                      color = color_common,
                      size = 22),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                prec_winter_temporal <- 
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = age,
                      y = prec_win,
                      group = long,
                      colour = long)
                    ) +
                  ggplot2::scale_color_gradient(
                    high = color_high_lat,
                    low = color_low_lat) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::geom_line(
                    aes(
                      x = age,
                      y = prec_win),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat2) +
                  ggplot2::theme_classic() +
                  ggplot2::labs(
                    x = element_blank(),
                    y = element_blank(),
                    colour = expression(
                      paste(
                        'Long ', (degree ~ E))
                      )
                    ) +
                  ggplot2::scale_x_continuous(
                    limits = c(0, 12000),
                    breaks = seq(0, 12000, 2000)) +
                  ggplot2::theme(
                    axis.title = element_text(
                      size = 22,
                      color = color_common),
                    axis.text = element_text(
                      size = 18,
                      color = color_common),
                    axis.text.x = element_text(
                      angle = 45,
                      hjust = 1),
                    legend.position = "right",
                    legend.background = element_rect(
                      fill = "transparent"),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                final_prec_winter <- 
                  ggpubr::ggarrange(
                    winter_precip_curve,
                    prec_winter_temporal,
                    ncol = 2,
                    nrow = 1) +
                  ggplot2::theme(
                    plot.margin = margin(1, 0, 0, 0.1, "cm"))  #t, r, b, l
                
                
                # Anthropogenic influence ----
                anthro_infl_curve <- 
                  .x %>% 
                  ggplot2::ggplot(
                    aes(
                      x = long,
                      y = anth_ind_taxa,
                      group = age,
                      colour = age)
                    ) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) + 
                  ggplot2::scale_color_gradient(
                    high = color_high_age,
                    low = color_low_age) +
                  ggplot2::theme_classic() + 
                  ggplot2::geom_line(
                    aes(
                      x = long,
                      y = anth_ind_taxa),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat1) +
                  ggplot2::labs(
                    x = expression(
                      paste(
                        'Longitude ', (degree ~ E))
                    ),
                    y = 'Anthr_infl',
                    colour = "Time \n(cal yr BP)") + 
                  ggplot2::theme(
                    axis.text.x = element_text(
                      color = color_common,
                      size = 18,
                      angle = 45,
                      hjust = 1),
                    axis.text.y = element_text(
                      color = color_common,
                      size = 18),
                    axis.title = element_text(
                      color = color_common,
                      size = 22),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                anthto_inf_temporal <- 
                  .x %>%
                  ggplot2::ggplot(
                    aes(
                      x = age,
                      y = anth_ind_taxa,
                      group = long,
                      colour = long)
                    ) +
                  ggplot2::scale_color_gradient(
                    high = color_high_lat,
                    low = color_low_lat) +
                  ggplot2::geom_line(
                    linewidth = 0.5,
                    alpha = 1) +
                  ggplot2::geom_line(
                    aes(
                      x = age,
                      y = anth_ind_taxa),
                    linewidth = 1.25,
                    colour = color_common,
                    data = dat2) +
                  ggplot2::theme_classic() +
                  ggplot2::labs(
                    x = "Time (cal yr BP)",
                    y = element_blank(),
                    colour = expression(
                      paste(
                        'Long ', (degree ~ E))
                    )
                  ) +
                  ggplot2::scale_x_continuous(
                    limits = c(0, 12000),
                    breaks = seq(0, 12000, 2000)) +
                  ggplot2::theme(
                    axis.title = element_text(
                      size = 22,
                      color = color_common),
                    axis.text = element_text(
                      size = 18,
                      color = color_common),
                    axis.text.x = element_text(
                      angle = 45,
                      hjust = 1),
                    legend.position = "right",
                    legend.background = element_rect(
                      fill = "transparent"),
                    legend.title = element_text(
                      size = 14,
                      color = color_common),
                    legend.text = element_text(
                      size = 12,
                      color = color_common)
                    )
                
                final_anthro_inf <- 
                  ggpubr::ggarrange(
                    anthro_infl_curve,
                    anthto_inf_temporal,
                    ncol = 2,
                    nrow = 1) +
                  ggplot2::theme(
                    plot.margin = margin(1, 0, 0, 0.1, "cm"))  #t, r, b, l
                
                region <- as.character(.y) 
                
                final_figure <- 
                  ggpubr::annotate_figure(
                    ggpubr::ggarrange(
                      final_annual_temp,
                      final_temp_cold,
                      final_ann_prec,
                      final_prec_summer, 
                      final_prec_winter, 
                      final_anthro_inf,
                      nrow = 6,
                      labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                      font.label = list(size = 25),
                      hjust = -2,
                      vjust = 1),
                    top = ggpubr::text_grob(region, size = 28))
                
                return(final_figure)
                }
              )

#-----------------------------------------------#
# Save final climate trends ----
#-----------------------------------------------#
ggplot2::ggsave(
  plot_long_climate[[1]],
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "europe_predictor_variable_longitudinal_trends_020225.tiff"),
  width = 25,
  height = 45,
  units = "cm",
  dpi = 400,
  compress = "lzw")


ggplot2::ggsave(
  plot_long_climate[[2]],
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "north_america_predictor_variable_longitudinal_trends_020225.tiff"),
  width = 25,
  height = 45,
  units = "cm",
  dpi = 400,
  compress = "lzw")

