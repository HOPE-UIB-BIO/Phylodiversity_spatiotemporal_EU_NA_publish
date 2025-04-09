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
# Plot the latitudinal/longitudinal-temporal pattern in PD ----
#                          
#-----------------------------------#
#--------------------------------------------------------#
# 1. Source configuration ----
#--------------------------------------------------------#
source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load GAM models ----
#--------------------------------------------------------#
gam_pd_latitudinal_temporal <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_latitudinal_temporal_221124.rds"))

gam_pd_longitudinal_temporal <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_longitudinal_temporal_221124.rds"))

# GAM summary shows 'mod3' (lat/long + time + lat/long:time) as the best-fit model
# To test if the latitudinal pattern in PD varied significantly with time during
#  the Holocene, we take 'mod4' and visualize it

#--------------------------------------------------------#
# 3. Extract model summary ----
#--------------------------------------------------------#
# 3.1 mod3: latitudinal model ----
gam_summary_latitudinal_mpd_europe <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[1, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_latitudinal_mpd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_mpd_europe.docx"))

gam_summary_latitudinal_mntd_europe <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[2, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_latitudinal_mntd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_mntd_europe.docx"))

gam_summary_latitudinal_mpd_NA <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[3, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_latitudinal_mpd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/",
                "latitudinal_mod_summary/",
                "latitudinal_mpd_NA.docx"))

gam_summary_latitudinal_mntd_NA <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[4, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_latitudinal_mntd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_mntd_NA.docx"))


# 3.2 mod4: latitudinal-temporal model ----
gam_summary_latitudinal_temporal_mpd_europe <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[1, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_latitudinal_temporal_mpd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_temporal_mpd_europe.docx"))

gam_summary_latitudinal_temporal_mntd_europe <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[2, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_latitudinal_temporal_mntd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_temporal_mntd_europe.docx"))

gam_summary_latitudinal_temporal_mpd_NA <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[3, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_latitudinal_temporal_mpd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_temporal_mpd_NA.docx"))

gam_summary_latitudinal_temporal_mntd_NA <-
  flextable::as_flextable(gam_pd_latitudinal_temporal[4, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_latitudinal_temporal_mntd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "latitudinal_mod_summary/",
                "latitudinal_temporal_mntd_NA.docx"))


# 3.3 mod3: longitudinal model ----
gam_summary_longitudinal_mpd_europe <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[1, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_longitudinal_mpd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_mpd_europe.docx"))

gam_summary_longitudinal_mntd_europe <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[2, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_longitudinal_mntd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_mntd_europe.docx"))

gam_summary_longitudinal_mpd_NA <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[3, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_longitudinal_mpd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_mpd_NA.docx"))

gam_summary_longitudinal_mntd_NA <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[4, ]$gam_models[[1]]$mod3)
flextable::save_as_docx(
  gam_summary_longitudinal_mntd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_mntd_NA.docx"))


# 3.4 mod4: longitudinal-temporal model ----
gam_summary_longitudinal_temporal_mpd_europe <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[1, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_longitudinal_temporal_mpd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_temporal_mpd_europe.docx"))

gam_summary_longitudinal_temporal_mntd_europe <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[2, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_longitudinal_temporal_mntd_europe,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_temporal_mntd_europe.docx"))

gam_summary_longitudinal_temporal_mpd_NA <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[3, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_longitudinal_temporal_mpd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_temporal_mpd_NA.docx"))

gam_summary_longitudinal_temporal_mntd_NA <-
  flextable::as_flextable(gam_pd_longitudinal_temporal[4, ]$gam_models[[1]]$mod4)
flextable::save_as_docx(
  gam_summary_longitudinal_temporal_mntd_NA,
  path = paste0("Outputs/Table/sampled_lat_long_151124/", 
                "longitudinal_mod_summary/",
                "longitudinal_temporal_mntd_NA.docx"))


#--------------------------------------------------------#
# 4. Predict models ----
#--------------------------------------------------------#

# 4.1 Latitudinal models predicted ----

gam_pd_latitudinal_temporal_predicted <- 
  gam_pd_latitudinal_temporal %>% 
  dplyr::mutate(
    mod_latitudinal_temporal = purrr::map(gam_models, ~ .x$mod3),
    mod_latitudinal_periodwise =  purrr::map(gam_models, ~ .x$mod4),
    predicted_gam_temporal_latitudewise =
      purrr::map2(
        .x = filtered_data, 
        .y = mod_latitudinal_temporal, 
        .f = ~ {
          
          new_data_gam_lat <-
            with(
              .x,
              base::expand.grid(
                lat = seq(min(lat), max(lat), by = 2),
                dataset_id = dataset_id[1],
                age = seq(0, 12000, by = 100))
              )
          
          not_inlude <-
            gratia::smooths(.y) %>%
            stringr::str_subset(., c("dataset_id"))
          
          crit <- qnorm((1 - 0.89) / 2, lower.tail = FALSE)
          
          set.seed(2037)
          
          predicted_mod_temporal_latitudewise <-
            new_data_gam_lat %>%
            dplyr::bind_cols(
              predict(
                .y,
                newdata = new_data_gam_lat,
                type = "response",
                se.fit = TRUE,
                exclude = not_inlude)
              ) %>%
            
            dplyr::mutate(
              var = fit,
              lwr = fit - (crit * se.fit),
              upr = fit + (crit * se.fit)) %>%
            
            dplyr::select(
              !dplyr::any_of(
                c("fit", "se.fit"))
              )
          
          return(predicted_mod_temporal_latitudewise)
          
        }
      ),
    
    predicted_gam_latitudinal_periodwise = 
      purrr::map2(
        .x = filtered_data, 
        .y = mod_latitudinal_periodwise, 
        .f = ~ {
          
          dat <-  
            .x %>% 
            dplyr::mutate_at("dataset_id", as.character) %>% 
            dplyr::arrange(age) %>% 
            dplyr::mutate(period = ceiling(age / 1000)) %>%
            dplyr::mutate(period = period * 1000) %>%
            dplyr::mutate_at("period", as.factor) %>% 
            dplyr::mutate_at("dataset_id", as.factor)
          
          new_data_gam <-
            with(
              dat,
              base::expand.grid(
                lat = seq(min(lat), max(lat), by = 0.5),
                dataset_id = dataset_id[1],
                period = seq(1000, 12000, by = 1000) %>% 
                  as_factor())
              )
          
          not_inlude <-
            gratia::smooths(.y) %>%
            str_subset(., "dataset_id|age")
          
          crit <- qnorm((1 - 0.89) / 2, lower.tail = FALSE)
          
          set.seed(2037)
          
          predicted_mod_latitudinal_periodwise <-
            new_data_gam %>%
            dplyr::bind_cols(
              predict(
                .y,
                newdata = new_data_gam,
                type = "response",
                se.fit = TRUE,
                exclude = not_inlude)
              ) %>%
            
            dplyr::mutate(
              var = fit,
              lwr = fit - (crit * se.fit),
              upr = fit + (crit * se.fit)) %>%
            
            dplyr::select(
              !dplyr::any_of(
                c("fit", "se.fit"))
              )
          
          return(predicted_mod_latitudinal_periodwise)
        }
      )
  )

readr::write_rds(
  gam_pd_latitudinal_temporal_predicted,
  file = paste0("Outputs/Data/sampled_lat_long_151124/",
                "gam_pd_latitudinal_temporal_predicted_262224.rds"),
  compress = "gz"
  )  


# 4.2 Longitudinal models predicted ----

gam_pd_longitudinal_temporal_predicted <- 
  gam_pd_longitudinal_temporal %>% 
  dplyr::mutate(
    mod_longitudinal_temporal = purrr::map(gam_models, ~ .x$mod3),
    mod_longitudinal_periodwise =  purrr::map(gam_models, ~ .x$mod4),
    predicted_gam_temporal_longitudewise =
      purrr::map2(
        .x = filtered_data, 
        .y = mod_longitudinal_temporal, 
        .f = ~ {
          
          new_data_gam_long <-
            with(
              .x,
              base::expand.grid(
                long = seq(min(long), max(long), by = 2),
                dataset_id = dataset_id[1],
                age = seq(0, 12000, by = 100))
              )
          
          not_inlude <-
            gratia::smooths(.y) %>%
            stringr::str_subset(., c("dataset_id"))
          
          crit <- qnorm((1 - 0.89) / 2, lower.tail = FALSE)
          
          set.seed(2037)
          
          predicted_mod_temporal_longitudewise <-
            new_data_gam_long %>%
            dplyr::bind_cols(
              predict(
                .y,
                newdata = new_data_gam_long,
                type = "response",
                se.fit = TRUE,
                exclude = not_inlude)
              ) %>%
            
            dplyr::mutate(
              var = fit,
              lwr = fit - (crit * se.fit),
              upr = fit + (crit * se.fit)) %>%
            
            dplyr::select(
              !dplyr::any_of(
                c("fit", "se.fit"))
              )
          
          return(predicted_mod_temporal_longitudewise)
          
        }
      ),
    
    predicted_gam_longitudinal_periodwise = 
      purrr::map2(
        .x = filtered_data, 
        .y = mod_longitudinal_periodwise, 
        .f = ~ {
          
          dat <-  
            .x %>% 
            dplyr::mutate_at("dataset_id", as.character) %>% 
            dplyr::arrange(age) %>% 
            dplyr::mutate(period = ceiling(age / 1000)) %>%
            dplyr::mutate(period = period * 1000) %>%
            dplyr::mutate_at("period", as.factor) %>% 
            dplyr::mutate_at("dataset_id", as.factor)
          
          new_data_gam <-
            with(
              dat,
              base::expand.grid(
                long = seq(min(long), max(long), by = 0.5),
                dataset_id = dataset_id[1],
                period = seq(1000, 12000, by = 1000) %>% 
                  as_factor())
              )
          
          not_inlude <-
            gratia::smooths(.y) %>%
            str_subset(., "dataset_id|age")
          
          crit <- qnorm((1 - 0.89) / 2, lower.tail = FALSE)
          
          set.seed(2037)
          
          predicted_mod_longitudinal_periodwise <-
            new_data_gam %>%
            dplyr::bind_cols(
              predict(
                .y,
                newdata = new_data_gam,
                type = "response",
                se.fit = TRUE,
                exclude = not_inlude)
              ) %>%
            
            dplyr::mutate(
              var = fit,
              lwr = fit - (crit * se.fit),
              upr = fit + (crit * se.fit)) %>%
            
            dplyr::select(
              !dplyr::any_of(
                c("fit", "se.fit"))
              )
          
          return(predicted_mod_longitudinal_periodwise)
        }
      )
  )

readr::write_rds(
  gam_pd_longitudinal_temporal_predicted,
  file = paste0("Outputs/Data/sampled_lat_long_151124/",
                "gam_pd_longitudinal_temporal_predicted_262224.rds"),
  compress = "gz"
  )  


#--------------------------------------------------------#
# 5. Plot GAM models ----
#--------------------------------------------------------#

# A. Latitudinal patterns ----

gam_pd_latitudinal_temporal_predicted <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
           "gam_pd_latitudinal_temporal_predicted_262224.rds"))


# 5.1 Latitudinal patterns across time periods during the Holocene ----

plot_latitudinal_patterns_periodwise <- 
  purrr::map2(
    .x = gam_pd_latitudinal_temporal_predicted$predicted_gam_latitudinal_periodwise,
    .y = gam_pd_latitudinal_temporal_predicted$vars,
    .f = ~ {
      data_overall_trend <- 
        .x %>% 
        dplyr::select(-c(dataset_id, period)) %>% #summarizing factorial period produces error
        dplyr::group_by(lat) %>%
        dplyr::summarise_all(list(mean)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = NA) #column of period is required to add general trend line in the figure
      
      dat <- 
        .x %>% 
        dplyr::mutate(period = as.numeric(as.character(period)))
      #to assign selected gradient of color to factorial 'period', converted as a continuous variable
        
      ggplot2::ggplot(
        aes(
        x = lat,
        y = var,
        group = period,
        colour = period), 
        data = dat) +
        
        ggplot2::theme_classic() +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::geom_ribbon(
          aes(
            ymin = lwr,
            ymax = upr,
            fill = period),
          alpha = 1/8, 
          colour = NA) + 
        
        ggplot2::scale_color_gradient(
          high = color_high_age, 
          low = color_low_age) + 
        
        ggplot2::scale_fill_gradient(
          high = color_high_age,
          low = color_low_age) +
        
        ggplot2::geom_line(
          aes(x = lat,
              y = var),
          linewidth = 1.5,
          colour = color_pd_curve,
          data = data_overall_trend ) +
        
        ggplot2::labs(
          fill = "Time (cal yr BP)",
          x = expression(
            paste(
              'Latitude ', (degree ~ N))
            ),
          y = paste(.y)) +
        
        ggplot2::theme(
          axis.title = element_text(
            size = 16,
            color = color_common),
          axis.text = element_text(
            size = 14, 
            color = color_common),
          #legend.position = "bottom",
          legend.position = "inside",
          legend.position.inside = c(0.5, 0.90),
          legend.background = element_rect(
            fill = "transparent"),
          legend.spacing.x = unit(0.55, "cm"),
          legend.title = element_text(
            size = 11),
          legend.text = element_text(
            size = 10,
            color = color_common),
          legend.direction = "horizontal",
          axis.title.x = element_text(
            margin = margin(t = -15, r = 0, b = -15, l = 0))
          #legend.margin = margin(-10, 0, 0, 0)
          ) + 
        
        ggplot2::guides(
          colour = "none",
          fill = guide_colorbar(
            barheight = 0.6, 
            barwidth = 7)
        )
        }
    )


mpd_latitudinal_europe <- 
  plot_latitudinal_patterns_periodwise[[1]] +
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_latitudinal_europe <- 
  plot_latitudinal_patterns_periodwise[[2]] +
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mpd_latitudinal_NA <- 
  plot_latitudinal_patterns_periodwise[[3]] +
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_latitudinal_NA <- 
  plot_latitudinal_patterns_periodwise[[4]] +
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 


# 5.2 Temporal patterns across latitude ----

plot_temporal_patterns_latitudewise <-
  purrr::map2(
    .x = gam_pd_latitudinal_temporal_predicted$predicted_gam_temporal_latitudewise,
    .y = gam_pd_latitudinal_temporal_predicted$vars,
    
    .f = ~ {
      dat <-
        .x %>%
        dplyr::select(-dataset_id)
      
      dat1 <-
        .x %>%
        dplyr::select(-dataset_id) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise_all(list(mean)) %>% 
        dplyr::ungroup()
      
      ggplot2::ggplot(
        aes(
          x = age,
          y = var,
          group = lat,
          colour = lat),
        data = dat) +
        
        ggplot2::geom_line(
          linewidth = 0.3) +
        
        ggplot2::scale_color_gradient(
          high = color_high_lat,
          low = color_low_lat) +
        
        ggplot2::geom_line(
          aes(
            x = age,
            y = var),
          linewidth = 1.5,
          colour = color_pd_curve,
          data = dat1) +
        
        ggplot2::theme_classic() +
        
        ggplot2::labs(
          x = "Time (cal yr BP)",
          y = paste(.y),
          colour = expression(
            paste(
              'Latitude ', (degree ~ N))
            )
          ) +
        
        ggplot2::scale_x_reverse(
          limits = c(12000, 0),
          breaks = seq(12000, 0, by = -2000)) +
        
        ggplot2::theme(
          axis.title = element_text(
            size = 16,
            color = color_common),
          axis.text = element_text(
            size = 14,
            color = color_common),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1),
          legend.position = "inside",
          legend.position.inside = c(0.6, 0.85),
          legend.background = element_rect(
            fill = "transparent"),
          legend.title = element_text(
            size = 12,
            color = color_common),
          legend.text = element_text(
            size = 11,
            color = color_common)
          #plot.margin = margin(0.1, 0.1, -0.5, 0.1, "cm")
          ) +
        
        ggplot2::guides(
          colour = guide_colorbar(
            barheight = 4,
            barwidth = 0.6)
          )
      }
    )


mpd_temporal_latitudinal_europe <- 
  plot_temporal_patterns_latitudewise[[1]] + 
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_temporal_latitudinal_europe <- 
  plot_temporal_patterns_latitudewise[[2]] + 
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mpd_temporal_latitudinal_NA <- 
  plot_temporal_patterns_latitudewise[[3]] + 
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 
  
mntd_temporal_latitudinal_NA <- 
  plot_temporal_patterns_latitudewise[[4]] + 
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 


# B. Longitudinal patterns ----
gam_pd_longitudinal_temporal_predicted <- 
  readr::read_rds(
    paste0("Outputs/Data/sampled_lat_long_151124/",
    "gam_pd_longitudinal_temporal_predicted_262224.rds"))


# 5.3 Longitudinal patterns across time periods during the Holocene ----

plot_longitudinal_patterns_periodwise <- 
  purrr::map2(
    .x = gam_pd_longitudinal_temporal_predicted$predicted_gam_longitudinal_periodwise,
    .y = gam_pd_longitudinal_temporal_predicted$vars,
    .f = ~ {
      data_overall_trend <- 
        .x %>% 
        dplyr::select(-c(dataset_id, period)) %>%
        dplyr::group_by(long) %>%
        dplyr::summarise_all(list(mean)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = NA)
      
      dat <- 
        .x %>% 
        dplyr::mutate(period = as.numeric(as.character(period)))
        
      ggplot2::ggplot(
        aes(
          x = long,
          y = var,
          group = period,
          colour = period),
        data = dat) +
        
        ggplot2::theme_classic() +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::geom_ribbon(
          aes(
            ymin = lwr,
            ymax = upr,
            fill = period),
          alpha = 1/8, 
          colour = NA) + 
        
        ggplot2::scale_color_gradient(
          high = color_high_age, 
          low = color_low_age) + 
        
        ggplot2::scale_fill_gradient(
          high = color_high_age,
          low = color_low_age) +
        
        ggplot2::geom_line(
          aes(x = long,
              y = var),
          linewidth = 1.5,
          colour = color_pd_curve,
          data = data_overall_trend) +
        
        ggplot2::labs(
          fill = "Time (cal yr BP)",
          x = expression(
            paste(
              'Longitude ', (degree ~ E))
            ),
          y = paste(.y)) +
        
        ggplot2::theme(
          axis.title = element_text(
            size = 16,
            color = color_common),
          axis.text = element_text(
            size = 14, 
            color = color_common),
          #legend.position = "bottom",
          legend.position = "inside",
          legend.position.inside = c(0.5, 0.9),
          legend.background = element_rect(
            fill = "transparent"),
          legend.spacing.x = unit(0.55, "cm"),
          legend.title = element_text(
            size = 11),
          legend.text = element_text(
            size = 10,
            color = color_common),
          legend.direction = "horizontal",
          axis.title.x = element_text(
            margin = margin(t = -15, r = 0, b = -15, l = 0))
          ) + 
        
        ggplot2::guides(
          colour = "none",
          fill = guide_colorbar(
            barheight = 0.6, 
            barwidth = 7)
        )
      }
  )


mpd_longitudinal_europe <- 
  plot_longitudinal_patterns_periodwise[[1]] +
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_longitudinal_europe <- 
  plot_longitudinal_patterns_periodwise[[2]] +
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mpd_longitudinal_NA <- 
  plot_longitudinal_patterns_periodwise[[3]] +
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_longitudinal_NA <- 
  plot_longitudinal_patterns_periodwise[[4]] +
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 


# 5.4 Temporal patterns across longitude ----
plot_temporal_patterns_longitudewise <-
  purrr::map2(
    .x = gam_pd_longitudinal_temporal_predicted$predicted_gam_temporal_longitudewise,
    .y = gam_pd_longitudinal_temporal_predicted$vars,
    
    .f = ~ {
      dat <-
        .x %>%
        dplyr::select(-dataset_id)
      
      dat1 <-
        .x %>%
        dplyr::select(-dataset_id) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise_all(list(mean)) %>% 
        dplyr::ungroup()
      
      ggplot2::ggplot(
        aes(
          x = age,
          y = var,
          group = long,
          colour = long),
        data = dat) +
        
        ggplot2::geom_line(
          linewidth = 0.3) +
        
        ggplot2::scale_color_gradient(
          high = color_high_lat,
          low = color_low_lat) +
        
        ggplot2::geom_line(
          aes(
            x = age,
            y = var),
          linewidth = 1.5,
          colour = color_pd_curve,
          data = dat1) +
        
        ggplot2::theme_classic() +
        
        ggplot2::labs(
          x = "Time (cal yr BP)",
          y = paste(.y),
          colour = expression(
            paste(
              'Longitude ', (degree ~ E))
          )
        ) +
        
        ggplot2::scale_x_reverse(
          limits = c(12000, 0),
          breaks = seq(12000, 0, by = -2000)) +
        
        ggplot2::theme(
          axis.title = element_text(
            size = 16,
            color = color_common),
          axis.text = element_text(
            size = 14,
            color = color_common),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1),
          legend.position = "inside",
          legend.position.inside = c(0.6, 0.85),
          legend.background = element_rect(
            fill = "transparent"),
          legend.title = element_text(
            size = 12,
            color = color_common),
          legend.text = element_text(
            size = 11,
            color = color_common)
          #plot.margin = margin(0.1, 0.1, -0.5, 0.1, "cm")
        ) +
        
        ggplot2::guides(
          colour = guide_colorbar(
            barheight = 4,
            barwidth = 0.6)
        )
    }
  )


mpd_temporal_longitudinal_europe <- 
  plot_temporal_patterns_longitudewise[[1]] + 
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_temporal_longitudinal_europe <- 
  plot_temporal_patterns_longitudewise[[2]] + 
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mpd_temporal_longitudinal_NA <- 
  plot_temporal_patterns_longitudewise[[3]] + 
  ggplot2::labs(y = "ses_MPD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 

mntd_temporal_longitudinal_NA <- 
  plot_temporal_patterns_longitudewise[[4]] + 
  ggplot2::labs(y = "ses_MNTD") +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) 


# 5.5 Composite figures ----
# Figure 1 ----
# Fig. 1a. mpd_latitudinal_europe ----
# Fig. 1b. mpd_temporal_latitudinal_europe ----
# Fig. 1c. mpd_longitudinal_europe ----
# Fig. 1d. mpd_temporal_longitudinal_europe ----

europe_mpd <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
    mpd_latitudinal_europe,
    mpd_temporal_latitudinal_europe,
    ncol = 1,
    nrow = 2,
    labels = c("(a)", "(b)"),
    label.x = -0.03,
    align = "h",
    font.label = list(size = 18)),
    
    ggpubr::ggarrange(
    mpd_longitudinal_europe,
    mpd_temporal_longitudinal_europe,
    ncol = 1,
    nrow = 2,
    labels = c("(c)", "(d)"),
    label.x = -0.03,
    align = "h",
    font.label = list(size = 18)),
    ncol = 2,
    nrow = 1)

ggplot2::ggsave(
  europe_mpd,
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "ses_mpd_europe.tiff"),
  height = 20,
  width = 20,
  units = "cm",
  dpi = 400,
  compression = "lzw")


# Figure2 ----
# Fig. 2a. mntd_latitudinal_europe ----
# Fig. 2b. mntd_temporal_latitudinal_europe ----
# Fig. 2c. mntd_longitudinal_europe ----
# Fig. 2d. mntd_temporal_longitudinal_europe ----

europe_mntd <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      mntd_latitudinal_europe,
      mntd_temporal_latitudinal_europe,
      ncol = 1,
      nrow = 2,
      labels = c("(a)", "(b)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 18)),
    
    ggpubr::ggarrange(
      mntd_longitudinal_europe,
      mntd_temporal_longitudinal_europe,
      ncol = 1,
      nrow = 2,
      labels = c("(c)", "(d)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 18)),
    ncol = 2,
    nrow = 1)

ggplot2::ggsave(
  europe_mntd,
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "ses_mntd_europe.tiff"),
  height = 20,
  width = 20,
  units = "cm",
  dpi = 400,
  compression = "lzw")


# Figure 3 ----
# Fig. 3a. mpd_latitudinal_NA ----
# Fig. 3b. mpd_temporal_latitudinal_NA ----
# Fig. 3c. mpd_longitudinal_NA ----
# Fig. 3d. mpd_temporal_longitudinal_NA ----
NA_mpd <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      mpd_latitudinal_NA,
      mpd_temporal_latitudinal_NA,
      ncol = 1,
      nrow = 2,
      labels = c("(a)", "(b)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 18)),
    
    ggpubr::ggarrange(
      mpd_longitudinal_NA,
      mpd_temporal_longitudinal_NA,
      ncol = 1,
      nrow = 2,
      labels = c("(c)", "(d)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 18)),
    ncol = 2,
    nrow = 1)

ggplot2::ggsave(
  NA_mpd,
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "ses_mpd_North_America.tiff"),
  height = 20,
  width = 20,
  units = "cm",
  dpi = 400,
  compression = "lzw")


# Figure 4 ----
# Fig. 4a. mntd_latitudinal_NA ----
# Fig. 4b. mntd_temporal_latitudinal_NA ----
# Fig. 4c. mntd_longitudinal_NA ----
# Fig. 4d. mntd_temporal_longitudinal_NA ----

NA_mntd <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      mntd_latitudinal_NA,
      mntd_temporal_latitudinal_NA,
      ncol = 1,
      nrow = 2,
      labels = c("(a)", "(b)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 18)),
    
    ggpubr::ggarrange(
      mntd_longitudinal_NA,
      mntd_temporal_longitudinal_NA,
      ncol = 1,
      nrow = 2,
      labels = c("(c)", "(d)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 18)),
    ncol = 2,
    nrow = 1)

ggplot2::ggsave(
  NA_mntd,
  filename = paste0("Outputs/Figures/sampled_lat_long_151124/",
                    "ses_mntd_North_America.tiff"),
  height = 20,
  width = 20,
  units = "cm",
  dpi = 400,
  compression = "lzw")
