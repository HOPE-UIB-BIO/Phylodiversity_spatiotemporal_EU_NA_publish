#----------------------------------------------------------#
#
# Spatio-temporal patterns of phylogenetic diversity in Europe and North America
#                           during the Holocene
#
#                            Kuber Bhatta
#                                2024
#
#----------------------------------------------------------#

# Number of taxa per sample and per sequence ----

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

data_pd_predictors_combined <- data_pd_predictors_combined[c(1,3),]

min_lat_eu <- min(data_pd_predictors_combined[1,]$filtered_data[[1]]$lat) # 37.60
max_lat_eu <- max(data_pd_predictors_combined[1,]$filtered_data[[1]]$lat) # 71.07
#low_lat: 37-48
#mid_lat: 48-60
#high_lat: 60-71.07

min_long_eu <- min(data_pd_predictors_combined[1,]$filtered_data[[1]]$long) # 6.16
max_long_eu <- max(data_pd_predictors_combined[1,]$filtered_data[[1]]$long) # 33.85
#min_long: 6.16-15
#mid_long: 15-25
#max_long: 25-33.85


min_lat_na <- min(data_pd_predictors_combined[2,]$filtered_data[[1]]$lat) # 27.20
max_lat_na <- max(data_pd_predictors_combined[2,]$filtered_data[[1]]$lat) # 73.46
#low_lat: 27-40
#mid_lat: 40-55
#high_lat: 55-71.07

min_long_na <- min(data_pd_predictors_combined[2,]$filtered_data[[1]]$long) # -129.83
max_long_na <- max(data_pd_predictors_combined[2,]$filtered_data[[1]]$long) # -52.66

#min_long: -52 - -77
#mid_long: -77 - -105
#max_long: -105 - -129.82

zonation_eu <- 
  data_pd_predictors_combined %>% 
  dplyr::select(region, filtered_data) %>% 
  dplyr::filter(region == "Europe") %>% 
  dplyr::mutate(filtered_data = 
                  purrr::map(.x = filtered_data,
                             ~ .x %>% 
                               dplyr::mutate(
                                 lat_zone = dplyr::case_when(
                                 lat <= 48 ~ "low_lat",
                                 lat > 48 & lat < 60 ~ "mid_lat",
                                 TRUE ~ "high_lat"),
                                 
                                 long_zone = dplyr::case_when(
                                   long <= 15 ~ "low_long",
                                   long > 15 & long < 25 ~ "mid_long",
                                   TRUE ~ "high_long")
                                 )
                             )
                ) 

zonation_na <- 
  data_pd_predictors_combined %>% 
  dplyr::select(region, filtered_data) %>% 
  dplyr::filter(region == "North_America") %>% 
  dplyr::mutate(filtered_data = 
                  purrr::map(.x = filtered_data,
                             ~ .x %>% 
                               dplyr::mutate(
                                 lat_zone = dplyr::case_when(
                                   lat <= 40 ~ "low_lat",
                                   lat > 40 & lat < 55 ~ "mid_lat",
                                   TRUE ~ "high_lat"),
                                 
                                 long_zone = dplyr::case_when(
                                   long >= -77 ~ "high_long",
                                   long > -105 & long < -77 ~ "mid_long",
                                   TRUE ~ "low_long")
                                 )
                             )
                ) 

lat_long_zonation_pd <- 
  bind_rows(zonation_eu, zonation_na) %>% 
  dplyr::select(-region) %>% 
  tidyr::unnest(filtered_data) %>% 
  dplyr::group_by(dataset_id, lat, long, region) %>% 
  tidyr::nest(.key = "filtered_data") %>% 
  dplyr::ungroup()

full_dat_counts <- 
  readr::read_rds("Inputs/Data/Phylodiversity_estimated_260624.rds") %>% 
  dplyr::select(dataset_id, lat, long, region, counts_apg_harmonised)

lat_long_zonation_pd_combined <- 
  lat_long_zonation_pd %>% 
  dplyr::left_join(full_dat_counts,
                   by = c("dataset_id", "lat", "long", "region")
                   ) %>% 
  dplyr::mutate(counts_apg_harmonised = 
                  purrr::map2(
                    .x = filtered_data,
                    .y = counts_apg_harmonised,
                    .f = ~ .y %>% 
                      dplyr::filter(sample_id %in% .x$sample_id)
                    ),
                sample_richness = purrr::map(
                  counts_apg_harmonised,
                  ~.x %>%
                    column_to_rownames("sample_id") %>% 
                    dplyr::mutate(
                      across(everything(), ~ ifelse(. > 1, 1, 0))
                      ) %>% 
                    dplyr::mutate(sample_richness = rowSums(.))%>% 
                    rownames_to_column("sample_id") %>% 
                    dplyr::select(sample_richness)
                  )
                ) %>% 
  dplyr::select(-counts_apg_harmonised) %>% 
  tidyr::unnest(c(filtered_data, sample_richness)) %>% 
  dplyr::select(region, lat, long, lat_zone, long_zone, sample_richness) %>% 
  dplyr::group_by(region) %>% 
  tidyr::nest(.key = "data") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(data = 
                  purrr::map(.x = data,
                             ~ .x %>% 
                               dplyr::arrange(lat) %>% 
                               dplyr::mutate(lat_bin = ceiling(lat / 2)) %>%
                               dplyr::mutate(lat_bin = lat_bin * 2) %>% 
                               dplyr::arrange(long) %>% 
                               dplyr::mutate(long_bin = ceiling(long / 2)) %>%
                               dplyr::mutate(long_bin = long_bin * 2)
                             )
                )

# Per zone richness ----
lat_zone <- 
  lat_long_zonation_pd_combined %>% 
  dplyr::mutate(mean_richness = 
                  purrr::map(data,
                             ~.x %>% 
                               dplyr::select(lat_zone, sample_richness) %>% 
                               dplyr::group_by(lat_zone) %>% 
                               summarise_all(mean) %>% 
                               dplyr::ungroup())
                ) %>% 
  dplyr::select(region, mean_richness) %>% 
  tidyr::unnest(mean_richness)

#region        lat_zone sample_richness
#<chr>         <chr>              <dbl>
#  1 Europe        high_lat         8.59
#2 Europe        low_lat            13.6 
#3 Europe        mid_lat            11.4 
#4 North_America high_lat            6.48
#5 North_America low_lat             8.97
#6 North_America mid_lat             8.39

long_zone <- 
  lat_long_zonation_pd_combined %>% 
  dplyr::mutate(mean_richness = 
                  purrr::map(data,
                             ~.x %>% 
                               dplyr::select(long_zone, sample_richness) %>% 
                               dplyr::group_by(long_zone) %>% 
                               summarise_all(mean) %>% 
                               dplyr::ungroup())
                ) %>% 
  dplyr::select(region, mean_richness) %>% 
  tidyr::unnest(mean_richness)

#region        long_zone sample_richness
#  1 Europe        high_long            8.45
#2 Europe        low_long            12.5 
#3 Europe        mid_long            11.5 
#4 North_America high_long            8.30
#5 North_America low_long             6.81
#6 North_America mid_long             9.86

  
plot_latitudinal_sample_richness <-
  purrr::map2(
    .x = lat_long_zonation_pd_combined$data,
    .y = lat_long_zonation_pd_combined$region,
    .f = ~ {
      
      dat <-
        .x %>%
        dplyr::select(lat_bin, sample_richness) %>% 
        dplyr::group_by(lat_bin) %>%
        dplyr::summarise_all(list(mean)) %>% 
        dplyr::ungroup()
      
      ggplot2::ggplot(
        aes(
          x = lat_bin,
          y = sample_richness 
          ),
        data = dat
        ) +
        
        #ggplot2::ggtitle(paste(.y)) +
        
        ggplot2::geom_smooth(
          stat = "smooth",
          method = "gam",
          linewidth = 1.5,
          se = FALSE,
          colour = color_pd_curve
          ) +
        
        ggplot2::geom_point(colour = color_common) +
        
        ggplot2::theme_classic() +
        
        ggplot2::labs(
          x = expression(
            paste(
              'Latitude ', (degree ~ N))
            ),
          y = ""
          ) +
        
        ggplot2::theme(
          axis.title = element_text(
            size = 16,
            color = color_common
            ),
          axis.text = element_text(
            size = 14,
            color = color_common
            )
        ) 
    }
  )

plot_longitudinal_sample_richness <-
  purrr::map2(
    .x = lat_long_zonation_pd_combined$data,
    .y = lat_long_zonation_pd_combined$region,
    .f = ~ {
      
      dat <-
        .x %>%
        dplyr::select(long_bin, sample_richness) %>% 
        dplyr::group_by(long_bin) %>%
        dplyr::summarise_all(list(mean)) %>% 
        dplyr::ungroup()
      
      ggplot2::ggplot(
        aes(
          x = long_bin,
          y = sample_richness
          ),
        data = dat
        ) +
        
        #ggplot2::ggtitle(paste(.y)) +
        
        ggplot2::geom_smooth(
          stat = "smooth",
          method = "gam",
          linewidth = 1.5,
          se = FALSE,
          colour = color_pd_curve
          ) +
        
        ggplot2::geom_point(colour = color_common) +
        
        ggplot2::theme_classic() +
        
        ggplot2::labs(
          x = expression(
            paste(
              'Longitude ', (degree ~ E))
            ),
          y = "") +
        
        ggplot2::theme(
          axis.title = element_text(
            size = 16,
            color = color_common
            ),
          axis.text = element_text(
            size = 14,
            color = color_common
            )
        ) 
      }
    )

lat_eu <- plot_latitudinal_sample_richness[[1]]
lat_na <- plot_latitudinal_sample_richness[[2]]

long_eu <- plot_longitudinal_sample_richness[[1]]
long_na <- plot_longitudinal_sample_richness[[2]]

eu_composite <- 
  ggpubr::annotate_figure(
  ggpubr::ggarrange(
    lat_eu,
    long_eu,
    ncol = 1,
    nrow = 2,
    labels = c("(a)", "(b)"),
    label.x = -0.03,
    align = "h",
    font.label = list(size = 16)),
  top = ggpubr::text_grob("Europe", size = 18),
  left = ggpubr::text_grob("Family richness", size = 17, rot = 90))

na_composite <- 
  ggpubr::annotate_figure(
    ggpubr::ggarrange(
      lat_na,
      long_na,
      ncol = 1,
      nrow = 2,
      labels = c("(c)", "(d)"),
      label.x = -0.03,
      align = "h",
      font.label = list(size = 16)),
    top = ggpubr::text_grob("North America", size = 18))

final_composite <-  
  ggpubr::ggarrange(
    eu_composite,
    na_composite,
    ncol = 2,
    nrow = 1)

ggplot2::ggsave(final_composite,
                file = paste0("Outputs/Figures/sampled_lat_long_151124/",
                              "Family_richness_020225.tiff"),
                width = 30,
                height = 15,
                units = "cm",
                dpi = 400,
                compress = "lzw")  
