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


#----------------------------------------------------------#

# Geographic distribution of the datasets to be analysed ----
                         
#----------------------------------------------------------#

#--------------------------------------------------------#
# 1. Source config file ----
#--------------------------------------------------------#
source("R/00_Config_file.R")

#--------------------------------------------------------#
# 2. Load the data ----
#--------------------------------------------------------#
phylodiversity_estimated <- 
  readr::read_rds("Inputs/Data/data_pd_predictors_combined_filtered_141124.rds") 

europe_data <- 
  phylodiversity_estimated %>% 
  dplyr::filter(region == "Europe") %>% 
  dplyr::filter(vars == "ses_mpd") %>% 
  dplyr::select(filtered_data) %>% 
  tidyr::unnest(filtered_data) %>% 
  dplyr::group_by(region, dataset_id, lat, long) %>% 
  tidyr::nest() %>% 
  dplyr::ungroup() #151 datasets


north_america_data <- 
  phylodiversity_estimated %>% 
  dplyr::filter(region == "North_America") %>% 
  dplyr::filter(vars == "ses_mpd") %>% 
  dplyr::select(filtered_data) %>% 
  tidyr::unnest(filtered_data) %>% 
  dplyr::group_by(region, dataset_id, lat, long) %>% 
  tidyr::nest() %>% 
  dplyr::ungroup() #314 datasets

#--------------------------------------------------------#
# 3. Base map with Köppen-Geiger climate zones (in Beck et al. 2018) ----
#--------------------------------------------------------#
# Read the raster points from the geo-tiff file published in Beck et al. 2018
raster_file <- 
  raster::raster("Inputs/Data/Biomes_spatial/Beck_KG_V1_present_0p083.tif")
  
# Read the raster value-climatic zone tranlation table
koppen_tranlation_table <-
  readr::read_csv("Inputs/Data/Biomes_spatial/koppen_link.csv") %>% 
  dplyr::rename(cellvalue = raster_values)
    
# Extract the required raster points
# Reduce the raster dimension (by factor of 10), otherwise would be too big file
raster_file1 <- raster::aggregate(raster_file, fact = 10) 

raster_df <- 
  # Convert raster points into a dataframe
  tabularaster::as_tibble(raster_file, xy = TRUE) %>% 
  # Assign the names of climate zone to the raster values
  dplyr::left_join(koppen_tranlation_table, by = c("cellvalue")) %>% 
  dplyr::filter(!cellvalue == 0) %>% 
  dplyr::rename(
    ecozone_koppen_30 = genzone,
    ecozone_koppen_15 = genzone_cluster,
    ecozone_koppen_5 = broadbiome
    ) %>% 
  tidyr::drop_na()


# Base map
get_base_map <- 
  function(mapdata = mapdata) {
    
    map <- 
      mapdata %>% 
      ggplot2::ggplot(
        aes(
          x = long, 
          y = lat
          )
        ) +
      ggplot2::coord_fixed(
        ylim = c(min(mapdata$lat), max(mapdata$lat)),
        xlim = c(min(mapdata$long), max(mapdata$long))
        ) +
      ggplot2::geom_tile(
        data = raster_df,
        aes(
          x = x, 
          y = y, 
          fill = ecozone_koppen_15
          ),
        inherit.aes = FALSE,
        alpha = 0.75
        ) +
      ggplot2::scale_fill_manual(values = my_palette) +
      labs(
        x = expression(
          paste('Longitude ', (degree ~ E))
          ),
        y = expression(
          paste('Latitude ', (degree ~ N))
          ),
        fill = "Climate zones"
        ) +
      ggplot2::theme_classic() +
      borders(
        colour = "black",
        linewidth = 0.2
        ) +
      guides(
        fill = guide_legend(
          nrow = 2,
          byrow = TRUE,
          title.position = "top"
          ),
        size = guide_legend(
          nrow = 1,
          byrow = TRUE,
          title.position = "left"
          )
        ) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.key.size = unit(0.60, "cm"),
        legend.title = element_text(
          size = 14
          ),
        legend.text = element_text(
          size = 10
          ),
        axis.title = element_text(
          color = "black", 
          size = 
            ),
        axis.text = element_text(
          colour = "black", 
          size = 12
          )
        ) 
    
    # Distribution of sequences across ecozones 
    main_fig <- 
      map +
      ggplot2::geom_point(
        color = "red", 
        size = 1
        ) 
      
    return(main_fig)
  }


europe_data_coverage <- 
  get_base_map(
    mapdata = europe_data
    )

north_america_data_coverage <- 
  get_base_map(
    mapdata = north_america_data
    )
 

ggplot2::ggsave(
  europe_data_coverage,
  file = paste0("Outputs/Figures/sampled_lat_long_151124/",
                "europe_data_coverage_151124.tiff"),
  height = 16,
  width = 16,
  units = "cm",
  dpi = 400,
  compression = "lzw"
  )

ggplot2::ggsave(
  north_america_data_coverage,
  file = paste0("Outputs/Figures/sampled_lat_long_151124/",
                "north_america_data_coverage_151124.tiff"),
  height = 16,
  width = 16,
  units = "cm",
  dpi = 400,
  compression = "lzw"
  )

