#----------------------------------------------------------#
#
#   Phylogenetic assembly of angiosperms through space and time in Europe and
#                     North America in the Holocene
#
# Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                             2025
#
#----------------------------------------------------------#

#-----------------------------------#
# Extract taxa names and harmonise to APG family level ----
#-----------------------------------#

#--------------------------------------------------------#
# 1. Source configuration ----
#--------------------------------------------------------#
source("R/00_Config_file.R")


data_input_age_filtered <- 
  readr::read_rds("Inputs/Data/data_input_age_filtered_100624.rds")

taxa_europe <- 
  data_input_age_filtered %>% 
  dplyr::filter(region == "Europe") %>% 
  dplyr::select(counts_age_filtered) %>% 
  tidyr::unnest(counts_age_filtered) %>% 
  colnames(.) %>% 
  as_tibble() %>% 
  dplyr::rename(taxon_name = value) %>% 
  distinct(taxon_name) %>% 
  dplyr::arrange(taxon_name)

# readr::write_csv(taxa_europe,
#                 file = "Inputs/Tables/Harmonisation_tables/Europe_100624.csv")

taxa_na <- 
  data_input_age_filtered %>% 
  dplyr::filter(region == "North_America") %>% 
  dplyr::select(counts_age_filtered) %>% 
  tidyr::unnest(counts_age_filtered) %>% 
  colnames(.) %>% 
  as_tibble() %>% 
  dplyr::rename(taxon_name = value) %>% 
  distinct(taxon_name) %>% 
  dplyr::arrange(taxon_name)

# readr::write_csv(taxa_na,
#                 file = "Inputs/Tables/Harmonisation_tables/North_America_100624.csv")

#-----------------------------------#
# Prepare full harmonisation table ----
#-----------------------------------#
# add manually ecological group for each taxon 
# filter out gymnosperm taxa
# add APG family for each taxon

#-----------------------------------#
# Load harmonisation tables ----
#-----------------------------------#
eu_harmonisation_table <- 
  readr::read_csv("Inputs/Tables/Harmonisation_tables/Europe_100624.csv",
           show_col_types = FALSE)

eu_harmonisation_table %>% 
  dplyr::filter(group == "herb") #174
eu_harmonisation_table %>% 
  dplyr::filter(group == "trsh") #93


na_harmonisation_table <- 
  readr::read_csv("Inputs/Tables/Harmonisation_tables/North_America_100624.csv",
           show_col_types = FALSE)

na_harmonisation_table %>% 
  dplyr::filter(group == "herb") #130
na_harmonisation_table %>% 
  dplyr::filter(group == "trsh") #138

#-----------------------------------#
# Load age-filtered data ----
#-----------------------------------#
data_input_age_filtered <- 
  readr::read_rds("Inputs/Data/data_input_age_filtered_100624.rds")

#-----------------------------------#
# Harmonise the filtered counts to APG family-level ----
#-----------------------------------#
data_input_for_phylodiversity <- 
  data_input_age_filtered %>% 
  dplyr::mutate(counts_apg_harmonised = 
                  purrr::pmap(.l = list(dataset_id,
                                        region,
                                        counts_age_filtered),
                              .f = harmonise_taxa
                              ),
# Filter out the levels (rows) with < 3 taxa (otherwise such levels would produce 
# NA/NaN during phylogenetic diversity analyses).
                counts_apg_harmonised = 
                  purrr::map(
                    counts_apg_harmonised,
                    ~ .x[rowSums(.x > 0) > 2,] %>%
                      
# Remove columns (taxa) with zero colsums      
                      tibble::column_to_rownames("sample_id") %>%
                      dplyr::select_if(colSums(.) != 0) %>%
                      tibble::rownames_to_column("sample_id") %>%
                      tibble::as_tibble()
                      ),
                  
# Match samples in levels to those in filtered "counts_apg_harmonised"
                levels_age_filtered = purrr::map2(
                  .x = counts_apg_harmonised,
                  .y = levels_age_filtered,
                  .f = ~ .x %>% 
                    dplyr::select(sample_id) %>% 
                    inner_join(
                      .y,
                      by = "sample_id"
                      )
                  ),
                n_sample = purrr::map_dbl(
                  counts_apg_harmonised,
                  ~ nrow(.x)
                  )
     ) %>% 
  dplyr::filter(n_sample >= 3) %>%  
  dplyr::select(-n_sample)
                
suppressWarnings(
  data_input_for_phylodiversity %>%
    dplyr::mutate(test = purrr::map_lgl(
      counts_apg_harmonised,
      ~ ifelse(
        is.null( #testes also as 'is.na' and 'is.nan'
          any(
            colSums(.x %>%
                      column_to_rownames("sample_id")
            )
          )
        ), TRUE, FALSE
      )
    )
    )
  ) %>%
  dplyr::select(test) %>%
  dplyr::filter(test == "TRUE") # 0, OK!

#-----------------------------------#
# Save the harmonised data ----
#-----------------------------------#
readr::write_rds(
  data_input_for_phylodiversity,
  file = "Inputs/Data/data_input_for_phylodiversity_140624.rds",
  compress = "gz")       



