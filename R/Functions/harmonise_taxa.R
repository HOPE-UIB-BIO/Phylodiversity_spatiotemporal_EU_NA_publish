#----------------------------------------------------------#
#
# Spatio-temporal patterns of phylogenetic diversity in Europe and North America
#                           during the Holocene
#
#                            Kuber Bhatta
#                                2024
#
#----------------------------------------------------------#

#-----------------------------------#
# Harmonise taxa to APG family-level ----
#-----------------------------------#
harmonise_taxa <-
  function(id = dataset_id,
           region = region,
           counts = counts_age_filtered) {
    
    harm_table <-
      readr::read_csv(
        paste(
          "Inputs/Tables/Harmonisation_tables/", 
          region,
          "_100624",
          ".csv", 
          sep = ""
        ),
        show_col_types = FALSE
      )
    
    message(
      msg = paste0(
        "dataset_id = ",
        id,
        ": ",
        region,
        ", ",
        paste(
          "Inputs/Tables/Harmonisation_tables/", 
          region,
          "_100624",
          ".csv",
          sep = ""
        ),
        "\n"
      )
    )
    # transform all taxa to numeric
    col_names <- names(counts)
    taxa_names <- col_names[!col_names %in% "sample_id"]
    
    suppressWarnings(dat <-
                       counts %>%
                       dplyr::mutate_at(
                         taxa_names,
                         as.numeric
                       )
                     )
    res <- 
      dat %>%
      as.data.frame() %>%
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ tidyr::replace_na(., 0)
        )
      ) %>%
      tidyr::gather(
        key = taxon_name, 
        value = counts,
        -sample_id
        ) %>%
      dplyr::left_join(
        harm_table, 
        by = "taxon_name"
        ) %>%
      dplyr::filter(!apg_family == "delete") %>%
      dplyr::filter(!gymnosperm == "TRUE") %>% 
      dplyr::group_by(
        sample_id,
        apg_family
        ) %>%
      dplyr::summarise(
        .groups = "keep",
        counts = sum(counts)
        ) %>%
      dplyr::rename(counts_apg_harmonised = `apg_family`) %>%
      tidyr::spread(
        counts_apg_harmonised, 
        counts
        ) %>%
      dplyr::ungroup() %>%
      tibble::column_to_rownames("sample_id") %>%
      dplyr::select_if(colSums(.) != 0) %>%
      tibble::rownames_to_column("sample_id") %>%
      tibble::as_tibble()
    
  }

  