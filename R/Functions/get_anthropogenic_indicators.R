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
# Harmonise anthropogenic indicator taxa ----
#-----------------------------------#
get_anthropogenic_indicators <-
  function(id,
           region,
           counts) {
    
    harm_table <-
      readr::read_csv(
        paste(
          "Inputs/Tables/Anthropogenic_indicators/", 
          region,
          "_121124",
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
          "Inputs/Tables/Anthropogenic_indicators/", 
          region,
          "_121124",
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
      dplyr::group_by(
        sample_id,
        indicator
        ) %>%
      dplyr::summarise(
        .groups = "keep",
        counts = sum(counts)
        ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(anth_ind_taxa = ifelse(indicator == TRUE, counts, 0)) %>%
      dplyr::group_by(sample_id) %>% 
      dplyr::summarise(
        .groups = "keep",
        anth_ind_taxa = sum(anth_ind_taxa)
        ) %>% 
      dplyr::ungroup() %>% 
      tibble::as_tibble() %>%
      dplyr::select(sample_id, anth_ind_taxa)
    }

  