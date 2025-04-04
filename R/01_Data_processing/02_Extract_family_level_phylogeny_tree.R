#----------------------------------------------------------#
#
#   Phylogenetic assembly of angiosperms through space and time in Europe and
#                     North America in the Holocene
#
#    Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                               2025
#
#----------------------------------------------------------#

#-----------------------------------#
# Extract time calibrated family-level phylogeny ----

# Process family level phylogeny of angiosperms from "Ramírez-Barahona et al. 
# 2020. The delayed and geographically heterogeneous diversification of 
# flowering plant families. Nature Ecology and Evolution. 4: 1232-1238"
#-----------------------------------#
library(tidyverse)
library(ape)

# We used maximum likelihood angiosperm phylogeny (NEWICK format) estimated with 
# RAxML: Data1_RAxMLTree.tre by Ramírez-Barahona et al. 2020 ----

tree_2 <-
  ape::read.tree(
    paste(
      "Inputs/Data/Ramirez_Barahona_etal_2020_phylogeny/",
      "Data1_RAxMLTree.tre",
      sep = ""
    )
  ) # 1216 tip labels, each label corresponds to authors' samples

tips <- tree_2$tip.label 

families_raw <- 
  tips %>% 
  strsplit(., "_") %>% 
  purrr::map(
    .,
    ~pluck(.x[1])
  ) %>% 
  unlist()

pruned_tree_2 <- 
  ape::drop.tip(
    tree_2, 
    tip = tips[
      which(
        duplicated(
          families_raw)
      )
    ]
  )

pruned_tree_2_tip_label <- 
  pruned_tree_2$tip.label %>% 
  strsplit(., "_") %>% 
  purrr::map(
    .,
    ~ pluck(.x[1])
  ) %>% 
  unlist()

pruned_tree_2$tip.label <- pruned_tree_2_tip_label
ape::is.rooted(pruned_tree_2)       # TRUE
ape::is.ultrametric(pruned_tree_2)  # FALSE

# ape::write.tree(
#  pruned_tree_2,
#  file = paste(
#    "Inputs/Data/Ramirez_Barahona_etal_2020_phylogeny/",
#    "Ramirez_Barahona_etal_2020_raxml_processed.tre",
#    sep = ""
#  )
#)

#-----------------------------------#
# Make regional trees retaining only taxa present in each region ----
#-----------------------------------#
data_input_for_phylodiversity <- 
  readr::read_rds("Inputs/Data/data_input_for_phylodiversity_140624.rds")

# To prune the backbone tree to keep only keep the taxa from data, we need 
# the list of taxa present in the data.
req_taxa_europe <-
  data_input_for_phylodiversity %>%
  dplyr::filter(region == "Europe") %>% 
  dplyr::select(counts_apg_harmonised) %>%
  dplyr::mutate(taxa = purrr::map(
    counts_apg_harmonised,
    ~ colnames(.x) %>%
      tibble::enframe(name = NULL,
              value = "taxa")
    )
  ) %>%
  dplyr::select(taxa) %>%
  tidyr::unnest(col = c(taxa)) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(taxa) # 96 taxa in the datasets

req_taxa_europe %>% 
  dplyr::filter(!taxa %in% pruned_tree_2$tip.label)

# Viburnaceae chenopodiaceae are not in the backbone tree
# Viburnaceae changed to Adoxaceae (synonym) in the harmonisation table
# Chenopodiaceae renamed to Amaranthaceae (closest pollen morphotype)
# in the harmonisation table


req_taxa_na <-
  data_input_for_phylodiversity %>%
  dplyr::filter(region == "North_America") %>% 
  dplyr::select(counts_apg_harmonised) %>%
  dplyr::mutate(taxa = purrr::map(
    counts_apg_harmonised,
    ~ colnames(.x) %>%
      tibble::enframe(name = NULL,
              value = "taxa")
    )
  ) %>%
  dplyr::select(taxa) %>%
  tidyr::unnest(col = c(taxa)) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(taxa) # 117 taxa in the datasets

req_taxa_na %>% 
  dplyr::filter(!taxa %in% pruned_tree_2$tip.label)
# Viburnaceae is not in the backbone tree
# Viburnaceae changed to Adoxaceae (synonym) in the harmonisation table

# Re-harmonised the taxa using 'Harmonise_taxa.R' before further analyses.
# Re-check the re-harmonised taxa
# Everything OK, all taxa in the data are in the global tree

#-----------------------------------------------#
# Prune the full backbone tree ----
#-----------------------------------------------#
# Taxa in the backbone tree that are absent in the datasets  
drop_list_europe <- 
  pruned_tree_2$tip.label[!pruned_tree_2$tip.label %in% req_taxa_europe$taxa] # 348
drop_list_na <- 
  pruned_tree_2$tip.label[!pruned_tree_2$tip.label %in% req_taxa_na$taxa] # 327

# Remove all the families that do not occur in our datasets using drop.tip() 
regional_tree_europe <- ape::drop.tip(pruned_tree_2, drop_list_europe) # 95 families  
regional_tree_na <- ape::drop.tip(pruned_tree_2, drop_list_na) # 116 families

#-----------------------------------------------#
# Save the regional backbone tree ----
#-----------------------------------------------#
ape::write.tree(
  regional_tree_europe, 
  paste(
    "Inputs/Data/Ramirez_Barahona_etal_2020_phylogeny/",
    "Regional_tree_Europe.tre",
    sep = ""
    )
 )

ape::write.tree(
  regional_tree_na, 
  paste(
    "Inputs/Data/Ramirez_Barahona_etal_2020_phylogeny/",
    "Regional_tree_North_America.tre",
    sep = ""
  )
)
