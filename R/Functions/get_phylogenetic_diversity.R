#-----------------------------------------------#
# Function for estimating phylogenetic diversity ----
#-----------------------------------------------#
get_phylogenetic_diversity <- 
  function(dataset_id,
           region,
           counts,
           type = "mpd",
           null.model = "phylogeny.pool", 
           abundance.weighted = TRUE,
           runs = 999,
           ...) {
    message(
      msg = paste(dataset_id, "_", region, sep = "")
      )
  
  dat <- 
    counts %>%
    as.data.frame() %>% 
    column_to_rownames("sample_id")
  
  regional_tree <- 
    ape::read.tree(
      paste(
        "Inputs/Data/Ramirez_Barahona_etal_2020_phylogeny/",
        "Regional_tree_",
        region,
        ".tre",
        sep = ""
        )
      )
  
  # Make a vector of families from the tree that are missing from the counts data
  drop_list <- 
    regional_tree$tip.label[!regional_tree$tip.label %in% colnames(dat)]
  
  # Remove all the families that do not occur in our sample using drop.tip() 
  #  function in the 'ape' package.
  pruned_tree <- ape::drop.tip(regional_tree, drop_list) 
  
  # Arrange taxa in the same order as the pruned tree
  data_ordered <- dat[,c(pruned_tree$tip.label)]
  
  # Calculate MPD and MNTD
  # Create cophenetic distance from the pruned tree.
  phy_dist <- cophenetic(pruned_tree) 
  
  if (type == "mpd") {
    set.seed(1234)
    mpd_phylogeny <- 
      picante::ses.mpd(
        data_ordered,
        phy_dist,
        null.model = null.model, 
        abundance.weighted = abundance.weighted,
        runs
        ) %>% 
      rownames_to_column("sample_id") %>% 
      as_tibble() 
    
    return(mpd_phylogeny) 
    
  } else if (type == "mntd") {
    
    set.seed(1234)
    mntd_phylogeny <- 
      picante::ses.mntd(
        data_ordered,
        phy_dist,
        null.model = null.model, 
        abundance.weighted = abundance.weighted,
        runs = runs
        ) %>% 
      rownames_to_column("sample_id") %>% 
      as_tibble() 
    
    return(mntd_phylogeny) 
    
  } else if (type == "beta_mpd") {

    set.seed(1234)
    beta_mpd <- 
      picante::comdist(
        data_ordered, 
        phy_dist, 
        abundance.weighted = abundance.weighted
        ) %>% 
      rstatix::pull_triangle(., 
                             triangle = "lower",
                             diagonal = FALSE
                             ) %>% 
      dplyr::rename(sample_id = rowname) %>% 
      tidyr::pivot_longer(-sample_id) %>% 
      dplyr::filter(!sample_id == name) %>% 
      dplyr::filter(!value == "") %>% 
      dplyr::mutate_at("value", as.numeric) %>% 
      dplyr::group_by(sample_id) %>% 
      
      # Mean, median, min, and max of mpd distance (beta MPD distance) of an
      # assemblage to rest of the assemblages 
      dplyr::summarise(
        mean_beta_mpd = mean(value),
        median_beta_mpd = median(value),
        min_beta_mpd = min(value),
        max_beta_mpd = max(value)
        ) 
    
    return(beta_mpd)
    
  } else if (type == "beta_mntd") {
    
    set.seed(1234)
    beta_mntd <- 
      picante::comdistnt(
        data_ordered, 
        phy_dist, 
        abundance.weighted = abundance.weighted
        ) %>% 
      rstatix::pull_triangle(., 
                             triangle = "lower",
                             diagonal = FALSE
                             ) %>% 
      dplyr::rename(sample_id = rowname) %>% 
      tidyr::pivot_longer(-sample_id) %>% 
      dplyr::filter(!sample_id == name) %>% 
      dplyr::filter(!value == "") %>% 
      dplyr::mutate_at("value", as.numeric) %>% 
      dplyr::group_by(sample_id) %>% 
      
      # Mean, median, min, and max of mntd (beta MNTD distance) of an assemblage to 
      #  rest of the assemblages 
      dplyr::summarise(
        mean_beta_mntd = mean(value),
        median_beta_mntd = median(value),
        min_beta_mntd = min(value),
        max_beta_mntd = max(value)
        ) 
    
    return(beta_mntd)
    
  } else {
    
    stop("type must be either 'mpd' or 'mntd', or 'beta_mpd', or 'beta_mntd'")
    
  }
}