#----------------------------------------------------------#
#
#     Phylogenetic assembly of angiosperms through space and time of Europe
#                   and North America in the Holocene
#
#                         Project setup
#
# Kuber P. Bhatta, Vivian A. Felde, Hilary H. Birks, and H. John B. Birks
#
#                             2025
#
#----------------------------------------------------------#
#----------------------------------------------------------#
# Configuration script with the variables that should be consistent throughout 
#   the whole repo. It loads packages, defines important variables, 
#   authorises the user, and saves config file.

#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

if(!exists("update_repo_packages")){
  update_repo_packages <- TRUE
}

if(update_repo_packages == TRUE){
  
  # install fossilpol from github
  if (!exists("already_installed_fossilpol")){
    already_installed_fossilpol <- FALSE
  }
  
  if(already_installed_fossilpol == FALSE){
    devtools::install_github("HOPE-UIB-BIO/fossilpol",
                             quiet = FALSE,
                             upgrade = FALSE)
    already_installed_fossilpol <- TRUE
  }
  
  if (!exists("already_synch")){
    already_synch <- FALSE
  }
  
  if(already_synch == FALSE){
    library(here)
    # synchronise the package versions
    renv::restore(lockfile = here::here( "renv.lock"))
    already_synch <- TRUE
    
    # save snapshot of package versions
    renv::snapshot(lockfile =  "renv.lock")  # do only for update
  }
}

# define packages
package_list <- 
  c(
    "assertthat",
    "devtools",
    "fossilpol",
    "here",      
    "renv",       
    "roxygen2",   
    "tidyverse",  
    "usethis" 
    )

# load all packages
sapply(package_list, library, character.only = TRUE)


#----------------------------------------------------------#
# 2. Define space -----
#----------------------------------------------------------#

current_date <- Sys.Date()

# project directory is set up by 'here' package, Adjust if needed 
current_dir <- here::here()

#----------------------------------------------------------#
# 3. Load functions -----
#----------------------------------------------------------#

# get vector of general functions
fun_list <- 
  list.files(
    path = "R/functions/",
    pattern = "*.R",
    recursive = TRUE) 

# source them
sapply(
  paste0("R/functions/", fun_list, sep = ""),
  source)


#----------------------------------------------------------#
# 4. Colour scheme ----
#----------------------------------------------------------#
color_pd_curve <- "#0072B2"
color_richness_curve <- "#D55E00"
color_phylo_age_curve <- "#666666" 
color_common <- "#000000" 

color_high_lat <- "#0099FF"
color_low_lat <- "#FF0000"

color_high_age <- "#009E73"
color_low_age <- "#E69F00"

# Assign unique colour to each climate zone
my_palette <-
  c("#FFCC99",
    "#0066CC",
    "#00CCCC",
    "#333333",
    "#993300",
    "#99CC00"
    ) %>% 
  rlang::set_names(
    nm = c(
      "Arid_Steppe",
      "Cold_Without_dry_season",
      "Temperate_Without_dry_season",
      "Polar_Tundra",
      "Temperate_Dry_Summer",
      "Cold_Dry_Summer"
      )
    )

#----------------------------------------------------------#
# 6. Graphical options -----
#----------------------------------------------------------#

## examples
#set ggplot output
ggplot2::theme_set(
  ggplot2::theme_classic())

# define general
text_size = 10
line_size = 0.1

# define output sizes
image_width <- 16
image_height <- 12
image_units <- "cm"


#----------------------------------------------------------#

