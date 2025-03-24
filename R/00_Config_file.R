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
# 4. Authorise the user -----
#----------------------------------------------------------#

auth_tibble <-
  tibble::tibble(
    name = c("kbh022", "kuber"),
    paths = c(
      "C:/Users/kbh022/OneDrive - University of Bergen/HOPE_data/Hope_main_external_storage/",
      "C:/Users/kuber/OneDrive - University of Bergen/HOPE_data/Hope_main_external_storage/"
    )
  )

sys_info <- Sys.info()

username <-
  sys_info["user"]

# Define the directory (external) for storing big data files
#   Default is in the current project
data_storage_path <-
  auth_tibble %>%
  dplyr::filter(name == username) %>%
  purrr::pluck("paths")

#----------------------------------------------------------#
# 5. Colour scheme ----
#----------------------------------------------------------#
color_pd_curve <- "#0072B2"
color_richness_curve <- "#D55E00"
color_phylo_age_curve <- "#666666" 
color_common <- "#000000" 

color_high_lat <- "#0099FF"
color_low_lat <- "#FF0000"

color_high_age <- "#009E73"
color_low_age <- "#E69F00"

#----------------------------------------------------------#

