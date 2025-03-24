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

# Script to prepare all necessary components of environment to run the Project.
# Needs to be run only once


#----------------------------------------------------------#
# Step 1: Install 'renv' package -----
#----------------------------------------------------------#

utils::install.packages("renv")


#----------------------------------------------------------#
# Step 2: Deactivate 'renv' package -----
#----------------------------------------------------------#

# deactivate to make sure that packages are updated on the machine
renv::deactivate()


#----------------------------------------------------------#
# Step 3: Create a list of packages
#----------------------------------------------------------#

package_list <- 
  c(
    "ape",
    "assertthat",
    "dbplyr",
    "devtools",
    "flextable",
    "ggpubr",
    "gratia",
    "here",
    "httpgd",
    "janitor",
    "jsonlite",
    "languageserver",
    "lemon",
    "picante",
    "rstatix",
    "raster",
    "renv",       
    "roxygen2",   
    "tabularaster",
    "tidyverse",  
    "usethis"
    )

# define helper function
install_packages <-
  function(pkgs_list) {

    # install all packages in the lst from CRAN
    sapply(pkgs_list, utils::install.packages, character.only = TRUE)

    #install 'RFossilpol' from GitHub
    remotes::install_github(
      "HOPE-UIB-BIO/R-Fossilpol-package"
      )
    
    # Install 'REcopol' from FitHub
    remotes::install_github(
      "HOPE-UIB-BIO/R-Ecopol-package")
    }

#----------------------------------------------------------#
# Step 4: Install packages to the machine
#----------------------------------------------------------#

install_packages(package_list)


#----------------------------------------------------------#
# Step 5: Activate 'renv' project
#----------------------------------------------------------#

renv::activate()


#----------------------------------------------------------#
# Step 6: Install packages to the project
#----------------------------------------------------------#

install_packages(package_list)


#----------------------------------------------------------#
# Step 7: Synchronize package versions with the project 
#----------------------------------------------------------#

library(here)

renv::snapshot(lockfile = here::here("renv.lock"))


#----------------------------------------------------------#
# Step 8: GitHub hook
#----------------------------------------------------------#

# Prevent commiting to the Main
#usethis::use_git_hook(
#  hook = "pre-commit",
#  script = '#!/bin/sh
#  branch="$(git rev-parse --abbrev-ref HEAD)"
#  if [ "$branch" = "main" ]; then
#  echo "You cannot commit directly to main branch. Please make a new branch"
#  exit 1
#  fi'
#)

#----------------------------------------------------------#
# Step 9: Run the project 
#----------------------------------------------------------#
