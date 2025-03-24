# Phylogenetic assembly of angiosperms through space and time of Europe and North America in the Holocene

## Authors
Kuber P Bhatta*, Vivian A. Felde, Hilary H. Birks, H. John B. Birks

### Corresponding author
Kuber P. Bhatta (kuber.bhatta@gmail.com)

### Project
This project is a part of European Research Council (ERC) under the European Union’s Horizon 2020 Research and Innovation Programme (grant agreement no. 741413) HOPE Humans on Planet Earth – longterm impacts on biosphere dynamics awarded to HJBB.

## Repo description
This repository contains data and R codes for analysing spatio-temporal variation in the phylogenetic dispersion in angiosperm pollen assemblages in Europe and North America and their relationship with regional climate and anthropogenic influence

## How to access the repo?
This R project is accessible in two ways:
1. If a user has a [GitHub account](https://github.com/), the easiest way is to create your own GitHub repo using this [Github link](https://github.com/HOPE-UIB-BIO/Fossil_vs_Modern_Pollen_Assemblage_Patterns)
2. A user can download the latest Release of the R project as a zip file from the "Fossil_vs_Modern_Pollen_Assemblage_Patterns" [Release page](https://github.com/HOPE-UIB-BIO/Fossil_vs_Modern_Pollen_Assemblage_Patterns/releases)

Different sections (folders) of the R project are as follows:
- `Inputs/`: All the pre-analysed and processed data, and tables are stored in this folder.
  - The subfolder `Inputs/Data/` contains a geo-tif file for the biome or climate-zone classification (`Inputs/Data/Biomes/`), backbone phylogeny used for construction of phylogeny (`Inputs/Data/Ramirez_Barahona_etal_2020_phylogeny/`), unprocessed surface sample data (`Inputs/Data/surface_samples_epd`), and processed data (.rds files) to be used in the analyses. The subfolder `Inputs/Tables/` contains the pollen taxa lists and pollen-taxa harmonisation tables used for standardising the pollen taxonomy. 
Please note that users will not find raw data of surface pollen data (`Inputs/Data/surface_samples_cao/`) and fossil pollen data (`Inputs/Data/Data_processed_2022-08-30.rds`) here because it cannot be shared publicly due to an embargo of the data contributors. 
- `Outputs/` contains outputs of all analyses in the form of data (`Outputs/Data/`), figures (`Outputs/Figure/`), and tables (`Outputs/Table/`).
- `___Init_project___.R`: This script is useful for the initial project setup (see below).
- `renv/`: This folder stores all the installed packages with the record of their versions.
- `R/`: The R project consists of R scripts and functions as follows:
  - R/00_Config_file.R: This script (referred to as "Config file" from here on) is useful for setting up the preferences that are applied to the whole project. Here all settings (configurations) and criteria used throughout the project are predefined by the user before running all the R scripts in the project. In addition, it prepares the current session by loading the required packages and saving all settings throughout the project.
  - `R/functions/`: This folder contains and specific R functions used in the project
  - `R/01_Data_processing/`: This folder contains all R scripts used for data processing prior to main analyses. 
  - `R/02_Main_analyses/`: This folder contains all R scripts used for the main analyses. Please note that implementation of some of the scripts maiy take substantial amount of time from hours to days. So, please check individual script carefully before executing it. Reduce the number of randomisations for a metric to be estimated, or reduce the number of 'k' while implementing the scripts of the generalised additive models.
 
## How to use the repo?
Once a user obtains the R project, there are several steps to be done before using it:
1. Update [R](https://en.wikipedia.org/wiki/R_(programming_language)) and [R-studio IDE](https://posit.co/products/open-source/rstudio/). There are many guides on how to do so (e.g. [here](https://jennhuck.github.io/workshops/install_update_R.html))
2. Execute all individual steps with the `___Init_project___.R` script. This will result in the preparation of all R-packages using the [`{renv}` package](https://rstudio.github.io/renv/articles/renv.html), which is an R dependency management of your projects. Mainly it will install a crucial R-package [`{REcopol}`](https://github.com/HOPE-UIB-BIO/R-Ecopol-package) and and all its dependencies. The latest release of {REcopol} is automatically installed in the project set-up stage. Note that installing all packages can take a substantial amount of time.
3. Run the `00_Config_file.R` at the beginning of running the R scripts so that the project configuration, required packages, and functions are loaded properly.


