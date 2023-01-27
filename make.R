###############################################################################
##
## Script to make all the analysis
##
## make.R
##
## 27/01/2023
##
## Camille Magneville
##
###############################################################################


# Clean the environnement:
rm(list = ls(all.names = TRUE), envir = .GlobalEnv)

# Install dependencies:
devtools::install_deps()


# Load the functions so make them available for use:
devtools::load_all()


# Load the analysis scripts:

# 1 - Get the number of species in each diet cat, piscivores number ...
# ... and a clean diet db:
source(here::here("analysis", "1_Nb_sp_feedact.R"))

# 2 - Merge dataframes from a given day (cameras pooled):
source(here::here("analysis", "2_Merge_cameras.R"))

# 3 - Species contribution to feeding activities (and plot):
source(here::here("analysis", "3_Feedact_contrib.R"))

# 4 - Plot heatmaps of feeding activities and nb of bites per seq per feedact:
source(here::here("analysis", "4_Heatmaps_feedact.R"))

# 5 - Compute GLMM to study the effect of protectionlevel and time (seq level):
source(here::here("analysis", "5_Models_seq_effect_time_protection.R"))

# 6 - Compute GLMM to study the effect of protectionlevel and time (timeslot):
source(here::here("analysis", "6_Models_timeslot_effect_time_protection.R"))





