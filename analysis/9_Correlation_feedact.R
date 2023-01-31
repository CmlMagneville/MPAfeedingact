################################################################################
##
## Script to study the correlation between feedact intensity to see if ...
## ... sequences with high values of a given activity are correlated with ...
## ... high values of another
##
## 9_Correlation_feedact.R
##
## 31/01/2023
##
## Camille Magneville
##
################################################################################


# 1 - Load data at the sequence level ####

bites_seq_03_df <- readRDS(here::here("transformed_data", "bites_seq_guilds_03_df.rds"))
bites_seq_04_df <- readRDS(here::here("transformed_data", "bites_seq_guilds_04_df.rds"))
bites_seq_05_df <- readRDS(here::here("transformed_data", "bites_seq_guilds_05_df.rds"))
bites_seq_06_df <- readRDS(here::here("transformed_data", "bites_seq_guilds_06_df.rds"))


# 2 - Link dataframes of the same site ####

FPA_bites_df <- dplyr::bind_rows(bites_seq_03_df, bites_seq_05_df)
PPA_bites_df <- dplyr::bind_rows(bites_seq_04_df, bites_seq_06_df)


# 3 - Compute and plot correlation between sequences ####


# FPA
bites_df <- FPA_bites_df
compute.plot.correl(bites_df)

# PPA
bites_df <- PPA_bites_df
compute.plot.correl(bites_df)










