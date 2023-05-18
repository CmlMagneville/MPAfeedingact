################################################################################
##
## Script to plot heatmaps of feedact across time
##
## 4_Heatmaps_feedact.R
##
## 27/01/2023
##
## Camille Magneville
##
################################################################################



# 1 - Load data ####

presabs_seq_03 <- readRDS(here::here("transformed_data",
                                         "presabs_seq_03.rds"))
presabs_seq_04 <- readRDS(here::here("transformed_data",
                                         "presabs_seq_04.rds"))
presabs_seq_05 <- readRDS(here::here("transformed_data",
                                         "presabs_seq_05.rds"))
presabs_seq_06 <- readRDS(here::here("transformed_data",
                                         "presabs_seq_06.rds"))

sp_diet <- readRDS(here::here("transformed_data", "sp_diet_final.rds"))

# 2 - Retrieve total number of bites and per trophic guilds across time ####


# 03:
day <- "03-11-2020"
seq_df <- presabs_seq_03
sp_diet <- sp_diet
bites_seq_guilds_03_df <- compute.bites.seq(day, seq_df, sp_diet)
saveRDS(bites_seq_guilds_03_df, here::here("transformed_data",
                                           "bites_seq_guilds_03_df.rds"))

# 04:
day <- "04-11-2020"
seq_df <- presabs_seq_04
sp_diet <- sp_diet
bites_seq_guilds_04_df <- compute.bites.seq(day, seq_df, sp_diet)
saveRDS(bites_seq_guilds_04_df, here::here("transformed_data",
                                           "bites_seq_guilds_04_df.rds"))

# 05:
day <- "05-11-2020"
seq_df <- presabs_seq_05
sp_diet <- sp_diet
bites_seq_guilds_05_df <- compute.bites.seq(day, seq_df, sp_diet)
saveRDS(bites_seq_guilds_05_df, here::here("transformed_data",
                                           "bites_seq_guilds_05_df.rds"))


# 06:
day <- "06-11-2020"
seq_df <- presabs_seq_06
sp_diet <- sp_diet
bites_seq_guilds_06_df <- compute.bites.seq(day, seq_df, sp_diet)
saveRDS(bites_seq_guilds_06_df, here::here("transformed_data",
                                           "bites_seq_guilds_06_df.rds"))



# 3 - Plot the activities intensity across time through heatmaps ####


bites_seq_df_list <- list(bites_seq_guilds_03_df,
                          bites_seq_guilds_04_df,
                          bites_seq_guilds_05_df,
                          bites_seq_guilds_06_df)

plot.heatmaps.act.intensity(bites_seq_df_list)
