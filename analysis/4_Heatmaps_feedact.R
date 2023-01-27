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


presabs_03_df <- readRDS(here::here("transformed_data", "presabs_03_df.rds"))
presabs_04_df <- readRDS(here::here("transformed_data", "presabs_04_df.rds"))
presabs_05_df <- readRDS(here::here("transformed_data", "presabs_05_df.rds"))
presabs_06_df <- readRDS(here::here("transformed_data", "presabs_06_df.rds"))

sp_diet <- readRDS(here::here("transformed_data", "sp_diet_final.rds"))


# 2 - Get data at the sequence scale ####

# with 5 minutes of annotations - 10 minutes of no annotations per pose

# 03:
start_times <- c("07:30:00", "11:45:00", "16:00:00")
end_times <- c("08:30:00", "12:45:00", "17:00:00")
seq_length <- "00:05:00"
no_seq_length <- "00:10:00"
bites_df <- presabs_03_df
presabs_seq_03 <- data.sequences.scale(start_times, end_times, seq_length,
                     no_seq_length, bites_df)

# 04:
start_times <- c("07:30:00", "11:45:00", "16:00:00")
end_times <- c("08:30:00", "12:45:00", "17:00:00")
seq_length <- "00:05:00"
no_seq_length <- "00:10:00"
bites_df <- presabs_04_df
presabs_seq_04 <- data.sequences.scale(start_times, end_times, seq_length,
                                             no_seq_length, bites_df)


# 05:
start_times <- c("07:30:00", "11:45:00", "16:00:00")
end_times <- c("08:30:00", "12:45:00", "17:00:00")
seq_length <- "00:05:00"
no_seq_length <- "00:10:00"
bites_df <- presabs_05_df
presabs_seq_05 <- data.sequences.scale(start_times, end_times, seq_length,
                                             no_seq_length, bites_df)

# 06:
start_times <- c("07:30:00", "11:45:00", "16:00:00")
end_times <- c("08:30:00", "12:45:00", "17:00:00")
seq_length <- "00:05:00"
no_seq_length <- "00:10:00"
bites_df <- presabs_06_df
presabs_seq_06 <- data.sequences.scale(start_times, end_times, seq_length,
                                             no_seq_length, bites_df)


# Add site information (needed for the next step):
presabs_seq_03$site <- rep("FPA", nrow(presabs_seq_03))
presabs_seq_05$site <- rep("FPA", nrow(presabs_seq_05))
presabs_seq_04$site <- rep("PPA", nrow(presabs_seq_04))
presabs_seq_06$site <- rep("PPA", nrow(presabs_seq_06))


# save:
saveRDS(presabs_seq_03, here::here("transformed_data",
                                         "presabs_seq_03.rds"))
saveRDS(presabs_seq_04, here::here("transformed_data",
                                         "presabs_seq_04.rds"))
saveRDS(presabs_seq_05, here::here("transformed_data",
                                         "presabs_seq_05.rds"))
saveRDS(presabs_seq_06, here::here("transformed_data",
                                         "presabs_seq_06.rds"))



# 3 - Retrieve total number of bites and per trophic guilds across time ####


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



# 4 - Plot the activities intensity across time through heatmaps ####


bites_seq_df_list <- list(bites_seq_guilds_03_df,
                          bites_seq_guilds_04_df,
                          bites_seq_guilds_05_df,
                          bites_seq_guilds_06_df)

plot.heatmaps.act.intensity(bites_seq_df_list)
