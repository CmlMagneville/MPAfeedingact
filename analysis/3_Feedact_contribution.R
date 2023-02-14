################################################################################
##
## Script to study the contribution of each trophic guild to all interactions
##
## 3_Feedact_contribution.R
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


# Load the species traits dataframe:
sp_diet <- readRDS(here::here("transformed_data", "sp_diet_final.rds"))
colnames(sp_diet)[1] <- "sp_nm"


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




# 3 - Plot the species contribution to bites according to their trophic guilds ####


# a - Get the names of species seen interacting with the substrated on each site:
allsp_FPA_vect <- unique(c(colnames(presabs_seq_03), colnames(presabs_seq_05)))
allsp_PPA_vect <- unique(c(colnames(presabs_seq_04), colnames(presabs_seq_06)))


# b - Compute the bites rates:

# FPA
studied_site <- "FPA"
df_list <- list(presabs_seq_03, presabs_seq_05)

FPA_interac_contrib <- compute.contrib.feedact(df_list = df_list,
                                            sp_FPA_vect = allsp_FPA_vect,
                                            sp_PPA_vect = allsp_PPA_vect,
                                            studied_site)
# mean nb of bites per seq (5min) per 10m2 (data for each day on each site separatedly):
FPA_day_contrib <- FPA_interac_contrib[[1]]
# mean nb of bites per seq (5min) per 10m2 (data pooled for the two days):
FPA_mean_contrib <- FPA_interac_contrib[[2]]
# total number of bites on both days:
FPA_tot_contrib <- FPA_interac_contrib[[3]]


saveRDS(FPA_day_contrib, here::here("transformed_data",
                                    "FPA_feedact_day_contrib.rds"))
saveRDS(FPA_mean_contrib, here::here("transformed_data",
                                     "FPA_feedact_mean_contrib.rds"))
saveRDS(FPA_tot_contrib, here::here("transformed_data",
                                    "FPA_feedact_tot_contrib.rds"))


# PPA
studied_site <- "PPA"
df_list <- list(presabs_seq_04, presabs_seq_06)

PPA_interac_contrib <- compute.contrib.feedact(df_list = df_list,
                                            sp_FPA_vect = allsp_FPA_vect,
                                            sp_PPA_vect = allsp_PPA_vect,
                                            studied_site)
PPA_day_contrib <- PPA_interac_contrib[[1]]
PPA_mean_contrib <- PPA_interac_contrib[[2]]
PPA_tot_contrib <- PPA_interac_contrib[[3]]


saveRDS(PPA_day_contrib, here::here("transformed_data",
                                    "PPA_feedact_day_contrib.rds"))
saveRDS(PPA_mean_contrib, here::here("transformed_data",
                                    "PPA_feedact_mean_contrib.rds"))
saveRDS(PPA_tot_contrib, here::here("transformed_data",
                                    "PPA_feedact_tot_contrib.rds"))


# c - Link with trophic guilds and gather invertivores####


FPA_mean_contrib_diet <- dplyr::left_join(FPA_mean_contrib, sp_diet, by = "sp_nm")
PPA_mean_contrib_diet <- dplyr::left_join(PPA_mean_contrib, sp_diet, by = "sp_nm")

FPA_mean_contrib_diet[which(FPA_mean_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Macroinvertivores",
                               "sessile invertivores",
                               "Microinvertivores")), "Diet_Parravicini_2020"] <- "Invertivores"
PPA_mean_contrib_diet[which(PPA_mean_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Macroinvertivores",
                               "sessile invertivores",
                               "Microinvertivores")), "Diet_Parravicini_2020"] <- "Invertivores"

# rename herbivores: HMD
FPA_mean_contrib_diet[which(FPA_mean_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Herbivores Microvores Detritivores")), "Diet_Parravicini_2020"] <- "HMD"
PPA_mean_contrib_diet[which(PPA_mean_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Herbivores Microvores Detritivores")), "Diet_Parravicini_2020"] <- "HMD"


# get the number of species in each trophic guild:
# FPA:
FPA_mean_contrib_diet %>%
  dplyr::group_by(Diet_Parravicini_2020) %>%
  dplyr::summarise(n())
# PPA:
PPA_mean_contrib_diet %>%
  dplyr::group_by(Diet_Parravicini_2020) %>%
  dplyr::summarise(n())


# d - Plot ####


# Corallivores
contrib_FPA_df <- FPA_mean_contrib_diet
contrib_PPA_df <- PPA_mean_contrib_diet
guild_nm <- "Corallivores"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
res_coral <- plot.interact.guild.contrib(contrib_FPA_df,
                                         contrib_PPA_df,
                                         guild_nm,
                                         sites_colors)
# mean number of bites per sequences (days pooled) in the PPA:
coral_contrib_PPA <- res_coral[[2]]
# mean number of bites per sequences (days pooled) in the FPA:
coral_contrib_FPA <- res_coral[[3]]
saveRDS(coral_contrib_PPA, here::here("transformed_data", "coral_contrib_PPA.rds"))
saveRDS(coral_contrib_FPA, here::here("transformed_data", "coral_contrib_FPA.rds"))


# HMD
contrib_FPA_df <- FPA_mean_contrib_diet
contrib_PPA_df <- PPA_mean_contrib_diet
guild_nm <- "HMD"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
res_herb <- plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)
herb_contrib_PPA <- res_herb[[2]]
herb_contrib_FPA <- res_herb[[3]]
saveRDS(herb_contrib_PPA, here::here("transformed_data", "herb_contrib_PPA.rds"))
saveRDS(herb_contrib_FPA, here::here("transformed_data", "herb_contrib_FPA.rds"))


# Invertivores
contrib_FPA_df <- FPA_mean_contrib_diet
contrib_PPA_df <- PPA_mean_contrib_diet
guild_nm <- "Invertivores"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
res_invert <- plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)
invert_contrib_PPA <- res_invert[[2]]
invert_contrib_FPA <- res_invert[[3]]
saveRDS(invert_contrib_PPA, here::here("transformed_data", "invert_contrib_PPA.rds"))
saveRDS(invert_contrib_FPA, here::here("transformed_data", "invert_contrib_FPA.rds"))


# Crustacivores
contrib_FPA_df <- FPA_mean_contrib_diet
contrib_PPA_df <- PPA_mean_contrib_diet
guild_nm <- "Crustacivores"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
res_crustac <- plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)
crustac_contrib_PPA <- res_crustac[[2]]
crustac_contrib_FPA <- res_crustac[[3]]
saveRDS(crustac_contrib_PPA, here::here("transformed_data", "crustac_contrib_PPA.rds"))
saveRDS(crustac_contrib_FPA, here::here("transformed_data", "crustac_contrib_FPA.rds"))
