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


# Load the four presabs dataframe gathering all species:
presabs_03_df <- readRDS(here::here("transformed_data",
                                          "presabs_03_df.rds"))
presabs_04_df <- readRDS(here::here("transformed_data",
                                          "presabs_04_df.rds"))
presabs_05_df <- readRDS(here::here("transformed_data",
                                          "presabs_05_df.rds"))
presabs_06_df <- readRDS(here::here("transformed_data",
                                          "presabs_06_df.rds"))

# Load the species traits dataframe:
sp_diet <- readRDS(here::here("transformed_data", "sp_diet_final.rds"))
colnames(sp_diet)[1] <- "sp_nm"


# 2 - Plot the species contribution to bites according to their trophic guilds ####


# a - Get the names of species seen interacting with the substrated on each site:
allsp_FPA_vect <- unique(c(colnames(presabs_03_df), colnames(presabs_05_df)))
allsp_PPA_vect <- unique(c(colnames(presabs_04_df), colnames(presabs_06_df)))


# b - Compute the bites rates:

# FPA
studied_site <- "FPA"
df_list <- list(presabs_03_df, presabs_05_df)

FPA_interac_contrib <- compute.contrib.feedact(df_list = df_list,
                                            sp_FPA_vect = allsp_FPA_vect,
                                            sp_PPA_vect = allsp_PPA_vect,
                                            studied_site)
FPA_day_contrib <- FPA_interac_contrib[[1]]
FPA_tot_contrib <- FPA_interac_contrib[[2]] # I have summed the nb of bites ...
# ... seen on both days and then divided by 120 (min annotated) and 5 (5*quadrat)

saveRDS(FPA_day_contrib, here::here("transformed_data",
                                    "FPA_feedact_day_contrib.rds"))
saveRDS(FPA_tot_contrib, here::here("transformed_data",
                                    "FPA_feedact_tot_contrib.rds"))


# PPA
studied_site <- "PPA"
df_list <- list(presabs_04_df, presabs_06_df)

PPA_interac_contrib <- compute.contrib.feedact(df_list = df_list,
                                            sp_FPA_vect = allsp_FPA_vect,
                                            sp_PPA_vect = allsp_PPA_vect,
                                            studied_site)
PPA_day_contrib <- PPA_interac_contrib[[1]]
PPA_tot_contrib <- PPA_interac_contrib[[2]] # I have summed the nb of bites ...
# ... seen on both days and then divided by 120 (min annotated) and 5 (5*quadrat)

saveRDS(PPA_day_contrib, here::here("transformed_data",
                                    "PPA_feedact_day_contrib.rds"))
saveRDS(PPA_tot_contrib, here::here("transformed_data",
                                    "PPA_feedact_tot_contrib.rds"))


# c - Link with trophic guilds and gather invertivores####


FPA_tot_contrib_diet <- dplyr::left_join(FPA_tot_contrib, sp_diet, by = "sp_nm")
PPA_tot_contrib_diet <- dplyr::left_join(PPA_tot_contrib, sp_diet, by = "sp_nm")

FPA_tot_contrib_diet[which(FPA_tot_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Macroinvertivores",
                               "sessile invertivores",
                               "Microinvertivores")), "Diet_Parravicini_2020"] <- "Invertivores"
PPA_tot_contrib_diet[which(PPA_tot_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Macroinvertivores",
                               "sessile invertivores",
                               "Microinvertivores")), "Diet_Parravicini_2020"] <- "Invertivores"

# rename herbivores: HMD
FPA_tot_contrib_diet[which(FPA_tot_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Herbivores Microvores Detritivores")), "Diet_Parravicini_2020"] <- "HMD"
PPA_tot_contrib_diet[which(PPA_tot_contrib_diet$Diet_Parravicini_2020 %in%
                             c("Herbivores Microvores Detritivores")), "Diet_Parravicini_2020"] <- "HMD"


# get the number of species in each trophic guild:
# FPA:
FPA_tot_contrib_diet %>%
  dplyr::group_by(Diet_Parravicini_2020) %>%
  dplyr::summarise(n())
# PPA:
PPA_tot_contrib_diet %>%
  dplyr::group_by(Diet_Parravicini_2020) %>%
  dplyr::summarise(n())


# d - Plot ####


# Corallivores
contrib_FPA_df <- FPA_tot_contrib_diet
contrib_PPA_df <- PPA_tot_contrib_diet
guild_nm <- "Corallivores"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)


# HMD
contrib_FPA_df <- FPA_tot_contrib_diet
contrib_PPA_df <- PPA_tot_contrib_diet
guild_nm <- "HMD"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)


# Invertivores
contrib_FPA_df <- FPA_tot_contrib_diet
contrib_PPA_df <- PPA_tot_contrib_diet
guild_nm <- "Invertivores"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)


# Crustacivores
contrib_FPA_df <- FPA_tot_contrib_diet
contrib_PPA_df <- PPA_tot_contrib_diet
guild_nm <- "Crustacivores"
sites_colors <- c("grey85", "#bf812d", "#80cdc1")
plot.interact.guild.contrib(contrib_FPA_df,
                            contrib_PPA_df,
                            guild_nm,
                            sites_colors)
