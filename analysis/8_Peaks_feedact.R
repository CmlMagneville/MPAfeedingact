################################################################################
##
## Script to study the five biggest events (sequence) for each feeding activity
##
## 8_Peaks_feedact.R
##
## 30/01/2023
##
## Camille Magneville
##
################################################################################



# 1 - Load data:

bites_seq_03_df <- readRDS(here::here("transformed_data",
                                      "presabs_seq_03.rds"))
bites_seq_04_df <- readRDS(here::here("transformed_data",
                                      "presabs_seq_04.rds"))
bites_seq_05_df <- readRDS(here::here("transformed_data",
                                      "presabs_seq_05.rds"))
bites_seq_06_df <- readRDS(here::here("transformed_data",
                                      "presabs_seq_06.rds"))

sp_diet <- readRDS(here::here("transformed_data", "sp_diet_final.rds"))



# 2 - Subset one dataframe per diet and per day ####


## 03

bites_seq_df <- bites_seq_03_df

# coral
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Corallivores")]
coral_bites_seq_03_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)

# herb
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Herbivores Microvores Detritivores")]
herb_bites_seq_03_df <- subset.df.feedact(bites_seq_df,
                                          sp_feedact_nm_vect)

# invert
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 %in%
                                               c("Macroinvertivores",
                                                 "Microinvertivores",
                                                 "sessile invertivores"))]
invert_bites_seq_03_df <- subset.df.feedact(bites_seq_df,
                                            sp_feedact_nm_vect)

# crust
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Crustacivores")]
crust_bites_seq_03_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)


## 04

bites_seq_df <- bites_seq_04_df

# coral
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Corallivores")]
coral_bites_seq_04_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)

# herb
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Herbivores Microvores Detritivores")]
herb_bites_seq_04_df <- subset.df.feedact(bites_seq_df,
                                          sp_feedact_nm_vect)

# invert
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 %in%
                                               c("Macroinvertivores",
                                                 "Microinvertivores",
                                                 "sessile invertivores"))]
invert_bites_seq_04_df <- subset.df.feedact(bites_seq_df,
                                            sp_feedact_nm_vect)

# crust
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Crustacivores")]
crust_bites_seq_04_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)


## 05

bites_seq_df <- bites_seq_05_df

# coral
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Corallivores")]
coral_bites_seq_05_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)

# herb
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Herbivores Microvores Detritivores")]
herb_bites_seq_05_df <- subset.df.feedact(bites_seq_df,
                                          sp_feedact_nm_vect)

# invert
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 %in%
                                               c("Macroinvertivores",
                                                 "Microinvertivores",
                                                 "sessile invertivores"))]
invert_bites_seq_05_df <- subset.df.feedact(bites_seq_df,
                                            sp_feedact_nm_vect)

# crust
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Crustacivores")]
crust_bites_seq_05_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)


## 06

bites_seq_df <- bites_seq_06_df

# coral
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Corallivores")]
coral_bites_seq_06_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)

# herb
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Herbivores Microvores Detritivores")]
herb_bites_seq_06_df <- subset.df.feedact(bites_seq_df,
                                          sp_feedact_nm_vect)

# invert
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 %in%
                                               c("Macroinvertivores",
                                                 "Microinvertivores",
                                                 "sessile invertivores"))]
invert_bites_seq_06_df <- subset.df.feedact(bites_seq_df,
                                            sp_feedact_nm_vect)

# crust
sp_feedact_nm_vect <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Crustacivores")]
crust_bites_seq_06_df <- subset.df.feedact(bites_seq_df,
                                           sp_feedact_nm_vect)


# 3 - Get the five strongest event / site for each feeding activity (days pooled) ####
# I thus get 10 sequences per feeding activity (5 per site)


#


