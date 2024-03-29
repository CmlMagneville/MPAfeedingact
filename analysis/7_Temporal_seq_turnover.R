################################################################################
##
## Script to compute temporal turnover between time slots per feedact and ...
## ... plot the turnover pairs (timeslot 1-2, 1-3, 2-3) for both days ...
## ... on each site
##
## 7_Temporal_seq_turnover.R
##
## 30/01/2023
##
## Camille Magneville
##
################################################################################


# 1 - Load data gathered at the sequence level for each day and traits data ####


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
                                                 "sessile invertivores",
                                                 "Crustacivores"))]
invert_bites_seq_03_df <- subset.df.feedact(bites_seq_df,
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
                                                 "sessile invertivores",
                                                 "Crustacivores"))]
invert_bites_seq_04_df <- subset.df.feedact(bites_seq_df,
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
                                                 "sessile invertivores",
                                                 "Crustacivores"))]
invert_bites_seq_05_df <- subset.df.feedact(bites_seq_df,
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
                                                 "sessile invertivores",
                                                 "Crustacivores"))]
invert_bites_seq_06_df <- subset.df.feedact(bites_seq_df,
                                            sp_feedact_nm_vect)


# 3 - Compute temporal turnover between timeslots and plot them ####
# Add together the bites of sequences from a given timeslot to get bites/timeslot
# For each feedact, get one dataframe with the values for the two sites and ...
# ... two days and one plot per feedact


# Corallivory:
list_df <- list(coral_bites_seq_03_df, coral_bites_seq_04_df,
             coral_bites_seq_05_df, coral_bites_seq_06_df)
turn_coral_df <- compute.temporal.turn(list_df)

# Herbivory:
list_df <- list(herb_bites_seq_03_df, herb_bites_seq_04_df,
                herb_bites_seq_05_df, herb_bites_seq_06_df)
turn_herb_df <- compute.temporal.turn(list_df)

# Invertivory:
list_df <- list(invert_bites_seq_03_df, invert_bites_seq_04_df,
                invert_bites_seq_05_df, invert_bites_seq_06_df)
turn_invert_df <- compute.temporal.turn(list_df)


# save:
saveRDS(turn_coral_df, here::here("transformed_data", "temp_turn_coral_df.rds"))
saveRDS(turn_herb_df, here::here("transformed_data", "temp_turn_herb_df.rds"))
saveRDS(turn_invert_df, here::here("transformed_data", "temp_turn_invert_df.rds"))
