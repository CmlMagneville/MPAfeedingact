################################################################################
##
## Script to study the change in dominant species identity across time
##
## 11_Species_identity_across_time.R
##
## 18/05/2023
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



# 2 - Get species relative activity for each timeslot ####
# In the final df, I have species relative activity per timeslot per day:
# ex: C. trifasciatus realise 60% of the activity of the time slot 1 on 03.


# Corallivory:
list_coral_bites_seq <- list(coral_bites_seq_03_df, coral_bites_seq_05_df)
coral_dominance_act_FPA <- sp.dominance.act(list_bites_seq = list_coral_bites_seq)

list_coral_bites_seq <- list(coral_bites_seq_04_df, coral_bites_seq_06_df)
coral_dominance_act_PPA <- sp.dominance.act(list_bites_seq = list_coral_bites_seq)


# Herbivory:
list_herb_bites_seq <- list(herb_bites_seq_03_df, herb_bites_seq_05_df)
herb_dominance_act_FPA <- sp.dominance.act(list_bites_seq = list_herb_bites_seq)

list_herb_bites_seq <- list(herb_bites_seq_04_df, herb_bites_seq_06_df)
herb_dominance_act_PPA <- sp.dominance.act(list_bites_seq = list_herb_bites_seq)


# Invertivory:
list_invert_bites_seq <- list(invert_bites_seq_03_df, invert_bites_seq_05_df)
invert_dominance_act_FPA <- sp.dominance.act(list_bites_seq = list_invert_bites_seq)

list_invert_bites_seq <- list(invert_bites_seq_04_df, invert_bites_seq_06_df)
invert_dominance_act_PPA <- sp.dominance.act(list_bites_seq = list_invert_bites_seq)


# 3 - Plot ####


## Corallivory:

# Set up the color vector:
sp_nm <- unique(c(colnames(coral_dominance_act_FPA)[1:(ncol(coral_dominance_act_FPA) - 4)],
                  colnames(coral_dominance_act_PPA)[1:(ncol(coral_dominance_act_PPA) - 4)]))
sp_nb <- length(sp_nm)
col_vect <- hcl.colors(sp_nb, palette = "GnBu")
names(col_vect) <- sort(unique(c(colnames(coral_dominance_act_FPA)[1:(ncol(coral_dominance_act_FPA) - 4)],
                               colnames(coral_dominance_act_PPA)[1:(ncol(coral_dominance_act_PPA) - 4)])))

# FPA:

dominance_df <- coral_dominance_act_FPA
guild_nm <- "Corallivores"

dominance_coral_FPA_plot <- dominance.sp.plot(dominance_df, guild_nm, col_vect)


# PPA:
dominance_df <- coral_dominance_act_PPA
guild_nm <- "Corallivores"

dominance_coral_PPA_plot <- dominance.sp.plot(dominance_df, guild_nm, col_vect)


## Herbivory

# Set up the color vector:
sp_nm <- unique(c(colnames(herb_dominance_act_FPA)[1:(ncol(herb_dominance_act_FPA) - 4)],
                  colnames(herb_dominance_act_PPA)[1:(ncol(herb_dominance_act_PPA) - 4)]))
sp_nb <- length(sp_nm)
col_vect <- hcl.colors(sp_nb, palette = "YlGnBu")
names(col_vect) <- sort(unique(c(colnames(herb_dominance_act_FPA)[1:(ncol(herb_dominance_act_FPA) - 4)],
                                 colnames(herb_dominance_act_PPA)[1:(ncol(herb_dominance_act_PPA) - 4)])))

# FPA:

dominance_df <- herb_dominance_act_FPA
guild_nm <- "Herbivores"

dominance_herb_FPA_plot <- dominance.sp.plot(dominance_df, guild_nm, col_vect)


# PPA:
dominance_df <- herb_dominance_act_PPA
guild_nm <- "Herbivores"

dominance_herb_PPA_plot <- dominance.sp.plot(dominance_df, guild_nm, col_vect)


## Invertivory:

# Set up the color vector:
sp_nm <- unique(c(colnames(invert_dominance_act_FPA)[1:(ncol(invert_dominance_act_FPA) - 4)],
                  colnames(invert_dominance_act_PPA)[1:(ncol(invert_dominance_act_PPA) - 4)]))
sp_nb <- length(sp_nm)
col_vect <- hcl.colors(sp_nb, palette = "Heat")
names(col_vect) <- sort(unique(c(colnames(invert_dominance_act_FPA)[1:(ncol(invert_dominance_act_FPA) - 4)],
                                 colnames(invert_dominance_act_PPA)[1:(ncol(invert_dominance_act_PPA) - 4)])))

# FPA:

dominance_df <- invert_dominance_act_FPA
guild_nm <- "Invertivores"

dominance_invert_FPA_plot <- dominance.sp.plot(dominance_df, guild_nm, col_vect)


# PPA:
dominance_df <- invert_dominance_act_PPA
guild_nm <- "Invertivores"

dominance_invert_PPA_plot <- dominance.sp.plot(dominance_df, guild_nm, col_vect)



