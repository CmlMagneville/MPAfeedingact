################################################################################
##
## Analysis to compute basic statistical analysis for the manuscript and compute
## ... key values needed for the results and discussion
##
## 10_Statistical_analysis_other_and_values.R
##
## 03/02/2023
##
## Camille Magneville
##
################################################################################



# Compare intensity differences between sequences in the FPA and PPA ####


# 1 - Load data:
bites_seq_guilds_03_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_03_df.rds"))
bites_seq_guilds_04_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_04_df.rds"))
bites_seq_guilds_05_df <- readRDS(here::here("transformed_data",
                                           "bites_seq_guilds_05_df.rds"))
bites_seq_guilds_06_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_06_df.rds"))

# 2 - Corallivory:

FPA_bites <- c(bites_seq_guilds_03_df$coral_bites, bites_seq_guilds_05_df$coral_bites)
PPA_bites <- c(bites_seq_guilds_04_df$coral_bites, bites_seq_guilds_06_df$coral_bites)

# Create a df to do a summary and plot data:
bites <-  c(FPA_bites, PPA_bites)
protection <- rep(c("FPA", "PPA"), each = 24)

# Now creating a dataframe
data <- data.frame(protection, bites, stringsAsFactors = TRUE)

dplyr::group_by(data, protection) %>%
  summarise(
    count = n(),
    median = median(bites, na.rm = TRUE),
    IQR = IQR(bites, na.rm = TRUE))

# loading package for boxplot
ggpubr::ggboxplot(data, x = "protection", y = "bites",
          color = protection, palette = c("#80cdc1", "#bf812d"),
          ylab = "bites number per sequence", xlab = "protection level")


# 3 - Herbivory test:

FPA_bites <- c(bites_seq_guilds_03_df$herb_bites, bites_seq_guilds_05_df$herb_bites)
PPA_bites <- c(bites_seq_guilds_04_df$herb_bites, bites_seq_guilds_06_df$herb_bites)

# Create a df to do a summary and plot data:
bites <-  c(FPA_bites, PPA_bites)
protection <- rep(c("FPA", "PPA"), each = 24)

# Now creating a dataframe
data <- data.frame(protection, bites, stringsAsFactors = TRUE)

dplyr::group_by(data, protection) %>%
  summarise(
    count = n(),
    median = median(bites, na.rm = TRUE),
    IQR = IQR(bites, na.rm = TRUE))

# loading package for boxplot
ggpubr::ggboxplot(data, x = protection, y = bites,
                  color = protection, palette = c("#80cdc1", "#bf812d"),
                  ylab = "bites number per sequence", xlab = "protection level")


# 4 - Invertivory test:

FPA_bites <- c(bites_seq_guilds_03_df$invert_bites, bites_seq_guilds_05_df$invert_bites)
PPA_bites <- c(bites_seq_guilds_04_df$invert_bites, bites_seq_guilds_06_df$invert_bites)


# Create a df to do a summary and plot data:
bites <-  c(FPA_bites, PPA_bites)
protection <- rep(c("FPA", "PPA"), each = 24)

# Now creating a dataframe
data <- data.frame(protection, bites, stringsAsFactors = TRUE)

dplyr::group_by(data, protection) %>%
  summarise(
    count = n(),
    median = median(bites, na.rm = TRUE),
    IQR = IQR(bites, na.rm = TRUE))

# loading package for boxplot
ggpubr::ggboxplot(data, x = protection, y = bites,
                  color = protection, palette = c("#80cdc1", "#bf812d"),
                  ylab = "bites number per sequence", xlab = "protection level")


# 5 - Crustacivory test:

FPA_bites <- c(bites_seq_guilds_03_df$crustac_bites, bites_seq_guilds_05_df$crustac_bites)
PPA_bites <- c(bites_seq_guilds_04_df$crustac_bites, bites_seq_guilds_06_df$crustac_bites)


# Create a df to do a summary and plot data:
bites <-  c(FPA_bites, PPA_bites)
protection <- rep(c("FPA", "PPA"), each = 24)

# Now creating a dataframe
data <- data.frame(protection, bites, stringsAsFactors = TRUE)

dplyr::group_by(data, protection) %>%
  summarise(
    count = n(),
    median = median(bites, na.rm = TRUE),
    IQR = IQR(bites, na.rm = TRUE))

# loading package for boxplot
ggpubr::ggboxplot(data, x = protection, y = bites,
                  color = protection, palette = c("#80cdc1", "#bf812d"),
                  ylab = "bites number per sequence", xlab = "protection level")



# Mean (on sequence) bite rates per feeding activity and test if significant difference ####

# 1 - Load data:
bites_seq_guilds_03_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_03_df.rds"))
bites_seq_guilds_04_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_04_df.rds"))
bites_seq_guilds_05_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_05_df.rds"))
bites_seq_guilds_06_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_06_df.rds"))

# 2 - Mean of the total activity:

FPA_bites_df <- dplyr::bind_rows(bites_seq_guilds_03_df, bites_seq_guilds_05_df)
PPA_bites_df <- dplyr::bind_rows(bites_seq_guilds_04_df, bites_seq_guilds_06_df)
all_bites_df <- dplyr::bind_rows(FPA_bites_df, PPA_bites_df)

# coral:
all_bites_df %>%
  dplyr::group_by(site) %>%
  rstatix::get_summary_stats(coral_bites, type = "common")

# herbivory:
all_bites_df %>%
  dplyr::group_by(site) %>%
  rstatix::get_summary_stats(herb_bites, type = "common")

# invert:
all_bites_df %>%
  dplyr::group_by(site) %>%
  rstatix::get_summary_stats(invert_bites, type = "common")

# crustac:
all_bites_df %>%
  dplyr::group_by(site) %>%
  rstatix::get_summary_stats(crustac_bites, type = "common")


# 3 - Kruskal-Wallis test if there is a significative difference between ...
# ... feedact intensity in the FPA and in the PPA separately:

# FPA
FPA_KW_df <- reshape2::melt(FPA_bites_df,
                            id.vars = c("site", "hour", "seq", "day"),
                            variable.name = 'feedact', value.name = 'bites_nb')
# remove tot_bites because not interesting:
FPA_KW_df <- FPA_KW_df[which(FPA_KW_df$feedact != "tot_bites"), ]
# plot:
ggpubr::ggboxplot(FPA_KW_df, x = "feedact", y = "bites_nb")
# test:
rstatix::kruskal_test(FPA_KW_df, bites_nb ~ feedact)
# compare pair by pair as there is a significative difference:
rstatix::dunn_test(FPA_KW_df, bites_nb ~ feedact,
                   p.adjust.method = "bonferroni")


# PPA
PPA_KW_df <- reshape2::melt(PPA_bites_df,
                            id.vars = c("site", "hour", "seq", "day"),
                            variable.name = 'feedact', value.name = 'bites_nb')
# remove tot_bites because not interesting:
PPA_KW_df <- PPA_KW_df[which(PPA_KW_df$feedact != "tot_bites"), ]
# plot:
ggpubr::ggboxplot(PPA_KW_df, x = "feedact", y = "bites_nb")
# test:
rstatix::kruskal_test(PPA_KW_df, bites_nb ~ feedact)
# compare pair by pair as there is a significative difference:
rstatix::dunn_test(PPA_KW_df, bites_nb ~ feedact,
                   p.adjust.method = "bonferroni")


# Activity of the dominant species ####


# 1 - Load data: summed nb of bites on both days / min annotated / 2m²
coral_contrib_PPA <- readRDS(here::here("transformed_data", "coral_contrib_PPA.rds"))
coral_contrib_FPA <- readRDS(here::here("transformed_data", "coral_contrib_FPA.rds"))
herb_contrib_PPA <- readRDS(here::here("transformed_data", "herb_contrib_PPA.rds"))
herb_contrib_FPA <- readRDS(here::here("transformed_data", "herb_contrib_FPA.rds"))
invert_contrib_PPA <- readRDS(here::here("transformed_data", "invert_contrib_PPA.rds"))
invert_contrib_FPA <- readRDS(here::here("transformed_data", "invert_contrib_FPA.rds"))

# 2 - Corallivory: what  of the total corallivory realised the 3 dominant species?

# FPA:
((coral_contrib_FPA[which(coral_contrib_FPA$sp_nm == "Chaetodon trifasciatus"), "feed_act"] +
    coral_contrib_FPA[which(coral_contrib_FPA$sp_nm == "Chaetodon auriga"), "feed_act"] +
    coral_contrib_FPA[which(coral_contrib_FPA$sp_nm == "Oxymonacanthus longirostris"), "feed_act"])/
  coral_contrib_FPA[which(coral_contrib_FPA$sp_nm == "All species"), "feed_act"])*100

# PPA:
((coral_contrib_PPA[which(coral_contrib_PPA$sp_nm == "Chaetodon trifasciatus"), "feed_act"] +
    coral_contrib_PPA[which(coral_contrib_PPA$sp_nm == "Chaetodon auriga"), "feed_act"] +
    coral_contrib_PPA[which(coral_contrib_PPA$sp_nm == "Oxymonacanthus longirostris"), "feed_act"])/
    coral_contrib_PPA[which(coral_contrib_PPA$sp_nm == "All species"), "feed_act"])*100

# 3 - Herbivory: what  of the total herb realised the 1 dominant species?

# FPA:
(herb_contrib_FPA[which(herb_contrib_FPA$sp_nm == "Ctenochaetus striatus"), "feed_act"]/
    herb_contrib_FPA[which(herb_contrib_FPA$sp_nm == "All species"), "feed_act"])*100

# PPA:
(herb_contrib_PPA[which(herb_contrib_PPA$sp_nm == "Ctenochaetus striatus"), "feed_act"]/
    herb_contrib_PPA[which(herb_contrib_PPA$sp_nm == "All species"), "feed_act"])*100


# 4 - Invertivory: what  of the total corallivory realised the 15 dominant species?

# FPA:
invert_contrib_FPA <- dplyr::arrange(invert_contrib_FPA, desc(feed_act))
(sum(invert_contrib_FPA[c(2:16), "feed_act"]) /
  invert_contrib_FPA[which(invert_contrib_FPA$sp_nm == "All species"), "feed_act"])*100

# PPA:
invert_contrib_PPA <- dplyr::arrange(invert_contrib_PPA, desc(feed_act))
(sum(invert_contrib_PPA[c(2:16), "feed_act"]) /
    invert_contrib_PPA[which(invert_contrib_PPA$sp_nm == "All species"), "feed_act"])*100

# 5 - Crustacivory: ?

# FPA:
((crustac_contrib_FPA[which(crustac_contrib_FPA$sp_nm == "Parapercis hexophtalma"), "feed_act"])/
    crustac_contrib_FPA[which(crustac_contrib_FPA$sp_nm == "All species"), "feed_act"])*100



# Percentage of the top 5 peaks realised by some key species ####


# 1 - Load data about peaks activity:

FPA_peaks_coral <- readRDS(here::here("transformed_data", "biggest_peaks_coral_FPA.rds"))
PPA_peaks_coral <-readRDS(here::here("transformed_data", "biggest_peaks_coral_PPA.rds"))
FPA_peaks_herb <- readRDS(here::here("transformed_data", "biggest_peaks_herb_FPA.rds"))
PPA_peaks_herb <-readRDS(here::here("transformed_data", "biggest_peaks_herb_PPA.rds"))
FPA_peaks_invert <- readRDS(here::here("transformed_data", "biggest_peaks_invert_FPA.rds"))
PPA_peaks_invert <-readRDS(here::here("transformed_data", "biggest_peaks_invert_PPA.rds"))
FPA_peaks_crust <- readRDS(here::here("transformed_data", "biggest_peaks_crust_FPA.rds"))
PPA_peaks_crust <-readRDS(here::here("transformed_data", "biggest_peaks_crust_PPA.rds"))


# 2 - Corallivory:


# percentage of peaks act realised by Chaetodon trifasciatus in the PPA?
(sum(PPA_peaks_coral[which(PPA_peaks_coral$species == "Chaetodon trifasciatus"), "bites_nb"])/
    sum(PPA_peaks_coral[, "bites_nb"]))*100

# percentage of peaks act realised by Chaetodon trifasciatus in the FPA?
(sum(FPA_peaks_coral[which(FPA_peaks_coral$species == "Chaetodon trifasciatus"), "bites_nb"])/
    sum(FPA_peaks_coral[, "bites_nb"]))*100

# percentage of peaks act realised by Oxymonacanthus longirostris in the PPA?
(sum(PPA_peaks_coral[which(PPA_peaks_coral$species == "Oxymonacanthus longirostris"), "bites_nb"])/
    sum(PPA_peaks_coral[, "bites_nb"]))*100

# 3 - Herbivory:

# percentage of peaks act realised by Ctenochaetus striatus in the PPA:
(sum(PPA_peaks_herb[which(PPA_peaks_herb$species == "Ctenochaetus striatus"), "bites_nb"])/
    sum(PPA_peaks_herb[, "bites_nb"]))*100

# percentage of peaks act realised by Ctenochaetus striatus in the FPA:
(sum(FPA_peaks_herb[which(FPA_peaks_herb$species == "Ctenochaetus striatus"), "bites_nb"])/
    sum(FPA_peaks_herb[, "bites_nb"]))*100


# 4 - Invertivory:

# Percentage of peaks realised by Coris formosa and Paramonacanthus ...
# ... japonicus in the PPA because important activity and unique to this site:
(sum(PPA_peaks_invert[which(PPA_peaks_invert$species %in% c(
                                "Coris formosa", "Paramonacanthus japonicus")), "bites_nb"])/
    sum(PPA_peaks_invert[, "bites_nb"]))*100



# Percentage of feeding activity in each timeslot + test difference btw timeslots? ####
# Finally not used as want to account for day as a random effect: samples are not independant)
# But some figures are interesting (boxplots)


# 1 - Load data:
bites_seq_guilds_03_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_03_df.rds"))
# add timeslot info:
bites_seq_guilds_03_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_guilds_04_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_04_df.rds"))
# add timeslot info:
bites_seq_guilds_04_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_guilds_05_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_05_df.rds"))
# add timeslot info:
bites_seq_guilds_05_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_guilds_06_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_06_df.rds"))
# add timeslot info:
bites_seq_guilds_06_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_final_df <- dplyr::bind_rows(bites_seq_guilds_03_df,
                                       bites_seq_guilds_04_df,
                                       bites_seq_guilds_05_df,
                                       bites_seq_guilds_06_df)

# 2 - Corallivory:

## Compute percentage of activity realised in each timeslot based on PPA/FPA:

# timeslot 1
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                              bites_seq_final_df$site == "PPA"), "coral_bites"])/
  sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "coral_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "FPA"), "coral_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "coral_bites"]))*100

# timeslot 2
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "PPA"), "coral_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "coral_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "FPA"), "coral_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "coral_bites"]))*100

# timeslot 3
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "PPA"), "coral_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "coral_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "FPA"), "coral_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "coral_bites"]))*100


## Compute Kruskall-Wallis and associated Dunn test to test for diff btw timeslots:

# PPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                      coral_bites ~ timeslot)

# FPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                      coral_bites ~ timeslot)


# loading package for boxplot
ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                  x = "timeslot", y = "coral_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")

ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                  x = "timeslot", y = "coral_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")

# Dunn tests:

# PPA:
rstatix::dunn_test(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                   coral_bites ~ timeslot,
                   p.adjust.method = "bonferroni")


# 3 - Herbivory:

## Compute percentage of activity realised in each timeslot based on PPA/FPA:

# timeslot 1
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "PPA"), "herb_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "herb_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "FPA"), "herb_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "herb_bites"]))*100

# timeslot 2
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "PPA"), "herb_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "herb_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "FPA"), "herb_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "herb_bites"]))*100

# timeslot 3
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "PPA"), "herb_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "herb_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "FPA"), "herb_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "herb_bites"]))*100


## Compute Kruskall-Wallis and associated Dunn test to test for diff btw timeslots:

# PPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                      herb_bites ~ timeslot)

# FPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                      herb_bites ~ timeslot)


# loading package for boxplot
ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                  x = "timeslot", y = "herb_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")

ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                  x = "timeslot", y = "herb_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")

# Dunn tests:

# PPA:
rstatix::dunn_test(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                   herb_bites ~ timeslot,
                   p.adjust.method = "bonferroni")


# 4 - Invertivory:

## Compute percentage of activity realised in each timeslot based on PPA/FPA:

# timeslot 1
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "PPA"), "invert_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "invert_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "FPA"), "invert_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "invert_bites"]))*100

# timeslot 2
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "PPA"), "invert_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "invert_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "FPA"), "invert_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "invert_bites"]))*100

# timeslot 3
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "PPA"), "invert_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "invert_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "FPA"), "invert_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "invert_bites"]))*100


## Compute Kruskall-Wallis and associated Dunn test to test for diff btw timeslots:

# PPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                      invert_bites ~ timeslot)

# FPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                      invert_bites ~ timeslot)


# loading package for boxplot
ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                  x = "timeslot", y = "invert_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")

ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                  x = "timeslot", y = "invert_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")


# 5 - Crustacivory:

## Compute percentage of activity realised in each timeslot based on PPA/FPA:

# timeslot 1
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "PPA"), "crustac_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "crustac_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_1" &
                                bites_seq_final_df$site == "FPA"), "crustac_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "crustac_bites"]))*100

# timeslot 2
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "PPA"), "crustac_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "crustac_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_2" &
                                bites_seq_final_df$site == "FPA"), "crustac_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "crustac_bites"]))*100

# timeslot 3
# PPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "PPA"), "crustac_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), "crustac_bites"]))*100
# FPA:
(sum(bites_seq_final_df[which(bites_seq_final_df$timeslot == "timeslot_3" &
                                bites_seq_final_df$site == "FPA"), "crustac_bites"])/
    sum(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), "crustac_bites"]))*100


## Compute Kruskall-Wallis and associated Dunn test to test for diff btw timeslots:

# PPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                      crustac_bites ~ timeslot)

# FPA:
rstatix::kruskal_test(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                      crustac_bites ~ timeslot)


# loading package for boxplot
ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "PPA"), ],
                  x = "timeslot", y = "crustac_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")

ggpubr::ggboxplot(bites_seq_final_df[which(bites_seq_final_df$site == "FPA"), ],
                  x = "timeslot", y = "crustac_bites",
                  fill = "timeslot", palette = c("#e9c46a", "#e76f51", "#2a9d8f"),
                  ylab = "bites number per sequence in the PPA", xlab = "timeslots")



# PERMANOVA: test for differences in the composition of sp pools realising activities btw time slots ####


# 1 - Load data:

turn_coral_df <- readRDS(here::here("transformed_data", "temp_turn_coral_df.rds"))
turn_herb_df <- readRDS(here::here("transformed_data", "temp_turn_herb_df.rds"))
turn_invert_df <- readRDS(here::here("transformed_data", "temp_turn_invert_df.rds"))
turn_crust_df <- readRDS(here::here("transformed_data", "temp_turn_crust_df.rds"))

# 2 - Corallivory:

# create the distance object:
turn_coral_df$pair1 <- paste0(turn_coral_df$day, sep = "_",
                              turn_coral_df$site, sep = "_",
                              "timeslot", sep = "_",
                              substr(turn_coral_df$pair_timeslots, 1, 1))
turn_coral_df$pair2 <- paste0(turn_coral_df$day, sep = "_",
                              turn_coral_df$site, sep = "_",
                              "timeslot", sep = "_",
                              substr(turn_coral_df$pair_timeslots, 3, 3))
dissim_coral_df <- turn_coral_df[, c("tot_dissim", "pair1", "pair2")]
dissim_coral_df$tot_dissim <- as.numeric(dissim_coral_df$tot_dissim)

coral_dist <- reshape2::acast(dissim_coral_df, pair1 ~ pair2, value.var='tot_dissim', fun.aggregate = sum, margins=FALSE)

# plot pcoa
pcoa_coral <- ape::pcoa(as.matrix(coral_dist)) # pcoa explained by only one eigenvalue...


# create the variable dataframe:
pair_info <- unique(c(dissim_coral_df$pair1, dissim_coral_df$pair2))
coral_var <- as.data.frame(matrix(ncol = 3, nrow = length(pair_info)))
colnames(coral_var) <- c("id", "site", "timeslot")
coral_var$id <- pair_info
coral_var$site <- substr(coral_var$id, 7, 9)
coral_var$timeslot <- substr(coral_var$id, 11, 20)
coral_var$site_timeslot <- paste0(coral_var$site, sep = "_", coral_var$timeslot)
coral_var$site <- as.factor(coral_var$site)
coral_var$timeslot <- as.factor(coral_var$timeslot)
coral_var$site_timeslot <- as.factor(coral_var$site_timeslot)
rownames(coral_var) <- coral_var$id


# Permdisp:
vegan::betadisper(as.dist(coral_dist), group = coral_var$site_timeslot)
vegan::adonis2(as.dist(coral_dist) ~ site*timeslot, data = coral_var, permutations = 999)

# Dissimilarity and Turnover between time slots ####


# 1 - Load data:
turn_coral_df <- readRDS(here::here("transformed_data", "temp_turn_coral_df.rds"))
turn_herb_df <- readRDS(here::here("transformed_data", "temp_turn_herb_df.rds"))
turn_invert_df <- readRDS(here::here("transformed_data", "temp_turn_invert_df.rds"))
turn_crust_df <- readRDS(here::here("transformed_data", "temp_turn_crust_df.rds"))

# 2 - Corallivory:

turn_coral_df$tot_dissim <- as.numeric(turn_coral_df$tot_dissim)
turn_coral_df$turnover <- as.numeric(turn_coral_df$turnover)

# mean dissim in the FPA:
mean(turn_coral_df[which(turn_coral_df$site == "FPA"), "tot_dissim"])
sd(turn_coral_df[which(turn_coral_df$site == "FPA"), "tot_dissim"])/sqrt(length((turn_coral_df[which(turn_coral_df$site == "FPA"), "tot_dissim"])))

# mean dissim in the PPA:
mean(turn_coral_df[which(turn_coral_df$site == "PPA"), "tot_dissim"])
sd(turn_coral_df[which(turn_coral_df$site == "PPA"), "tot_dissim"])/sqrt(length((turn_coral_df[which(turn_coral_df$site == "PPA"), "tot_dissim"])))


# 3 - Herbivory:

turn_herb_df$tot_dissim <- as.numeric(turn_herb_df$tot_dissim)
turn_herb_df$turnover <- as.numeric(turn_herb_df$turnover)

# mean dissim in the FPA:
mean(turn_herb_df[which(turn_herb_df$site == "FPA"), "tot_dissim"])
sd(turn_herb_df[which(turn_herb_df$site == "FPA"), "tot_dissim"])/sqrt(length((turn_herb_df[which(turn_herb_df$site == "FPA"), "tot_dissim"])))

# mean dissim in the PPA:
mean(turn_herb_df[which(turn_herb_df$site == "PPA"), "tot_dissim"])
sd(turn_herb_df[which(turn_herb_df$site == "PPA"), "tot_dissim"])/sqrt(length((turn_herb_df[which(turn_herb_df$site == "PPA"), "tot_dissim"])))


# 3 - Invertivory:

turn_invert_df$tot_dissim <- as.numeric(turn_invert_df$tot_dissim)
turn_invert_df$turnover <- as.numeric(turn_invert_df$turnover)

# mean dissim in the FPA:
mean(turn_invert_df[which(turn_invert_df$site == "FPA"), "tot_dissim"])
sd(turn_invert_df[which(turn_invert_df$site == "FPA"), "tot_dissim"])/sqrt(length((turn_invert_df[which(turn_invert_df$site == "FPA"), "tot_dissim"])))

# mean dissim in the PPA:
mean(turn_invert_df[which(turn_invert_df$site == "PPA"), "tot_dissim"])
sd(turn_invert_df[which(turn_invert_df$site == "PPA"), "tot_dissim"])/sqrt(length((turn_invert_df[which(turn_invert_df$site == "PPA"), "tot_dissim"])))


# 3 - Crustacivory:

turn_crust_df$tot_dissim <- as.numeric(turn_crust_df$tot_dissim)
turn_crust_df$turnover <- as.numeric(turn_crust_df$turnover)

# mean dissim in the FPA:
mean(turn_crust_df[which(turn_crust_df$site == "FPA"), "tot_dissim"])
sd(turn_crust_df[which(turn_crust_df$site == "FPA"), "tot_dissim"])/sqrt(length((turn_crust_df[which(turn_crust_df$site == "FPA"), "tot_dissim"])))

# mean dissim in the PPA:
mean(turn_crust_df[which(turn_crust_df$site == "PPA"), "tot_dissim"])
sd(turn_crust_df[which(turn_crust_df$site == "PPA"), "tot_dissim"])/sqrt(length((turn_crust_df[which(turn_crust_df$site == "PPA"), "tot_dissim"])))



# Percentages of total activities realised on the 5 greatest ####

# 1 - Load data:
FPA_peaks_coral <- readRDS(here::here("transformed_data", "biggest_peaks_coral_FPA.rds"))
PPA_peaks_coral <-readRDS(here::here("transformed_data", "biggest_peaks_coral_PPA.rds"))
FPA_peaks_herb <- readRDS(here::here("transformed_data", "biggest_peaks_herb_FPA.rds"))
PPA_peaks_herb <-readRDS(here::here("transformed_data", "biggest_peaks_herb_PPA.rds"))
FPA_peaks_invert <- readRDS(here::here("transformed_data", "biggest_peaks_invert_FPA.rds"))
PPA_peaks_invert <-readRDS(here::here("transformed_data", "biggest_peaks_invert_PPA.rds"))
FPA_peaks_crust <- readRDS(here::here("transformed_data", "biggest_peaks_crust_FPA.rds"))
PPA_peaks_crust <-readRDS(here::here("transformed_data", "biggest_peaks_crust_PPA.rds"))


bites_seq_guilds_03_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_03_df.rds"))
bites_seq_guilds_04_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_04_df.rds"))
bites_seq_guilds_05_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_05_df.rds"))
bites_seq_guilds_06_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_06_df.rds"))
FPA_bites_df <- dplyr::bind_rows(bites_seq_guilds_03_df, bites_seq_guilds_05_df)
PPA_bites_df <- dplyr::bind_rows(bites_seq_guilds_04_df, bites_seq_guilds_06_df)
all_bites_df <- dplyr::bind_rows(FPA_bites_df, PPA_bites_df)


# 2 - Corallivory:

# FPA:
(sum(FPA_peaks_coral$bites_nb) / sum(FPA_bites_df$coral_bites))*100

# PPA:
(sum(PPA_peaks_coral$bites_nb) / sum(PPA_bites_df$coral_bites))*100


# 3 - Herbivory:

# FPA:
(sum(FPA_peaks_herb$bites_nb) / sum(FPA_bites_df$herb_bites))*100

# PPA:
(sum(PPA_peaks_herb$bites_nb) / sum(PPA_bites_df$herb_bites))*100


# 4 - Invertivory:

# FPA:
(sum(FPA_peaks_invert$bites_nb) / sum(FPA_bites_df$invert_bites))*100

# PPA:
(sum(PPA_peaks_invert$bites_nb) / sum(PPA_bites_df$invert_bites))*100


# 5 - Crustacivory:

# FPA:
(sum(FPA_peaks_crust$bites_nb) / sum(FPA_bites_df$crustac_bites))*100

# PPA:
(sum(PPA_peaks_crust$bites_nb) / sum(PPA_bites_df$crustac_bites))*100
