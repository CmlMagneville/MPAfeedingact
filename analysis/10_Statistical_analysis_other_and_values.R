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



# Mann-Whitey test: test intensity differences between sequences in the FPA and PPA ####


# 1 - Load data:
bites_seq_guilds_03_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_03_df.rds"))
bites_seq_guilds_04_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_04_df.rds"))
bites_seq_guilds_05_df <- readRDS(here::here("transformed_data",
                                           "bites_seq_guilds_05_df.rds"))
bites_seq_guilds_06_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_06_df.rds"))

# 2 - Corallivory test:

FPA_bites <- c(bites_seq_guilds_03_df$coral_bites, bites_seq_guilds_05_df$coral_bites)
PPA_bites <- c(bites_seq_guilds_04_df$coral_bites, bites_seq_guilds_06_df$coral_bites)

wilcox.test(x = FPA_bites, y = PPA_bites, paired = FALSE, exact = FALSE)

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

wilcox.test(x = FPA_bites, y = PPA_bites, paired = FALSE, exact = FALSE)

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

wilcox.test(x = FPA_bites, y = PPA_bites, paired = FALSE, exact = FALSE)

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

wilcox.test(x = FPA_bites, y = PPA_bites, paired = FALSE, exact = FALSE)

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



