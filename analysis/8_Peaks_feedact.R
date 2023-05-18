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



# 3 - Get the five strongest event / site for each feeding activity (days pooled) ####
# I thus get 10 sequences per feeding activity (5 per site)


# Corallivory:
list_FPA_df <- list(coral_bites_seq_03_df, coral_bites_seq_05_df)
coral_FPA_peaks <- identify.peaks.species(list_df = list_FPA_df, peak_nb = 5)
list_PPA_df <- list(coral_bites_seq_04_df, coral_bites_seq_06_df)
coral_PPA_peaks <- identify.peaks.species(list_df = list_PPA_df, peak_nb = 5)

# Herbivory:
list_FPA_df <- list(herb_bites_seq_03_df, herb_bites_seq_05_df)
herb_FPA_peaks <- identify.peaks.species(list_df = list_FPA_df, peak_nb = 5)
list_PPA_df <- list(herb_bites_seq_04_df, herb_bites_seq_06_df)
herb_PPA_peaks <- identify.peaks.species(list_df = list_PPA_df, peak_nb = 5)

# Invertivory:
list_FPA_df <- list(invert_bites_seq_03_df, invert_bites_seq_05_df)
invert_FPA_peaks <- identify.peaks.species(list_df = list_FPA_df, peak_nb = 5)
list_PPA_df <- list(invert_bites_seq_04_df, invert_bites_seq_06_df)
invert_PPA_peaks <- identify.peaks.species(list_df = list_PPA_df, peak_nb = 5)


# 4 - Plot the five biggest events and species contribution to it ####


## Corallivory:

# Create a vector that contain the names of all corallivores species seen ...
# ... and affect a color to ecah species (so that same color between FPA and PPA plots):
FPA_sp <- colnames(coral_FPA_peaks[, which(! colnames(coral_FPA_peaks)
                                %in% c("site", "hour",
                                       "seq", "day",
                                       "summed_bites"))])
PPA_sp <- colnames(coral_PPA_peaks[, which(! colnames(coral_PPA_peaks)
                                           %in% c("site", "hour",
                                                  "seq", "day",
                                                  "summed_bites"))])
sp_coral <- unique(c(FPA_sp, PPA_sp, "Others"))

# Correct Scaridae names:
sp_coral <- sub("Scarus_ferrugineuscoral", "Scarus_ferrugineus", sp_coral)
sp_coral <- sub("Chlorurus_sordiduscoral", "Chlorurus_sordidus", sp_coral)
sp_coral <- sub("Scarus_nigercoral", "Scarus_niger", sp_coral)
sp_coral <- sub("Scarus_frenatuscoral", "Scarus_frenatus", sp_coral)
# remove "_" and replace by empty space for species names:
sp_coral <- stringr::str_replace_all(sp_coral, "_", " ")

# Create the color palette (colorblindfriendly using colorBlindness::cvdPlot(plot))
sp_nb <- length(sp_coral)
coral_colour_vect <- hcl.colors(sp_nb, palette = "GnBu")
names(coral_colour_vect) <- sp_coral
coral_colour_vect[which(names(coral_colour_vect) == "Others")] <- "gray75"
col_vect <- coral_colour_vect

# FPA:
biggest_peaks_df <- coral_FPA_peaks
guild_nm <- "Corallivores"
site <- "FPA"
FPA_coral <- plot.peaks.contrib(biggest_peaks_df, guild_nm, site, col_vect)
FPA_coral_plot <- FPA_coral[[1]]
# PPA:
biggest_peaks_df <- coral_PPA_peaks
guild_nm <- "Corallivores"
site <- "PPA"
PPA_coral <- plot.peaks.contrib(biggest_peaks_df, guild_nm, site, col_vect)
PPA_coral_plot <- PPA_coral[[1]]


coral_peaks <- (FPA_coral_plot + PPA_coral_plot) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 1, nrow = 2, guides = "collect") +
  patchwork::plot_annotation(title = "Corallivores")

ggplot2::ggsave(filename = here::here("outputs", "Coral_peaks_tot.pdf"),
                plot = coral_peaks,
                device = "pdf",
                scale = 1,
                height = 4000,
                width = 5000,
                units = "px",
                dpi = 600)

# save the five biggest peaks data:
saveRDS(FPA_coral[[2]], here::here("transformed_data", "biggest_peaks_coral_FPA.rds"))
saveRDS(PPA_coral[[2]], here::here("transformed_data", "biggest_peaks_coral_PPA.rds"))


## Herbivory:

# Create a vector that contain the names of all corallivores species seen ...
# ... and affect a color to each species (so that same color between FPA and PPA plots):
FPA_sp <- colnames(herb_FPA_peaks[, which(! colnames(herb_FPA_peaks)
                                           %in% c("site", "hour",
                                                  "seq", "day",
                                                  "summed_bites"))])
PPA_sp <- colnames(herb_PPA_peaks[, which(! colnames(herb_PPA_peaks)
                                           %in% c("site", "hour",
                                                  "seq", "day",
                                                  "summed_bites"))])
sp_herb <- unique(c(FPA_sp, PPA_sp, "Others"))
# remove "_" and replace by empty space for species names:
sp_herb <- stringr::str_replace_all(sp_herb, "_", " ")

# Create the color palette (colorblindfriendly)
sp_nb <- length(sp_herb)
herb_colour_vect <- hcl.colors(sp_nb, palette = "YlGnBu")
names(herb_colour_vect) <- sp_herb
herb_colour_vect[which(names(herb_colour_vect) == "Others")] <- "gray75"
col_vect <- herb_colour_vect


# FPA:
biggest_peaks_df <- herb_FPA_peaks
guild_nm <- "Herbivores"
site <- "FPA"
FPA_herb <- plot.peaks.contrib(biggest_peaks_df, guild_nm, site, col_vect)
FPA_herb_plot <- FPA_herb[[1]]

# PPA:
biggest_peaks_df <- herb_PPA_peaks
guild_nm <- "Herbivores"
site <- "PPA"
col_vect <- herb_colour_vect
PPA_herb <- plot.peaks.contrib(biggest_peaks_df, guild_nm, site, col_vect)
PPA_herb_plot <- PPA_herb[[1]]

herb_peaks <- (FPA_herb_plot + PPA_herb_plot) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 1, nrow = 2, guides = "collect") +
  patchwork::plot_annotation(title = "Herbivores")

ggplot2::ggsave(filename = here::here("outputs", "Herb_peaks_tot.pdf"),
                plot = herb_peaks,
                device = "pdf",
                scale = 1,
                height = 4000,
                width = 5000,
                units = "px",
                dpi = 600)

# save the five biggest peaks data:
saveRDS(FPA_herb[[2]], here::here("transformed_data", "biggest_peaks_herb_FPA.rds"))
saveRDS(PPA_herb[[2]], here::here("transformed_data", "biggest_peaks_herb_PPA.rds"))


## Invertivory:

# Create a vector that contain the names of all corallivores species seen ...
# ... and affect a color to ecah species (so that same color between FPA and PPA plots):
FPA_sp <- colnames(invert_FPA_peaks[, which(! colnames(invert_FPA_peaks)
                                           %in% c("site", "hour",
                                                  "seq", "day",
                                                  "summed_bites"))])
PPA_sp <- colnames(invert_PPA_peaks[, which(! colnames(invert_PPA_peaks)
                                           %in% c("site", "hour",
                                                  "seq", "day",
                                                  "summed_bites"))])
sp_invert <- unique(c(FPA_sp, PPA_sp, "Others"))

# Correct Scaridae names:
sp_invert <- sub("Chaetodon_trifasciatusinv", "Chaetodon_trifasciatus", sp_invert)
sp_invert <- sub("Chaetodon_aurigainv", "Chaetodon_auriga", sp_invert)
# remove "_" and replace by empty space for species names:
sp_invert <- stringr::str_replace_all(sp_invert, "_", " ")

# Create the color palette (colorblindfriendly)
sp_nb <- length(sp_invert)
invert_colour_vect <- hcl.colors(sp_nb, palette = "Heat")
names(invert_colour_vect) <- sp_invert
invert_colour_vect[which(names(invert_colour_vect) == "Others")] <- "gray75"
col_vect <- invert_colour_vect


# FPA:
biggest_peaks_df <- invert_FPA_peaks
guild_nm <- "Invertivores"
site <- "FPA"
FPA_invert <- plot.peaks.contrib(biggest_peaks_df, guild_nm, site, col_vect)
FPA_invert_plot <- FPA_invert[[1]]
# PPA:
biggest_peaks_df <- invert_PPA_peaks
guild_nm <- "Invertivores"
site <- "PPA"
PPA_invert <- plot.peaks.contrib(biggest_peaks_df, guild_nm, site, col_vect)
PPA_invert_plot <- PPA_invert[[1]]


invert_peaks <- (FPA_invert_plot + PPA_invert_plot) +
  patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                         ncol = 1, nrow = 2, guides = "collect") +
  patchwork::plot_annotation(title = "Invertivores")

ggplot2::ggsave(filename = here::here("outputs", "Invert_peaks_tot.pdf"),
                plot = invert_peaks,
                device = "pdf",
                scale = 1,
                height = 4000,
                width = 5000,
                units = "px",
                dpi = 600)

# save the five biggest peaks data:
saveRDS(FPA_invert[[2]], here::here("transformed_data", "biggest_peaks_invert_FPA.rds"))
saveRDS(PPA_invert[[2]], here::here("transformed_data", "biggest_peaks_invert_PPA.rds"))



# 5 - Retrieve pres/abs dataframe for each feedact to check individual nb ####


presabs_03_df <- readRDS(here::here("transformed_data",
                                    "presabs_03_df.rds"))
presabs_04_df <- readRDS(here::here("transformed_data",
                                    "presabs_04_df.rds"))
presabs_05_df <- readRDS(here::here("transformed_data",
                                    "presabs_05_df.rds"))
presabs_06_df <- readRDS(here::here("transformed_data",
                                    "presabs_06_df.rds"))

# coral:
sp_coral_nm <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Corallivores")]
coral_03_df <- presabs_03_df[, which(colnames(presabs_03_df) %in% sp_coral_nm)]
coral_04_df <- presabs_04_df[, which(colnames(presabs_04_df) %in% sp_coral_nm)]
coral_05_df <- presabs_05_df[, which(colnames(presabs_05_df) %in% sp_coral_nm)]
coral_06_df <- presabs_06_df[, which(colnames(presabs_06_df) %in% sp_coral_nm)]



