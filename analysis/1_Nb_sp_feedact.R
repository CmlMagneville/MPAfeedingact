################################################################################
##
## Script to compute the number of species on interaction with ...
## ... the substrate and check number of piscivores species in FPA vs PPA (data
## ... from another paper submitted in Conservation Biology)
##
## 1_Nb_herb_sp.R
##
## 08/11/2022
##
## Camille Magneville
##
################################################################################



# 1 - Load data ####

# Load LD data:
presabs_03_A1 <- readRDS(here::here("raw_data", "presabs_03_A1.rds"))
presabs_03_B1 <- readRDS(here::here("raw_data", "presabs_03_B1.rds"))
presabs_03_C2 <- readRDS(here::here("raw_data", "presabs_03_C2.rds"))
presabs_04_A1 <- readRDS(here::here("raw_data", "presabs_04_A1.rds"))
presabs_04_B1 <- readRDS(here::here("raw_data", "presabs_04_B1.rds"))
presabs_04_C2 <- readRDS(here::here("raw_data", "presabs_04_C2.rds"))
presabs_05_A1 <- readRDS(here::here("raw_data", "presabs_05_A1.rds"))
presabs_05_B1 <- readRDS(here::here("raw_data", "presabs_05_B1.rds"))
presabs_05_C2 <- readRDS(here::here("raw_data", "presabs_05_C2.rds"))
presabs_06_A1 <- readRDS(here::here("raw_data", "presabs_06_A1.rds"))
presabs_06_A1 <- readRDS(here::here("raw_data", "presabs_06_A1.rds"))
presabs_06_B1 <- readRDS(here::here("raw_data", "presabs_06_B1.rds"))
presabs_06_C2 <- readRDS(here::here("raw_data", "presabs_06_C2.rds"))

# Load SD data:
presabs_03_camE_Pose1 <- readRDS(here::here("raw_data", "presabs_03_camE_Pose1.rds"))
presabs_03_camE_Pose2 <- readRDS(here::here("raw_data", "presabs_03_camE_Pose2.rds"))
presabs_03_camE_Pose3 <- readRDS(here::here("raw_data", "presabs_03_camE_Pose3.rds"))
presabs_03_camF_Pose1 <- readRDS(here::here("raw_data", "presabs_03_camF_Pose1.rds"))
presabs_03_camF_Pose2 <- readRDS(here::here("raw_data", "presabs_03_camF_Pose2.rds"))
presabs_03_camF_Pose3 <- readRDS(here::here("raw_data", "presabs_03_camF_Pose3.rds"))
presabs_04_camD_Pose1 <- readRDS(here::here("raw_data", "presabs_04_camD_Pose1.rds"))
presabs_04_camD_Pose2 <- readRDS(here::here("raw_data", "presabs_04_camD_Pose2.rds"))
presabs_04_camD_Pose3 <- readRDS(here::here("raw_data", "presabs_04_camD_Pose3.rds"))
presabs_04_camI_Pose1 <- readRDS(here::here("raw_data", "presabs_04_camI_Pose1.rds"))
presabs_04_camI_Pose2 <- readRDS(here::here("raw_data", "presabs_04_camI_Pose2.rds"))
presabs_04_camI_Pose3 <- readRDS(here::here("raw_data", "presabs_04_camI_Pose3.rds"))
presabs_05_camE_Pose1 <- readRDS(here::here("raw_data", "presabs_05_camE_Pose1.rds"))
presabs_05_camE_Pose2 <- readRDS(here::here("raw_data", "presabs_05_camE_Pose2.rds"))
presabs_05_camE_Pose3 <- readRDS(here::here("raw_data", "presabs_05_camE_Pose3.rds"))
presabs_05_camF_Pose1 <- readRDS(here::here("raw_data", "presabs_05_camF_Pose1.rds"))
presabs_05_camF_Pose2 <- readRDS(here::here("raw_data", "presabs_05_camF_Pose2.rds"))
presabs_05_camF_Pose3 <- readRDS(here::here("raw_data", "presabs_05_camF_Pose3.rds"))
presabs_06_camD_Pose1 <- readRDS(here::here("raw_data", "presabs_06_camD_Pose1.rds"))
presabs_06_camD_Pose2 <- readRDS(here::here("raw_data", "presabs_06_camD_Pose2.rds"))
presabs_06_camD_Pose3 <- readRDS(here::here("raw_data", "presabs_06_camD_Pose3.rds"))
presabs_06_camI_Pose1 <- readRDS(here::here("raw_data", "presabs_06_camI_Pose1.rds"))
presabs_06_camI_Pose2 <- readRDS(here::here("raw_data", "presabs_06_camI_Pose2.rds"))
presabs_06_camI_Pose3 <- readRDS(here::here("raw_data", "presabs_06_camI_Pose3.rds"))



# 2 - Get the names of species interacting with the substrate (annotated) ####


df_list <- list(presabs_03_A1, presabs_03_B1, presabs_03_C2,
             presabs_03_camE_Pose1, presabs_03_camE_Pose2, presabs_03_camE_Pose3,
             presabs_03_camF_Pose1, presabs_03_camF_Pose2, presabs_03_camF_Pose3,
             presabs_04_A1, presabs_04_B1, presabs_04_C2,
             presabs_04_camD_Pose1, presabs_04_camD_Pose2, presabs_04_camD_Pose3,
             presabs_04_camI_Pose1, presabs_04_camI_Pose2, presabs_04_camI_Pose3,
             presabs_05_A1, presabs_05_B1, presabs_05_C2,
             presabs_05_camE_Pose1, presabs_05_camE_Pose2, presabs_05_camE_Pose3,
             presabs_05_camF_Pose1, presabs_05_camF_Pose2, presabs_05_camF_Pose3,
             presabs_06_A1, presabs_06_B1, presabs_06_C2,
             presabs_06_camD_Pose1, presabs_06_camD_Pose2, presabs_06_camD_Pose3,
             presabs_06_camI_Pose1, presabs_06_camI_Pose2, presabs_06_camI_Pose3)


sp_nm_interact_all <- get.sp.nm(df_list)


# 3 - Check species according to their diet and subset ####


# Traits data from Parraviccini et al 2021 ProcB:
sp_diet <- read.csv(here::here("raw_data", "diet_Parravicini2.csv"))

# Create a new column with genus + species information:
sp_diet <- sp_diet %>%
            dplyr::mutate(sp_diet, Latin_nm = paste0(Genus, sep = "_", Species))

# Only keep species diet and latin name:
sp_diet <- sp_diet[, c("Latin_nm", "Diet_Mouillot_2014",
                       "Diet_Parravicini_2020")]

# Check that species annotated are all on the diet data:
setdiff(sp_nm_interact_all, sp_diet$Latin_nm)

# Note: In fact Ac_Cten_dark -> Ctenochaetus striatus: rename ,
# ... Scarus_sordidus -> Chlorurus sordidus: rename ,
# ... Labridae_Thalassoma -> only occur on 04 camD Pose 1 and Pose 2: remove
# ... Scarus_ferrugineus -> does not exist in the db but cf Paraviccini et ...
# ... al 2020 PlosB: Herbivores Microvores Detritivores
# ... Cheilinus quinquecinctus -> Cheilinus fasciatus

# Correct accordingly:
sp_nm_interact_all <- sp_nm_interact_all[which(! sp_nm_interact_all %in%
                                                 c("Ac_Cten_dark",
                                                   "Scarus_sordidus",
                                                   "Labridae_Thalassoma"))]
sp_nm_interact_all <- append(sp_nm_interact_all, "Ctenochaetus_striatus")
sp_nm_interact_all <- append(sp_nm_interact_all, "Chlorurus_sordidus")


sp_diet <- sp_diet %>%
  dplyr::add_row(Latin_nm = "Scarus_ferrugineus",
                 Diet_Mouillot_2014 = NA,
                 Diet_Parravicini_2020 = "Herbivores Microvores Detritivores")

sp_nm_interact_all <- sp_nm_interact_all[which(sp_nm_interact_all != "Cheilinus_quinquecinctus")]

# Only keep the annotated species:
sp_diet <- sp_diet %>%
  dplyr::filter(Latin_nm %in% sp_nm_interact_all)

saveRDS(sp_diet, here::here("transformed_data", "sp_diet_final.rds"))

# Note: Only Cheilinus_quinquecinctus has no info but it's a predatory ...
# ... species, thus it will not be kept (annotation error)

# Count the diet of the annotated species ie those which interacted ...
# ... with the substrate:

sp_diet %>%
  dplyr::group_by(Diet_Parravicini_2020) %>%
  count(Diet_Parravicini_2020)


# 4 - Which species on each site (FPA vs PPA) ####


# FPA: 03 and 05, PPA: 04 and 06

# FPA:
df_FPA_list <- list(presabs_03_A1, presabs_03_B1, presabs_03_C2,
                presabs_03_camE_Pose1, presabs_03_camE_Pose2, presabs_03_camE_Pose3,
                presabs_03_camF_Pose1, presabs_03_camF_Pose2, presabs_03_camF_Pose3,
                presabs_05_camE_Pose1, presabs_05_camE_Pose2, presabs_05_camE_Pose3,
                presabs_05_camF_Pose1, presabs_05_camF_Pose2, presabs_05_camF_Pose3)

sp_nm_interact_FPA <- get.sp.nm(df_FPA_list)
sort(sp_nm_interact_FPA)

# rename Act_Cten_dark:
sp_nm_interact_FPA <- sp_nm_interact_FPA[which(! sp_nm_interact_FPA %in%
                                                 c("Ac_Cten_dark"))]
sp_nm_interact_FPA <- append(sp_nm_interact_FPA, "Ctenochaetus_striatus")


# PPA:
df_PPA_list <- list(presabs_04_A1, presabs_04_B1, presabs_04_C2,
                    presabs_04_camD_Pose1, presabs_04_camD_Pose2, presabs_04_camD_Pose3,
                    presabs_04_camI_Pose1, presabs_04_camI_Pose2, presabs_04_camI_Pose3,
                    presabs_06_A1, presabs_06_B1, presabs_06_C2,
                    presabs_06_camD_Pose1, presabs_06_camD_Pose2, presabs_06_camD_Pose3,
                    presabs_06_camI_Pose1, presabs_06_camI_Pose2, presabs_06_camI_Pose3)

sp_nm_interact_PPA <- get.sp.nm(df_PPA_list)
sort(sp_nm_interact_PPA)

# rename Act_Cten_dark:
sp_nm_interact_PPA <- sp_nm_interact_PPA[which(! sp_nm_interact_PPA %in%
                                                 c("Ac_Cten_dark",
                                                   "Labridae_Thalassoma",
                                                   "Scarus_sordidus"))]
sp_nm_interact_PPA <- append(sp_nm_interact_PPA, "Ctenochaetus_striatus")
# as Chlorurus sordidus already in the list of names: does not have to be added


# 5 - Number of predators and diet in each site ####

# Load data with species names (first Mayotte Paper subm in Cons Biol):
sp_nm_FPA <- readRDS(here::here("raw_data", "sp_nm_NG.rds"))
sp_nm_PPA <- readRDS(here::here("raw_data", "sp_nm_B.rds"))

# Load data from the first Mayotte paper to see species diet:
sp_tr_may1 <- readRDS(here::here("raw_data", "sp_tr_final.rds"))

# get the sp*tr data for species in the FPA and in the PPA:
sp_tr_FPA <- dplyr::filter(sp_tr_may1, rownames(sp_tr_may1) %in% sp_nm_FPA)
sp_tr_PPA <- dplyr::filter(sp_tr_may1, rownames(sp_tr_may1) %in% sp_nm_PPA)

# plot the diet for each site:
diet_FPA_plot <- ggplot2::ggplot(data = sp_tr_FPA) +
  ggplot2::geom_histogram(ggplot2::aes(x = Diets),
                          stat = "count") +
  ggplot2::ylim(0, 31) +
  ggplot2::ggtitle("Diets in the FPA")


diet_PPA_plot <- ggplot2::ggplot(data = sp_tr_PPA) +
  ggplot2::geom_histogram(ggplot2::aes(x = Diets),
                          stat = "count") +
  ggplot2::ylim(0, 31) +
  ggplot2::ggtitle("Diets in the PPA")


# compute the proportion of piscivore species:
FPA_pisc <- (nrow(dplyr::filter(sp_tr_FPA, Diets == "Pis"))/nrow(sp_tr_FPA))*100
PPA_pisc <- (nrow(dplyr::filter(sp_tr_PPA, Diets == "Pis"))/nrow(sp_tr_PPA))*100

# only keep species unique to a site:
FPA_only_tr <- dplyr::filter(sp_tr_FPA, ! rownames(sp_tr_FPA) %in% rownames(sp_tr_PPA))
PPA_only_tr <- dplyr::filter(sp_tr_PPA, ! rownames(sp_tr_PPA) %in% rownames(sp_tr_FPA))

# compute the proportion of unique species which are piscivores:
FPA_only_pisc <- (nrow(dplyr::filter(FPA_only_tr, Diets == "Pis"))/nrow(FPA_only_tr))*100
PPA_only_pisc <- (nrow(dplyr::filter(PPA_only_tr, Diets == "Pis"))/nrow(PPA_only_tr))*100

# plot the diet for each site for species unique to a site:
diet_FPA_only_plot <- ggplot2::ggplot(data = FPA_only_tr) +
  ggplot2::geom_histogram(ggplot2::aes(x = Diets),
                          stat = "count") +
  ggplot2::ylim(0, 11) +
  ggplot2::ggtitle("Diets in the FPA - only seen in the FPA")


diet_PPA_only_plot <- ggplot2::ggplot(data = PPA_only_tr) +
  ggplot2::geom_histogram(ggplot2::aes(x = Diets),
                          stat = "count") +
  ggplot2::ylim(0, 11) +
  ggplot2::ggtitle("Diets in the PPA - only seen in the PPA")
