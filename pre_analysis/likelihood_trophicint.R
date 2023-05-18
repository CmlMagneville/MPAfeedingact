################################################################################
##
## Script check if each trophic categorisation is ok when looking at ...
## ... likelihood ratios in Parravicini et al.
##
## likelihood_trophicint.R.R
##
## 18/05/2023
##
## Camille Magneville
##
################################################################################



# 1 - Load data ####

# Load Likelihood Ratio - Table S3 - Parravicini:
likelihood_df <- read.csv(here::here("raw_data", "Table_S3_Parravicinietal.csv"))

# Load my diet data (based on Parravicini and ...):
diet_df <- readRDS(here::here("transformed_data", "sp_diet_final.rds"))


# 2 - Analyse ####

# Link the two tables:
colnames(diet_df)[1] <- "species"
tot_data <- dplyr::inner_join(diet_df, likelihood_df, by = "species")

# Remove useless columns:
tot_data <- tot_data[, -c(2, 4)]

# Add new column which will contain the max likelihood ration value:
tot_data <- dplyr::mutate(tot_data, max_lr = pmax(tot_data$p1_m, tot_data$p2_m,
                                        tot_data$p3_m, tot_data$p4_m,
                                        tot_data$p5_m,tot_data$p6_m,
                                        tot_data$p7_m, tot_data$p8_m))

# Remove planktivores:
tot_data <- tot_data[which(tot_data$Diet_Parravicini_2020 != "Planktivores"), ]

# 3 - Visualise and conclusions ####

# Corallivores:
corallivores_data <- tot_data[which(tot_data$Diet_Parravicini_2020 == "Corallivores"), ]
# 70% of species have a LR above 70% for the choosen Trophic level
# species under 70% are: Heniochus acuminatus (0.27 - p5 (0.23)),
# Chaetodon auriga (0.43 - p5 (0.23) p7 (0.18) p4 (0.12))
# Labrichthys unilineatus (0.54 - p5 (0.21))


herb_data <- tot_data[which(tot_data$Diet_Parravicini_2020 == "Herbivores Microvores Detritivores"), ]
# All species have a LR above 88% for the choosen Trophic level

invert_data <- tot_data[which(tot_data$Diet_Parravicini_2020 %in%
                                c("Microinvertivores", "Crustacivores",
                                  "sessile invertivores","Macroinvertivores")), ]
# 42% of pecies have a LR above 70% for the choosen trophic level (more detailed
# than in the paper ie micro inv,macro inv, sessile inv, crustac) but if we look
# at close LR, they are in the mico/macro/sessile/crustac group so it's ok
# as we will pull them in the paper ;)
