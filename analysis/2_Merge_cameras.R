################################################################################
##
## Script to merge dataframes from each cameras and get one df per sampling day
##
## 2_Merge_cameras.R
##
## 27/01/2023
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


# 2 - Gather dataframes to get one dataframe per day with all cameras pooled ####


start_time <- "07:30:00"
end_time <- "17:00:00"


# Day 03 - FPA:
list_LD_dfs <- list(presabs_03_A1, presabs_03_B1, presabs_03_C2)

list_SD_dfs <- list(presabs_03_camE_Pose1, presabs_03_camE_Pose2,
                    presabs_03_camE_Pose3, presabs_03_camF_Pose1,
                    presabs_03_camF_Pose2, presabs_03_camF_Pose3)

presabs_03_df <- merge.cam.vid.df(list_LD_dfs, list_SD_dfs,
                                  start_time, end_time)

saveRDS(presabs_03_df, here::here("transformed_data",
                                  "presabs_03_df.rds"))


# Day 05 - FPA:
list_LD_dfs <- list(presabs_05_A1, presabs_05_B1, presabs_05_C2)

list_SD_dfs <- list(presabs_05_camE_Pose1, presabs_05_camE_Pose2,
                    presabs_05_camE_Pose3, presabs_05_camF_Pose1,
                    presabs_05_camF_Pose2, presabs_05_camF_Pose3)

presabs_05_df <- merge.cam.vid.df(list_LD_dfs, list_SD_dfs,
                                  start_time, end_time)

saveRDS(presabs_05_df, here::here("transformed_data",
                                  "presabs_05_df.rds"))


# Day 04 - PPA:
list_LD_dfs <- list(presabs_04_A1, presabs_04_B1, presabs_04_C2)

list_SD_dfs <- list(presabs_04_camD_Pose1, presabs_04_camI_Pose2,
                    presabs_04_camD_Pose3, presabs_04_camI_Pose1,
                    presabs_04_camD_Pose2, presabs_04_camI_Pose3)

presabs_04_df <- merge.cam.vid.df(list_LD_dfs, list_SD_dfs,
                                  start_time, end_time)

saveRDS(presabs_04_df, here::here("transformed_data",
                                  "presabs_04_df.rds"))


# Day 06 - PPA:
list_LD_dfs <- list(presabs_06_A1, presabs_06_B1, presabs_06_C2)

list_SD_dfs <- list(presabs_06_camD_Pose1, presabs_06_camI_Pose2,
                    presabs_06_camD_Pose3, presabs_06_camI_Pose1,
                    presabs_06_camD_Pose2, presabs_06_camI_Pose3)

presabs_06_df <- merge.cam.vid.df(list_LD_dfs, list_SD_dfs,
                                  start_time, end_time)

saveRDS(presabs_06_df, here::here("transformed_data",
                                  "presabs_06_df.rds"))



