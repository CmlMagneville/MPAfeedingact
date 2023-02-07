################################################################################
##
## Functions to get one dataframe per feeding act and day and plot temporal ...
## ... turnover
##
## 30/01/2023
##
## Camille Magneville
##
################################################################################





#' Title
#'
#' @param bites_seq_df a dataframe with species bites through sequences
#' @param sp_feedact_nm_vect a vector containing te name of the feedact to study
#'
#' @return a dataframe with only the species to keep (those of the studied
#' feedact)
#' @export
#'
#' @examples
#'


subset.df.feedact <- function(bites_seq_df, sp_feedact_nm_vect) {


  # subset and only keep species from the feedact studied:
  feedact_seq_df <- dplyr::select(bites_seq_df, any_of(sp_feedact_nm_vect))

  # add sequences, hours and site information:
  feedact_seq_df <- tibble::add_column(feedact_seq_df,
                                       bites_seq_df[, c(ncol(bites_seq_df),
                                                       ncol(bites_seq_df) - 1,
                                                       ncol(bites_seq_df) - 2)])

  return(feedact_seq_df)

}


#' Compute temporal turnover between time slots of the same day
#'
#' @param list_df a vector gathering the dataframes of bites taken from species
#' of a given feedact (the one studied) over sequences
#'
#' @return a dataframe with beta dissim and turnover values for the three
#' timeslots pairs for all days and all sites
#'
#' @export
#'
#' @examples
#'

compute.temporal.turn <- function(list_df) {


  # Create a new df that will contain turnover and dissim values dor all days:
  dissim_turn_df <- as.data.frame(matrix(ncol = 5, nrow = 1, NA))
  colnames(dissim_turn_df) <- c("day", "site", "pair_timeslots",
                                "tot_dissim", "turnover")

  # loop on each df (gathering the 4 sampling days):
  for (i in (1:length(list_df))) {

    studied_df <- list_df[[i]]
    rownames(studied_df) <- NULL

    # compute new dataframe with timelsots instead of sequences (add seq bites):
    timeslot_df <- as.data.frame(matrix(ncol = ncol(studied_df) - 1,
                                      nrow = 1, NA))
    colnames(timeslot_df) <- colnames(studied_df[, -ncol(studied_df)])


    # for each timeslot: add bites from associated sequences:
    for (j in c(1, 5, 9)) {

        studied_rows <- c(j, j + 1, j + 2, j + 3)

        bites_timeslot <- apply(studied_df[studied_rows,
                                          -c(ncol(studied_df),
                                             ncol(studied_df) - 1,
                                             ncol(studied_df) - 2)],
                                           2, sum)

        timeslot_df <- dplyr::bind_rows(timeslot_df, c(bites_timeslot,
                                        site = unique(studied_df$site),
                                        hour = studied_df$hour[j]))


    } # end for each timeslot


    # add a "timeslot" column:
    timeslot_df <- timeslot_df[-1, ]
    timeslot_df[, "timeslot_nb"] <- c("07:30", "11:45", "16:00")

    # Compute tot dissim and turnover:
    prepare_timeslot_df <- timeslot_df
    rownames(prepare_timeslot_df) <- prepare_timeslot_df$timeslot_nb
    prepare_timeslot_df <- prepare_timeslot_df[, -c(ncol(prepare_timeslot_df),
                                                    ncol(prepare_timeslot_df) - 1,
                                                    ncol(prepare_timeslot_df) - 2)]
    prepare_timeslot_df <- apply(prepare_timeslot_df, 2, as.numeric)
    prepare_timeslot_df[prepare_timeslot_df > 0] <- 1
    rownames(prepare_timeslot_df) <- c("07:30", "11:45", "16:00")
    beta_results <- betapart::beta.pair(prepare_timeslot_df, index.family = "jaccard")

    # Get tot dissim and turnover results:
    pair1_tot <- beta_results$beta.jac[1]
    pair2_tot <- beta_results$beta.jac[2]
    pair3_tot <- beta_results$beta.jac[3]
    pair1_turn <- beta_results$beta.jtu[1]
    pair2_turn <- beta_results$beta.jtu[2]
    pair3_turn <- beta_results$beta.jtu[3]

    # Put values in the df:
    # First pair:
    dissim_turn_df <- dplyr::bind_rows(dissim_turn_df,
                                       c(day = paste0("day", sep ="_", i),
                                         site = unique(studied_df$site),
                                         pair_timeslots = "1_2",
                                         tot_dissim = pair1_tot,
                                         turnover = pair1_turn))
    # Second pair:
    dissim_turn_df <- dplyr::bind_rows(dissim_turn_df,
                                       c(day = paste0("day", sep ="_", i),
                                         site = unique(studied_df$site),
                                         pair_timeslots = "1_3",
                                         tot_dissim = pair2_tot,
                                         turnover = pair2_turn))
    # Third pair:
    dissim_turn_df <- dplyr::bind_rows(dissim_turn_df,
                                       c(day = paste0("day", sep ="_", i),
                                         site = unique(studied_df$site),
                                         pair_timeslots = "2_3",
                                         tot_dissim = pair3_tot,
                                         turnover = pair3_turn))


  } # end loop on dfs

  # remove first row filled with NAs:
  dissim_turn_df <- dissim_turn_df[-1, ]

  # Create a Pturn column which gather the propo of dissim due to turn:
  dissim_turn_df$Pturn <- (as.numeric(dissim_turn_df$turnover) /
                            as.numeric(dissim_turn_df$tot_dissim))*100


  return(dissim_turn_df)

}



#' Compute temporal turnover between all timeslots (site and days pooled)
#'
#' @param list_df a vector gathering the dataframes of bites taken from species
#' of a given feedact (the one studied) over sequences
#'
#' @return a dataframe with beta dissim and turnover values for the three
#' timeslots pairs for all days and all sites
#'
#' @export
#'
#' @examples
#'

compute.temporal.turn.allday <- function(list_df) {


  # create a vector that will contain rownames values of the global df:
  # day_site_timeslot type
  global_timeslot_rownm <- c()

  # loop on each df (gathering the 4 sampling days):
  for (i in (1:length(list_df))) {

    studied_df <- list_df[[i]]
    rownames(studied_df) <- NULL

    # compute new dataframe with timelsots instead of sequences (add seq bites):
    timeslot_df <- as.data.frame(matrix(ncol = ncol(studied_df) - 1,
                                        nrow = 1, NA))
    colnames(timeslot_df) <- colnames(studied_df[, -ncol(studied_df)])


    # for each timeslot: add bites from associated sequences:
    for (j in c(1, 5, 9)) {

      studied_rows <- c(j, j + 1, j + 2, j + 3)

      bites_timeslot <- apply(studied_df[studied_rows,
                                         -c(ncol(studied_df),
                                            ncol(studied_df) - 1,
                                            ncol(studied_df) - 2)],
                              2, sum)

      timeslot_df <- dplyr::bind_rows(timeslot_df, c(bites_timeslot,
                                                     site = unique(studied_df$site),
                                                     hour = studied_df$hour[j]))


    } # end for each timeslot


    # add a "timeslot" column:
    timeslot_df <- timeslot_df[-1, ]
    timeslot_df[, "timeslot_nb"] <- c("07:30", "11:45", "16:00")
    global_timeslot_rownm <- append(global_timeslot_rownm, paste0("day", sep = "_", i,
                                    sep = "_", timeslot_df$site,
                                    sep = "_", timeslot_df$timeslot_nb))


    #  dataframe: bites number"timeslot1" per timeslots (all timeslots from all days):
    if (i == 1) {
      global_timeslot_df <- timeslot_df[, -c(ncol(timeslot_df),
                                             ncol(timeslot_df) - 1,
                                             ncol(timeslot_df) -2) ]
    }

    else {
      timeslot_df <- timeslot_df[, -c(ncol(timeslot_df),
                                      ncol(timeslot_df) - 1,
                                      ncol(timeslot_df) -2) ]
      global_timeslot_df <- dplyr::bind_rows(global_timeslot_df,
                                             timeslot_df)

    }

  }

  # give rownames toglobal_timeslot_df:
  rownames(global_timeslot_df) <- global_timeslot_rownm
  # where NA -> 0
  global_timeslot_df[is.na(global_timeslot_df)] <- 0



  # Compute tot dissim and turnover:
  global_timeslot_df <- apply(global_timeslot_df, 2, as.numeric)
  global_timeslot_df[global_timeslot_df > 0] <- 1
  rownames(global_timeslot_df) <- global_timeslot_rownm
  beta_results <- betapart::beta.pair(global_timeslot_df, index.family = "jaccard")

  # Get tot dissim and turnover results:
  tot_dissim <- beta_results[[3]]

  # Get turn:
  turn_dissim <- beta_results[[1]]

  return(list(tot_dissim, turn_dissim))

}
