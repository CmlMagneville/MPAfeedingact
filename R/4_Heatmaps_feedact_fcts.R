################################################################################
##
## Functions to gather data at the sequence scale, compute the number ...
## ... of bites per feeding activity and plot heatmaps of feeding activity
##
## 4_Heatmaps_feedact_fcts.R
##
## 14/11/2022
##
## Camille Magneville
##
################################################################################




#' Gathers data at the sequence scale when annotations are not continuous
#'
#' @param start_times
#' @param end_times
#' @param seq_length
#' @param no_seq_length
#' @param bites_df
#'
#' @return
#' @export
#'
#' @examples

data.sequences.scale <- function(start_times, end_times,
                                 seq_length, no_seq_length,
                                 bites_df) {

  # build the new dataframe:
  seq_final_bites_df <- as.data.frame(matrix(nrow = 1, ncol = ncol(bites_df) + 2, NA))
  colnames(seq_final_bites_df) <- c(colnames(bites_df), "seq", "hour")

  # count the number of sequences already computes:
  seq_nb <- 1

  # loop on the poses
  for (i in c(1:length(start_times))) {

    # get the time the pose starts:
    pose_start <- hms::as_hms(start_times[i])

    # get the time the pose ends:
    pose_end <- hms::as_hms(end_times[i])

    # select the rows in the studied pose:
    pose_data <- dplyr::filter(bites_df, hms::as_hms(rownames(bites_df)) >= hms::as_hms(pose_start)
                                        & hms::as_hms(rownames(bites_df)) <= hms::as_hms(pose_end))


    # define a counter:
    n <- hms::as_hms(rownames(pose_data)[1])

    # while n is <= to the end of the pose:
    while (n < hms::as_hms(rownames(pose_data)[nrow(pose_data)])) {


      # get the data only in the studied sequence:
      seq_data <- dplyr::filter(pose_data, hms::as_hms(rownames(pose_data)) >= n
                                & hms::as_hms(rownames(pose_data)) <= hms::as_hms(n +
                                                                       hms::as_hms(seq_length)))

      # add a new row with no information:
      seq_final_bites_df <- dplyr::add_row(seq_final_bites_df)

      # loop on the column = species:
      for (j in c(1:ncol(seq_data))) {

        sp_nm <- colnames(seq_data)[j]

        sum_sp <- apply(seq_data, 2, sum)[[j]]

        # fill the final df:
        seq_final_bites_df[nrow(seq_final_bites_df), sp_nm] <- sum_sp

      }

      # add sequence and hour information:
      seq_final_bites_df[nrow(seq_final_bites_df), "hour"] <- n
      seq_final_bites_df[nrow(seq_final_bites_df), "seq"] <- paste0("seq", sep = "_",
                                                                    seq_nb)

      seq_nb <- seq_nb + 1

      # update n:
      n <- hms::as_hms(n + hms::as_hms(seq_length) + hms::as_hms(no_seq_length))

    } # end while

  } # end loop on poses

  # remove the first row which only has NA:
  seq_final_bites_df <- seq_final_bites_df[-1, ]

  seq_final_bites_df$hour <- hms::as_hms(seq_final_bites_df$hour)

  return(seq_final_bites_df)

}




#' Compute the number of bites per annotated sequence and trophic guilds
#'
#' @param day
#' @param seq_df
#' @param sp_diet
#'
#' @return
#' @export
#'
#' @examples


compute.bites.seq <- function(day,
                              seq_df,
                              sp_diet) {


  # create the new dataframe:
  final_seq_guild_df <- as.data.frame(matrix(ncol = 8, nrow = nrow(seq_df), NA))
  colnames(final_seq_guild_df) <- c("site", "day", "seq", "hour", "tot_bites",
                                    "coral_bites",
                                    "invert_bites", "herb_bites")
  final_seq_guild_df$site <- seq_df$site
  final_seq_guild_df$seq <- seq_df$seq
  final_seq_guild_df$hour <- seq_df$hour
  final_seq_guild_df$day <- rep(day, nrow(final_seq_guild_df))

  # gather invertivores:
  sp_diet[which(sp_diet$Diet_Parravicini_2020 %in%
                               c("Macroinvertivores",
                                 "sessile invertivores",
                                 "Microinvertivores",
                                 "Crustacivores")), "Diet_Parravicini_2020"] <- "Invertivores"

  # rename HMD:
  sp_diet[which(sp_diet$Diet_Parravicini_2020 %in%
                               c("Herbivores Microvores Detritivores")), "Diet_Parravicini_2020"] <- "HMD"

  # get coral sp names:
  coral_sp <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Corallivores")]
  # get invert sp names:
  invert_sp <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "Invertivores")]
  # get herb sp names:
  herb_sp <- sp_diet$Latin_nm[which(sp_diet$Diet_Parravicini_2020 == "HMD")]

  # remove the 3 last columns of seq df which does not contain species info:
  seq_df <- seq_df[, -c(ncol(seq_df), ncol(seq_df) - 1, ncol(seq_df) - 2)]

  # compute the total number of bites:
  tot_bites <- apply(seq_df, 1, sum)
  final_seq_guild_df$tot_bites <- tot_bites

  # coral bites
  coral_df <- seq_df[, colnames(seq_df[which(colnames(seq_df) %in% coral_sp)])]
  coral_bites <- apply(coral_df, 1, sum)
  final_seq_guild_df$coral_bites <- coral_bites

  # herb bites
  herb_df <- seq_df[, colnames(seq_df[which(colnames(seq_df) %in% herb_sp)])]
  herb_bites <- apply(herb_df, 1, sum)
  final_seq_guild_df$herb_bites <- herb_bites

  # invert bites
  invert_df <- seq_df[, colnames(seq_df[which(colnames(seq_df) %in% invert_sp)])]
  invert_bites <- apply(invert_df, 1, sum)
  final_seq_guild_df$invert_bites <- invert_bites

  return(final_seq_guild_df)


}





#' Plot heatmaps of activity through time with one heatmap per trophic guild
#'
#' @param bites_seq_df_list
#' @param color_palette
#'
#' @return
#' @export
#'
#' @examples

plot.heatmaps.act.intensity <- function(bites_seq_df_list) {


  # link the tables:
  bites_seq_final <- dplyr::bind_rows(bites_seq_df_list[[1]],
                                      bites_seq_df_list[[2]],
                                      bites_seq_df_list[[3]],
                                      bites_seq_df_list[[4]])


  # loop on the trophic guilds (and total activity):
  trophic_guilds <- c("tot_bites", "coral_bites", "herb_bites",
                      "invert_bites")

  # create a list that will contain all plots:
  plot_list <- list()

  # from 2 (and not 1) because do not need the total bites heatmap:
  for (i in c(2:length(trophic_guilds))) {


    guild_nm <- trophic_guilds[i]

    # only keep the studied column:
    subset_data <- dplyr::select(bites_seq_final, site, day, seq, hour, any_of(guild_nm))

    # prepare FPA data:
    FPA_subset_data <- dplyr::filter(subset_data, site == "FPA")

    # prepare PPA data:
    PPA_subset_data <- dplyr::filter(subset_data, site == "PPA")

    # factor and order the sequence column:
    FPA_subset_data$seq <- factor(FPA_subset_data$seq,
              levels = paste0("seq", sep ="_", c(1:(nrow(FPA_subset_data)/2))))
    PPA_subset_data$seq <- factor(PPA_subset_data$seq,
              levels = paste0("seq", sep ="_", c(1:(nrow(PPA_subset_data)/2))))


    # compute 5 classes of bites intensity based on FPA and PPA values:
    min_value <- min(min(FPA_subset_data[, ncol(FPA_subset_data)]),
                     min(PPA_subset_data[, ncol(PPA_subset_data)]))
    max_value <- max(max(FPA_subset_data[, ncol(FPA_subset_data)]),
                     max(PPA_subset_data[, ncol(PPA_subset_data)]))
    interval_span <- round((max_value - min_value)/5)
    first_interval <- c(1, 1 + interval_span)
    second_interval <- c(first_interval[2] + 1, first_interval[2] + interval_span)
    third_interval <- c(second_interval[2] + 1, second_interval[2] + interval_span)
    fourth_interval <- c(third_interval[2] + 1, third_interval[2] + interval_span)
    fifth_interval <- c(fourth_interval[2] + 1, max_value)

    # Add new column to store this new variable:
    # FPA
    FPA_subset_data$bites_class <- rep(NA, nrow(FPA_subset_data))
    FPA_subset_data[which(FPA_subset_data[, ncol(FPA_subset_data) - 1] == 0),
                    "bites_class"] <- "0"
    FPA_subset_data[which(FPA_subset_data[, ncol(FPA_subset_data) - 1] <= first_interval[2] &
                          FPA_subset_data[, ncol(FPA_subset_data) - 1] >= first_interval[1]),
                    "bites_class"] <- paste0(first_interval[1], sep = "-", first_interval[2])
    FPA_subset_data[which(FPA_subset_data[, ncol(FPA_subset_data) - 1] <= second_interval[2] &
                            FPA_subset_data[, ncol(FPA_subset_data) - 1] >= second_interval[1]),
                    "bites_class"] <- paste0(second_interval[1], sep = "-", second_interval[2])
    FPA_subset_data[which(FPA_subset_data[, ncol(FPA_subset_data) - 1] <= third_interval[2] &
                            FPA_subset_data[, ncol(FPA_subset_data) - 1] >= third_interval[1]),
                    "bites_class"] <- paste0(third_interval[1], sep = "-", third_interval[2])
    FPA_subset_data[which(FPA_subset_data[, ncol(FPA_subset_data) - 1] <= fourth_interval[2] &
                            FPA_subset_data[, ncol(FPA_subset_data) - 1] >= fourth_interval[1]),
                    "bites_class"] <- paste0(fourth_interval[1], sep = "-", fourth_interval[2])
    FPA_subset_data[which(FPA_subset_data[, ncol(FPA_subset_data) - 1] <= fifth_interval[2] &
                            FPA_subset_data[, ncol(FPA_subset_data) - 1] >= fifth_interval[1]),
                    "bites_class"] <- paste0(fifth_interval[1], sep = "-", fifth_interval[2])
    # PPA
    PPA_subset_data$bites_class <- rep(NA, nrow(PPA_subset_data))
    PPA_subset_data[which(PPA_subset_data[, ncol(PPA_subset_data) - 1] == 0),
                    "bites_class"] <- "0"
    PPA_subset_data[which(PPA_subset_data[, ncol(PPA_subset_data) - 1] <= first_interval[2] &
                            PPA_subset_data[, ncol(PPA_subset_data) - 1] >= first_interval[1]),
                    "bites_class"] <- paste0(first_interval[1], sep = "-", first_interval[2])
    PPA_subset_data[which(PPA_subset_data[, ncol(PPA_subset_data) - 1] <= second_interval[2] &
                            PPA_subset_data[, ncol(PPA_subset_data) - 1] >= second_interval[1]),
                    "bites_class"] <- paste0(second_interval[1], sep = "-", second_interval[2])
    PPA_subset_data[which(PPA_subset_data[, ncol(PPA_subset_data) - 1] <= third_interval[2] &
                            PPA_subset_data[, ncol(PPA_subset_data) - 1] >= third_interval[1]),
                    "bites_class"] <- paste0(third_interval[1], sep = "-", third_interval[2])
    PPA_subset_data[which(PPA_subset_data[, ncol(PPA_subset_data) - 1] <= fourth_interval[2] &
                            PPA_subset_data[, ncol(PPA_subset_data) - 1] >= fourth_interval[1]),
                    "bites_class"] <- paste0(fourth_interval[1], sep = "-", fourth_interval[2])
    PPA_subset_data[which(PPA_subset_data[, ncol(PPA_subset_data) - 1] <= fifth_interval[2] &
                            PPA_subset_data[, ncol(PPA_subset_data) - 1] >= fifth_interval[1]),
                    "bites_class"] <- paste0(fifth_interval[1], sep = "-", fifth_interval[2])


    # get the right color palette according to the studied feedact:
    if (guild_nm == "coral_bites") {
      col_vect_feedact <-  c("white", rev(hcl.colors(5, palette = "GnBu")))
      names(col_vect_feedact) <- c("0", paste0(first_interval[1], sep = "-", first_interval[2]),
                                   paste0(second_interval[1], sep = "-", second_interval[2]),
                                   paste0(third_interval[1], sep = "-", third_interval[2]),
                                   paste0(fourth_interval[1], sep = "-", fourth_interval[2]),
                                   paste0(fifth_interval[1], sep = "-", fifth_interval[2]))
    }
    if (guild_nm == "herb_bites") {
      col_vect_feedact <-  c("white", rev(hcl.colors(5, palette = "YlGnBu")))
      names(col_vect_feedact) <- c("0", paste0(first_interval[1], sep = "-", first_interval[2]),
                                   paste0(second_interval[1], sep = "-", second_interval[2]),
                                   paste0(third_interval[1], sep = "-", third_interval[2]),
                                   paste0(fourth_interval[1], sep = "-", fourth_interval[2]),
                                   paste0(fifth_interval[1], sep = "-", fifth_interval[2]))
    }
    if (guild_nm == "invert_bites") {
      col_vect_feedact <-  c("white", rev(hcl.colors(5, palette = "Heat")))
      names(col_vect_feedact) <- c("0", paste0(first_interval[1], sep = "-", first_interval[2]),
                                   paste0(second_interval[1], sep = "-", second_interval[2]),
                                   paste0(third_interval[1], sep = "-", third_interval[2]),
                                   paste0(fourth_interval[1], sep = "-", fourth_interval[2]),
                                   paste0(fifth_interval[1], sep = "-", fifth_interval[2]))
    }


    # factor and order the bites_class:
    FPA_subset_data$bites_class <- factor(FPA_subset_data$bites_class,
                                  levels = c("0", paste0(first_interval[1], sep = "-", first_interval[2]),
                                             paste0(second_interval[1], sep = "-", second_interval[2]),
                                             paste0(third_interval[1], sep = "-", third_interval[2]),
                                             paste0(fourth_interval[1], sep = "-", fourth_interval[2]),
                                             paste0(fifth_interval[1], sep = "-", fifth_interval[2])))
    PPA_subset_data$bites_class <- factor(PPA_subset_data$bites_class,
                                  levels = c("0", paste0(first_interval[1], sep = "-", first_interval[2]),
                                             paste0(second_interval[1], sep = "-", second_interval[2]),
                                             paste0(third_interval[1], sep = "-", third_interval[2]),
                                             paste0(fourth_interval[1], sep = "-", fourth_interval[2]),
                                             paste0(fifth_interval[1], sep = "-", fifth_interval[2])))


    # plot FPA:
    heatmap_FPA <- ggplot2::ggplot(FPA_subset_data,
                  ggplot2::aes(x = seq, y = day, fill = bites_class)) +

      ggplot2::geom_tile(color = "grey") +

      ggplot2::scale_fill_manual(values = col_vect_feedact) +

      # harrypotter::scale_fill_hp(option = "Always", name = "Number of bites",
                                 # limits = c(min_value, max_value), oob = scales::squish) +

      ggplot2::scale_x_discrete(labels = FPA_subset_data$hour) +

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey"),
                     panel.grid.major = ggplot2::element_line(colour = "white")) +

      ggplot2::xlab("") +
      ggplot2::ylab("") +

      ggplot2::ggtitle("N'Gouja")


    # plot PPA:
    heatmap_PPA <- ggplot2::ggplot(PPA_subset_data,
                                   ggplot2::aes(x = seq, y = day, fill = bites_class)) +

      ggplot2::geom_tile(color = "grey") +

      ggplot2::scale_fill_manual(values = col_vect_feedact) +

      #harrypotter::scale_fill_hp(option = "Always", name = "Number of bites",
                                 #limits = c(min_value, max_value), oob = scales::squish) +

      ggplot2::scale_x_discrete(labels = PPA_subset_data$hour) +

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey"),
                     panel.grid.major = ggplot2::element_line(colour = "white")) +

      ggplot2::xlab("") +
      ggplot2::ylab("") +

      ggplot2::ggtitle("Bouéni")


    # combine both:
    if (guild_nm == "tot_bites") {
      plot_both <- (heatmap_FPA + heatmap_PPA) +
        patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                               ncol = 1, nrow = 2, guides = "collect") +
        patchwork::plot_annotation(title = "All feeding activities")
    }
    if (guild_nm == "coral_bites") {
      plot_both <- (heatmap_FPA + heatmap_PPA) +
        patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                               ncol = 1, nrow = 2, guides = "collect") +
        patchwork::plot_annotation(title = "Corallivores")
    }
    if (guild_nm == "herb_bites") {
      plot_both <- (heatmap_FPA + heatmap_PPA) +
        patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                               ncol = 1, nrow = 2, guides = "collect") +
        patchwork::plot_annotation(title = "Herbivores")
    }
    if (guild_nm == "invert_bites") {
      plot_both <- (heatmap_FPA + heatmap_PPA) +
        patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                               ncol = 1, nrow = 2, guides = "collect") +
        patchwork::plot_annotation(title = "Invertivores")
    }

    plot_list <- append(plot_list, plot_both)

    # save:
    ggplot2::ggsave(filename = here::here("outputs",
                                          paste0("heatmap", sep = "_",
                                                 guild_nm,
                                                 sep = "", ".pdf")),
                    plot = plot_both,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 10000,
                    units = "px",
                    dpi = 800)

  }

  return(plot_list)

}

