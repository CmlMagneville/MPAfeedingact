################################################################################
##
## Script to code function to plot species contributions to interactions ...
## ... for each trophic guild (one plot per guild)
##
## 5_Interactions_guilds_contribution_fcts.R
##
## 10/11/2022
##
## Camille Magneville
##
################################################################################



#' Compute the contribution to the feeding activity process for one site
#'
#' @param df_list a list containing the grazing dataframe of the studied
#' site with one dataframe for each day
#'
#' @param sp_FPA_vect a vector containing the name of the species seen on
#' the FPA site
#'
#' @param sp_PPA_vect a vector containing the name of the species seen on
#' the PPA site
#'
#' @param studied_site the name of the type of site studied: either "FPA" or
#' "PPA"
#'
#' @return two dataframes with the bite rate per species per day (site info) and
#' the mean bite rate over the days (per 5min per 10m2)
#'
#' @export
#'
#' @examples


compute.contrib.feedact <- function(df_list,
                                 sp_FPA_vect,
                                 sp_PPA_vect,
                                 studied_site) {


  # 1 - Build the df that will contain bites information per day:
  if (studied_site == "FPA") {
    bites_info_day_df <- as.data.frame(matrix(ncol = 5,
                                              nrow = 1,
                                              NA))
    colnames(bites_info_day_df) <- c("sp_nm", "feed_act_mean",
                                    "feed_act_sd", "site", "day")
  }

  if (studied_site == "PPA") {
    bites_info_day_df <- as.data.frame(matrix(ncol = 5,
                                              nrow = 1,
                                              NA))
    colnames(bites_info_day_df) <- c("sp_nm", "feed_act_mean",
                                     "feed_act_sd", "site", "day")
  }


  # Fill this df:
  for (i in c(1:length(df_list))) {

    studied_df <- df_list[[i]]

    # use numeric format:
    studied_df[, -c(ncol(studied_df), ncol(studied_df) - 1,
                    ncol(studied_df) - 2)] <- apply(studied_df[,
                                        -c(ncol(studied_df),
                                        ncol(studied_df) - 1,
                                        ncol(studied_df) - 2)], 2, as.numeric)

    # mean bites nb per annotated sequence: have bites nb per annotated seq (5min):
    # all cameras pooled: nb of bites / 5min/ 10m2:
    bites_annotated_seq_mean <- apply(studied_df[, -c(ncol(studied_df),
                                                      ncol(studied_df) - 1,
                                                      ncol(studied_df) - 2)], 2,
                                      mean)
    bites_annotated_seq_sd <- apply(studied_df[, -c(ncol(studied_df),
                                                    ncol(studied_df) - 1,
                                                    ncol(studied_df) - 2)], 2,
                                      sd)

    # get the data to fill the table:
    day <- paste0("day", sep = "_", i)

    for (sp_nm in names(bites_annotated_seq_mean)) {

      feed_act_mean <- round(bites_annotated_seq_mean[[sp_nm]], 2)
      feed_act_sd <- round(bites_annotated_seq_sd[[sp_nm]], 2)


      if (sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
        site <- "both"
      }

      if (sp_nm %in% sp_FPA_vect & ! sp_nm %in% sp_PPA_vect) {
        site <- "FPA_only"
      }

      if (! sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
        site <- "PPA_only"
      }

      bites_info_day_df <- dplyr::add_row(bites_info_day_df,
                                          sp_nm = sp_nm,
                                          feed_act_mean = feed_act_mean,
                                          feed_act_sd = feed_act_sd,
                                          site = site,
                                          day = day)


    } # end add each species information of a studied day

  } # end loop on days


  # remove first row filled with NAs:
  bites_info_day_df <- bites_info_day_df[-1, ]


  # 2 - Build the df with SUMMED bites over the days on the studied site:
  summed_bites_info_tot_df <- as.data.frame(matrix(ncol = 3,
                                            nrow = 1,
                                            NA))
  colnames(summed_bites_info_tot_df) <- c("sp_nm", "feed_act",
                                   "site")


  # add the two dfs altogether:
  all_df <- dplyr::bind_rows(df_list[[1]], df_list[[2]])
  # where NA -> 0:
  all_df[is.na(all_df)] <- 0
  # put the three informative column at the front:
  all_df <- dplyr::relocate(all_df, seq, hour, site)

  # use numeric format:
  all_df[, -c(1:3)] <- apply(all_df[, -c(1:3)], 2, as.numeric)

  # compute the sum on both days: total nb of bites per day per 10m2:
  sum_bites_tot <- apply(all_df[, -c(1:3)], 2, sum)

  for (sp_nm in names(sum_bites_tot)) {

    feed_act <- sum_bites_tot[[sp_nm]]

    if (sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
      site <- "both"
    }

    if (sp_nm %in% sp_FPA_vect & ! sp_nm %in% sp_PPA_vect) {
      site <- "FPA_only"
    }

    if (! sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
      site <- "PPA_only"
    }

    summed_bites_info_tot_df <- dplyr::add_row(summed_bites_info_tot_df,
                                        sp_nm = sp_nm,
                                        feed_act = feed_act,
                                        site = site)

  } # end add each species information


  # remove first row:
  summed_bites_info_tot_df <- summed_bites_info_tot_df[-1, ]


  # 3 - Build the df with mean bites values over the days on the studied site:
  # = total number of bites / 24 (nb of sequences both days)
  bites_info_tot_df <- as.data.frame(matrix(ncol = 4,
                                            nrow = 1,
                                            NA))
  colnames(bites_info_tot_df) <- c("sp_nm", "feed_act_mean",
                                   "feed_act_sd", "site")


  # add the two dfs altogether:
  all_df <- dplyr::bind_rows(df_list[[1]], df_list[[2]])
  # where NA -> 0:
  all_df[is.na(all_df)] <- 0
  # put the three informative column at the front:
  all_df <- dplyr::relocate(all_df, seq, hour, site)

  # use numeric format:
  all_df[, -c(1:3)] <- apply(all_df[, -c(1:3)], 2, as.numeric)

  # compute the mean on both days: mean nb of bites per day per 10m2:
  mean_bites_tot <- apply(all_df[, -c(1:3)], 2, mean)
  # compute the sd on both days:
  sd_bites_tot <- apply(all_df[, -c(1:3)], 2, sd)

  for (sp_nm in names(mean_bites_tot)) {

    feed_act_mean <- mean_bites_tot[[sp_nm]]
    feed_act_sd <- sd_bites_tot[[sp_nm]]


    if (sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
      site <- "both"
    }

    if (sp_nm %in% sp_FPA_vect & ! sp_nm %in% sp_PPA_vect) {
      site <- "FPA_only"
    }

    if (! sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
      site <- "PPA_only"
    }

    bites_info_tot_df <- dplyr::add_row(bites_info_tot_df,
                                        sp_nm = sp_nm,
                                        feed_act_mean = feed_act_mean,
                                        feed_act_sd = feed_act_sd,
                                        site = site)

  } # end add each species information


  # remove first row:
  bites_info_tot_df <- bites_info_tot_df[-1, ]

  return(list(bites_info_day_df, bites_info_tot_df, summed_bites_info_tot_df))

}




#' Title
#'
#' @param contrib_FPA_df the dataframe showing the contribution of herbivores on
#' the FPA
#'
#' @param contrib_PPA_df the dataframe showing the contribution of herbivores on
#' the PPA
#'
#' @param scale a caracter string either "total" if total herbivory contribution
#' is to be plotted or "day" if daily herbivory contribution is to be plotted
#'
#' @param type equals "mean" if mean contrib per seq to be plotted or
#' "total" if total number of bites to be plotted
#'
#' @return a barplot showing species contribution to herbivory for the two
#' protection level
#'
#' @export
#'
#' @examples


plot.interact.guild.contrib <- function(contrib_FPA_df,
                              contrib_PPA_df,
                              guild_nm,
                              sites_colors,
                              type) {


    # only keep data from the studied guild:
    data_subset_FPA <- dplyr::filter(contrib_FPA_df, Diet_Parravicini_2020 == guild_nm)
    data_subset_PPA <- dplyr::filter(contrib_PPA_df, Diet_Parravicini_2020 == guild_nm)

    # if there is a Corallivore names spnmcoral -> rename:
    if (guild_nm == "Corallivores") {

      # Correct for FPA:
      if ("Scarus_ferrugineuscoral" %in% data_subset_FPA$sp_nm) {
        data_subset_FPA[which(data_subset_FPA$sp_nm == "Scarus_ferrugineuscoral"), "sp_nm"] <-
          "Scarus_ferrugineus"
      }
      if ("Chlorurus_sordiduscoral" %in% data_subset_FPA$sp_nm) {
        data_subset_FPA[which(data_subset_FPA$sp_nm == "Chlorurus_sordiduscoral"), "sp_nm"] <-
          "Chlorurus_sordidus"
      }
      if ("Scarus_nigercoral" %in% data_subset_FPA$sp_nm) {
        data_subset_FPA[which(data_subset_FPA$sp_nm == "Scarus_nigercoral"), "sp_nm"] <-
          "Scarus_niger"
      }
      if ("Scarus_frenatuscoral" %in% data_subset_FPA$sp_nm) {
        data_subset_FPA[which(data_subset_FPA$sp_nm == "Scarus_frenatuscoral"), "sp_nm"] <-
          "Scarus_frenatus"
      }

      # Correct for PPA:
      if ("Scarus_ferrugineuscoral" %in% data_subset_PPA$sp_nm) {
        data_subset_PPA[which(data_subset_PPA$sp_nm == "Scarus_ferrugineuscoral"), "sp_nm"] <-
          "Scarus_ferrugineus"
      }
      if ("Chlorurus_sordiduscoral" %in% data_subset_PPA$sp_nm) {
        data_subset_PPA[which(data_subset_PPA$sp_nm == "Chlorurus_sordiduscoral"), "sp_nm"] <-
          "Chlorurus_sordidus"
      }
      if ("Scarus_nigercoral" %in% data_subset_PPA$sp_nm) {
        data_subset_PPA[which(data_subset_PPA$sp_nm == "Scarus_nigercoral"), "sp_nm"] <-
          "Scarus_niger"
      }
      if ("Scarus_frenatuscoral" %in% data_subset_PPA$sp_nm) {
        data_subset_PPA[which(data_subset_PPA$sp_nm == "Scarus_frenatuscoral"), "sp_nm"] <-
          "Scarus_frenatus"
      }

    }

    # if there is Invertivore names spnminv -> rename:
    if (guild_nm == "Invertivores") {

      # Correct for FPA:
      if ("Chaetodon_trifasciatusinv" %in% data_subset_FPA$sp_nm) {
        data_subset_FPA[which(data_subset_FPA$sp_nm == "Chaetodon_trifasciatusinv"), "sp_nm"] <-
          "Chaetodon_trifasciatus"
      }
      if ("Chaetodon_aurigainv" %in% data_subset_FPA$sp_nm) {
        data_subset_FPA[which(data_subset_FPA$sp_nm == "Chaetodon_aurigainv"), "sp_nm"] <-
          "Chaetodon_auriga"
      }

      # Correct for PPA:
      if ("Chaetodon_trifasciatusinv" %in% data_subset_PPA$sp_nm) {
        data_subset_PPA[which(data_subset_PPA$sp_nm == "Chaetodon_trifasciatusinv"), "sp_nm"] <-
          "Chaetodon_trifasciatus"
      }
      if ("Chaetodon_aurigainv" %in% data_subset_PPA$sp_nm) {
        data_subset_PPA[which(data_subset_PPA$sp_nm == "Chaetodon_aurigainv"), "sp_nm"] <-
          "Chaetodon_auriga"
      }

    }

    # rename all species by removing "_":
    data_subset_FPA$sp_nm <- stringr::str_replace_all(data_subset_FPA$sp_nm, "_", " ")
    data_subset_PPA$sp_nm <- stringr::str_replace_all(data_subset_PPA$sp_nm, "_", " ")

    # compute ic to put on the plot (24 = number of sequences on both days):
    # data_subset_FPA$ic <- (data_subset_FPA$feed_act_sd/sqrt(24)) * qt((1-0.05)/2 + 0.5, 24-1)
    # data_subset_PPA$ic <- (data_subset_PPA$feed_act_sd/sqrt(24)) * qt((1-0.05)/2 + 0.5, 24-1)

    # compute top and botton bars:
    # data_subset_FPA$bottom <- data_subset_FPA$feed_act_mean - data_subset_FPA$ic
    # data_subset_FPA$top <- data_subset_FPA$feed_act_mean + data_subset_FPA$ic
    # data_subset_PPA$bottom <- data_subset_PPA$feed_act_mean - data_subset_PPA$ic
    # data_subset_PPA$top <- data_subset_PPA$feed_act_mean + data_subset_PPA$ic

    # when bar below 0 -> 0:
    # data_subset_FPA[which(data_subset_FPA$bottom < 0), "bottom"] <- 0
    # data_subset_PPA[which(data_subset_PPA$bottom < 0), "bottom"] <- 0

    # Change HMD -> Herbivores
    if (guild_nm == "HMD") {
      guild_nm <- "Herbivores"
    }

    if (type == "total") {
      plot_FPA <- ggplot2::ggplot(data = data_subset_FPA) +

        ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
                                       x = feed_act_mean*24,
                                       fill = site),
                          stat = "identity") +

        # ggplot2::geom_errorbar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
        #                        xmin = bottom,
        #                        xmax = top), width = 0.1,
        #                        colour = "grey50",
        #                        size = 0.7) +

        ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                              sites_colors[3]),
                                   name = "Site presence",
                                   labels = c("Both", "N'Gouja")) +

        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13),
                       axis.text.y = ggplot2::element_text(size = 13,
                                                           face = "italic"),
                       axis.title.x = ggplot2::element_text(size = 13),
                       axis.title.y = ggplot2::element_text(size = 13),
                       panel.background = ggplot2::element_rect(fill = "white",
                                                                colour = "grey90"),
                       panel.grid.major = ggplot2::element_line(colour = "grey90"),
                       legend.text = ggplot2::element_text(size = 13),
                       legend.title = ggplot2::element_text(size = 13)) +

        ggplot2::xlab("Total bites number/ 1h /10m²") +

        ggplot2::xlim(c(0, max(max(data_subset_FPA$feed_act_mean*24),
                               max(data_subset_PPA$feed_act_mean*24)) + 0.1)) +

        ggplot2::ylab("") +

        ggplot2::ggtitle(guild_nm)


      plot_PPA <- ggplot2::ggplot(data = data_subset_PPA) +

        ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
                                       x = feed_act_mean*24,
                                       fill = site),
                          stat = "identity") +

        # ggplot2::geom_errorbar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
        #                                     xmin = bottom,
        #                                     xmax = top), width = 0.1,
        #                        colour = "grey50",
        #                        size = 0.7) +

        ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                              sites_colors[2]),
                                   name = "Site presence",
                                   labels = c("Both", "Bouéni")) +

        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13),
                       axis.text.y = ggplot2::element_text(size = 13,
                                                           face = "italic"),
                       axis.title.x = ggplot2::element_text(size = 13),
                       axis.title.y = ggplot2::element_text(size = 13),
                       panel.background = ggplot2::element_rect(fill = "white",
                                                                colour = "grey90"),
                       panel.grid.major = ggplot2::element_line(colour = "grey90"),
                       legend.text = ggplot2::element_text(size = 13),
                       legend.title = ggplot2::element_text(size = 13)) +

        ggplot2::xlab("Total bites number/ 1h /10m²") +

        ggplot2::xlim(c(0, max(max(data_subset_FPA$feed_act_mean*24),
                               max(data_subset_PPA$feed_act_mean*24)) + 0.1)) +

        ggplot2::ylab("")
    }


    if (type == "mean") {
      plot_FPA <- ggplot2::ggplot(data = data_subset_FPA) +

        ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
                                       x = feed_act_mean,
                                       fill = site),
                          stat = "identity") +

        # ggplot2::geom_errorbar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
        #                        xmin = bottom,
        #                        xmax = top), width = 0.1,
        #                        colour = "grey50",
        #                        size = 0.7) +

        ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                              sites_colors[3]),
                                   name = "Site presence",
                                   labels = c("Both", "N'Gouja")) +

        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13),
                       axis.text.y = ggplot2::element_text(size = 13,
                                                           face = "italic"),
                       axis.title.x = ggplot2::element_text(size = 13),
                       axis.title.y = ggplot2::element_text(size = 13),
                       panel.background = ggplot2::element_rect(fill = "white",
                                                                colour = "grey90"),
                       panel.grid.major = ggplot2::element_line(colour = "grey90"),
                       legend.text = ggplot2::element_text(size = 13),
                       legend.title = ggplot2::element_text(size = 13)) +

        ggplot2::xlab("Mean bites number/ 5min /10m²") +

        ggplot2::xlim(c(0, max(max(data_subset_FPA$feed_act_mean),
                               max(data_subset_PPA$feed_act_mean)) + 0.1)) +

        ggplot2::ylab("") +

        ggplot2::ggtitle(guild_nm)


      plot_PPA <- ggplot2::ggplot(data = data_subset_PPA) +

        ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
                                       x = feed_act_mean,
                                       fill = site),
                          stat = "identity") +

        # ggplot2::geom_errorbar(ggplot2::aes(y = reorder(sp_nm, - feed_act_mean),
        #                                     xmin = bottom,
        #                                     xmax = top), width = 0.1,
        #                        colour = "grey50",
        #                        size = 0.7) +

        ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                              sites_colors[2]),
                                   name = "Site presence",
                                   labels = c("Both", "Bouéni")) +

        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13),
                       axis.text.y = ggplot2::element_text(size = 13,
                                                           face = "italic"),
                       axis.title.x = ggplot2::element_text(size = 13),
                       axis.title.y = ggplot2::element_text(size = 13),
                       panel.background = ggplot2::element_rect(fill = "white",
                                                                colour = "grey90"),
                       panel.grid.major = ggplot2::element_line(colour = "grey90"),
                       legend.text = ggplot2::element_text(size = 13),
                       legend.title = ggplot2::element_text(size = 13)) +

        ggplot2::xlab("Mean bites number/ 5min /10m²") +

        ggplot2::xlim(c(0, max(max(data_subset_FPA$feed_act_mean),
                               max(data_subset_PPA$feed_act_mean)) + 0.1)) +

        ggplot2::ylab("")
    }



    plot_both <- (plot_FPA + plot_PPA) +
      patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                             ncol = 2, nrow = 1, guides = "collect") +
      patchwork::plot_annotation(title = guild_nm)


    ggplot2::ggsave(filename = here::here("outputs", paste0("Tot", sep = "_", guild_nm,
                                                            sep = "_", "contrib.pdf")),
                    plot = plot_both,
                    device = "pdf",
                    scale = 0.6,
                    height = 10000,
                    width = 18000,
                    units = "px",
                    dpi = 800)

    return(list(plot_both, data_subset_PPA, data_subset_FPA, plot_FPA, plot_PPA))

}
