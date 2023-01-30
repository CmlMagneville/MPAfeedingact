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
#' the mean bite rate over the days
#'
#' @export
#'
#' @examples


compute.contrib.feedact <- function(df_list,
                                 sp_FPA_vect,
                                 sp_PPA_vect,
                                 studied_site) {


  # Build the df that will contain bites information per day:
  if (studied_site == "FPA") {
    bites_info_day_df <- as.data.frame(matrix(ncol = 4,
                                              nrow = 1,
                                              NA))
    colnames(bites_info_day_df) <- c("sp_nm", "feed_act", "site", "day")
  }

  if (studied_site == "PPA") {
    bites_info_day_df <- as.data.frame(matrix(ncol = 4,
                                              nrow = 1,
                                              NA))
    colnames(bites_info_day_df) <- c("sp_nm", "feed_act", "site", "day")
  }


  # Fill this df:
  for (i in c(1:length(df_list))) {

    studied_df <- df_list[[i]]

    # compute the total sum for a given day:
    summed_bites <- apply(studied_df, 2, sum)
    # divide by the number of annotated minutes: have per annotated min data:
    bites_annotated_min <- summed_bites / 60
    # divide by the number of m2: have per quadrat data
    bites_annotated_min_surf <- bites_annotated_min / 5

    # get the data to fill the table:
    day <- paste0("day", sep = "_", i)

    for (sp_nm in names(bites_annotated_min_surf)) {

      feed_act <- bites_annotated_min_surf[[sp_nm]]

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
                                          feed_act = feed_act,
                                          site = site,
                                          day = day)


    } # end add each species information of a studied day

  } # end loop on days


  # remove first row filled with NAs:
  bites_info_day_df <- bites_info_day_df[-1, ]



  # Build the df with total bites values over the days on the studied site:
  bites_info_tot_df <- as.data.frame(matrix(ncol = 3,
                                            nrow = 1,
                                            NA))
  colnames(bites_info_tot_df) <- c("sp_nm", "feed_act", "site")

  # Fill this df:
  for (i in c(1:length(df_list))) {

    studied_df <- df_list[[i]]

    # compute the total sum for a given day:
    summed_bites_tot <- apply(studied_df, 2, sum)

    for (sp_nm in names(summed_bites_tot)) {

      feed_act <- summed_bites_tot[[sp_nm]]

      if (sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
        site <- "both"
      }

      if (sp_nm %in% sp_FPA_vect & ! sp_nm %in% sp_PPA_vect) {
        site <- "FPA_only"
      }

      if (! sp_nm %in% sp_FPA_vect & sp_nm %in% sp_PPA_vect) {
        site <- "PPA_only"
      }

      # if the species has been seen on the day(s) before:
      if (sp_nm %in% bites_info_tot_df$sp_nm) {
        bites_info_tot_df[which(bites_info_tot_df$sp_nm == sp_nm), "feed_act"] <- feed_act +
          bites_info_tot_df[which(bites_info_tot_df$sp_nm == sp_nm), "feed_act"]
      }

      # if it is the first time that the species is seen (not days before):
      if (! sp_nm %in% bites_info_tot_df$sp_nm) {
        bites_info_tot_df <- dplyr::add_row(bites_info_tot_df,
                                            sp_nm = sp_nm,
                                            feed_act = feed_act,
                                            site = site)
      }

    } # end add each species information of a studied day

  } # end loop on days


  # now compute bites per annotated minute:
  bites_info_tot_df$feed_act <- bites_info_tot_df$feed_act/120
  # now compute bites per annotated minute per m2:
  bites_info_tot_df$feed_act <- bites_info_tot_df$feed_act/5

  # remove first row:
  bites_info_tot_df <- bites_info_tot_df[-1, ]

  return(list(bites_info_day_df, bites_info_tot_df))

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
#' @return a barplot showing species contribution to herbivory for the two
#' protection level
#'
#' @export
#'
#' @examples


plot.interact.guild.contrib <- function(contrib_FPA_df,
                              contrib_PPA_df,
                              guild_nm,
                              sites_colors) {


    # only keep data from the studied guild:
    data_subset_FPA <- dplyr::filter(contrib_FPA_df, Diet_Parravicini_2020 == guild_nm)
    data_subset_PPA <- dplyr::filter(contrib_PPA_df, Diet_Parravicini_2020 == guild_nm)

    # add a rox with all bites:
    data_subset_FPA <- tibble::add_row(data_subset_FPA, sp_nm = "All species",
                                       feed_act = sum(data_subset_FPA$feed_act),
                                       site = "both", Diet_Mouillot_2014 = NA,
                                       Diet_Parravicini_2020 = guild_nm)
    data_subset_PPA <- tibble::add_row(data_subset_PPA, sp_nm = "All species",
                                       feed_act = sum(data_subset_PPA$feed_act),
                                       site = "both", Diet_Mouillot_2014 = NA,
                                       Diet_Parravicini_2020 = guild_nm)


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

    }

    # rename all species by removing "_":
    data_subset_FPA$sp_nm <- stringr::str_replace_all(data_subset_FPA$sp_nm, "_", " ")
    data_subset_PPA$sp_nm <- stringr::str_replace_all(data_subset_PPA$sp_nm, "_", " ")


    plot_FPA <- ggplot2::ggplot(data = data_subset_FPA) +

      ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - feed_act), x = feed_act,
                                     fill = site),
                        stat = "identity") +

      ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                            sites_colors[3]),
                                 name = "Site presence",
                                 labels = c("Both", "Fully Protected")) +

      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8),
                     axis.text.y = ggplot2::element_text(size = 8,
                                                         face = "italic"),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey90"),
                     panel.grid.major = ggplot2::element_line(colour = "grey90")) +

      ggplot2::xlab("Bites number / minutes / quadrat") +

      ggplot2::xlim(c(0, max(max(data_subset_FPA$feed_act),
                             max(data_subset_PPA$feed_act)) + 0.1)) +

      ggplot2::ylab("")


    plot_PPA <- ggplot2::ggplot(data = data_subset_PPA) +

      ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - feed_act), x = feed_act,
                                     fill = site),
                        stat = "identity") +

      ggplot2::scale_fill_manual(values = c(sites_colors[1],
                                            sites_colors[2]),
                                 name = "Site presence",
                                 labels = c("Both", "Poorly Protected")) +

      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8),
                     axis.text.y = ggplot2::element_text(size = 8,
                                                         face = "italic"),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey90"),
                     panel.grid.major = ggplot2::element_line(colour = "grey90")) +

      ggplot2::xlab("Bites number / minutes / quadrat") +

      ggplot2::xlim(c(0, max(max(data_subset_FPA$feed_act),
                             max(data_subset_PPA$feed_act)) + 0.1)) +

      ggplot2::ylab("")


    plot_both <- (plot_FPA + plot_PPA) +
      patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                             ncol = 2, nrow = 1, guides = "collect") +
      patchwork::plot_annotation(tag_levels = "A", title = guild_nm)


    ggplot2::ggsave(filename = here::here("outputs", paste0("Tot", sep = "_", guild_nm,
                                                            sep = "_", "contrib.pdf")),
                    plot = plot_both,
                    device = "pdf",
                    scale = 0.6,
                    height = 10000,
                    width = 18000,
                    units = "px",
                    dpi = 800)

    return(plot_both)

}
