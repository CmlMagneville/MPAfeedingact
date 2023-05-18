################################################################################
##
## Script to study the change in dominant species identity across time
##
## 10_Sp_dominanace_id.R
##
## 18/05/2023
##
## Camille Magneville
##
################################################################################



#' Title
#'
#' @param list_bites_seq a list two dataframes for a given activity and
#' a given site. Must be given in the chronological order.
#'
#' @return
#' @export
#'
#' @examples


sp.dominance.act <- function(list_bites_seq) {


  # Add day:
  df1 <- list_bites_seq[[1]]
  df1 <- tibble::add_column(df1, day = rep("day_1", nrow(df1)))
  df2 <- list_bites_seq[[2]]
  df2 <- tibble::add_column(df2, day = rep("day_2", nrow(df2)))

  # Add timeslots:
  df1 <- tibble::add_column(df1, timeslot = c(rep("timeslot_1", 4),
                                              rep("timeslot_2", 4),
                                              rep("timeslot_3", 4)))
  df2 <- tibble::add_column(df2, timeslot = c(rep("timeslot_1", 4),
                                              rep("timeslot_2", 4),
                                              rep("timeslot_3", 4)))

  # Create a new df that will contain species relative act per timeslot:
  final_df <- as.data.frame(matrix(ncol = length(unique(c(colnames(df1)[1:(ncol(df1) - 5)],
                                                 colnames(df2)[1:(ncol(df2) - 5)]))) + 4,
                                   nrow = 6,
                                   NA))
  colnames(final_df) <- c(unique(c(colnames(df1)[1:(ncol(df1) - 5)],
                               colnames(df2)[1:(ncol(df2) - 5)])),
                          "tot_bites", "site", "day", "timeslot")

  # Counter to fill the df:
  n <- 1

  # Loop on days to fill the final_df:
  for (studied_df in list(df1, df2)) {

    # retrieve day nb:
    day_nb <- unique(studied_df$day)

    # retrieve site nm:
    site_nm <- unique(studied_df$site)

    # Loop on timeslots:
    for (t in c(1:length(unique(studied_df$timeslot)))) {

      # retrieve day nb:
      timeslot_nb <- paste0("timeslot", sep = "_", t)

      # subset the df:
      sub_df <- studied_df[which(studied_df$timeslot == timeslot_nb), ]

      # get the total nb of bites:
      tot_bites <- sum(sub_df[, -c((ncol(sub_df) - 4):ncol(sub_df))])

      # Fill the df with information collected so far:
      final_df[n, "site"] <- site_nm
      final_df[n, "day"] <- day_nb
      final_df[n, "timeslot"] <- timeslot_nb
      final_df[n, "tot_bites"] <- tot_bites

      # Now fill species relative activity:
      for (i in c(1:(ncol(final_df) - 4))) {

        # if species is present on this day and timeslot:
        if (colnames(final_df)[i] %in% colnames(sub_df)) {

          # get species total bites nb for this timeslot and day:
          sp_bites <- sum(sub_df[, colnames(sub_df) == colnames(final_df)[i]])

          # get species relative activity:
          sp_relat_act <- (sp_bites/tot_bites)*100

          # fill the df:
          final_df[n, colnames(final_df)[i]] <- round(sp_relat_act, 2)

        }

      } # end loop on species

      n <- n + 1

    } # end loop on timeslots

  } # end loop on days

  # Fill NA -> 0:
  final_df[is.na(final_df)] <- 0

  return(final_df)

}



#' Title
#'
#' @param dominance_df_FPA dataframe from `sp.dominance.act` function for
#' the FPA site
#'
#' @param guild_nm a caracter string refering to the trophic guild to study
#'
#' @col_vect
#'
#' @return
#' @export
#'
#' @examples

dominance.sp.plot <- function(dominance_df,
                              guild_nm,
                              col_vect) {


  # Correct species names:
  # if there is a Corallivore names spnmcoral -> rename:
  if (guild_nm == "Corallivores") {

    if ("Scarus_ferrugineuscoral" %in% colnames(dominance_df)) {
      colnames(dominance_df)[which(colnames(dominance_df) == "Scarus_ferrugineuscoral")] <-
        "Scarus_ferrugineus"
      names(col_vect)[which(names(col_vect) == "Scarus_ferrugineuscoral")] <- "Scarus_ferrugineus"
    }
    if ("Chlorurus_sordiduscoral" %in% colnames(dominance_df)) {
      colnames(dominance_df)[which(colnames(dominance_df) == "Chlorurus_sordiduscoral")] <-
        "Chlorurus_sordidus"
      names(col_vect)[which(names(col_vect) == "Chlorurus_sordiduscoral")] <- "Chlorurus_sordidus"
    }
    if ("Scarus_nigercoral" %in% colnames(dominance_df)) {
      colnames(dominance_df)[which(colnames(dominance_df) == "Scarus_nigercoral")] <-
        "Scarus_niger"
      names(col_vect)[which(names(col_vect) == "Scarus_nigercoral")] <- "Scarus_niger"
    }
    if ("Scarus_frenatuscoral" %in% colnames(dominance_df)) {
      colnames(dominance_df)[which(colnames(dominance_df) == "Scarus_frenatuscoral")] <-
        "Scarus_frenatus"
      names(col_vect)[which(names(col_vect) == "Scarus_frenatuscoral")] <- "Scarus_frenatus"
    }

  }

  # if there is Invertivore names spnminv -> rename:
  if (guild_nm == "Invertivores") {

    # Correct for FPA:
    if ("Chaetodon_trifasciatusinv" %in% colnames(dominance_df)) {
      colnames(dominance_df)[which(colnames(dominance_df) == "Chaetodon_trifasciatusinv")] <-
        "Chaetodon_trifasciatus"
      names(col_vect)[which(names(col_vect) == "Chaetodon_trifasciatusinv")] <- "Chaetodon_trifasciatus"
    }
    if ("Chaetodon_aurigainv" %in% colnames(dominance_df)) {
      colnames(dominance_df)[which(colnames(dominance_df) == "Chaetodon_aurigainv")] <-
        "Chaetodon_auriga"
      names(col_vect)[which(names(col_vect) == "Chaetodon_aurigainv")] <- "Chaetodon_auriga"
    }

  }


  # Get the dataframe in the right format with species being one column:
  plot_df <- reshape2::melt(dominance_df,
                                  id.vars = c("tot_bites", "site", "day", "timeslot"),
                                  variable.name = 'species', value.name = 'bites_prop')

  # change the name of the graph:
  if (unique(plot_df$site) == "FPA") {
    site_nm <- "N'Gouja"
  }
  if (unique(plot_df$site) == "PPA") {
    site_nm <- "Bouéni"
  }

  # add a column: day + timeslot:
  plot_df$day_timeslot <- paste0(plot_df$day, sep = "_", plot_df$timeslot)

  # order the column:
  plot_df$day_timeslot <- factor(plot_df$day_timeslot,
                                  levels = c("day_1_timeslot_1",
                                             "day_2_timeslot_1",
                                             "day_1_timeslot_2",
                                             "day_2_timeslot_2",
                                             "day_1_timeslot_3",
                                             "day_2_timeslot_3"))


  # for the color manual setting, must remove species which are not in the studied site:
  col_vect_site <- col_vect[which(names(col_vect) %in% unique(plot_df$species))]
  plot_df$species <- factor(plot_df$species,
                                          levels = names(col_vect_site))

  # Negative values for day 2 so days can be symetric:
  for (i in (1:nrow(plot_df))) {

    if (plot_df$day[i] == "day_2") {
      plot_df[i, "bites_prop"] <- -1*plot_df[i, "bites_prop"]
    }

  }


  # plot:
  dominance_plot <- ggplot2::ggplot(plot_df,
                                ggplot2::aes(fill = species,
                                             y = bites_prop, x = timeslot)) +

    ggplot2::geom_bar(position = "stack", stat = "identity") +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90"),
                   legend.text = ggplot2::element_text(face = "italic")) +

    ggplot2::scale_x_discrete(labels = c("07:30 - 08:30",
                                         "11:30 - 12:30",
                                         "15:30 - 16:30")) +

    ggplot2::scale_fill_manual(values = col_vect_site) +

    ggplot2::xlab("") +

    ggplot2::ylab("Relative activity") +

    ggplot2::labs(fill = "Species") +

    ggplot2::ggtitle(site_nm)


  # save:
  ggplot2::ggsave(filename = here::here("outputs", paste0(guild_nm, sep ="_",
                                                          "dominance_graph", sep ="_",
                                                          site_nm, sep = ".",
                                                          "pdf")),
                  plot = dominance_plot,
                  device = "pdf",
                  scale = 1,
                  height = 5000,
                  width = 5000,
                  units = "px",
                  dpi = 600)

  return(dominance_plot)

}
