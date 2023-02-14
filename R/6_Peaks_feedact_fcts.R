################################################################################
##
## Functions to study the five biggest events (sequence) for each ...
## ... feeding activity: identify them and species contributing to them (plot)
##
## 8_Peaks_feedact.R
##
## 30/01/2023
##
## Camille Magneville
##
################################################################################




#' Identify peaks and species realising them
#'
#' @param list_df a list of dataframes resuming species bites across sequences.
#' In this analysis, it's a list of two dataframes, each for a day in a site.
#'
#' @param peak_nb the number of peaks we want to study. In this analysis, we
#' study the first five biggest events.
#'
#' @return a dataframe with day, sequence_nb, species and bites information
#'
#' @export
#'
#' @examples
#'


identify.peaks.species <- function(list_df, peak_nb) {


  # Bring the two dataframes together as I take the five biggest events...
  # ... days pooled:
  df1 <- list_df[[1]]
  df1 <- tibble::add_column(df1, day = rep("day_1", nrow(df1)))
  df2 <- list_df[[2]]
  df2 <- tibble::add_column(df2, day = rep("day_2", nrow(df2)))
  both_dfs <- dplyr::bind_rows(df1, df2)

  # Add 0 instead of NA (Na is when species not seen during the other day)
  both_dfs <- replace(both_dfs, is.na(both_dfs), 0)

  # Compute the sum of bites for each sequence in a new column:
  both_dfs <- tibble::add_column(both_dfs, summed_bites = rep(NA, nrow(both_dfs)))
  both_dfs$summed_bites <- apply(both_dfs[, which(! colnames(both_dfs)
                                                  %in% c("site", "hour",
                                                         "seq", "day",
                                                         "summed_bites"))],
                                 1, sum)

  # Only keep the first peak_nb:
  biggest_peaks <- both_dfs
  biggest_peaks <- dplyr::arrange(biggest_peaks, desc(summed_bites))
  biggest_peaks <- biggest_peaks[c(1:peak_nb), ]

  # redo the dataframe in a clean way (information at the end):
  cleaned_biggest_peaks_df <- biggest_peaks[, which(! colnames(biggest_peaks)
                                               %in% c("site", "hour",
                                                      "seq", "day",
                                                      "summed_bites"))]
  cleaned_biggest_peaks_df <- tibble::add_column(cleaned_biggest_peaks_df,
                                                 summed_bites = biggest_peaks$summed_bites,
                                                 site = biggest_peaks$site,
                                                 hour = biggest_peaks$hour,
                                                 seq = biggest_peaks$seq,
                                                 day = biggest_peaks$day)

  return(cleaned_biggest_peaks_df)

}




#' Plot the n biggest events and species contribution
#'
#' @param biggest_peaks_df the dataframe gathering the n biggest events, species
#' contributing to them
#'
#' @param guild_nm the name of the feeding activity to study
#'
#' @param site the name of the site to study
#'
#' @param col_vect a vector containing the names of species and associated colors
#'
#' @return
#' @export
#'
#' @examples
#'

plot.peaks.contrib <- function(biggest_peaks_df, guild_nm, site, col_vect) {


  # Correct species names:
  # if there is a Corallivore names spnmcoral -> rename:
  if (guild_nm == "Corallivores") {

    if ("Scarus_ferrugineuscoral" %in% colnames(biggest_peaks_df)) {
      colnames(biggest_peaks_df)[which(colnames(biggest_peaks_df) == "Scarus_ferrugineuscoral")] <-
        "Scarus_ferrugineus"
    }
    if ("Chlorurus_sordiduscoral" %in% colnames(biggest_peaks_df)) {
      colnames(biggest_peaks_df)[which(colnames(biggest_peaks_df) == "Chlorurus_sordiduscoral")] <-
        "Chlorurus_sordidus"
    }
    if ("Scarus_nigercoral" %in% colnames(biggest_peaks_df)) {
      colnames(biggest_peaks_df)[which(colnames(biggest_peaks_df) == "Scarus_nigercoral")] <-
        "Scarus_niger"
    }
    if ("Scarus_frenatuscoral" %in% colnames(biggest_peaks_df)) {
      colnames(biggest_peaks_df)[which(colnames(biggest_peaks_df) == "Scarus_frenatuscoral")] <-
        "Scarus_frenatus"
    }

  }

  # if there is Invertivore names spnminv -> rename:
  if (guild_nm == "Invertivores") {

    # Correct for FPA:
    if ("Chaetodon_trifasciatusinv" %in% colnames(biggest_peaks_df)) {
      colnames(biggest_peaks_df)[which(colnames(biggest_peaks_df) == "Chaetodon_trifasciatusinv")] <-
        "Chaetodon_trifasciatus"
    }
    if ("Chaetodon_aurigainv" %in% colnames(biggest_peaks_df)) {
      colnames(biggest_peaks_df)[which(colnames(biggest_peaks_df) == "Chaetodon_aurigainv")] <-
        "Chaetodon_auriga"
    }

  }

  # Get the dataframe in the right format with species being one column:
  plot_peaks_df <- reshape2::melt(biggest_peaks_df,
                                  id.vars = c("site", "hour", "seq", "day", "summed_bites"),
                                  variable.name = 'species', value.name = 'bites_nb')

  # add column with all info about seq:
  plot_peaks_df$seq_info <- paste0(plot_peaks_df$day,
                                   sep = "_",
                                   plot_peaks_df$seq,
                                   sep = "_",
                                   plot_peaks_df$hour)

  # remove "_" and replace by empty space for species names:
  plot_peaks_df$species <- stringr::str_replace_all(plot_peaks_df$species, "_", " ")

  # remove species which have 0 bite:
  plot_peaks_df <- plot_peaks_df[which(plot_peaks_df$bites_nb != 0), ]

  # remove species which contribute to less than 10% of the sequence activity -> Other:
  # create a new df (cf after):
  cleaned_plot_peaks_df <- as.data.frame(matrix(ncol = ncol(plot_peaks_df),
                                                nrow = 1,
                                                NA))
  colnames(cleaned_plot_peaks_df) <- colnames(plot_peaks_df)

  # loop on each sequence to only keep species contibuting more (striclty) than 10 percent:
  for (seq_nm in (unique(plot_peaks_df$seq_info))) {

    tot_act <- unique(plot_peaks_df[which(plot_peaks_df$seq_info == seq_nm), "summed_bites"])
    ten_percent <- round(0.10*tot_act)

    to_add <- dplyr::filter(plot_peaks_df[which(plot_peaks_df$seq_info == seq_nm), ],
                                                                              bites_nb > ten_percent)

    cleaned_plot_peaks_df <- dplyr::bind_rows(cleaned_plot_peaks_df, to_add)

    # add another row: other to complete:
    cleaned_plot_peaks_df <- tibble::add_row(cleaned_plot_peaks_df,
                                             site = unique(to_add$site),
                                             hour = unique(to_add$hour),
                                             seq = unique(to_add$seq),
                                             day = unique(to_add$day),
                                             summed_bites = unique(to_add$summed_bites),
                                             species = "Others",
                                             bites_nb = unique(to_add$summed_bites) -
                                               sum(to_add$bites_nb),
                                             seq_info = unique(to_add$seq_info))

  }

  # remove the first row of the df : NA:
  cleaned_plot_peaks_df <- cleaned_plot_peaks_df[-1, ]


  # for the color manual setting, must remove species whch are not in the studied site...
  # ... and species contributing less than 10% of the total peak activity:
  col_vect_site <- col_vect[which(names(col_vect) %in% unique(cleaned_plot_peaks_df$species))]

  cleaned_plot_peaks_df$species <- factor(cleaned_plot_peaks_df$species,
                                          levels = names(col_vect_site))


  # plot:
  peaks_plot <- ggplot2::ggplot(cleaned_plot_peaks_df,
                                ggplot2::aes(fill = species,
                                             y = bites_nb, x = reorder(seq_info, - summed_bites))) +

    ggplot2::geom_bar(position = "stack", stat = "identity") +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90"),
                   legend.text = ggplot2::element_text(face = "italic")) +

    ggplot2::scale_x_discrete(labels = NULL, breaks = NULL) +

    ggplot2::scale_fill_manual(values = col_vect_site) +

    ggplot2::xlab("Sequences") +

    ggplot2::ylab("Bites number/5min/10m²") +

    ggplot2::labs(fill = "Species") +

    ggplot2::ggtitle(site)


  ggplot2::ggsave(filename = here::here("outputs", paste0(guild_nm, sep ="_",
                                                          "peaks", sep = "_",
                                                          site, sep = ".",
                                                          "pdf")),
                  plot = peaks_plot,
                  device = "pdf",
                  scale = 1,
                  height = 4000,
                  width = 5000,
                  units = "px",
                  dpi = 600)

  return(list(peaks_plot, cleaned_plot_peaks_df))

}
