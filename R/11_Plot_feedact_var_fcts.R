################################################################################
##
## Script to plot boxplots of bites number per sequence for each feedact and site
##
## 11_Plot_feedact_var_fcts.R
##
## 02/06/2023
##
## Camille Magneville
##
################################################################################



plot.boxplots.act <- function(bites_seq_df_list) {

  # link the tables:
  bites_seq_final <- dplyr::bind_rows(bites_seq_df_list[[1]],
                                      bites_seq_df_list[[2]],
                                      bites_seq_df_list[[3]],
                                      bites_seq_df_list[[4]])


  # create a new df which contains a column showing feedact:
  bites_seq_plot <- reshape2::melt(bites_seq_final,
                                   id.vars = c("tot_bites", "site", "day", "seq", "hour"),
                                   variable.name = 'feedact', value.name = 'bites_nb')

  # Change FPA, PPA by site names:
  bites_seq_plot[which(bites_seq_plot$site == "FPA"), "site"] <- "N'Gouja"
  bites_seq_plot[which(bites_seq_plot$site == "PPA"), "site"] <- "Bouéni"

  # Order: N'Gouja, then Boueni
  bites_seq_plot$site <- factor(bites_seq_plot$site, levels = c("N'Gouja", "Bouéni"))


  # plot the boxplot:
  plot_box_herb <- ggplot2::ggplot(data = bites_seq_plot[which(bites_seq_plot$feedact == "herb_bites"), ],
                                   ggplot2::aes(y = bites_nb, x = feedact)) +

    ggplot2::geom_boxplot(outlier.shape = NA) +

    ggplot2::geom_jitter(ggplot2::aes(colour = site), show.legend = FALSE) +

    ggplot2::scale_color_manual(values = c("#80cdc1", "#bf812d"),
                                name = "Site") +

    ggplot2::facet_grid( ~ site) +

    ggplot2::xlab("") +

    ggplot2::ylab("Bites number / 5min / 10m²") +

    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ggtitle("Herbivores")


  plot_box_coral <- ggplot2::ggplot(data = bites_seq_plot[which(bites_seq_plot$feedact == "coral_bites"), ],
                                   ggplot2::aes(y = bites_nb, x = feedact)) +

    ggplot2::geom_boxplot(outlier.shape = NA) +

    ggplot2::geom_jitter(ggplot2::aes(colour = site), show.legend = FALSE) +

    ggplot2::scale_color_manual(values = c("#80cdc1", "#bf812d"),
                                name = "Site") +

    ggplot2::facet_grid( ~ site) +

    ggplot2::xlab("") +

    ggplot2::ylab("Bites number / 5min / 10m²") +

    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ggtitle("Corallivores")


  plot_box_invert <- ggplot2::ggplot(data = bites_seq_plot[which(bites_seq_plot$feedact == "invert_bites"), ],
                                   ggplot2::aes(y = bites_nb, x = feedact)) +

    ggplot2::geom_boxplot(outlier.shape = NA) +

    ggplot2::geom_jitter(ggplot2::aes(colour = site), show.legend = FALSE) +

    ggplot2::scale_color_manual(values = c("#80cdc1", "#bf812d"),
                                name = "Site") +

    ggplot2::facet_grid( ~ site) +

    ggplot2::xlab("") +

    ggplot2::ylab("Bites number / 5min / 10m²") +

    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::ggtitle("Invertivores")


  # Link plots:
  plot_all <- (plot_box_coral + plot_box_herb + plot_box_invert) +

    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(0.5, 0.5),
                           ncol = 2, nrow = 2, guides = "collect")

  ggplot2::ggsave(filename = here::here("outputs", "boxplots_feedact.pdf"),
                  plot = plot_all,
                  device = "pdf",
                  scale = 0.7,
                  height = 15000,
                  width = 14000,
                  units = "px",
                  dpi = 800)


  return(plot_all)


}
