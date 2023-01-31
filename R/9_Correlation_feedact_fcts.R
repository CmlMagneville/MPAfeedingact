################################################################################
##
## Functions to compute and plot the correlation between feedact intensity ...
## ... to see if sequences with high values of a given activity are ...
## ... correlated with high values of another
##
## 9_Correlation_feedact.R
##
## 31/01/2023
##
## Camille Magneville
##
################################################################################



#' Title
#'
#' @param bites_df a datafarme containing the number of bites per trophic guild
#' and per sequence
#'
#' @return
#' @export
#'
#' @examples
#'

compute.plot.correl <- function(bites_df) {


  # Prepare data for plotting:
  bites_df$seq <- factor(bites_df$seq, levels = c(paste0(rep("seq", 12), sep = "_", c(1:12))))


  ## Coral - Invertivory:

  # Correlation computation:
  cor_coral_inv <- cor.test(bites_df$coral_bites, bites_df$invert_bites, method = "spearman")


  # Plot:
  plot_coral_inv <- ggplot2::ggplot(bites_df,
                                    ggplot2::aes(color = seq,
                                                 shape = day,
                                                 y = coral_bites, x = invert_bites)) +

    ggplot2::geom_abline(color = "gray70", linewidth = 0.8) +

    ggplot2::geom_point(size = 2) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Invertivores bites/sequence/10m²") +

    ggplot2::ylab("Corallivores bites/sequence/10m²")


  ## Coral - Herbivory:

  # Correlation computation:
  cor_coral_herb <- cor.test(bites_df$coral_bites, bites_df$herb_bites, method = "spearman")

  # Plot:
  plot_coral_herb <- ggplot2::ggplot(bites_df,
                                    ggplot2::aes(color = seq,
                                                 shape = day,
                                                 y = coral_bites, x = herb_bites)) +

    ggplot2::geom_abline(color = "gray70", linewidth = 0.8) +

    ggplot2::geom_point(size = 2) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Herbivores bites/sequence/10m²") +

    ggplot2::ylab("Corallivores bites/sequence/10m²")



  ## Coral - Crustacivores:

  # Correlation computation:
  cor_coral_crust <- cor.test(bites_df$coral_bites, bites_df$crustac_bites, method = "spearman")

  # Plot:
  plot_coral_crust <- ggplot2::ggplot(bites_df,
                                     ggplot2::aes(color = seq,
                                                  shape = day,
                                                  y = coral_bites, x = crustac_bites)) +

    ggplot2::geom_abline(color = "gray70", linewidth = 0.8) +

    ggplot2::geom_point(size = 2) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Crustacivores bites/sequence/10m²") +

    ggplot2::ylab("Corallivores bites/sequence/10m²")


  ## Herbivory - Invertivory:

  # Correlation computation:
  cor_invert_herb <- cor.test(bites_df$invert_bites, bites_df$herb_bites, method = "spearman")

  # Plot:
  plot_invert_herb <- ggplot2::ggplot(bites_df,
                                     ggplot2::aes(color = seq,
                                                  shape = day,
                                                  y = invert_bites, x = herb_bites)) +

    ggplot2::geom_abline(color = "gray70", linewidth = 0.8) +

    ggplot2::geom_point(size = 2) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Herbivory bites/sequence/10m²") +

    ggplot2::ylab("Invertivores bites/sequence/10m²")


  ## Crustacivory - Herbivory:

  # Correlation computation:
  cor_crust_herb <- cor.test(bites_df$crustac_bites, bites_df$herb_bites, method = "spearman")

  # Plot:
  plot_crust_herb <- ggplot2::ggplot(bites_df,
                                     ggplot2::aes(color = seq,
                                                  shape = day,
                                                  y = crustac_bites, x = herb_bites)) +

    ggplot2::geom_abline(color = "gray70", linewidth = 0.8) +

    ggplot2::geom_point(size = 2) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Herbivory bites/sequence/10m²") +

    ggplot2::ylab("Crustacivores bites/sequence/10m²")


  ## Crustacivory - Invertivory:

  # Correlation computation:
  cor_crust_invert <- cor.test(bites_df$crustac_bites, bites_df$invert_bites, method = "spearman")

  # Plot:
  plot_crust_invert <- ggplot2::ggplot(bites_df,
                                     ggplot2::aes(color = seq,
                                                  shape = day,
                                                  y = crustac_bites, x = invert_bites)) +

    ggplot2::geom_abline(color = "gray70", linewidth = 0.8) +

    ggplot2::geom_point(size = 2) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +

    ggplot2::xlab("Invertivores bites/sequence/10m²") +

    ggplot2::ylab("Crustacivores bites/sequence/10m²")


  all_correl_plots <- (plot_invert_herb + plot_crust_herb + plot_coral_herb +
                        patchwork::plot_spacer() + plot_crust_invert + plot_coral_inv +
                         patchwork::plot_spacer() + patchwork::plot_spacer() +
                         plot_coral_crust) +

    patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                           ncol = 3, nrow = 3, guides = "collect")

  return(all_correl_plots)

}




