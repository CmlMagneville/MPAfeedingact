################################################################################
##
## Script to code functions to gather cameras and study species contribution...
## ... to herbivory
##
## 2_Herb_contribution_fcts.R
##
## 09/11/2022
##
## Camille Magneville
##
################################################################################




#' Merge dataframes from LD and SD camera for a given day and correct names for
#' AcCtendark (-> C. striatus) and Scarus sordidus (-> Chlorurus)and remove
#' all planktivore species except H. fasciatus (wrong annotation)
#'
#' @param list_LD_dfs a list containing the grazingdataframe
#' from the long-duration cameras ie one dataframe per camera
#'
#' @param list_SD_dfs a list containing the grazing dataframe
#' from the short-duration cameras ie one dataframe per camera and per pose
#'
#' @param start_time a time in a character format like "hh:mm:ss" to tell when
#' the final dataframe must start
#'
#' @param end_time a time in a character format like "hh:mm:ss" to tell when
#' the final dataframe must end
#'
#' @return a presence-absence dataframe with the data gathered from all the
#' dataframes (long and short duration): time is continuous from the start_hour
#' to the end_hour even if short-duration dataframes only have annotation on
#' certain time spans.
#'
#' @export
#'
#' @examples


merge.cam.vid.df <- function(list_LD_dfs, list_SD_dfs,
                             start_time, end_time,
                             herb_nm_all_vect) {


  # Get species names:
  sp_nm_day_vect <- c()

  for (i in c(1:length(list_LD_dfs))) {

    # Correct for Ac_Cten_dark and rename:
    if ("Ac_Cten_dark" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::rename(list_LD_dfs[[i]],
                                        Ctenochaetus_striatus = Ac_Cten_dark)
    }

    # Correct for Scarus sordidus -> Chlorurus sordidus:
    if ("Scarus_sordidus" %in% colnames(list_LD_dfs[[i]])) {

      # if a column is already called Chlorurus sordidus: add and delete
      if ("Chlorurus_sordidus" %in% colnames(list_LD_dfs[[i]])) {
        list_LD_dfs[[i]][, "Chlorurus_sordidus"] <- list_LD_dfs[[i]][, "Chlorurus_sordidus"] +
          list_LD_dfs[[i]][, "Scarus_sordidus"]
        list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Scarus_sordidus)
      }

      # if no column is already called Chlorurus sordidus: rename:
      if (! "Chlorurus_sordidus" %in% colnames(list_LD_dfs[[i]])) {
        list_LD_dfs[[i]] <- dplyr::rename(list_LD_dfs[[i]],
                                          Chlorurus_sordidus = Scarus_sordidus)
      }

    }

    # Correct for Cheilinus_quinquecinctus -> Cheilinus_fasciatus:
    if ("Cheilinus_quinquecinctus" %in% colnames(list_LD_dfs[[i]])) {

      # if a column is already called Cheilinus_fasciatus: add and delete
      if ("Cheilinus_fasciatus" %in% colnames(list_LD_dfs[[i]])) {
        list_LD_dfs[[i]][, "Cheilinus_fasciatus"] <- list_LD_dfs[[i]][, "Cheilinus_fasciatus"] +
          list_LD_dfs[[i]][, "Cheilinus_quinquecinctus"]
        list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Cheilinus_quinquecinctus)
      }

      # if no column is already called Chlorurus sordidus: rename:
      if (! "Cheilinus_fasciatus" %in% colnames(list_LD_dfs[[i]])) {
        list_LD_dfs[[i]] <- dplyr::rename(list_LD_dfs[[i]],
                                          Cheilinus_fasciatus = Cheilinus_quinquecinctus)
      }

    }

    # Remove Labridae_Thalassoma if present:
    if ("Labridae_Thalassoma" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Labridae_Thalassoma)
    }

    # Remove Chromis_chromis if present:
    if ("Chromis_chromis" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Chromis_chromis)
    }
    # Remove Chromis_viridis if present:
    if ("Chromis_viridis" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Chromis_viridis)
    }
    # Remove Pomacentrus_pikei if present:
    if ("Pomacentrus_pikei" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Pomacentrus_pikei)
    }
    # Remove Pomacentrus_sulfureus if present:
    if ("Pomacentrus_sulfureus" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Pomacentrus_sulfureus)
    }
    # Remove Thalassoma_lunare if present:
    if ("Thalassoma_lunare" %in% colnames(list_LD_dfs[[i]])) {
      list_LD_dfs[[i]] <- dplyr::select(list_LD_dfs[[i]], ! Thalassoma_lunare)
    }




    sp_nm_vect <- colnames(list_LD_dfs[[i]])
    sp_nm_vect <- sp_nm_vect[which(! sp_nm_vect %in%
                                     c("time", "vid_length"))]

    sp_nm_day_vect <- append(sp_nm_day_vect, sp_nm_vect)
    sp_nm_day_vect <- unique(sp_nm_day_vect)

  }

  for (i in c(1:length(list_SD_dfs))) {

    # Correct for Ac_Cten_dark and rename:
    if ("Ac_Cten_dark" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::rename(list_SD_dfs[[i]],
                                        Ctenochaetus_striatus = Ac_Cten_dark)
    }

    # Correct for Scarus sordidus -> Chlorurus sordidus:
    if ("Scarus_sordidus" %in% colnames(list_SD_dfs[[i]])) {

      # if a column is already called Chlorurus sordidus: add and delete
      if ("Chlorurus_sordidus" %in% colnames(list_SD_dfs[[i]])) {
        list_SD_dfs[[i]][, "Chlorurus_sordidus"] <- list_SD_dfs[[i]][, "Chlorurus_sordidus"] +
          list_SD_dfs[[i]][, "Scarus_sordidus"]
        list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Scarus_sordidus)
      }

      # if no column is already called Chlorurus sordidus: rename:
      if (! "Chlorurus_sordidus" %in% colnames(list_SD_dfs[[i]])) {
        list_SD_dfs[[i]] <- dplyr::rename(list_SD_dfs[[i]],
                                          Chlorurus_sordidus = Scarus_sordidus)
      }

    }

    # Correct for Cheilinus_quinquecinctus -> Cheilinus_fasciatus:
    if ("Cheilinus_quinquecinctus" %in% colnames(list_SD_dfs[[i]])) {

      # if a column is already called Cheilinus_fasciatus: add and delete
      if ("Cheilinus_fasciatus" %in% colnames(list_SD_dfs[[i]])) {
        list_SD_dfs[[i]][, "Cheilinus_fasciatus"] <- list_SD_dfs[[i]][, "Cheilinus_fasciatus"] +
          list_SD_dfs[[i]][, "Cheilinus_quinquecinctus"]
        list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Cheilinus_quinquecinctus)
      }

      # if no column is already called Chlorurus sordidus: rename:
      if (! "Cheilinus_fasciatus" %in% colnames(list_SD_dfs[[i]])) {
        list_SD_dfs[[i]] <- dplyr::rename(list_SD_dfs[[i]],
                                          Cheilinus_fasciatus = Cheilinus_quinquecinctus)
      }

    }

    # Remove Labridae_Thalassoma if present:
    if ("Labridae_Thalassoma" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Labridae_Thalassoma)
    }

    # Remove Chromis_chromis if present:
    if ("Chromis_chromis" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Chromis_chromis)
    }
    # Remove Chromis_viridis if present:
    if ("Chromis_viridis" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Chromis_viridis)
    }
    # Remove Pomacentrus_pikei if present:
    if ("Pomacentrus_pikei" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Pomacentrus_pikei)
    }
    # Remove Pomacentrus_sulfureus if present:
    if ("Pomacentrus_sulfureus" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Pomacentrus_sulfureus)
    }
    # Remove Thalassoma_lunare if present:
    if ("Thalassoma_lunare" %in% colnames(list_SD_dfs[[i]])) {
      list_SD_dfs[[i]] <- dplyr::select(list_SD_dfs[[i]], ! Thalassoma_lunare)
    }

    sp_nm_vect <- colnames(list_SD_dfs[[i]])
    sp_nm_vect <- sp_nm_vect[which(! sp_nm_vect %in%
                                     c("time", "vid_length"))]

    sp_nm_day_vect <- append(sp_nm_day_vect, sp_nm_vect)
    sp_nm_day_vect <- unique(sp_nm_day_vect)

  }

  # Get the number of timesteps:
  time_vect <- hms::as_hms(c(hms::as_hms(start_time):hms::as_hms(end_time)))


  # Create the final day all species dataframe:
  final_day_allsp_df <- as.data.frame(matrix(ncol = length(sp_nm_day_vect),
                                            nrow = length(time_vect),
                                            0))
  colnames(final_day_allsp_df) <- sp_nm_day_vect
  rownames(final_day_allsp_df) <- time_vect


  # loop on each LD dataframe that should be merged:
  for (i in (1:length(list_LD_dfs))) {

    studied_df <- list_LD_dfs[[i]]
    studied_df <- studied_df[which(rownames(studied_df) <= end_time &
                                   rownames(studied_df) >= start_time), ]

    # loop on the columns ...
    # ... and if the column is in the studied dfs then add 1 values:

    for (j in c(1:(ncol(studied_df) - 2))) {

      # add the species in the allsp dataframe:
      # numerise the column values:
      studied_df[, j] <- as.numeric(studied_df[, j])

      # create a new dataframe which only keeps values >= 1 of the studied df:
      sp_nm <- colnames(studied_df)[j]
      temp_df <- dplyr::filter(studied_df, get(sp_nm) >= 1)

      # add these values into the final_day_herb_df:
      for (k in rownames(temp_df)) {
        final_day_allsp_df[k, sp_nm] <- temp_df[k, sp_nm] + final_day_allsp_df[k, sp_nm]
      }

    } # end loop on the studied df colnames

  } # end loop on LD dfs


  # loop on each SD dataframe that should be merged:
  for (i in (1:length(list_SD_dfs))) {

    studied_df <- list_SD_dfs[[i]]
    studied_df <- studied_df[which(rownames(studied_df) <= end_time &
                                     rownames(studied_df) >= start_time), ]

    # loop on the columns ...
    # ... and if the column is in the studied dfs then add 1 values:

    for (j in c(1:(ncol(studied_df) - 2))) {

      # add the species to the allsp dataframe:
      # numerise the column values:
      studied_df[, j] <- as.numeric(studied_df[, j])

      # create a new dataframe which only keeps values >= 1 of the studied df:
      sp_nm <- colnames(studied_df)[j]
      temp_df <- dplyr::filter(studied_df, get(sp_nm) >= 1)

      # add these values into the final_day_herb_df:
      for (k in rownames(temp_df)) {
        final_day_allsp_df[k, sp_nm] <- temp_df[k, sp_nm] + final_day_allsp_df[k, sp_nm]
      }

    } # end loop on the studied df colnames

  } # end loop on SD dfs


  return(final_day_allsp_df)

}





#' Compute the contribution to the feeding_act for one site
#'
#' @param df_list a list containing the grazing dataframe of the studied
#' site with one dataframe for each day
#'
#' @param all_sp_FPA_vect a vector containing the name of the species seen on
#' the FPA site
#'
#' @param all_sp_PPA_vect a vector containing the name of the species seen on
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
                                 all_sp_FPA_vect,
                                 all_sp_PPA_vect,
                                 studied_site) {


  # Build the df that will contain bites information per day:
  if (studied_site == "FPA") {
    bites_info_day_df <- as.data.frame(matrix(ncol = 4,
                                              nrow = 1,
                                              NA))
    colnames(bites_info_day_df) <- c("sp_nm", "herb_act", "site", "day")
  }

  if (studied_site == "PPA") {
    bites_info_day_df <- as.data.frame(matrix(ncol = 4,
                                              nrow = 1,
                                              NA))
    colnames(bites_info_day_df) <- c("sp_nm", "herb_act", "site", "day")
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

      herb_act <- bites_annotated_min_surf[[sp_nm]]

      if (sp_nm %in% all_sp_FPA_vect & sp_nm %in% all_sp_PPA_vect) {
        site <- "both"
      }

      if (sp_nm %in% all_sp_FPA_vect & ! sp_nm %in% all_sp_PPA_vect) {
        site <- "FPA_only"
      }

      if (! sp_nm %in% all_sp_FPA_vect & sp_nm %in% all_sp_PPA_vect) {
        site <- "PPA_only"
      }

    bites_info_day_df <- dplyr::add_row(bites_info_day_df,
                                        sp_nm = sp_nm,
                                        herb_act = herb_act,
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
  colnames(bites_info_tot_df) <- c("sp_nm", "herb_act", "site")

  # Fill this df:
  for (i in c(1:length(df_list))) {

    studied_df <- df_list[[i]]

    # compute the total sum for a given day:
    summed_bites_tot <- apply(studied_df, 2, sum)

    for (sp_nm in names(summed_bites_tot)) {

      herb_act <- summed_bites_tot[[sp_nm]]

      if (sp_nm %in% all_sp_FPA_vect & sp_nm %in% all_sp_PPA_vect) {
        site <- "both"
      }

      if (sp_nm %in% all_sp_FPA_vect & ! sp_nm %in% all_sp_PPA_vect) {
        site <- "FPA_only"
      }

      if (! sp_nm %in% all_sp_FPA_vect & sp_nm %in% all_sp_PPA_vect) {
        site <- "PPA_only"
      }

      # if the species has been seen on the day(s) before:
      if (sp_nm %in% bites_info_tot_df$sp_nm) {
        bites_info_tot_df[which(bites_info_tot_df$sp_nm == sp_nm), "herb_act"] <- herb_act +
          bites_info_tot_df[which(bites_info_tot_df$sp_nm == sp_nm), "herb_act"]
      }

      # if it is the first time that the species is seen (not days before):
      if (! sp_nm %in% bites_info_tot_df$sp_nm) {
        bites_info_tot_df <- dplyr::add_row(bites_info_tot_df,
                                            sp_nm = sp_nm,
                                            herb_act = herb_act,
                                            site = site)
      }

    } # end add each species information of a studied day

  } # end loop on days


  # now compute bites per annotated minute:
  bites_info_tot_df$herb_act <- bites_info_tot_df$herb_act/120
  # now compute bites per annotated minute per m2:
  bites_info_tot_df$herb_act <- bites_info_tot_df$herb_act/5

  # remove firstr row:
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


plot.herb.contrib <- function(contrib_FPA_df,
                              contrib_PPA_df,
                              scale,
                              site_colors) {


  if (scale == "total") {

    plot_FPA <- ggplot2::ggplot(data = contrib_FPA_df) +

      ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - herb_act), x = herb_act,
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

      ggplot2::xlim(c(0, max(max(contrib_FPA_df$herb_act),
                             max(contrib_PPA_df$herb_act)) + 0.5)) +

      ggplot2::ylab("")


    plot_PPA <- ggplot2::ggplot(data = contrib_PPA_df) +

      ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - herb_act), x = herb_act,
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

      ggplot2::xlim(c(0, max(max(contrib_FPA_df$herb_act),
                             max(contrib_PPA_df$herb_act)) + 0.5)) +

      ggplot2::ylab("")


    plot_both <- (plot_FPA + plot_PPA) +
      patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                             ncol = 2, nrow = 1, guides = "collect") +
      patchwork::plot_annotation(tag_levels = "A")


    ggplot2::ggsave(filename = here::here("outputs", "Tot_herb_contrib.pdf"),
                    plot = plot_both,
                    device = "pdf",
                    scale = 0.6,
                    height = 10000,
                    width = 18000,
                    units = "px",
                    dpi = 800)

  }


  if (scale == "day") {

    plot_FPA <- ggplot2::ggplot(data = contrib_FPA_df) +

      ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - herb_act), x = herb_act,
                                     fill = site),
                        stat = "identity") +
      ggplot2::facet_wrap(~ day) +

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

      ggplot2::xlim(c(0, max(max(contrib_FPA_df$herb_act),
                             max(contrib_PPA_df$herb_act)) + 0.5)) +

      ggplot2::ylab("")


    plot_PPA <- ggplot2::ggplot(data = contrib_PPA_df) +

      ggplot2::geom_bar(ggplot2::aes(y = reorder(sp_nm, - herb_act), x = herb_act,
                                     fill = site),
                        stat = "identity") +
      ggplot2::facet_wrap(~ day) +

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

      ggplot2::xlim(c(0, max(max(contrib_FPA_df$herb_act),
                             max(contrib_PPA_df$herb_act)) + 0.5)) +

      ggplot2::ylab("")


    plot_both <- (plot_FPA + plot_PPA) +
      patchwork::plot_layout(byrow = TRUE, heights = c(1, 1), widths = c(1, 1),
                             ncol = 1, nrow = 2, guides = "collect") +
      patchwork::plot_annotation(tag_levels = "A")


    ggplot2::ggsave(filename = here::here("outputs", "Day_herb_contrib.pdf"),
                    plot = plot_both,
                    device = "pdf",
                    scale = 0.6,
                    height = 15000,
                    width = 18000,
                    units = "px",
                    dpi = 800)

  }


  return(plot_both)


}
