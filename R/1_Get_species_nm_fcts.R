################################################################################
##
## Script to code the function to get the name herbivorous species on ...
## ... interaction with the substrate
##
## 1_Nb_herb_sp.R
##
## 08/11/2022
##
## Camille Magneville
##
################################################################################



#' Retrieve the species names in the column of the presence-absence dataframes
#'
#' @param df_vect a vector of the presence-absence dataframes
#'
#' @return a vector containing the names of all the species present in the
#' presence-absence datfarames columns
#'
#' @export
#'
#' @examples

get.sp.nm <- function(df_list) {

  # create a vector that will contain the species names:
  all_nm_vect <- c()

  for (i in c(1:length(df_list))) {
     sp_nm <- colnames(df_list[[i]])
     all_nm_vect <- append(all_nm_vect, sp_nm)
  }

  # remove duplicates:
  all_nm_vect <- unique(all_nm_vect)

  # remove time and vid_length names:
  all_nm_vect <- all_nm_vect[which(! all_nm_vect %in% c("vid_length",
                                                        "time"))]

  return(all_nm_vect)

}
