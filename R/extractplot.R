#' Title
#'
#' @param data data.frame Paracou dataset, for example
#' @param plotcol Column indicating the plot IDs, that are ideally integers
#' @param plot Integer. The number corresponding to the plot you wanna extract
#' @param sizecol Character. The column indicating the tree size measurement : 
#' DBH or circonference. The latter will need to be converted, which is done if "circonference = T"
#' @param alivecol Character. Column indicating if measured trees are alive, 
#' dead, fallen, etc. Only live trees are retained.
#' @param circonference Logical. If true, conversion to dbh will be done. 
#' If false, you mean that size measurements in your data are diameters at breast height. 
#' @param Genre Character. The column indicating the individuals' genera.
#' @param Espece Character. The column indicating the species names.
#' @param species_name Logical. False, if you have genus and species
#'  name separatedly. True if you already have a column aggregating genus and species names 
#'  separated by an under_score.
#' @param centimeters Logical. If your DBH are in centimeters, function converts it into milimeters. 
#' @param Paracou_correct Logical. Specific of Paracou, to prevent crashes from database-inherent singularities
#'
#' @return Output is a dataframe corresponding to all the inventories for the plot you selected,
#'  with species column added and circonference converted to dbh if ever needed.
#' @export
#'
#' @examples


extractplot <- function(data, plotcol = "n_parcelle", plot = 11, sizecol = "circ_corr", centimeters = T, alivecol = "code_vivant", circonference = T, Genre = "Genre", Espece ="Espece", species_name = FALSE, Paracou_correct = TRUE)
{
  if (!(plotcol %in% names(data))) 
    stop("plotcol is not one of the data columns !")
  data <- subset(data, data[,which(names(data) == plotcol)] == plot)
  circol <- which(names(data) == sizecol)
  # Individuals with error-code circonference
  if(Paracou_correct == TRUE){data[which(data[,circol] == 888),circol] <- 80*pi }
  if (dim(data)[1] == 0){stop("No data for this plot !")}
  if (!(sizecol %in% names(data))){stop("You indicated a wrong size (circonference or dbh) column")}
  # deprecated : data <- mutate(data, plotcol = as.character(plotcol))
  if (!(alivecol %in% names(data))) {stop("alivecol is not one of the data columns !")}
  data <- subset(data, data[,which(names(data) == alivecol)] == 1)
  if(circonference == T) {data[,which(names(data) == sizecol)] <- data[,which(names(data) == sizecol)]/3.14}
  if(centimeters == TRUE){data[,which(names(data) == sizecol)] <- 10*data[which(names(data) == sizecol)]}
  names(data)[which(names(data) == sizecol)] <- "dbh"
  data <- data[which(data$dbh > 100),]
  if(species_name == FALSE)
  {
    data$species <- paste(data[,which(names(data)== Genre)],data[,which(names(data)== Espece)], sep = "_")  
  }
  
  return(data)
}

#test

# plot11 <- extractplot(DataParacou)

