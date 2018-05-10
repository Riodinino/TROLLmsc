#' Title
#'
#' @param dataplot Data.frame... Data obtained from extractplot()
#' @param speciescol Character. name of the species column
#' @param sp species-traits dataset..
#'
#' @return A dataframe with the plot's species that match with TROLL sp list
#' and the corresponding "regional" frequencies to calibrate the seedrain on.
#' @export
#'
#' @examples
speciesplot <- function (dataplot, speciescol = "species", sp = read.table("C:/Users/nino.page/Desktop/TROLL project/data/species_nino_paracou.txt", header = T))
{

  # Safety checks -----------------------------------------------------------

  if("X...." %in% names(sp)){names(sp)[which(names(sp)== "X....")] <- "species"}

  # Match plot species with troll species and create species tab ------------

  rows <- which(dataplot[,which(names(dataplot) == speciescol)] %in% sp$species) # rows of the species column that match with species list
  splist <- as.vector(unique(dataplot[rows,which(names(dataplot) == speciescol)]))

  spplot <- sp[sp$species %in% splist,];rm(splist)
  print(paste(nrow(spplot),"species in the plot"))


  # Calculate regional frequencies ------------------------------------------


  ### abundances of each species.

  #should dataplot[,which(names(dataplot)==speciescol)] class be character ?
  # if(!(class(dataplot[,which(names(dataplot)==speciescol)] == "character))){dataplot[,which(names(dataplot)==speciescol)] <- as.character(dataplot[,which(names(dataplot)==speciescol)])}

  ab <- tapply(as.vector(dataplot[,which(names(dataplot)==speciescol)]),as.vector(dataplot[,which(names(dataplot)==speciescol)]),length)#length or nrow ?
  abundances <- as.data.frame.array(ab)
  abundances$sp = rownames(ab) ; rm(ab)
  abundances$ab <- as.numeric(abundances$ab)
  rownames(abundances) <- NULL
  colnames(abundances)[1] <- 'n'


  ###regional frequencies
  ntot <- sum(abundances$n)
  abundances$frequencies <- abundances$n / ntot
  # abundances$frequencies <- abundances$frequencies[which(abundances$sp %in%spplot)]

  # spplot$Freg <- abundances$frequencies[which(abundances$sp %in%spplot)] #correspondance issue
  for (i in 1:nrow(spplot))
  {
    spplot$Freg[i] <- abundances[which(abundances$sp == spplot$species[i]),which(names(abundances) == "frequencies")]
  }
  ### Last step : normalization to sum to 1
  f <- sum(spplot$Freg)
  spplot$Freg <- spplot$Freg/f
  row.names(spplot) <- spplot[,which(names(spplot) == speciescol)]
  return(spplot)
}


