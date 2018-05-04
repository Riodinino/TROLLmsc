#' Title
#'
#' @param dataplot Data.frame... Data obtained from extractplot()
#' @param datastation whole dataset of your station...
#' @param speciescol Character. name of the species column
#' @param sp species-traits dataset..
#'
#' @return
#' @export
#'
#' @examples
speciesstation <- function (datastation, speciescol = "species", sp = read.table("species.txt", header = T))
{

# Safety checks -----------------------------------------------------------

  if("X...." %in% names(sp)){names(sp)[which(names(sp)== "X....")] <- "species"}
  
# Match plot species with troll species and create species tab ------------

  splist <- (unique(datastation[,which(names(datastation) == speciescol)]) %in% sp$species)
  spstation <- sp$species[sp$species %in% splist,];rm(splist)
  

# Calculate regional frequencies ------------------------------------------

  
  ### abundances of each species.
  
  #should datastation[,which(names(datastation)==speciescol)] class be character ?
  # if(!(class(datastation[,which(names(datastation)==speciescol)] == "character))){datastation[,which(names(datastation)==speciescol)] <- as.character(datastation[,which(names(datastation)==speciescol)])}
  
  ab <- tapply(datastation[,which(names(datastation)==speciescol)],datastation[,which(names(datastation)==speciescol)],nrow)#length or nrow ?
  abundances <- as.data.frame.array(ab)
  abundances$sp = rownames(ab) ; rm(ab)
  abundances$ab <- as.numeric(abundances$ab)
  rownames(abundances) <- NULL
  colnames(abundances)[1] <- 'n'
  
  ###regional frequencies
  ntot <- sum(abundances$n)
  abundances$frequencies <- abundances$n / ntot
  # abundances$frequencies <- abundances$frequencies[which(abundances$sp %in%spstation)]
  
  spstation$Freg <- abundances$frequencies[which(abundances$sp %in%spstation)] #correspondance issue
  
  ### Last step : normalization to sum to 1
  f <- sum(spstation$Freg)
  spstation$Freg <- spstation$Freg/f 
  row.names(sppstation) <- spstation[,which(names(spstation) == speciescol)]
  return(spstation)
}
