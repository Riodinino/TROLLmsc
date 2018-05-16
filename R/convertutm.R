#' @import tidyverse
library(tidyverse)
#' Title
#'
#' @param data Data.frame - Your simulated plot with TROLL, out of a real data plot treated with the precedent functions
#' @param datastation Data.frame - The whole dataset for your research station (here, Paracou)
#' @param Xutm Character - The name of the column corresponding to UTM (here, 22S) X coordinates
#' @param Yutm Character - The name of the column corresponding to UTM (here, 22S) Y coordinates
#' @param X Character - The name of the column corresponding to intra-plot X coordinates
#' @param Y Character - The name of the column corresponding to intra-plot Y coordinates
#' @param realedges Data.frame - table containing your plots' edges, in UTM, extracted from some GIS software, with a column indicating the plot number.
#' @param plotcol Character - The name of the column indicating plot numbers in datastation
#' @param plotedge Character - The name of the column indicating plot numbers in realedges
#' @param plotnum Integer - Number of the plot corresponding to your data station
#' @param year The year you work on
#' @param yearcol The column of years
#'
#' @return
#' @export
#'
#' @examples
convertutm <- function(data, plotnum, yearcol = "campagne", year = 1984, datastation = DataParacou, Xutm = "Xutm", Yutm = "Yutm", X = "X", Y = "Y", plotcol = "n_parcelle", plotedge = "Plot", realedges = coordinates <- read.csv(file = file.path("C:/Users/nino.page/Desktop/TROLL project/treated_data/sim_prepare_paracou/understoreys","coordinates.csv"), header = T, sep = ";", dec = ","))
{

# Checks and colnames preparation -----------------------------------------

# Datastation
  if(!(plotcol %in% names(datastation))) stop("The name you indicated for the plot indices does not match the dataset names")
  names(datastation)[which(names(datastation) == plotcol)] <- "plot"
  if(!(yearcol %in% names(datastation))) stop("The name you indicated for year does not match the dataset names")
  names(datastation)[which(names(datastation) == yearcol)] <- "campagne"
  if(!(X %in% names(datastation))) stop("The name you indicated for X does not match the dataset names")
  names(datastation)[which(names(datastation) == X)] <- "X"
  if(!(Y %in% names(datastation))) stop("The name you indicated for Y does not match the dataset names")
  names(datastation)[which(names(datastation) == Y)] <- "Y"
  if(!(Xutm %in% names(datastation))) stop("The name you indicated for Xutm does not match the dataset names")
  names(datastation)[which(names(datastation) == Xutm)] <- "Xutm"
  if(!(Yutm %in% names(datastation))) stop("The name you indicated for Yutm does not match the dataset names")
  names(datastation)[which(names(datastation) == Yutm)] <- "Yutm"

# Data

  if(!(X %in% names(data))) stop("The name you indicated for X does not match the dataset names")
  names(data)[which(names(data) == X)] <- "X"
  if(!(Y %in% names(data))) stop("The name you indicated for Y does not match the dataset names")
  names(data)[which(names(data) == Y)] <- "Y"


# Realedges

  if(!(plotedge %in% names(realedges))) stop("The name you indicated for the plot indices does not match the dataset names")
  names(realedges)[which(names(realedges) == plotedge)] <- "plot"
  if(!(Xutm %in% names(realedges))) stop("The name you indicated for Xutm does not match the dataset names")
  names(realedges)[which(names(realedges) == Xutm)] <- "Xutm"
  if(!(Yutm %in% names(realedges))) stop("The name you indicated for Yutm does not match the dataset names")
  names(realedges)[which(names(realedges) == Yutm)] <- "Yutm"


# Plot data extraction ---------------------------------------------------------
 library(tidyverse)
  datastation <- datastation[which(datastation$campagne == year),]
  datastation <- datastation[which(datastation$plot == plotnum),] %>% dplyr::filter(circ_corr > 31.4) %>% dplyr::select(X,Y,Xutm,Yutm)
  realedges <- realedges[which(realedges$plot == plotnum),] #Take into account the useless center point I had in my extraction or rather make it generic ?


# Corner correspondance ---------------------------------------------------

# Distances to the 3 points we need

  datastation$distance_0_0 <- rep(NA, nrow(datastation))
  datastation$distance_250_0 <- rep(NA, nrow(datastation))
  datastation$distance_0_250 <- rep(NA, nrow(datastation))
  # print(paste("nrow datastation", nrow(datastation)))

  # print(datastation$distance_0_0)

  for(i in 1:nrow(datastation))
  {
    print(paste(i,"/",nrow(datastation)))
    datastation$distance_0_0[i] <- sqrt((datastation$X[i])^2+(datastation$Y[i])^2)
    datastation$distance_250_0[i] <- sqrt((datastation$X[i]-250)^2+(datastation$Y[i]-0)^2)
    datastation$distance_0_250[i] <- sqrt((datastation$X[i]-0)^2+(datastation$Y[i]-250)^2)
  }

# Pick the existing point that is closer to these 3 points in order to get the UTM coord.

  near_origin <- datastation[which(datastation$distance_0_0 == min(datastation$distance_0_0)),c("Xutm","Yutm")]
  # print(min(datastation$distance_0_0))
  # print(summary(datastation$distance_0_0))
  print(paste("nrowrigin",nrow(near_origin)))
  if(nrow(near_origin)> 1){near_origin <- near_origin[sample(1:nrow(near_origin),size = 1),]}
  near_X250 <- datastation[which(datastation$distance_250_0 == min(datastation$distance_250_0)),c("Xutm","Yutm")]
  print(nrow(near_X250))
  if(nrow(near_X250)> 1){near_X250 <- near_X250[sample(1:nrow(near_X250),size = 1),]}
  near_Y250 <- datastation[which(datastation$distance_0_250 == min(datastation$distance_0_250)),c("Xutm","Yutm")]
  print(nrow(near_Y250))
  if(nrow(near_Y250)> 1){near_Y250 <- near_Y250[sample(1:nrow(near_Y250),size = 1),]}
# Then calculate the distance to these existing points in order to get the plot referential orientation (which is for the moment unknown)

  realedges$dist_near_origin <- rep(NA,nrow(realedges))
  realedges$dist_near_X250 <- rep(NA,nrow(realedges))
  realedges$dist_near_Y250 <- rep(NA,nrow(realedges))

  for(i in 1:nrow(realedges))
  {
    realedges$dist_near_origin[i] <- sqrt((near_origin$Xutm - realedges$Xutm[i])^2 +(near_origin$Yutm -realedges$Yutm[i])^2)
    realedges$dist_near_X250[i] <- sqrt((near_X250$Xutm - realedges$Xutm[i])^2 +(near_X250$Yutm -realedges$Yutm[i])^2)
    realedges$dist_near_Y250[i] <-  sqrt((near_Y250$Xutm - realedges$Xutm[i])^2 +(near_Y250$Yutm -realedges$Yutm[i])^2)
  }

  # print(realedges$dist_near_origin)
  # print(realedges$dist_near_X250)
  # print(realedges$dist_near_Y250)
  # print(paste("1: X",realedges$Xutm[1], ", Y", realedges$Yutm[1],";","5: X",realedges$Xutm[5], ", Y", realedges$Yutm[5]))

# Then attribute the role to our real edge points...

  real_origin <- unique(realedges[which(realedges$dist_near_origin == min(realedges$dist_near_origin)),c("Xutm","Yutm")])
  real_X_corner <- unique(realedges[which(realedges$dist_near_X250 == min(realedges$dist_near_X250)),c("Xutm","Yutm")])
  real_Y_corner <- unique(realedges[which(realedges$dist_near_Y250 == min(realedges$dist_near_Y250)),c("Xutm","Yutm")])

# We can now calculate the axis vectors and the unit vector corresponding to (0,1) and (1,0)

  X_axis <- real_X_corner - real_origin
  Y_axis <- real_Y_corner - real_origin

  X_unit <- X_axis/250 #Could be more generix or accurate
  Y_unit <- Y_axis/250 #idem


# Coordinates conversion --------------------------------------------------

  data$Xutm <- rep(NA, nrow(data))
  data$Yutm <- rep(NA, nrow(data))

  for(i in 1:nrow(data))
  {
    data$Xutm[i] <- real_origin$Xutm +(data$X[i]*X_unit$Xutm)+(data$Y[i]*Y_unit$Xutm)
    data$Yutm[i] <- real_origin$Yutm + (data$X[i]*X_unit$Yutm)+(data$Y[i]*Y_unit$Yutm)
  }


  return(data)

}
