#' @import tidyverse
library(tidyverse)
#'
#' Title
#'
#' @param data data.frame ; Basically the censuses for the plot you want to work on.
#' @param year Integer. The year corresponding to your simulation startpoint.
#' @param censuscol Character. Name of the column indicating the censuses years.
#' @param label Character.Name of the column indicating species labels, if you have defined them.
#' @param speciescol Character. The name of the column indicating species identity. Should be of the form Genus_species matching case and separator.
#' @param speciestraits Data.frame. The table containing your species-specific traits that will be used to parametrise TROLL.
#' @param replacement Character. Procedure you choose to replace individuals belonging to missing species.
#' @param X Character. The name of the column containing X coordinates. At the plot scale, don't put an UTM or other shit !
#' @param Y Character. The name of the column containing Y coordinates. At the plot scale, don't put an UTM or other shit !
#' @param dbh Character. Name of the column containing the diameters at breast height. Circumferences are handler in the "extractplot" function.
#' @param milimeters Logical. Indicates whether the diameters are in milimeters or not.
#' Defaults to FALSE as this unit is unusual in most censuses.
#'
#' @return
#' @export
#'
#' @examples
prepareforest <- function (data, year = 1992, censuscol="campagne",label = "sp_lab", speciescol = "species",
                          speciestraits, replacement = "local",i_arbre = "i_arbre", X = "X", Y = "Y", dbh = "dbh", milimeters = T, plotcol = "n_parcelle") #???duplicated_method = "Strange"
{
data <- filter(data, dbh >100)
# Safety checks -----------------------------------------------------------

  if(!(speciescol %in% names(data)))
    stop("The species column you indicated doesn't match with dataset labels")

  if (!(censuscol %in% names(data)))
    stop("The census colum you indicated doesn't match with dataset labels")

  data <- subset(data, data[which(names(data) == censuscol)] == year)

  # print(summary(data[,which(names(data) == dbh)]))
  if (dim(data)[1] == 0) {stop("No data for this census year !")}

  if(!"sp_lab" %in% names(speciestraits)){speciestraits$sp_lab = 1:nrow(speciestraits)}

  if(class(speciestraits[,which(names(speciestraits) == speciescol)]) == "factor")
  {
    speciestraits[,which(names(speciestraits) == speciescol)] <- as.character(speciestraits[,which(names(speciestraits) == speciescol)])
  }

  if(class(data[,which(names(data) == speciescol)]) == "factor")
  {
    data[,which(names(data) == speciescol)] <- as.character(data[,which(names(data) == speciescol)])
  }

# Isolating parametrized and unparametrized species -----------------------

  ## Parametrized
  # print(paste("nrow data" , nrow(data)))

  param <- data[which(data[,which(names(data) == speciescol)]%in%speciestraits[,which(names(speciestraits) == speciescol)]),]
  param$sp_lab = rep(NA,nrow(param))

  # print(paste("nrow param" , nrow(param))) #debug check

  ## Missing
  miss <- data[which(!(data[,which(names(data) == speciescol)]%in%speciestraits[,which(names(speciestraits) == speciescol)])),]
  miss$sp_lab = rep(NA,nrow(miss))
  miss <- miss %>% arrange(dbh)

  # print(paste("nrow miss" , nrow(miss))) #debug check

  ## Diagnostics
  print(paste0("Il y a ",nrow(unique(param[which(names(param) == speciescol)])),
      " especes correspondant a celles de TROLL, et ", nrow(unique(miss[which(names(miss) == speciescol)])),
      " especes manquantes, ce qui représente ", nrow(miss)*100/nrow(data), " % des individus de la parcelle."))


     warn <- paste0("Il y a ",nrow(unique(param[which(names(param) == speciescol)])),
                       " especes correspondant a celles de TROLL, et ", nrow(unique(miss[which(names(miss) == speciescol)])),
                       " especes manquantes, ce qui représente ", nrow(miss)*100/nrow(data), " % des individus de la parcelle", unique(data[,which(names(data)== plotcol)]) )
     write.table(warn, file = file.path("C:/Users/nino.page/Desktop/TROLL project/treated_data/sim_prepare_paracou/warnings2",paste0(unique(data[,which(names(data)== plotcol)])[1],year,".txt")))


# Sp_lab correspondance ---------------------------------------------------
  # print(length(which(is.na(param$sp_lab == T))))#debug
  # print(length(param[,which(names(param) == speciescol)]))#debug
  for(i in 1:length(param[,which(names(param) == speciescol)]))
  {
    param$sp_lab[i] <- speciestraits[which(speciestraits[,which(names(speciestraits) == speciescol)] == param[i,which(names(param) == speciescol)]), which(names(speciestraits) == label)]
  }
  # print(paste("is it working",length(which(is.na(param$sp_lab == T)))))#debug

# Replacement procedure ---------------------------------------------------

  ## Sampling weighted by local frequencies
  if (replacement == "local")
  {
    if(nrow(miss)*100/nrow(data) > 50)
      sampl <- dplyr::sample_n(param, nrow(miss), replace = T)
    else
      sampl <- dplyr::sample_n(param, nrow(miss), replace = F)
  }
  else if (replacement == "regional")
  {
    if(!(Freg %in% names(speciestraits)))
      stop("The Regional Frequencies column you indicated doesn't match with the dataset labels.")
   sampl <- sample(unique(speciestraits[which(names(speciestraits) == speciescol)]),size = nrow(miss),prob = as.vector(speciestraits[which(names(speciestraits) == Freg)]), replace = F)
  }

  else if(replacement == "diameter")
  {

    breaks <- c(100,150,300,420,675,1000,99999)

    sampl = param[c(1,2),]
    dbhcol <- which(names(param)==dbh)

    for(i in 2:length(breaks))
    {
      target <- param[which(param[,dbhcol] >= breaks[i-1] & param[,dbhcol] < breaks[i]),]
      classe <- miss[which(miss[,dbhcol] >= breaks[i-1] & miss[,dbhcol] < breaks[i]),]
      lengthclass <- nrow(classe)

      print(paste("Classe ", breaks[i-1], ":", breaks[i], " - ",lengthclass, "Individus"))
      print(paste("Param ", breaks[i-1], ":", breaks[i], " - ",nrow(target), "Individus"))

      if(lengthclass > 0) #If there are individuals belonging to missing species in thisdiameter class
      {
        if(nrow(target)>0) #If there are individuals of known species in this same clas
        {
          if(nrow(target)>= lengthclass)
          {
            temp <- dplyr::sample_n(target, size = lengthclass, replace = F)
          }
          else
          {
            temp <- dplyr::sample_n(target, size = lengthclass, replace = T)
          }
        }
        else #If not then use a neighbour class to sample
        {
          if(i == 2) # If this is the first diameter class, jump to next
          {
            target <- param[which(param[,dbhcol] >= breaks[i] & param[,dbhcol] < breaks[i+1]),]
          }
          else # Else, jump to previous
          {
            target <- param[which(param[,dbhcol] >= breaks[i-2] & param[,dbhcol] < breaks[i-1]),]
          }
          #Sampling
          if(nrow(target) >= lengthclass)
          {
            temp <-  dplyr::sample_n(target,size = lengthclass,replace = F)
          }
          else
          {
            temp <-  dplyr::sample_n(target,size = lengthclass,replace = T)
          }
        }
        # if(nrow(temp)>1){
          sampl <- rbind(sampl, temp) #Update sample
          # temp <- temp[-c(2:nrow(temp)),]
      }
 print(paste("nrow temp", nrow(temp)))

 }

    }
     sampl <- sampl[c(-1,-2),] #Take off the 2 first lines that are extra (gotodef)
  }

  print(nrow(miss));print(nrow(sampl))

  #Replace the sample tree characteristics by those of the missing (could be done reversely, but I orignially did this dirty way and it works since these are the variables we keep at the end : dbh, species, x, y, ID ; should be adapted if derived for every other use.)
  sampl$X <- miss[,which(names(miss) == X)] #May be written x
  sampl$Y <- miss[,which(names(miss) == Y)] #%idem y
  sampl$dbh <- miss[,which(names(miss) == dbh)] #the DBH is kept, obviously; the only risk is to obtain irrealistically big individuals but TROLL corrects when dbh > 1.5 dmax.
  sampl$i_arbre <- miss[,which(names(miss) == i_arbre)]

  forest <- rbind(param,sampl)
  # else if (replacement =="classes")
  # {
  #
  # }



# Cleaning coordinates -------------------------------------

  forest$suppr <- rep(F, nrow(forest)) #Logical vector to tag individuals to be supressed

  # Scaling and rounding coordinates prior to process

  if(!(min(forest[,which(names(forest)== X)]) >= 0))
  {
    warning("Please note that you have negative coordinates for X")
    forest[,which(names(forest)== X)] <- forest[,which(names(forest)== X)] - min(forest[,which(names(forest)== X)])
  }

  if(!(min(forest[,which(names(forest)== Y)]) >= 0))
  {
    warning("Please note that you have negative coordinates for Y")
    forest[,which(names(forest)== Y)] <- forest[,which(names(forest)== Y)] - min(forest[,which(names(forest)== Y)])
  }

  # print(paste(class(forest[,which(names(forest)== X)]), class(forest[,which(names(forest)== Y)])))#debug
  if(class(forest[,which(names(forest)== X)]) != "integer" | class(forest[,which(names(forest)== Y)]) != "integer")
  warning("Coordinates are rounded in this function for the sake of safety")

  forest[which(names(forest)== X)] <- round(forest[which(names(forest)== X)],digits = 0)
  forest[which(names(forest)== Y)] <- round(forest[which(names(forest)== Y)],digits = 0)





final_forest <- forest[,c(which(names(forest)== X),which(names(forest)== Y),which(names(forest)== dbh),which(names(forest)== label), which(names(forest)== "i_arbre"))]
if(milimeters == FALSE)
{
  warning("dbh must be milimeters. It will be mutiplicated by 10 assuming you use centimeters. If not, consider pre-treating data.")
  data[,which(names(data) == dbh)] <- 10*data[which(names(data) == dbh)]
}
  return(final_forest)
}
