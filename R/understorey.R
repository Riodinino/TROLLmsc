#' Title
#'
#' @param realdata
#' @param trollsim
#' @param X
#' @param Y
#' @param dbh
#' @param sp_lab
#' @param path
#'
#' @return
#' @export
#'
#' @examples
understorey <- function(realdata, trollsim, plotnum, X = "X", Y = "Y", dbh = "dbh", sp_lab = "sp_lab", path)
{

# Safety checks -----------------------------------------------------------

  if(!(X %in% names(realdata)& X %in% names(trollsim)))
    stop("X coordinates column label do not match between your real data and the TROLL simulated forest. Please check it.")

  if(!(Y %in% names(realdata)& Y %in% names(trollsim)))
    stop("Y coordinates column label do not match between your real data and the TROLL simulated forest. Please check it.")

  if(!(dbh %in% names(realdata)& dbh %in% names(trollsim)))
    stop("diameters at breast height column label do not match between your real data and the TROLL simulated forest. Please check it.")

  if(!(sp_lab %in% names(realdata)& sp_lab %in% names(trollsim)))
    stop("sp_lab column label do not match between your real data and the TROLL simulated forest. Please check it.")

  ninit <- nrow(realdata)
# Matching individuals ----------------------------------------------------

  Xdat <- which(names(realdata)== X)
  Ydat <- which(names(realdata)== Y)
  dbhdat <- which(names(realdata)== dbh)
  sp_labdat <- which(names(realdata)== sp_lab)

  Xsim <- which(names(trollsim)== X)
  Ysim <- which(names(trollsim)== Y)
  dbhsim <- which(names(trollsim)== dbh)
  sp_labsim <- which(names(trollsim)== sp_lab)

  if(!(Xsim == Xdat & Ysim == Ydat & dbhsim == dbhdat & sp_labsim == sp_labdat))
  {
    trollsim <- trollsim[,c(Xsim,Ysim,dbhsim,sp_labsim)]
    realdata <- realdata[,c(Xdat,Ydat,dbhdat,sp_labdat)]
    warning("Datasets were re-ordered in order to be able to switch rows from data to sim")
  }

  # row_Xdat <- which(realdata[,Xdat] %in% trollsim[,Xsim])
  # print(length(row_Xdat))
  # row_Xsim <- which(trollsim[,Xsim] %in% realdata[,Xdat])
  # print(length(row_Xsim))
  # row_XYdat <- which(realdata[row_Xdat,Ydat]%in% trollsim[row_Xsim,Ysim])
  # print(length(row_XYdat))
  # row_XYsim <- which(trollsim[row_Xsim,Ysim] %in% realdata[row_Xdat,Ydat])
  # print(length(row_XYsim))
 # print(realdata[row_XYdat, sp_labdat]%in% trollsim[row_XYsim,sp_labsim])

 # for()
  # commons <- which(realdata[row_XYdat, sp_labdat] %in% trollsim[row_XYsim,sp_labsim])

  # if(length(commons == 0))
    # stop("There is apparently no possibly common individual between both datasets. Are you sure you didn't mismatch the inventories ?")

# Replacement procedure ---------------------------------------------------

  # parcourir le tableau, regarder chaque individu de la parcelle de vraies données, si il existe dans la simulation on le remplace sinon, on l'ajoute.

  # for(i in 1:nrow(realdata))
  # {
  #   rows_xi <- which(trollsim[,Xsim] == realdata[i,Xdat])
  #   row_xyi <- which(trollsim[rows_xi, Ysim] == realdata[i,Ydat])
  #
  #   if(length(row_xyi) > 1)
  #     stop("You have duplicates in your dataset. You should have used my replacement procedure prior to simulation, of have found a way to handle it. Come back with proper datasets !")
  #
  #   else if(length(row_xyi) == 1)
  #   {
  #     trollsim[row_xyi, dbhsim] <- realdata[i,dbhdat]
  #     trollsim[row_xyi, sp_labsim] <- realdata[i,sp_labdat]
  #   }
  #   else
  #   {
  #     trollsim <- rbind(trollsim, realdata[i,])
  #   }
  # }
  # realdata[,-"i_arbre"]
  count_same <- 0
  count_not_same <- 0

  for(i in 1:nrow(trollsim))
  {


    rows_xi <- which(realdata[,Xdat] == trollsim[i,Xsim])
    row_xyi <- which(realdata[rows_xi, Ydat]== trollsim[i,Ysim])
    # print(row_xyi)

    if(length(row_xyi) > 1)
      stop("You have duplicates in your dataset. You should have used my replacement procedure prior to simulation, of have found a way to handle it. Come back with proper datasets !")

    else if(length(row_xyi) == 1)
    {
      if(realdata[row_xyi,sp_labdat] == trollsim[i,sp_labsim]){
        # print("same individual !")
        # print("same individual !")
        print(paste(i,"/",nrow(trollsim)))
        count_same = count_same +1}

      else {
        # print("not the same individual !")
        count_not_same = count_not_same +1}

    }
    else
    {
      if(trollsim[i,dbhsim]<100) #Gotta get sure, because TROLLsims have dbhs very low (under 1)
      {
        # print(paste("trollsim",ncol(trollsim)))

        # print(i)
        # print(dplyr::as.tbl(trollsim[i,]))
        line <- cbind(trollsim[i,-5],i_arbre = "new")
        realdata <- rbind(realdata,line)
      }

    }
  }

  print(length(which(trollsim[,dbhsim]<10)))
  print(paste("realdat incremented",nrow(realdata)))
  print(paste("same ",count_same))
  print(paste("not same ",count_not_same))

  # journal <- file(file.path(path, 'understorey_journal_',plotnum,'.txt'), open = 'a')
  # cat(paste0('Number of individuals in the initial plot : ',ninit,'/n'),file = journal, append = T)
  # cat(paste0('Number of individuals in the treated forest : ',nrow(realdata),'/n'),file = journal, append = T)
  # cat(paste0('Number of individuals over 10 dbh in the simulation : ',length(which(trollsim[,dbhsim]<10)),'/n'),file = journal, append = T)
  # cat(paste0('Number of individuals corresponding to initial indivs in the simulation : ',count_same,'/n'),file = journal, append = T)
  # cat(paste0('Number of new individuals on the same position in the simulation : ',count_not_same,'/n'),file = journal, append = T)
  # close(journal)
  return(realdata)
}

