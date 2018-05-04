#' @import tidyverse
library(tidyverse)

#' Title
#'
#' @param data A one-plot, one-census dataset you want to process
#' @param X Character. The name of the column containing plot-scale \code{X} coordinates.
#' Coordinates must be in the plot's referential, not UTM or others.
#' @param Y Character. The name of the column containing plot-scale Y coordinates.
#' Coordinates must be in the plot's referential, not UTM or others.
#' @param dbh Character. The name of the column containing dbh measurements
#' @param ID Character. The name of the column containing unique individual IDs
#' @param dmax Maximum distance you wannt to move a tree from its conflicting
#' position. If tree cannot be replace within that range, the biggest dbh are kept
#' and the smallest, removed.
#'
#' @return Function returns a new inventory with modified coordinates,
#' where there is one individual per cell.
#' @export
#'
#' @examples

# replacement <- function(data, X = "X", Y = "Y", dbh ="dbh", ID = "i_arbre", dmax = 3)
replacement <- function(data, X, Y, dbh, ID, dmax = 5)
  {

  # Variable preparation for plyr syntaxis ----------------------------------

  X <- enquo(X)
  Y <- enquo(Y)
  dbh <- enquo(dbh)
  ID <- enquo(ID)

  # Definition of conflicts matrix ------------------------------------------

  count_conflicts <- data %>%
    select(!!X, !!Y) %>%
    group_by(!!X, !!Y) %>%
    summarise(n = n()) %>%
    #as.numeric %>%
    #Is this line to be removed ? it can avoid to do useless tests.
    filter(n > 1)
  names(count_conflicts) <- c("X","Y","n")

  # Core loop for each conflicting cell ---------------------------

  for (i in 1:nrow(count_conflicts))
  {
    cat("iteration ", i, "sur ", nrow(count_conflicts),  "\n")
    # Declaring new coordinates matrix ----------------------------------------


    # First line will be NA and is removed after the loop.

    newcoord <- matrix(ncol = 3) %>% as.data.frame
    names(newcoord) <- c("X","Y","d")


    # Scope loop for one conflicting cell -------------------------------------

    # Scoping until there are enough free cells for the number  of trees we want to replace
    d = 1
    while((newcoord %>% filter(!is.na(X)) %>% filter(!is.na(Y)) %>% filter(!is.na(d)) %>% nrow()) < (count_conflicts$n[i]-1) && d < dmax)
    {

      # Creating the scope matrix : coordinates, availability, distance ---------
      x <- count_conflicts$X[i]
      y <- count_conflicts$Y[i]

      Scope <- matrix(NA, ncol = 4, nrow = 8*d)
      Scope <- as.data.frame(Scope)
      colnames(Scope) <- c("X","Y","Available","d")
      ### Scope coordinates (a square crown of distance d to the focus conflict cell.)
      Scope$X <- c(rep(x-d, 2*d+1), ((x-d+1):(x+d-1)), rep(x+d,2*d+1), sort((x-d+1):(x+d-1), decreasing = T))
      Scope$Y <- c(sort((y-d):(y+d), decreasing = T), rep(y-d,2*d-1),(y-d):(y+d), rep(y+d,2*d-1))
      ### Scope distance is stored to move less bigger dbhs when several trees have to be replaced
      Scope$d <- rep(d, 8*d)


      # Filling the Scope matrix -------------------------------------------------

      for(j in 1:nrow(Scope))
      {
        # Safety check : avoiding edge effects by declaring coordinates un --------

        if(!(Scope$X[j] >  min(data %>% select(!!X)) &  Scope$X[j] < max(data %>% select(!!X))))
        {
          Scope$Available[j] <- FALSE
        }
        else if(!(Scope$Y[j] >  min(data %>% select(!!Y)) &  Scope$Y[j] < max(data %>% select(!!Y))))
        {
          Scope$Available[j] <- FALSE
        }
        else
        {
          row_x <- which(data[,as.character(X)[2]] == Scope$X[j])
          row_xy <- which(data[row_x,as.character(Y)[2]]== Scope$Y[j])

          if(length(row_xy) > 0) {Scope$Available[j] <- FALSE}
          else{Scope$Available[j] <- TRUE}
        }
        # Availability of the cells -----------------------------------------------
        ## TO DEBUG
      }



      # Test for sufficiency of the number of available cells in the scope ------


      # print(newcoord)
      #If enough, end while-loop

        newcoord <- rbind(newcoord, Scope %>% filter(Available == T) %>% select(X,Y,d))


      # count_conflicts$n[i] <- count_conflicts$n[i] - nrow(newcoord %>% filter(!is.na(newcoord$X) & !is.na(newcoord$Y)))
      d <- d+1
    }
    # print(newcoord)

    newcoord <- newcoord %>%
      filter(!is.na(X)) %>%
      filter(!is.na(Y)) %>%
      filter(!is.na(d))

    #   filter(!is.na(X)) %>% filter(!is.na(Y)) %>% filter(!is.na(d)) %>%
    #   sample_n(size = (count_conflicts$n[i])-1, replace = FALSE)

    # print("conflict number");print((count_conflicts$n[i])-1)
    # debugueuh
    # print("Nombre de lignes de newcoord")
    # print(newcoord)

    focus <- data %>%
      select(!!X,!!Y,!!dbh,!!ID) %>%
      filter(!!X == count_conflicts$X[i]) %>%
      filter(!!Y == count_conflicts$Y[i]) %>%
      rename(X = !!X) %>%
      rename(Y = !!Y) %>%
      rename(ID = !!ID)
    # temp <-temp %>% filter(!is.na(!!dbh) & !is.na(!!X) & !is.na(!!Y) & !is.na(!!ID))
    # Error in `$<-.data.frame`(`*tmp*`, "X", value = 66.5) :
      # le tableau de remplacement a 1 lignes, le tableau remplacé en a 0
    #The one with the biggest dbh will stay there for being to heavy lol
   ngros <- focus %>% filter(!!dbh == max(focus %>% select(!!dbh))) %>% nrow()
    if(ngros == 1) {focus <- focus %>% filter(!!dbh != max(focus %>% select(!!dbh))) %>%
      arrange(desc(!!dbh))}
    else
      {
      spl <- focus %>% filter(!!dbh == max(focus %>% select(!!dbh))) %>% sample_n(size = ngros-1, replace = FALSE)
      focus <- focus %>% filter(!!dbh != max(focus %>% select(!!dbh))) %>% rbind(spl) %>%
      arrange(desc(!!dbh))

      }



    # Replacement -------------------------------------------------------------
    # print(data[,which(names(data)== ID)])

      ## If the new coordinates are sufficiently numerous, then replace all trees..
   IDcol_data <- which(names(data) == as.character(ID)[2])
      if(nrow(newcoord) >= (count_conflicts$n[i]-1))
      {
        newcoord <- newcoord %>%
          sample_n(size = (count_conflicts$n[i])-1, replace = FALSE)

        focus$X <- newcoord$X
        focus$Y <- newcoord$Y

       for(k in 1:nrow(focus))
        {
         # to fix#

          row_k <- which(data[,IDcol_data] == focus$ID[k])
          data[row_k, as.character(X)[2]] <- focus$X[k]
          data[row_k, as.character(Y)[2]] <- focus$Y[k]

          # data[which(data[,which(names(data) == quo(ID))] == focus$ID[k]), quo(X)] <- focus$X[k]
          # data[which(data[,which(names(data) == quo(ID))] == focus$ID[k]), quo(Y)] <- focus$Y[k]
        }
      }

      else
      {
        if(nrow(newcoord)>0)
        {
          newcoord <- newcoord %>%
            sample_n(size = nrow(newcoord), replace = FALSE)
          tosuppr <- focus[(nrow(newcoord)+1):nrow(focus),]
          focus <- focus[1:nrow(newcoord),]

          focus$X <- newcoord$X
          focus$Y <- newcoord$Y

          for(k in 1:nrow(focus))
          {

            row_k <- which(data[,IDcol_data] == focus$ID[k])
            data[row_k, as.character(X)[2]] <- focus$X[k]
            data[row_k, as.character(Y)[2]] <- focus$Y[k]

          }

          for(s in 1:nrow(tosuppr))
          {
            row_s <- which(data[,IDcol_data] == tosuppr$ID[s])
            data <- data[-row_s,]
          }

        }
        else
        {
          for(s in 1:nrow(focus))
          {
            row_t <- which(data[,IDcol_data] == focus$ID[s])
            data <- data[-row_t,]
          }
        }

      }


    # would we have to movethe bigger dbhs lesser than small dbhs in case the new coordinates differ in terms of distance to the old ones ?

  }
  # Return ! ----------------------------------------------------------------
  return(data)
}



# # Test
# dat %>%
#   select(X, Y, dbh, i_arbre) %>%
#   rename(Xcol = X, Ycol = Y, dbhcol = dbh) %>%
#   replacement(Xcol, Ycol, dbhcol, i_arbre)

# # Dirty "Biggest dbh" keeping
#
# if(duplicated_method == "Dirty")
# {
#   for (i in 1:nrow(forest))
#   {
#     if(forest$suppr[i] != T)
#     {
#       for (j in 1:nrow(forest))
#       {
#         if(forest[,which(names(forest)== X)][i] == forest[,which(names(forest)== X)][j] &&
#            forest[,which(names(forest)== Y)][i] == forest[,which(names(forest)== Y)][j])
#         {
#           if(forest$suppr[j] != T && j!= i)
#           {
#             if(forest[,which(names(forest)== dbh)][i] > forest[,which(names(forest)== dbh)][j])
#             {
#               forest$suppr[j] <- T
#             }
#             else if(forest[,which(names(forest)== dbh)][j] > forest[,which(names(forest)== dbh)][i])
#             {
#               forest$suppr[i] <- T
#             }
#             else
#             {
#               forest$suppr[sample(x = c(i,j), 1)] <- T
#             }
#           }
#         }
#       }
#     }
#   }
# }
#
# else if(duplicated_method == "Strange")
# {
#   forest <- replacement(forest)
# }





