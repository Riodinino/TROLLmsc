#' #' Title
#' #'
#' #' @param realdata
#' #' @param trollsim
#' #' @param speciescol
#' #' @param dbhcol
#' #' @param path
#' #' @param nameplot
#' #' @param n_resample
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' comparegof <- function(realdata, trollsim, speciescol, dbhcol,path,nameplot,n_resample = 10000){
#'
#'
#' # Names handling and checks -----------------------------------------------
#'
#' if(!speciescol %in% names(realdata) & !speciescol %in% names(trollsim))
#'   stop("Species column names are wrong or not the same among datasets")
#'   if(!dbhcol %in% names(realdata) & !dbhcol %in% names(trollsim))
#'     stop("Diameters column names are wrong or not the same among datasets")
#'
#'   realdata[,which(names(realdata) == speciescol)] <- "species"
#'   trollsim[,which(names(trollsim) == speciescol)] <- "species"
#'
#'   realdata[,which(names(realdata) == dbhcol)] <- "dbh"
#'   trollsim[,which(names(trollsim) == dbhcol)] <- "dbh"
#'
#'
#' # Real dataset abundances and frequencies table ---------------------------
#'
#'   ab <- tapply(as.vector(realdata$species),as.vector(realdata$species),length)#length or nrow ?
#'   data_ab <- as.data.frame.array(ab)
#'   data_ab$sp = rownames(ab) ; rm(ab)
#'   data_ab$ab <- as.numeric(data_ab$ab)
#'   rownames(data_ab) <- NULL
#'   colnames(data_ab)[1] <- 'n'
#'   data_ab$freq <- data_ab$n/sum(data_ab$n)
#'   data_ab <- data_ab %>% arrange(freq)
#'
#'
#' # Matrix conception : distances of multinomial draws from real data -------
#'
#'   distances = matrix(NA, nrow = n_resample, ncol = 2)
#'   distances = as.data.frame(distances)
#'   names(distances) = c("dist","chid") # We will use a homemade distance and the khi2 distance
#'
#' # Resampling loop ---------------------------------------------------------
#'
#'   for(i in 1:n_resample){
#'
#'     # Obtain distributions from random draws according to the probabilities
#'     draw <- sample(x = data_ab$species, size = nrow(trollsim), replace = FALSE, prob = data_ab$freq)
#'     # Calculate abundances and frequencies
#'     ab <- tapply(as.vector(draw),as.vector(draw),length)#length or nrow ?
#'     draw_ab <- as.data.frame.array(ab)
#'     draw_ab$sp = rownames(ab) ; rm(ab)
#'     draw_ab$ab <- as.numeric(draw_ab$ab)
#'     rownames(draw_ab) <- NULL
#'     colnames(draw_ab)[1] <- 'n'
#'     draw_ab$freq <- draw_ab$n/sum(draw_ab$n)
#'
#'     ord_draw_ab = draw_ab[c(1,2),]
#'
#'     for(i in 1:nrow(data_ab)){
#'       if(data_ab$species[i]%in%draw_ab$species){
#'         rbind(ord_draw_ab, draw_ab[which(draw_ab$species == data_ab$species[i]),])
#'       }
#'       else{
#'         line_temp <- data_ab[which(data$species == data_ab$species[i]),]
#'         line_temp$n <- 0; line_temp$freq <- 0
#'         rbind(ord_draw_ab,line_temp); rm(line_temp)
#'       }
#'     }
#'     ord_draw_ab = ord_draw_ab[c(-1,-2),]
#'     rm(draw_ab)
#'
#'     # distemp = sqrt(sum((data_ab$freq - ord_draw_ab$freq)^2)/nrow(data_ab$freg))
#'     distemp = sqrt(sum((data_ab$freq - ord_draw_ab$freq)^2)/nrow(data_ab))
#'     chi <- (sum((ord_draw_ab$freq-data_ab$freq)^2)/data_ab$freq)
#'     distances$dist[i] <- distemp
#'     distances$chid[i] <- chi
#'   }
#'   rm(distemp)
#'   rm(chi)
#'
#'
#' # Trollsim : same procedure -----------------------------------------------
#'
#'   ab <- tapply(as.vector(trollsim$species),as.vector(trollsim$species),length)#length or nrow ?
#'   trollsim_ab <- as.data.frame.array(ab)
#'   trollsim_ab$sp = rownames(ab) ; rm(ab)
#'   trollsim_ab$ab <- as.numeric(trollsim_ab$ab)
#'   rownames(trollsim_ab) <- NULL
#'   colnames(trollsim_ab)[1] <- 'n'
#'   trollsim_ab$freq <- trollsim_ab$n/sum(trollsim_ab$n)
#'
#'   ord_trollsim_ab = trollsim_ab[c(1,2),]
#'
#'   for(i in 1:nrow(data_ab)){
#'     if(data_ab$species[i]%in% trollsim$species){
#'       rbind(ord_trollsim_ab, trollsim_ab[which(trollsim_ab$species == data_ab$species[i]),])
#'     }
#'     else{
#'       line_temp <- data_ab[which(data$species == data_ab$species[i]),]
#'       line_temp$n <- 0; line_temp$freq <- 0
#'       rbind(ord_trollsim_ab,line_temp) #rm(line_temp)
#'     }
#'   }
#'   ord_trollsim_ab = ord_trollsim_ab[c(-1,-2),]
#'   rm(trollsim_ab)
#'
#'   distroll = sqrt(sum((data_ab$freq - ord_troll_ab$freq)^2)/length(data_ab$freg))
#'   chitroll <- (sum((ord_troll_ab$freq-data_ab$freq)^2)/data_ab$freq)
#'
#'
#' # Graphical diagnostics ---------------------------------------------------
#'
#'   jpeg(file.path(path, paste0("plot_density_dist",nameplot, ".jpg")))
#'   hist(density(distances$dist)); abline(v = distroll)
#'   dev.off()
#'   distances$dist = sort(distances$dist, decreasing = F); distances$chid = sort(distances$chid, decreasing = F)
#'
#'   jpeg(file.path(path, paste0("plot_density_chi",nameplot, ".jpg")))
#'   hist(density(distances$chid)); abline(v = chitroll)
#'   dev.off()
#'
#'
#' # Approximated pval of quantile computation -------------------------------
#'
#'   distances$dist = sort(distances$dist, decreasing = F); distances$chid = sort(distances$chid, decreasing = F)
#'
#'   for(i in 1:nrow(distances)){
#'     if(distances$dist[i]>distroll){
#'       cat("iteration ", i, "the approximated p-value is ", (i-0.5)/nrow(dist), "\n")
#'       if(0.1<i/nrow(distances)<0.9) cat("distributions are not significantly different \n")
#'       else cat("distributions are significantly different")
#'
#'       break
#'     }
#'   }
#'
#'   for(i in 1:nrow(distances)){
#'     if(distances$chid[i]>chitroll){
#'       cat("iteration ", i, "the approximated p-value is ", (i-0.5)/nrow(dist), "\n")
#'       if(0.1<i/nrow(distances)<0.9) cat("distributions are not significantly different \n")
#'       else cat("distributions are significantly different")
#'       break
#'     }
#'   }
#'
#'   print("Distance matrix is returned with trollsim distances in the last line")
#'   return(rbind(distances, c(distroll,chitroll)))
#'
#' }
