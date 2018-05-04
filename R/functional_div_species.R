#' Troll Model Validation : functional diversity with Leinster-Cobbold Entropy
#' using resampling and "rolled under the armpits" Goodness of Fit statistics.
#'
#' @param data Data.frame ; An inventory which contains at least species name. 
#' @param trollsim Data.frame (yet not adapted to trollsim S4 objects)with species name
#' Diameters should correspon in terms of lower bound (generally 10cm) between 
#' field data and trollsim : pre-filtering is recommended to make consistent comparisons.
#' @param speciescol Character indicating the name of the species identity column
#' @param speciestraits Data.frame the contains species name and the 7 traits (species-specific mean values) 
#' @param LMA Character indicating the name of the LMA species means column
#' @param N Character indicating the name of the N species means column
#' @param P Character indicating the name of the P species means column
#' @param wsg Character indicating the name of the wsg species means column
#' @param ah Character indicating the name of the ah species means column
#' @param hmax Character indicating the name of the hmax species means column
#' @param dmax Character indicating the name of the dmax species means column
#' @param n_resample Integer ; the number of times you wish to resample to infer sampling effects.
#' @param path Character ; The path to the directory you want to save the graphical outputs into.
#' @param nameplot Character ; the name of your plot (used to generate outputs' filenames)
#'
#' @return A matrix of computed distances of the resampled Leinster-Cobbold Entropy profiles (LCEP)
#' and the original dataset's LCEP using two distance statistics : (is the first one alright ??)
#'     dist = sqrt(sum((LC_data - LC_sample)^2)/nrow(LC_data)) and chi =(sum((LC_sample-LC_data)^2)/LC_data)
#' @ @@@@@@ the last line contains the distances computed for the troll simulated dataset
#' @export
#'
#' @examples
fonctional_div_species <- function(data, trollsim, speciescol, speciestraits, LMA="LMA", 
                                   N="N",P="P", wsg="wsg", ah="ah", hmax="hmax", 
                                   dmax="dmax", n_resample = 10000, 
                                   path, nameplot){

# Prep n checks -----------------------------------------------------------

 #To be a bit refined because it is pretty dirty right now, but one's not supposed to
 #screw it because one should RTFM.
   if(!(N %in% names(speciestraits) & P %in% names(speciestraits) &
       LMA %in% names(speciestraits) & wsg %in% names(speciestraits) &
       ah %in% names(speciestraits) & hmax %in% names(speciestraits) & 
       dmax %in% names(speciestraits) & speciescol %in% names(speciestraits) &
       speciescol %in% names(data) & speciescol %in% names(trollsim))){
     stop("There is a problem with the names you indiated. Please, RTFM !")
   }
  data$species <- data[,which(names(data) == speciescol)]
  
  speciestraits$species <- data[,which(names(speciestraits) == speciescol)]
  
  # Species trait standardisation -------------------------------------------
  # i.e. from raw to centered-reduced distributions
  
  #should sd() be used here ? Because variance has two formula depending on if sample or not,
  # and we are in the case of a sample so I guess that yes, it should..
  
  speciestraits$N <- (speciestraits$N-mean(speciestraits$N))/sd(speciestraits$N)
  speciestraits$P <- (speciestraits$P-mean(speciestraits$P))/sd(speciestraits$P)
  speciestraits$LMA <- (speciestraits$LMA-mean(speciestraits$LMA))/sd(speciestraits$LMA)
  speciestraits$wsg <- (speciestraits$wsg-mean(speciestraits$wsg))/sd(speciestraits$wsg)
  speciestraits$ah <- (speciestraits$ah-mean(speciestraits$ah))/sd(speciestraits$ah)
  speciestraits$hmax <- (speciestraits$hmax-mean(speciestraits$hmax))/sd(speciestraits$hmax)
  speciestraits$dmax <- (speciestraits$dmax-mean(speciestraits$dmax))/sd(speciestraits$dmax)
  

# Frequencies computation for real dataset --------------------------------

  # Maybe is there a less annoying way to do that
  ab <- tapply(as.vector(data$species),as.vector(data$species),length)#length or nrow ?
  data_ab <- as.data.frame.array(ab)
  data_ab$sp = rownames(ab) ; rm(ab)
  data_ab$ab <- as.numeric(data_ab$ab)
  rownames(data_ab) <- NULL
  colnames(data_ab)[1] <- 'n'
  data_ab$freq <- data_ab$n/sum(data_ab$n)
  
  speciestraits$freq <- NA
  # A column containing the frequencies is directly added to the speciestraits tab for it is more convenient
  
  for(i in 1:speciestraits){
    if(speciestraits$species[i] %in% data_ab$species){
      speciestraits$freq[i] <- data_ab[which(data_ab$species == speciestraits$species[i]),"freq"]
    }
    else{
      speciestraits$freq[i] <- 0
    }
  }
  rm(data_ab)
# Distance matrix computation for all species -----------------------------


  distances <- matrix(NA, nrow = nrow(speciestraits), ncol = nrow(speciestraits))
  for(i in 1:nrow(speciestraits)){
    for(j in 1:nrow(speciestraits)){
      distances[i,j] = sqrt(sum((speciestraits[i,c("N","P","LMA","wsg","ah","hmax","dmax")] - speciestraits[j,c("N","P","LMA","wsg","ah","hmax","dmax")])^2))
    }
  }
  
  similarities <- distances # This is generally not true
  # I just needed to declare the variable outside the loop
  # Maybe I should stop doing c++ for mental health's sake 
  

# Leinster-Cobbold entropy profile for real dataset -----------------------


  LC_data <- matrix(NA, nrow = seq(from = 0, to = 15, by = 0.1) %>% length, ncol = 2)
  LC_data[,1] <- seq(from = 0, to = 15, by = 0.1)
  LC_data <- as.data.frame(LC_data) ; names(LC_data) <- c("u","Entropy")
  
  for(u in seq(from = 0, to = 15, by = 0.1)){
  similarities <- exp(-u*distances)
  speciestraits$originalities <- 1/(similarities %*% speciestraits$freq)
  LC_data[u, "Entropy"] <- sum(speciestraits$freq*log(speciestraits$originalities))
  # Leinster_Cobbold[u, "Hill"] should be done elsewhere I guess ? btw is it really necessary ?
  }


# Definition of the matrix containing Leinster-Cobbold for resampled ------

  LC_sample <- matrix(NA, nrow = seq(from = 0, to = 15, by = 0.1) %>% length, ncol = 1+n_resample)
  LC_sample[,1] <- seq(from = 0, to = 15, by = 0.1)
  LC_sample <- as.data.frame(LC_sample) ; names(LC_sample) <- c("u",as.character(1:n_resample))


# Resampling loop ---------------------------------------------------------

  sampletraits <- speciestraits

  for(i in 1:n_resample){
    
    # Obtain distributions from random draws according to the freq
    draw <- sample(x = data_ab$species, size = nrow(trollsim), replace = FALSE, prob = data_ab$freq)
    
    # Calculate abundances and frequencies
    ab <- tapply(as.vector(draw),as.vector(draw),length)#length or nrow ?
    draw_ab <- as.data.frame.array(ab)
    draw_ab$sp = rownames(ab) ; rm(ab)
    draw_ab$ab <- as.numeric(draw_ab$ab)
    rownames(draw_ab) <- NULL
    colnames(draw_ab)[1] <- 'n'
    draw_ab$freq <- draw_ab$n/sum(draw_ab$n)
  
    # Ordering
    for(j in 1:sampletraits){
      if(sampletraits$species[j] %in% draw_ab$species){
        sampletraits$freq[j] <- draw_ab[which(draw_ab$species == sampletraits$species[j]),"freq"]
      }
      else{
        sampletraits$freq[j] <- 0
      }
    }
    
    # Leinster-Cobbold Entropy computation for sample i
    for(u in seq(from = 0, to = 15, by = 0.1)){
      similarities <- exp(-u*distances)
      # Does it change the order ?
      sampletraits$originalities <- 1/(similarities %*% sampletraits$freq)
     
      LC_sample[u,i+1] <- sum(sampletraits$freq*log(sampletraits$originalities))
      # LC_sample[u, "Hill"]
    }
  }

  # Frequencies computation for trolsim dataset -----------------------------
  
  simtraits <- speciestraits
  
  ab <- tapply(as.vector(trollsim$species),as.vector(trollsim$species),length)#length or nrow ?
  trollsim_ab <- as.trollsim.frame.array(ab)
  trollsim_ab$sp = rownames(ab) ; rm(ab)
  trollsim_ab$ab <- as.numeric(trollsim_ab$ab)
  rownames(trollsim_ab) <- NULL
  colnames(trollsim_ab)[1] <- 'n'
  trollsim_ab$freq <- trollsim_ab$n/sum(trollsim_ab$n)
  simtraits$freq <- NA
  
  for(i in 1:simtraits){
    if(simtraits$species[i] %in% data_ab$species){
      simtraits$freq[i] <- data_ab[which(data_ab$species == simtraits$species[i]),"freq"]
    }
    else{
      simtraits$freq[i] <- 0
    }
  }
  
  # Leinster-Cobbold entropy profile for trollsim dataset --------------------
  
  
  LC_troll <- matrix(NA, nrow = seq(from = 0, to = 15, by = 0.1) %>% length, ncol = 2)
  LC_trol[,1] <- seq(from = 0, to = 15, by = 0.1)
  LC_troll <- as.data.frame(LC_troll) ; names(LC_troll) <- c("u","Entropy")
  
  for(u in seq(from = 0, to = 15, by = 0.1)){
    similarities <- exp(-u*distances)
    simtraits$originalities <- 1(similarities %*% simtraits$freq)
    LC_troll[u, "Entropy"] <- sum(simtraits$freq*log(simtraits$originalities))
  
  }
  
  # Graphical diagnostics 1 -------------------------------------------------
  
  jpeg(file.path(path, paste0("plot_profiles_LC_",nameplot, ".jpg")))
  # How to make a graph that could be like : the real community profile in one color
  # and the resampled profiles confidence intervals (say 95% or 90) in sligthly transparent same color,
  # plus the trolsim profile ?
  # Moreover the armpits-rolled p-values could be added to look like a pro's graph !
  dev.off()
  
  # Goodness of fit computation for the profile -----------------------------
  
  distances = matrix(NA, nrow = n_resample, ncol = 2)
  distances = as.data.frame(distances)
  names(distances) = c("dist","chid") # We will use a homemade distance and the khi2 distance
  
  for(i in 1:n_resample){
    distances$dist[i] = sqrt(sum((LC_data[,2] - LC_sample[,i+1])^2)/nrow(LC_data))
    distances$chi[i] <- (sum((LC_sample[,i+1]-LC_data[,2])^2)/LC_data[,2])
  }
    distroll <- sqrt(sum((LC_data[,2] - LC_troll[,2])^2)/nrow(LC_data))
    chitroll <- (sum((LC_troll[,2]-LC_data[,2])^2)/LC_data[,2])
  
  # Graphical diagnostics 2 -------------------------------------------------
  
  jpeg(file.path(path, paste0("plot_density_dist_LC_",nameplot, ".jpg")))
  plot(density(distances$dist)); abline(v = distroll)
  dev.off()
# This must be ggplotised because it is certainly ugly++
  jpeg(file.path(path, paste0("plot_density_chi_LC_",nameplot, ".jpg")))
  plot(density(distances$chid)); abline(v = chitroll)
  dev.off()
  
  
  # Approximated pval of quantile computation -------------------------------

  #Well I don't know how to call it, but I guess p-value is clearly abusive
  distances$dist = sort(distances$dist, decreasing = F); distances$chid = sort(distances$chid, decreasing = F)

  for(i in 1:nrow(distances)){
    if(distances$dist[i]>distroll){ # (i-0.5) is because dist[i-1] < distroll < dist[i] but maybe interpolation would be + correct
      cat("iteration ", i, "the approximated p-value is ", (i-0.5)/nrow(dist), "\n")
      if(0.1<i/nrow(distances)<0.9) cat("distributions are not significantly different \n")
      else cat("distributions are significantly different")

      break
    }
  }

  for(i in 1:nrow(distances)){
    if(distances$chid[i]>chitroll){
      cat("iteration ", i, "the approximated p-value is ", (i-0.5)/nrow(dist), "\n")
      if(0.1<i/nrow(distances)<0.9) cat("distributions are not significantly different, which for once is a good news \n")
      else cat("distributions are significantly different, bad news...")
      break
    }
  }

  print("Distance matrix is returned with trollsim distances in the last line")
  return(rbind(distances, c(distroll,chitroll)))
    
}
