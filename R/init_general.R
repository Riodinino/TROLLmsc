#' General and Species Parameters Input-File Maker for TROLL Forest Simulaor
#'
#' @param macros Character. MUST BE "species-specific", "intraspecific-only" or "intraspecific-covar".
#' Describes the macros that you use for this simulation, so that the function adapts the input file to avoid crashing TROLL.
#' @param path
#' @param input
#' @param climate
#' @param overwrite
#' @param nbcol
#' @param nbrows
#' @param nbiter
#' @param iter
#' @param NV
#' @param NH
#' @param nbout
#' @param numesp
#' @param p
#' @param klight
#' @param phi
#' @param g1
#' @param vC
#' @param DBH0
#' @param H0
#' @param ra0
#' @param ra1
#' @param de0
#' @param de1
#' @param dens
#' @param fbranchstem
#' @param fcanopy
#' @param seedrain
#' @param nbseeds
#' @param sigma_height
#' @param sigma_CR
#' @param sigma_CD
#' @param sigma_P
#' @param sigma_N
#' @param sigma_LMA
#' @param sigma_wsg
#' @param sigma_dmax
#' @param corr_CR_height
#' @param corr_N_P
#' @param corr_N_LMA
#' @param corr_P_LMA
#' @param leafdem_resolution
#' @param p_tfsecondary
#' @param hurt_decay
#' @param mindeathrate
#' @param m1
#' @param CO2
#' @param species
#'
#' @return
#' @export
#'
#' @examples
init_general <- function (
                      macros = "species_specific",#New
                      path = getOption("RconTroll.path"),
                      input = getOption("RconTroll.init"),
                      overwrite = TRUE,
                      nbcol = 400,
                      nbrows = 400,
                      nbiter = 12,
                      iter = 12,
                      NV = 1,
                      NH = 1,
                      nbout = 4,
                      numesp = 163,
                      p = 0.05,
                      klight = 0.9,
                      phi = 0.06,
                      g1 = 3.77,
                      vC = 0.1,
                      DBH0 = 0.01,
                      H0 = 1,
                      ra0 = 0.5,
                      ra1 = 10,
                      de0 = 0.3,
                      de1 = 0.12,
                      dens = 0.8,
                      fbranchstem = 0.35,
                      fcanopy = 0.3,
                      seedrain = 5000,
                      nbseeds = 10,
                      sigma_height = 0.05, #Add
                      sigma_CR = 0.05,#Add
                      sigma_CD = 0.05,#Add
                      sigma_P = 0.05,#Add
                      sigma_N = 0.05,#Add
                      sigma_LMA = 0.05,#Add
                      sigma_wsg = 0.05,#Add
                      sigma_dmax = 0.05,#Add
                      corr_CR_height = 0.75,#Add
                      corr_N_P = 0.65,#Add
                      corr_N_LMA = -0.43,#Add
                      corr_P_LMA = -0.39,#Add
                      leafdem_resolution = 30, #Add
                      p_tfsecondary = 0.0, #Add
                      hurt_decay = 0.0, #Add
                      mindeathrate = 0.035,
                      m1 = 0.035,
                      CO2 = 360,
                      species = read.table(getOption("RconTroll.species"), header = TRUE, dec = ".", sep = ""),
                      )
{

# Safety checks -----------------------------------------------------------

  if (!overwrite){
    if (input %in% list.files(path)) # Check for species/general input
      stop("The input file already exists, use overwrite = T.")
  }
  if(macros != "species_specific" & macros != "intraspecific_only" & macros != "intraspecific_covariance"){
    stop(
      paste(
        "The macros you indicated is not part of the accepted propositions : ",
        "It must be one of these 3 :",
        "- species_specific",
        "- intraspecific_only",
        "- intraspecific_covariance",
        "Please pick the one corresponding to your simulation options (macros)",
        sep = "\n")
      )
  }


# Open file connection ----------------------------------------------------

  fileConn <- file(file.path(path, input))

# Species-specific input file ---------------------------------------------
if(macros = "species_specific"){
  writeLines(c("#############################################\t",
               "###  Parameter file for the TROLL program ###\t",
               "#############################################\t",
               "###\tGENERAL PARAMETERS",
               paste(nbcol, "/* cols # nb of columns */", sep = "\t"),
               paste(nbrows, "/* rows # nb of rows  */", sep = "\t"),
               paste(nbiter, "/* nbiter # total nb of timesteps */", sep = "\t"),
               paste(iter, "/* number of iteration per year */", sep = "\t"),
               paste(NV, "/* NV # vertical nb of cells (nb per m) */", sep = "\t"),
               paste(NH, "/* NH # horizontal nb of cells (nb per m) */", sep = "\t"),
               paste(nbout, "/* nbout # Number of outputs */ ", sep = "\t"),
               paste(numesp, "/* numesp # Number of species */", sep = "\t"),
               paste(p, "/* p # light incidence param (diff through turbid medium) */", sep = "\t"),
               "\n",
               "###\tCharacters shared by species\t\t\t\t\t\t\t\t\t\t\t",
               paste(klight, "/* klight # light attenuation in the canopy Beer-Lambert */", sep = "\t"),
               paste(phi, "/* phi # quantum yield (in micromol C/micromol photon) */", sep = "\t"),
               paste(g1, "/* parameter g1 of Medlyn et al stomatal conductance model */", sep = "\t"),
               paste(vC, "/* vC  # variance of the flexion moment */", sep = "\t"),
               paste(DBH0, "/* DBH0 # initial dbh (m) */", sep = "\t"),
               paste(H0, "/* H0 # initial height (m) */", sep = "\t"),
               paste(ra0, "/* ra0 # initial crown radius (in m) */", sep = "\t"),
               paste(ra1, "/* ra1 # crown radius - dbh slope */", sep = "\t"),
               paste(de0, "/* de0 # initial crown depth(in m) */", sep = "\t"),
               paste(de1, "/* de1 # Crown_Depth/height slope (m/m) */", sep = "\t"),
               paste(dens, "/* dens # leaf density (m^2/m^2) */", sep = "\t"),
               paste(fbranchstem, "/* fraction of biomass allocated to above ground wood (branches+stem) */", sep = "\t"),
               paste(format(fcanopy, nsmall = 2),"/* fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) */\t\t\t\t\t\t\t\t\t\t\t", sep = "\t"),
               paste(seedrain, "/* constant used to scale total seed rain per hectare across species (in next computation) */", sep = "\t"),
               paste(nbseeds, "/* nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined */", sep = "\t"),
               paste(leafdem_resolution, "/* leafdem_resolution # resolution of leaf demography model */", sep = "\t"),
               paste(p_tfsecondary, "/* p_tfsecondary # probability of secondary treefall */", sep = "\t"),
               paste(hurt_decay, "	/* hurt_decay # parameter determining how tree damages are repaired */", sep = "\t"),
               paste(mindeathrate, "/* minimal death rate", sep = "\t"),
               paste(m1, "/* m1 (slope of death rate)",sep = "\t"),
               paste(CO2, "/* atmospheric CO2 concentration in micromol/mol */",sep = "\t"),
               "\t\t\t\t\t\n###\tSpecies description\t\t\t\t\t\t\t\t\t\t\t"),fileConn)
}

# Intraspecific-only file -------------------------------------------------
else if(macro = "intraspecific_only"){
  writeLines(c("#############################################\t",
               "###  Parameter file for the TROLL program ###\t",
               "#############################################\t",
               "###\tGENERAL PARAMETERS",
               paste(nbcol, "/* cols # nb of columns */", sep = "\t"),
               paste(nbrows, "/* rows # nb of rows  */", sep = "\t"),
               paste(nbiter, "/* nbiter # total nb of timesteps */", sep = "\t"),
               paste(iter, "/* number of iteration per year */", sep = "\t"),
               paste(NV, "/* NV # vertical nb of cells (nb per m) */", sep = "\t"),
               paste(NH, "/* NH # horizontal nb of cells (nb per m) */", sep = "\t"),
               paste(nbout, "/* nbout # Number of outputs */ ", sep = "\t"),
               paste(numesp, "/* numesp # Number of species */", sep = "\t"),
               paste(p, "/* p # light incidence param (diff through turbid medium) */", sep = "\t"),
               "\n",
               "###\tCharacters shared by species\t\t\t\t\t\t\t\t\t\t\t",
               paste(klight, "/* klight # light attenuation in the canopy Beer-Lambert */", sep = "\t"),
               paste(phi, "/* phi # quantum yield (in micromol C/micromol photon) */", sep = "\t"),
               paste(g1, "/* parameter g1 of Medlyn et al stomatal conductance model */", sep = "\t"),
               paste(vC, "/* vC  # variance of the flexion moment */", sep = "\t"),
               paste(DBH0, "/* DBH0 # initial dbh (m) */", sep = "\t"),
               paste(H0, "/* H0 # initial height (m) */", sep = "\t"),
               paste(ra0, "/* ra0 # initial crown radius (in m) */", sep = "\t"),
               paste(ra1, "/* ra1 # crown radius - dbh slope */", sep = "\t"),
               paste(de0, "/* de0 # initial crown depth(in m) */", sep = "\t"),
               paste(de1, "/* de1 # Crown_Depth/height slope (m/m) */", sep = "\t"),
               paste(dens, "/* dens # leaf density (m^2/m^2) */", sep = "\t"),
               paste(fbranchstem, "/* fraction of biomass allocated to above ground wood (branches+stem) */", sep = "\t"),
               paste(format(fcanopy, nsmall = 2),"/* fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) */\t\t\t\t\t\t\t\t\t\t\t", sep = "\t"),
               paste(seedrain, "/* constant used to scale total seed rain per hectare across species (in next computation) */", sep = "\t"),
               paste(nbseeds, "/* nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined */", sep = "\t"),
               paste(sigma_height, "/* sigma_height # intraspecific variation in tree height (lognormal) */", sep = "\t"),
               paste(sigma_CR, "/* sigma_CR # intraspecific variation in crown radius (lognormal) */", sep = "\t"),
               paste(sigma_CD, "/* sigma_CD # intraspecific variation in crown depth (lognormal) */", sep = "\t"),
               paste(sigma_P, "/* sigma_P # intraspecific variation in leaf phosphorus (lognormal) */", sep = "\t"),
               paste(sigma_N, "/* sigma_N # intraspecific variation in leaf nitrogen (lognormal) */", sep = "\t"),
               paste(sigma_LMA, "/* sigma_LMA # intraspecific variation in LMA (lognormal) */", sep = "\t"),
               paste(sigma_wsg, "/* sigma_wsg # intraspecific variation in wood specific gravity */", sep = "\t"),
               paste(sigma_dmax, "/* sigma_dmax # intraspecific variation in maximum diameter */", sep = "\t"),
               paste(leafdem_resolution, "/* leafdem_resolution # resolution of leaf demography model */", sep = "\t"),
               paste(p_tfsecondary, "/* p_tfsecondary # probability of secondary treefall */", sep = "\t"),
               paste(hurt_decay, "/* hurt_decay # parameter determining how tree damages are repaired */", sep = "\t"),
               paste(mindeathrate, "/* minimal death rate", sep = "\t"),
               paste(m1, "/* m1 (slope of death rate)",sep = "\t"),
               paste(CO2, "/* atmospheric CO2 concentration in micromol/mol */",sep = "\t"),
               "\t\t\t\t\t\n###\tSpecies description\t\t\t\t\t\t\t\t\t\t\t"),fileConn)
}

# Intraspecific covariance file -------------------------------------------
else if (macros = "intraspecific_covariance"){
  writeLines(c("#############################################\t",
               "###  Parameter file for the TROLL program ###\t",
               "#############################################\t",
               "###\tGENERAL PARAMETERS",
               paste(nbcol, "/* cols # nb of columns */", sep = "\t"),
               paste(nbrows, "/* rows # nb of rows  */", sep = "\t"),
               paste(nbiter, "/* nbiter # total nb of timesteps */", sep = "\t"),
               paste(iter, "/* number of iteration per year */", sep = "\t"),
               paste(NV, "/* NV # vertical nb of cells (nb per m) */", sep = "\t"),
               paste(NH, "/* NH # horizontal nb of cells (nb per m) */", sep = "\t"),
               paste(nbout, "/* nbout # Number of outputs */ ", sep = "\t"),
               paste(numesp, "/* numesp # Number of species */", sep = "\t"),
               paste(p, "/* p # light incidence param (diff through turbid medium) */", sep = "\t"),
               "\n",
               "###\tCharacters shared by species\t\t\t\t\t\t\t\t\t\t\t",
               paste(klight, "/* klight # light attenuation in the canopy Beer-Lambert */", sep = "\t"),
               paste(phi, "/* phi # quantum yield (in micromol C/micromol photon) */", sep = "\t"),
               paste(g1, "/* parameter g1 of Medlyn et al stomatal conductance model */", sep = "\t"),
               paste(vC, "/* vC  # variance of the flexion moment */", sep = "\t"),
               paste(DBH0, "/* DBH0 # initial dbh (m) */", sep = "\t"),
               paste(H0, "/* H0 # initial height (m) */", sep = "\t"),
               paste(ra0, "/* ra0 # initial crown radius (in m) */", sep = "\t"),
               paste(ra1, "/* ra1 # crown radius - dbh slope */", sep = "\t"),
               paste(de0, "/* de0 # initial crown depth(in m) */", sep = "\t"),
               paste(de1, "/* de1 # Crown_Depth/height slope (m/m) */", sep = "\t"),
               paste(dens, "/* dens # leaf density (m^2/m^2) */", sep = "\t"),
               paste(fbranchstem, "/* fraction of biomass allocated to above ground wood (branches+stem) */", sep = "\t"),
               paste(format(fcanopy, nsmall = 2),"/* fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) */\t\t\t\t\t\t\t\t\t\t\t", sep = "\t"),
               paste(seedrain, "/* constant used to scale total seed rain per hectare across species (in next computation) */", sep = "\t"),
               paste(nbseeds, "/* nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined */", sep = "\t"),
               paste(nbseeds, "/* sigma_height # intraspecific variation in tree height (lognormal) */", sep = "\t"),
               paste(sigma_height, "/* sigma_height # intraspecific variation in tree height (lognormal) */", sep = "\t"),
               paste(sigma_CR, "/* sigma_CR # intraspecific variation in crown radius (lognormal) */", sep = "\t"),
               paste(sigma_CD, "/* sigma_CD # intraspecific variation in crown depth (lognormal) */", sep = "\t"),
               paste(sigma_P, "/* sigma_P # intraspecific variation in leaf phosphorus (lognormal) */", sep = "\t"),
               paste(sigma_N, "/* sigma_N # intraspecific variation in leaf nitrogen (lognormal) */", sep = "\t"),
               paste(sigma_LMA, "/* sigma_LMA # intraspecific variation in LMA (lognormal) */", sep = "\t"),
               paste(sigma_wsg, "/* sigma_wsg # intraspecific variation in wood specific gravity */", sep = "\t"),
               paste(sigma_dmax, "/* sigma_dmax # intraspecific variation in maximum diameter */", sep = "\t"),
               paste(corr_CR_height, "/* corr_CR_height # correlation coefficient between crown radius and tree height */", sep = "\t"),
               paste(corr_N_P, "/* corr_N_P # correlation coefficient between leaf nitrogen and leaf phosphorus */", sep = "\t"),
               paste(corr_N_LMA, "/* corr_N_LMA # correlation coefficient between leaf nitrogen and LMA */", sep = "\t"),
               paste(corr_P_LMA, "/* corr_P_LMA # correlation coefficient between leaf phosphorus and LMA */", sep = "\t"),
               paste(leafdem_resolution, "/* leafdem_resolution # resolution of leaf demography model */", sep = "\t"),
               paste(p_tfsecondary, "/* p_tfsecondary # probability of secondary treefall */", sep = "\t"),
               paste(hurt_decay, "/* hurt_decay # parameter determining how tree damages are repaired */", sep = "\t"),
               paste(mindeathrate, "/* minimal death rate", sep = "\t"),
               paste(m1, "/* m1 (slope of death rate)",sep = "\t"),
               paste(CO2, "/* atmospheric CO2 concentration in micromol/mol */",sep = "\t"),
               "\t\t\t\t\t\n###\tSpecies description\t\t\t\t\t\t\t\t\t\t\t"),fileConn)
  }


# Close file connection ---------------------------------------------------

  close(fileConn)


# Add species description -------------------------------------------------


  names(species)[1] <- "****"

  suppressWarnings(write.table(species, file.path(path, input),sep = "\t", append = TRUE, row.names = FALSE, quote = FALSE))
}
