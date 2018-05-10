init_general <- function (path = getOption("RconTroll.path"),
                          input = getOption("RconTroll.init"),
                          add_climate = T,#New  #CLIMATE
                          climate = "input_climate.txt",#New  #CLIMATE
                          overwrite = TRUE,
                          nbcol = 400, #GENERAL
                          nbrows = 400, #GENERAL
                          nbiter = 12, #GENERAL
                          iter = 12, #GENERAL
                          NV = 1, #GENERAL
                          NH = 1, #GENERAL
                          nbout = 4, #GENERAL
                          numesp = 163, #GENERAL
                          p = 0.05,#GENERAL
                          #CLIMATE
                          daylight = c(0.03, 0.108, 0.208, 0.304, 0.473, 0.617, 0.723,
                                       0.823, 0.925, 0.965, 0.995, 0.988, 1, 0.942, 0.854,
                                       0.766, 0.646, 0.493, 0.36, 0.277, 0.183, 0.102, 0.058,
                                       0.022),
                          #CLIMATE
                          dayT = c(0.012, 0.018, 0.042, 0.087, 0.175,
                                   0.304, 0.445, 0.569, 0.672, 0.767, 0.856, 0.915, 0.961,
                                   0.986, 1, 0.985, 0.952, 0.908, 0.863, 0.82, 0.767, 0.699,
                                   0.611, 0.498),
                          #CLIMATE
                          dayVPD = c(0.768, 0.776, 0.795, 0.82,
                                     0.851, 0.884, 0.912, 0.934, 0.95, 0.966, 0.98, 0.989,
                                     0.997, 0.999, 1, 0.9996, 0.99, 0.982, 0.974, 0.966,
                                     0.957, 0.945, 0.929, 0.906),
                          klight = 0.9, #GENERAL
                          phi = 0.06, #GENERAL
                          g1 = 3.77, #GENERAL
                          vC = 0.1, #GENERAL
                          DBH0 = 0.01, #GENERAL
                          H0 = 1, #GENERAL
                          ra0 = 0.5, #GENERAL
                          ra1 = 10, #GENERAL
                          de0 = 0.3, #GENERAL
                          de1 = 0.12, #GENERAL
                          dens = 0.8, #GENERAL
                          fbranchstem = 0.35, #GENERAL
                          fcanopy = 0.3, #GENERAL
                          seedrain = 5000, #GENERAL
                          nbseeds = 10, #GENERAL
                          #Add sigma_height #GENERAL
                          #Add sigma_CR #GENERAL
                          #Add sigma_CD #GENERAL
                          #Add sigma_P #GENERAL
                          #Add sigma_N #GENERAL
                          #Add sigma_LMA #GENERAL
                          #Add sigma_wsg #GENERAL
                          #Add sigma_dmax #GENERAL
                          #Add corr_CR_height #GENERAL
                          #Add corr_N_P #GENERAL
                          #Add corr_N_LMA #GENERAL
                          #Add corr_P_LMA #GENERAL
                          #Add leafdem_resolution = 30 #GENERAL
                          #Add p_tfsecondary #GENERAL
                          #Add hurt_decay #GENERAL
                          mindeathrate = 0.035, #GENERAL
                          m1 = 0.035, #GENERAL
                          CO2 = 360, #GENERAL
                          species = read.table(getOption("RconTroll.species"), header = TRUE, dec = ".", sep = ""), #GENERAL
                          #CLIMATE
                          Tyear = c(24.64982014,
                                    24.60624211, 24.49933474, 24.96279395, 24.88365139,
                                    24.87594184, 24.98313802, 25.68910135, 26.58033805,
                                    26.98619405, 25.87259696, 25.16530008),
                          #CLIMATE
                          maxT = c(29.08402,
                                   28.86072, 28.51707, 29.56514, 29.0068, 29.83177, 30.19183,
                                   31.34605, 32.93941, 33.14729, 31.72165, 30.32959),
                          #CLIMATE
                          nightmeanT = c(23.22247,
                                         23.15235, 23.21073, 23.35492, 23.58619, 23.29334, 23.3578,
                                         23.6699, 24.09092, 24.55815, 23.71159, 23.34976),
                          #CLIMATE
                          rainfall = c(296.726,
                                       253.528, 454.302, 320.562, 425.2, 277.966, 163.426,
                                       102.012, 43.834, 26.466, 76, 157.1),
                          #CLIMATE
                          wind = c(0.901671724,
                                   0.864544643, 0.836516801, 0.851327994, 0.787419637,
                                   0.774398649, 0.831167015, 0.91152957, 1.021909722, 1.053285523,
                                   1.041843217, 0.921361559),
                          #CLIMATE
                          maxIrradiance = c(719.7763,
                                            765.6474679, 769.9658806, 794.82337, 717.8013194, 795.2024929,
                                            791.62932, 797.9236677, 874.5701367, 857.4397219, 858.60655323,
                                            779.2854645),
                          #CLIMATE
                          irradiance = c(133.0579717, 158.3033498,
                                         148.2984963, 164.1783546, 143.7356799, 167.5139082,
                                         172.4494762, 190.3358903, 229.3920961, 225.1016445,
                                         196.6685388, 170.289151),
                          #CLIMATE
                          e_s = c(3.126835644, 3.122050934,
                                  3.097285304, 3.194593888, 3.167241554, 3.173096221,
                                  3.197669097, 3.351896181, 3.552115706, 3.634188446,
                                  3.388384875, 3.239834054),
                          #CLIMATE
                          e_a = c(2.820456547, 2.781722195,
                                  2.794117369, 2.81884079, 2.918981525, 2.889089767, 2.862294539,
                                  2.823627394, 2.730098975, 2.774015541, 2.794606935, 2.801060958),
                          #CLIMATE
                          VPD = c(0.306379098, 0.340328739, 0.303167936,
                                  0.375753098, 0.24826003, 0.284006455, 0.335374559, 0.528268788,
                                  0.822016731, 0.860172905, 0.59377794, 0.438773096),
                          #CLIMATE
                          dailymeanVPD = c(0.5873121, 0.6411128, 0.5661197, 0.72542,
                                           0.4913662, 0.583989, 0.7444557, 1.0074374, 1.4987548,
                                           1.5640231, 1.1442166, 0.8511218),
                          #CLIMATE
                          dailymaxVPD = c(1.183852,
                                          1.178975, 1.074576, 1.305361, 1.034367, 1.280059, 1.439207,
                                          1.787562, 2.418552, 2.471909, 2.020966, 1.567758))
{


  # Safety checks -----------------------------------------------------------

  if (!overwrite){
    if (input %in% list.files(path)) # Check for species/general input
      stop("The input file already exists, use overwrite = T.")
    if (climate %in% list.files(path)) # Check for climate input
      stop("The climate file already exists, use overwrite = T.")
  }


  # Global and species input file (should be subsequently splitted) ---------


  fileConn <- file(file.path(path, input))

  writeLines(c("#############################################\t",
               "###  Parameter file for the TROLL program ###\t",
               "#############################################\t",
               "###\tGENERAL PARAMETERS",  #GENERAL
               paste(nbcol, "/* cols # nb of columns */", sep = "\t"),  #GENERAL
               paste(nbrows, "/* rows # nb of rows  */", sep = "\t"),  #GENERAL
               paste(nbiter, "/* nbiter # total nb of timesteps */", sep = "\t"),  #GENERAL
               paste(iter, "/* number of iteration per year */", sep = "\t"),  #GENERAL
               paste(NV, "/* NV # vertical nb of cells (nb per m) */", sep = "\t"),  #GENERAL
               paste(NH, "/* NH # horizontal nb of cells (nb per m) */", sep = "\t"),  #GENERAL
               paste(nbout, "/* nbout # Number of outputs */ ", sep = "\t"),  #GENERAL
               paste(numesp, "/* numesp # Number of species */", sep = "\t"),  #GENERAL
               paste(p, "/* p # light incidence param (diff through turbid medium) */", sep = "\t"),

               do.call(paste, as.list(c(format(daylight, nsmall = 3), "  /* normalized daily light course (from 7am to 7pm, with a half-hour time-step */",
                                        sep = "\t"))), do.call(paste, as.list(c(format(dayT, nsmall = 3), "  /* normalized daily VPD course (from 7am to 7pm, with a half-hour time-step */",
                                                                                sep = "\t"))), do.call(paste, as.list(c(format(dayVPD, nsmall = 3), "  /* normalized daily T course (from 7am to 7pm, with a half-hour time-step */",
                                                                                                                        sep = "\t"))),
               "###\tCharacters shared by species\t\t\t\t\t\t\t\t\t\t\t",  #GENERAL
               paste(klight, "/* klight # light attenuation in the canopy Beer-Lambert */", sep = "\t"),  #GENERAL
               paste(phi, "/* phi # quantum yield (in micromol C/micromol photon) */", sep = "\t"),  #GENERAL
               paste(g1, "/* parameter g1 of Medlyn et al stomatal conductance model */", sep = "\t"),  #GENERAL
               paste(vC, "/* vC  # variance of the flexion moment */", sep = "\t"),  #GENERAL
               paste(DBH0, "/* DBH0 # initial dbh (m) */", sep = "\t"),  #GENERAL
               paste(H0, "/* H0 # initial height (m) */", sep = "\t"),  #GENERAL
               paste(ra0, "/* ra0 # initial crown radius (in m) */", sep = "\t"),  #GENERAL
               paste(ra1, "/* ra1 # crown radius - dbh slope */", sep = "\t"),  #GENERAL
               paste(de0, "/* de0 # initial crown depth(in m) */", sep = "\t"),  #GENERAL
               paste(de1, "/* de1 # Crown_Depth/height slope (m/m) */", sep = "\t"),  #GENERAL
               paste(dens, "/* dens # leaf density (m^2/m^2) */", sep = "\t"),  #GENERAL
               paste(fbranchstem, "/* fraction of biomass allocated to above ground wood (branches+stem) */", sep = "\t"),  #GENERAL
               paste(format(fcanopy, nsmall = 2),"/* fraction of biomass allocated to canopy (leaves + reproductive organs + twigs) */\t\t\t\t\t\t\t\t\t\t\t", sep = "\t"),  #GENERAL
               paste(seedrain, "/* constant used to scale total seed rain per hectare across species (in next computation) */", sep = "\t"),  #GENERAL
               paste(nbseeds, "/* nb of seeds produced and dispersed by each mature tree when SEEDTRADEOFF is not defined */", sep = "\t"),  #GENERAL
               paste(mindeathrate, "/* minimal death rate", sep = "\t"),  #GENERAL
               paste(m1, "/* m1 (slope of death rate)",sep = "\t"),  #GENERAL
               paste(CO2, "/* atmospheric CO2 concentration in micromol/mol */",sep = "\t"),  #GENERAL
               "\t\t\t\t\t\n###\tSpecies description\t\t\t\t\t\t\t\t\t\t\t"),fileConn)  #GENERAL

  close(fileConn)
  names(species)[1] <- "****"  #GENERAL
  #GENERAL
  suppressWarnings(write.table(species, file.path(path, input),sep = "\t", append = TRUE, row.names = FALSE, quote = FALSE))

  cat(c("\n###\tClimate (input environment)\n",
        do.call(paste, as.list(c(Tyear, "/*Temperature in degree C*/", sep = "\t"))), "\n",
        do.call(paste, as.list(c(maxT, "/*mean daily max temperature in degree C*/", sep = "\t"))), "\n",
        do.call(paste, as.list(c(nightmeanT, "/*Night mean temperature in degree C*/", sep = "\t"))), "\n",
        do.call(paste, as.list(c(rainfall, "/*Rainfall in mm*/\t/", sep = "\t"))), "\n",
        do.call(paste, as.list(c(wind, "/* Wind speed in m.s-1 */", sep = "\t"))), "\n",
        do.call(paste, as.list(c(maxIrradiance, "/* Daily max irradiance mean in W.m-2 */", sep = "\t"))), "\n",
        do.call(paste, as.list(c(irradiance,"/* Irradiance mean in W.m-2 */", sep = "\t"))), "\n",
        do.call(paste, as.list(c(e_s, "/* e_s in kPa */",sep = "\t"))), "\n",
        do.call(paste, as.list(c(e_a, "/* e_a in kPa */", sep = "\t"))), "\n",
        do.call(paste, as.list(c(VPD, "/* VPD in kPa */", sep = "\t"))), "\n",
        do.call(paste, as.list(c(dailymeanVPD, "/* Daily mean VPD in kPa */", sep = "\t"))), "\n",
        do.call(paste, as.list(c(dailymaxVPD, "/* daily max VPD in kPa */", sep = "\t")))),
      file = file.path(path, input), append = TRUE)
}
