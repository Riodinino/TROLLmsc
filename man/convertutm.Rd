% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertutm.R
\name{convertutm}
\alias{convertutm}
\title{Title}
\usage{
convertutm(data, plotnum, yearcol = "campagne", year = 1984,
  datastation = DataParacou, Xutm = "Xutm", Yutm = "Yutm", X = "X",
  Y = "Y", plotcol = "n_parcelle", plotedge = "Plot",
  realedges = coordinates <- read.csv(file =
  file.path("C:/Users/nino.page/Desktop/TROLL project/treated_data/sim_prepare_paracou/understoreys",
  "coordinates.csv"), header = T, sep = ";", dec = ","))
}
\arguments{
\item{data}{Data.frame - Your simulated plot with TROLL, out of a real data plot treated with the precedent functions}

\item{plotnum}{Integer - Number of the plot corresponding to your data station}

\item{yearcol}{The column of years}

\item{year}{The year you work on}

\item{datastation}{Data.frame - The whole dataset for your research station (here, Paracou)}

\item{Xutm}{Character - The name of the column corresponding to UTM (here, 22S) X coordinates}

\item{Yutm}{Character - The name of the column corresponding to UTM (here, 22S) Y coordinates}

\item{X}{Character - The name of the column corresponding to intra-plot X coordinates}

\item{Y}{Character - The name of the column corresponding to intra-plot Y coordinates}

\item{plotcol}{Character - The name of the column indicating plot numbers in datastation}

\item{plotedge}{Character - The name of the column indicating plot numbers in realedges}

\item{realedges}{Data.frame - table containing your plots' edges, in UTM, extracted from some GIS software, with a column indicating the plot number.}
}
\description{
Title
}
