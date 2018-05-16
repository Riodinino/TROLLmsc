#' Title
#'
#' @param path
#' @param shape
#' @param limits
#' @param plot_num
#' @param year
#'
#' @return
#' @export
#'
#' @examples
extractshapes <- function(shape = "Gaps",
                          plot_num,
                          year)
  {

  # Libs --------------------------------------------------------------------
  # library(tidyverse)
  # library(raster)
  # library(rgdal)

  path <- "C:/Users/nino.page/Desktop/TROLL project/Paracou/damages"
  # setwd(file.path(path,"data"))
  print(getwd())


  pathdat <- "C:/Users/nino.page/Desktop/TROLL project/Paracou"



  # Load general shapefiles -------------------------------------------------
  setwd(file.path(pathdat,"logging"))
  print(getwd())
  gaps <- shapefile(paste0(shape,".shp"))
  print("ok")

  setwd(file.path(pathdat,"limits"))
  print(getwd())
  limits <- shapefile(paste0("OverallPlots.shp"))
  print("ok")

  # Extract, save and reload ------------------------------------------------


    limitdata <- limits[which(limits$Plot == plot_num),]
    plotdata <- gaps[which(gaps$Plot == plot_num),]

    setwd(file.path(path,"results"))

    writeOGR(plotdata,
             ".",
             paste0("plotdata_",plot_num,"_",shape),
             driver="ESRI Shapefile",
             overwrite_layer = TRUE)
    # plot <-  readOGR(".","plotdata")
    # ggplot_name <- paste0("Plot_",plot_num,"_",shape,".pdf")

    # pdf(ggplot_name)
    # g <- ggplot() +
    #   geom_polygon(data=plotdata,
    #                aes(x=long,
    #                    y=lat,
    #                    group=group),
    #                fill = "dark red")+
    #   labs(title= paste("Post-logging data :",
    #                     shape,
    #                     "layer",
    #                     sep = " "),
    #        subtitle = paste("Plot",
    #                         plot_num,
    #                         "year",
    #                         year,
    #                         sep = " "))+
    #   geom_polygon(data = limitdata,
    #                aes(x = long, y = lat),
    #                col = "black",alpha = 0.3) +
    #   coord_fixed()

    # g <- ggplot() +
    #   geom_polygon(data=plotdata,
    #                aes(x=long,
    #                    y=lat,
    #                    group=group),
    #                fill = "red") +
    #   labs(x = "longitude",
    #        y = "lattitude",
    #        title= paste("Post-logging data :",
    #                     shape,
    #                     "layer",
    #                     sep = " "),
    #        subtitle =  paste0("Plot ",
    #                          plot_num,
    #                          " - (year ",
    #                          year,")",
    #                          sep = " "))+
    #   theme(plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = 0.5),
    #         plot.subtitle = element_text(hjust = 0.5))+
    #   geom_polygon(data = limitdata,
    #                aes(x = long, y = lat),
    #                alpha = 0.2) +
    #   coord_fixed()
    # dev.off

    # ggsave(ggplot_name, device = "pdf")
    # print(getwd())
    return(g)
}

