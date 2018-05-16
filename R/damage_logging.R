# library('rgdal')
# library(raster)
#'
#'   Title
#'
#' @param trollsim data.frame - Forest simulated with TROLL or trollsim object
#' @param coords
#' @param plotnum
#' @param pathshp
#' @param isUTM
#'
#' @return
#' @export
#'
#' @examples
# f <- convertutm(forest, plotnum = 5)# Supposed to be done already
damage_logging <- function(trollsim,
                           shape = "TotalDamages",
                           coords = c("Xutm","Yutm"),
                           plotnum,
                           year,
                           isUTM = T,
                           # compare = DataParacou,
                           path_graph="C:/Users/nino.page/Desktop/TROLL project/Paracou/damages/graphs/leaflets",
                           pathshp ="C:/Users/nino.page/Desktop/TROLL project/Paracou/damages/results",
                           pathlim = "C:/Users/nino.page/Desktop/TROLL project/Paracou/limits"
                           )
  {

# Safety checks -----------------------------------------------------------
  if(!coords %in% names(trollsim)){
    stop("The names you indicated for X and Y columns are not in the trollsim data !")
  }
  if(shape != "DisturbedAreas" & shape != "Gaps" & shape !="SkidTrails"){
    stop("Indicate a correct name for shape please.")
  }

# Check if X is in UTM ----------------------------------------------------
  # if(!isUTM){
  #
  # }


# Extraction of the polygon from the shapefile ----------------------------
  nameshape <- paste0("plotdata_",plot_num,"_",shape) #define plot num. Should the loop be inside ?
  damage <-  readOGR(".",file.path(pathshp,nameshape)) # Already for the plot
  limit <-  readOGR(".",file.path(pathlim,"OverallPlots.shp"))


# Prepare the datas to be used as SPDF -------------------------------------

  # trollsim <- trollsim %>% mutate(lat = Yutm, long =Xutm) # Name the coords
  coordinates(trollsim_pre) <-~Xutm+lat # Declare that this is coords
  proj4string(trollsim_pre) <- CRS(proj4string(damage)) # Tune projection according to damages
  trollsim <- spTransform(trollsim_pre, CRS(proj4string(damage))) # Transform

  coordinates(trollsim_post) <-~Xutm+lat # Declare that this is coords
  proj4string(trollsim_post) <- CRS(proj4string(damage)) # Tune projection according to damages
  trollsim <- spTransform(trollsim_post, CRS(proj4string(damage))) # Transform


# Extract the points that are not inside any polygon ----------------------
  trollsim@data$crossdamaged <- FALSE
  logicscross <- !is.na(over(trollsim,as(damage,"SpatialPolygons")))
  for(i in 1:length(logicscross)){
    if(logicscross[i] == "bunas"){
      trollsim@data[i,"damagedcross"] <- "in"
    }
    else trollsim@data[i,"damagedcross"] <- "out"
  }

  # trollsim_in <- trollsim[!is.na(over(trollsim, as(damage,"SpatialPolygons"))),]
  # trollsim_out <- trollsim[is.na(over(trollsim, as(damage,"SpatialPolygons"))),]

  # trollsim_in <- as.data.frame(trollsim_in)
  # names(trollsim_in)[names(trollsim_in) == 'long'] <- "x"
  # names(trollsim_in)[names(trollsim_in) == 'lat'] <- "y"

  # trollsim_out <- as.data.frame(trollsim_out)
  # names(trollsim_out)[names(trollsim_out) == 'long'] <- "x"
  # names(trollsim_out)[names(trollsim_out) == 'lat'] <- "y"

  # trollsim_in <- trollsim_in %>% mutate(damaged = TRUE)
  # trollsim_out <- trollsim_out %>% mutate(damaged = FALSE)

  # trollsimdiff <- rbin(trollsim_in,trollsim_out)
# Plot the result ---------------------------------------------------------
#
#   plotname <- paste0("Plot_",plotnum)
#
#   ggplot() +
#     geom_polygon(data=damage, aes(x=long, y=lat, group=group), fill = "red") +
#     geom_polygon(data = limit, aes(x = long, y = lat),col = "black", alpha = 0.3) +
#     labs(x = "", y = "", title= paste0("Paracou census - ",year," Plot ",plotnum," : "), subtitle = "Trees damaged by logging")+
#     theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
#           axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
#           plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5))+
#     geom_point(data = f33, aes(x = x, y = y, color = In),alpha = 0.3)+
#     scale_color_manual("State of the trees", values=c("dark green" ,"red"),
#                        labels = c("Undamaged trees","Damaged trees /n with corresponding \ndamage polygons"))+ # change color scale
#     coord_equal(ratio=1)
#
#
#   ggsave(plotname, device = pdf)

  crs <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
  coordinates(trollsim) <- ~Xutm+Yutm
  proj4string(trollsim)<- "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  trollsim <- spTransform(trollsim, CRSobj = crs)
  damage <- spTransform(damage, CRSobj = crs)
  limit <- spTransform(limit, CRSobj = crs)

  trollsim@data$damaged <- FALSE
  logics <- !is.na(over(trollsim,as(damage,"SpatialPolygons")))
  for(i in 1:length(logics)){
    if(logics[i] == T){
      trollsim@data[i,"damaged"] <- TRUE
    }
    else trollsim@data[i,"damaged"] <- FALSE
  }

  #Create palete for beautiful map


  damagePal <- colorFactor(c('dark green','dark red'), as.factor(trollsim$damaged), reverse = F)
  #Create map
  m <- leaflet() %>% addPolylines(data = limit, color = 'light brown') %>%
    addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
    # or
    # addTiles() %>%
    addPolygons(data = limit, opacity = 0.1, fillOpacity = 0.1, fill = T,
                fillColor = "grey", color = 'grey' ,label = paste("Plot",limit$Plot),
                popup = paste("Treatment: ", limits$Treatment)) %>%
    addPolygons(data = damage, opacity = 0.1, fillOpacity = 0.3, fill = T, color = "red") %>%
    addCircles(data = trollsim, radius = 0.1, color  =~damagePal(trollsim$damaged),
               fillColor  =~damagePal(trollsim$damaged), fillOpacity =  0.2) %>%
    addLegend(position = "bottomright", pal = damagePal, values = trollsim$damaged,
              title = "Is the tree likely to be damaged ?",
              opacity = 0.3) %>%
    addLayersControl(overlayGroups = c('limit','trollsim','damage'),
                     options = layersControlOptions(collapsed = F))

# Save the map in html and pdf format -------------------------------------
  # library('mapview')
  mapview::mapshot(m, url = paste0(path_graph,"/",plotnum,"_",year,"_",shape,"_", "map.html"))
  mapview::mapshot(m, file = paste0(path_graph,"/",plotnum,"_",year,"_",shape,"_", "map.pdf"))

# Compare with Paracou ----------------------------------------------------
#
#   compare <- compare %>%
#     filter(circ_Corr > 31) %>%
#     mutate(species = paste(Genre,espece, sep = "_")) %>%
#     filter(campagne == year) %>%
#     filter(n_parcelle == plotnum) %>%
#     filter(code_vivant == 1)
# #
#   compare_in <- as.data.frame(compare_in)
#   names(compare_in)[names(compare_in) == 'long'] <- "x"
#   names(compare_in)[names(compare_in) == 'lat'] <- "y"
# #
#   compare_out <- as.data.frame(compare_out)
#   names(compare_out)[names(compare_out) == 'long'] <- "x"
#   names(compare_out)[names(compare_out) == 'lat'] <- "y"
#
#   trollsim_in <- trollsim[!is.na(over(trollsim, as(damage,"SpatialPolygons"))),]
#   trollsim_out <- trollsim[is.na(over(trollsim, as(damage,"SpatialPolygons"))),]
#
#   trollsim_in <- as.data.frame(trollsim_in)
#   names(trollsim_in)[names(trollsim_in) == 'long'] <- "x"
#   names(trollsim_in)[names(trollsim_in) == 'lat'] <- "y"
#
#   trollsim_out <- as.data.frame(trollsim_out)
#   names(trollsim_out)[names(trollsim_out) == 'long'] <- "x"
#   names(trollsim_out)[names(trollsim_out) == 'lat'] <- "y"
#
#   trollsim_in <- trollsim_in %>% mutate(damaged = TRUE)
#   trollsim_out <- trollsim_out %>% mutate(damaged = FALSE)


# print(paste("TROLLSIM - Plot",plotnum,":",nrow(trollsim_in),"damaged /",nrow(trollsim_out),"undamaged\n"))
# print(paste("DATA - Plot",plotnum,":",nrow(compare_in),"damaged /",nrow(compare_out),"undamaged"))
  # which(is.na(over(f,as(limit5,"SpatialPolygons")) )) %>% length

  trollsim <- as.data.frame(trollsim)
  return(trollsim)
}

