#' Title
#'
#' @param trollsim_pre data.frame, inventory from trollsim pre-logging for mature understoreys
#' @param trollsim_post data.frame, inventory from trollsim post-logging for immature understoreys
#' @param shape character. Name of the shapefile used for damage without any extension.
#' @param coords
#' @param plotnum
#' @param year
#' @param isUTM
#' @param path_graph
#' @param pathshp
#' @param pathlim
#'
#' @return
#' @export
#'
#' @examples
assembly_understoreys <- function(trollsim_pre,
                                  trollsim_post,
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
  library(sp)
  library(rgdal)

  # Safety checks -----------------------------------------------------------

  if(!(coords[1] %in% names(trollsim_pre)&
       coords[2] %in% names(trollsim_pre)&
       coords[1] %in% names(trollsim_post)&
       coords[1] %in% names(trollsim_post))){
    stop("The names you indicated for X and Y columns are not in the trollsim data !")
  }
  if(shape != "DisturbedAreas" & shape != "Gaps" & shape !="SkidTrails"& shape !="TotalDamages"){
    stop("Indicate a correct name for shape please.")
  }
  if(!isUTM){
    stop("You must imput data with UTM coordinates. Use the convertutm() function")
  }

  # Extraction of the polygon from the shapefile ----------------------------

  nameshape <- paste0("plotdata_",plotnum,"_",shape) #define plot num. Should the loop be inside ?
  setwd(pathshp)
  damage <-  readOGR(".",nameshape) # Already for the plot
  setwd(pathlim)
  limit <-  readOGR(".","OverallPlots")


  # Prepare the datas to be used as SPDF -------------------------------------

  coordinates(trollsim_pre) <-~Xutm+Yutm # Declare that this is coords
  proj4string(trollsim_pre) <- CRS(proj4string(damage)) # Tune projection according to damages
  trollsim_pre <- spTransform(trollsim_pre, CRS(proj4string(damage))) # Transform

  coordinates(trollsim_post) <-~Xutm+Yutm # Declare that this is coords
  proj4string(trollsim_post) <- CRS(proj4string(damage)) # Tune projection according to damages
  trollsim_post <- spTransform(trollsim_post, CRS(proj4string(damage))) # Transform


  # Extract the trollsim_pre points outside damage --------------------------

  trollsim_pre_out <- trollsim_pre[is.na(over(trollsim_pre, as(damage,"SpatialPolygons"))),]

  # Extract the trollsim_post inside damage ---------------------------------
  trollsim_post_in <- trollsim_post[!is.na(over(trollsim_post, as(damage,"SpatialPolygons"))),]


# Switch to data.frame ----------------------------------------------------

  trollsim_post_in <- as.data.frame(trollsim_post_in)
  trollsim_pre_out <- as.data.frame(trollsim_pre_out)
  trollsim_post_in <- trollsim_post_in %>% mutate(damaged = TRUE)
  trollsim_pre_out <- trollsim_pre_out %>% mutate(damaged = FALSE)

  trollsimcomposite <- rbind(trollsim_post_in,trollsim_pre_out)

# Do a little map ---------------------------------------------------------

  # Transform data

  trollsimgraph <- trollsimcomposite
  crs <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
  coordinates(trollsimgraph) <- ~Xutm+Yutm
  proj4string(trollsimgraph)<- "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  trollsimgraph <- spTransform(trollsimgraph, CRSobj = crs)
  damage <- spTransform(damage, CRSobj = crs)
  limit <- spTransform(limit, CRSobj = crs)

  trollsimgraph@data$damaged <- FALSE
  logics <- !is.na(over(trollsimgraph,as(damage,"SpatialPolygons")))
  for(i in 1:length(logics)){
    if(logics[i] == T){
      trollsimgraph@data[i,"damaged"] <- TRUE
    }
    else trollsimgraph@data[i,"damaged"] <- FALSE
  }

  #Create palete for beautiful map

  damagePal <- colorFactor(c('dark green','dark red'), as.factor(trollsimgraph$damaged), reverse = F)
  #Create map
  m <- leaflet() %>% addPolylines(data = limit, color = 'light brown') %>%
    # addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
    # or
    # addTiles() %>%
    addPolygons(data = limit, opacity = 0.1, fillOpacity = 0.1, fill = T,
                fillColor = "grey", color = 'grey' ,label = paste("Plot",limit$Plot),
                popup = paste("Treatment: ", limit$Treatment)) %>%
    addPolygons(data = damage, opacity = 0.1, fillOpacity = 0.3, fill = T, color = "red") %>%
    addCircles(data = trollsimgraph, radius = 0.1, color  =~damagePal(trollsimgraph$damaged),
               fillColor  =~damagePal(trollsimgraph$damaged), fillOpacity =  0.2) %>%
    addLegend(position = "bottomright", pal = damagePal, values = trollsimgraph$damaged,
              title = "Is the tree likely to be damaged ?",
              opacity = 0.3) %>%
    addLayersControl(overlayGroups = c('limit','trollsimgraph','damage'),
                     options = layersControlOptions(collapsed = F))

  # Save the map in html and pdf format -------------------------------------
  library('mapview')
  urlt <- paste0(path_graph,"/",plotnum,"_",year,"_",shape,"_", "map.html")
  mapview::mapshot(m, url = urlt)
  # mapview::mapshot(m, file = paste0(path_graph,"/",plotnum,"_",year,"_",shape,"_", "map.pdf"))

  return(trollsimcomposite)
}

