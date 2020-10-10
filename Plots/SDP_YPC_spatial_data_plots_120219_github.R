library(rgdal)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(sp)
library(raster)
library(ggsn)
library(ggspatial)

#' Note: the data files are witheld because of
#' commercial confidentiality

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Yield prediction/Oil_palm_yield_prediction_GitHub/Oil_palm_yield_prediction/")

kml_dir <- "Data/kml_files/"

kml_files <- list.files(kml_dir,pattern='.kml$')

kml_filename <- paste0(kml_dir,kml_files)

layer_names <- c("F1",paste0("E",seq(2,5)))

kml_names <- paste0("E",seq(1,5))

estates_list <- list()

for (i in 1:length(kml_files)) {
  
  estate <- readOGR(kml_filename[i],layer_names[i])
  
  estate_trans <- spTransform(estate, CRS( "+init=epsg:4326" ) )

  estates_list[[i]] <- estate_trans
  
}

poly_list <- list()

for (i in 1:length(kml_files)) {
  
  coords <- matrix(c(xmin(estates_list[[i]]),ymin(estates_list[[i]]),
                     xmin(estates_list[[i]]),ymax(estates_list[[i]]),
                     xmax(estates_list[[i]]),ymax(estates_list[[i]]),
                     xmax(estates_list[[i]]),ymin(estates_list[[i]]),
                     xmin(estates_list[[i]]),ymin(estates_list[[i]])),
                   ncol = 2, byrow = TRUE)
  
  p <- Polygon(coords)
  
  ps <- Polygons(list(p),1)
  
  sps <-  SpatialPolygons(list(ps), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  poly_list[[i]] <- sps
  
}



register_google(key = "INSERT_API_KEY")

map_list <- list()

for(i in 1:length(estates_list)) {

  if(i != 3) {

    zoom_level <- 15

  } else {

    zoom_level <- 14

  }

  map_list[[i]] <- get_map(location=rowMeans(bbox(estates_list[[i]])), maptype = "satellite",zoom=zoom_level)

}

plot_list <- list()

for (i in 1:length(estates_list)) {
  
  map_bb <- bb2bbox(attr(map_list[[i]],"bb"))
  
  p <- ggmap(map_list[[i]]) + 
    geom_path(data=estates_list[[i]],aes(long,lat,group=group),color="yellow",size=1.25) +
    ggtitle(paste0("F",i)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (i != 2 & i != 3 & i != 4 ) {

    plot_list[[i]] <- p +
      scalebar(x.min = map_bb[[1]], x.max = map_bb[[3]]-(map_bb[[3]]-map_bb[[1]])*0.1,
               y.min = map_bb[[2]]+(map_bb[[4]]-map_bb[[2]])*0.05, y.max = map_bb[[4]],
               dist = 0.5, dist_unit = "km",
               st.size = 3,
               st.dist = 0.04,
               st.bottom = FALSE,
               st.color="white",
               transform = TRUE, model = "WGS84") +
      annotation_north_arrow(style=north_arrow_fancy_orienteering)

  } else if ( i == 2 ) {

    plot_list[[i]] <- p +
      scale_x_continuous(limits = c(103.335, 103.36),breaks = seq(103.34, 103.36, 0.01),  expand = c(0, 0)) +
      scalebar(x.min = 103.335, x.max = 103.36-(103.36-103.335)*0.1,
               y.min = map_bb[[2]]+(map_bb[[4]]-map_bb[[2]])*0.05, y.max = map_bb[[4]],
               dist = 0.5, dist_unit = "km",
               st.size = 3,
               st.dist = 0.04,
               st.bottom = FALSE,
               st.color="white",
               transform = TRUE, model = "WGS84") +
      annotation_north_arrow(style=north_arrow_fancy_orienteering)
    
  }
  
  else if ( i == 3 ) {
    
    plot_list[[i]] <- p +
      scale_x_continuous(limits = c(101.325, 101.38),breaks = seq(101.33, 101.38, 0.02),  expand = c(0, 0)) +
      scalebar(x.min = map_bb[[1]], x.max = map_bb[[3]]-(map_bb[[3]]-map_bb[[1]])*0.1,
               y.min = map_bb[[2]]+(map_bb[[4]]-map_bb[[2]])*0.05, y.max = map_bb[[4]],
               dist = 1, dist_unit = "km",
               st.size = 3,
               st.dist = 0.04,
               st.bottom = FALSE,
               st.color="white",
               transform = TRUE, model = "WGS84") +
      annotation_north_arrow(style=north_arrow_fancy_orienteering)

  } else {

    plot_list[[i]] <- p +
      scale_x_continuous(limits = c(101.386, 101.41),breaks = seq(101.39, 101.41, 0.01),  expand = c(0, 0)) +
      scalebar(x.min = 101.386, x.max = 101.41-(101.41-101.386)*0.1,
               y.min = map_bb[[2]]+(map_bb[[4]]-map_bb[[2]])*0.05, y.max = map_bb[[4]],
               dist = 0.5, dist_unit = "km",
               st.size = 3,
               st.dist = 0.04,
               st.bottom = FALSE,
               st.color="white",
               transform = TRUE, model = "WGS84") +
      annotation_north_arrow(style=north_arrow_fancy_orienteering)

  }
  
}

malaysia_map <- get_map(location= "malaysia", maptype = "hybrid", zoom = 7)

lon <- vector(mode = "double", length = length(poly_list))

lat <- vector(mode = "double", length = length(poly_list))

label <- vector(mode = "character", length = length(poly_list))

for (i in 1:length(poly_list)) {
  
  lon[i] <- mean(bbox(poly_list[[i]])[1,])
  
  lat[i] <- mean(bbox(poly_list[[i]])[2,])
  
  label[i] <- paste0("F",i)
  
}

df<-data.frame(lon,lat,label)

malaysia_points <- ggmap(malaysia_map) +
  geom_point(data=df, aes(lon,lat),
             color="red", size=2, shape=19) +
  geom_label_repel(data=df, 
                   aes(lon,lat,label = label),
                   size=2.5) +
  scale_x_continuous(limits = c(101, 104), expand = c(0, 0)) +
  scale_y_continuous(limits = c(1, 4), expand = c(0, 0)) +
  scalebar(x.min = 101, x.max = 103.1,
           y.min = 1.15, y.max = 4,
           dist = 50, dist_unit = "km",
           st.size = 3,
           st.dist = 0.04,
           st.bottom = FALSE,
           st.color="white",
           transform = TRUE, model = "WGS84") +
  annotation_north_arrow(style=north_arrow_fancy_orienteering)

plot_list[[length(estates_list)+1]] <- malaysia_points

dir <- "Manuscript_figures/"

file_path <- paste0(dir,"SDP_YPC_spatial_data_plots_120219.png")

g <- do.call("grid.arrange", c(plot_list, ncol=2))

# dev.new(width=6.5,height=8,noRStudioGD = TRUE)
# 
# grid.arrange(g)

ggsave(file_path, g, width=6.5, height=8, units="in", dpi=100)
