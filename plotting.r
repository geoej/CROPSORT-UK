library(plyr)
library(dplyr)
library(sf)
library(ggplot2)
library(terra)


#setwd("/Users/ej/CFFRC/04-Research/UK crops/Analysis")

ukbnd <- st_read("./in/gadm41_GBR_shp/gadm41_GBR_0.shp")

climsoil_ID_crops_gridpoints_topcrop = read.csv("climsoil_ID_crops_gridpoints_topcrop.csv")
climsoil_ID_crops_gridpoints = readRDS("climsoil_ID_crops_gridpoints")

#-------------------
# Creatig maps for suitability classes
#------------------

Permanently_unsuitable =read.csv("Permanently_unsuitable.csv")
Unsuitable = read.csv("Unsuitable.csv")
Moderately_suitable = read.csv("Moderately_suitable.csv")
Suitable = read.csv("Suitable.csv")
Highly_suitable =read.csv("Highly_suitable.csv")


for (i in c("Permanently_unsuitable", "Unsuitable", "Moderately_suitable", "Suitable", "Highly_suitable")){
  dat = get(i)
  dat = dat[complete.cases(dat),]
  datasf = st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)
  options(bitmapType='cairo')
  g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = datasf, aes(col = name))+
    ggtitle(paste0("map of ", i, "crop"))
  ggsave(paste("map of", i ,"crop.png"),g)  
}

#--------------------
# getting top 10 crops 
#-------------------

top10 <- list()
#length(climsoil_ID_crops_gridpoints
for (i in 1:length(climsoil_ID_crops_gridpoints)){
    
    top10[[i]] <- climsoil_ID_crops_gridpoints[[i]] %>%
    arrange(desc(avgclimsoil)) %>%
    slice_head(n = 10)
}

# maps for top5 
first <- list()
second <- list()
third <- list()
fourth <- list()
fifth <- list()

# length(top10)
for (i in 1:length(top10)){

    first[[i]] = top10[[i]][1,]
    second[[i]] = top10[[i]][2,]
    third[[i]] = top10[[i]][3,]
    fourth[[i]] = top10[[i]][4,]
    fifth[[i]] = top10[[i]][5,]
    }
  
# making map for the top fifth crops
for (i in c("first", "second", "third", "fourth", "fifth")){
  dat = ldply(get(i))
  dat = dat[complete.cases(dat),]
  dat = filter(dat, avgclimsoil >= 70)
  datasf = st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)
  options(bitmapType='cairo')
  g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = datasf, aes(col = name))+
    ggtitle(paste0("map of ", i, "crop")) + theme(legend.position = "none")
  ggsave(paste("map of", i ,"crop.png"), g)  
}


# creating a graph for 
ukbnd <- st_read("./in/gadm41_GBR_shp/gadm41_GBR_0.shp")
grid <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/grid/uk_gridlo543.shp")

options(bitmapType='cairo')
g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = grid, size = 0.1, color = 'red') 
ggsave("uk_boundary_grid.png", g,dpi = 300, scale = 2)



#---------------
# creating a rotated map stack 
#---------------
# code from https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/

#' Rotate simple features for 3D layers
#' Rotates a simple features layer using a shear matrix transformation on the 
#' \code{geometry} column. This can get nice for visualisation and works with
#' points, lines and polygons.
#'
#' @param data an object of class \code{sf}
#' @param x_add integer; x value to move geometry in space
#' @param y_add integer; x value to move geometry in space
#'
#' #' @importFrom magrittr %>%

rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}

# reducing the density of the grid points
#grid_demo <- grid[sample(nrow(grid), 2000), ]
grid_demo <- grid
# plotting the array
options(bitmapType='cairo')
g = ggplot() + geom_sf(data = rotate_sf(ukbnd),
           fill = NA,
           color = "black")+
  geom_sf(data = rotate_sf(grid_demo),
          aes(col = "black", alpha = 0.2)) +
  # geom_sf(data = rotate_sf(ukbnd, y_add = 9),
  #           fill = NA,
  #           color = "gray", alpha = 0.2)  +
  geom_sf(data = rotate_sf(grid_demo, y_add = 3), 
          aes(col = "red", alpha = 0.2))   +
  # geom_sf(data = rotate_sf(ukbnd, y_add = 18),
  #         fill = NA,
  #         color = "gray", alpha = 0.2)  +
  geom_sf(data = rotate_sf(grid_demo, y_add = 6), 
          aes(col = "green", alpha = 0.2))  +
  # geom_sf(data = rotate_sf(ukbnd, y_add = 27),
  #         fill = NA,
  #         color = "gray", alpha = 0.2)  +
  geom_sf(data = rotate_sf(grid_demo, y_add = 9), 
          aes(col = "blue", alpha = 0.2))+
  # geom_sf(data = rotate_sf(ukbnd, y_add = 14),
  #         fill = NA,
  #         color = "gray", alpha = 0.2)  +
  # geom_sf(data = rotate_sf(grid_demo, y_add = 14), 
  #         aes(col = "black", alpha = 0.2)) + 
theme(legend.position = "none") 


ggsave("uk_boundary_rotated_lowgrid2.png", g,dpi = 300, scale = 3)
ggsave("uk_boundary_rotated_lowgrid2.png", g)

#-------
# make map for wheat
#--------

# getting the map for wheat
wheat <- list()
#length(climsoil_ID_crops_gridpoints
for (i in 1:length(climsoil_ID_crops_gridpoints)){
  
  wheat[[i]] <- 
    filter(climsoil_ID_crops_gridpoints[[i]], name == "Common Wheat" ) #& avgclimsoil >= 40)
  }


# plotting with ggmap
cw <- ldply(wheat)

# qmplot(lon, lat, data = cw, maptype = "toner-lite") this works on a non-sf object


cw$Suitability <- cut(cw$avgclimsoil,
    breaks=c(0,20,40,60,80,100),
    labels=c('Permanently unsuitable', 'Unsuitable', 'Moderately suitable', 'Suitable', 'Highly suitable'))

options(bitmapType='cairo')
library(ggmap)
uk = c(-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085)
stamen = get_stamenmap(uk, zoom = 6, maptype = "toner-lite") #%>% ggmap() 
gmap = get_map(uk, maptype = "terrain") #, inherit.aes = FALSE

g = ggmap(stamen) +
  geom_point(data=cw, aes(x = lon,y = lat, color = Suitability, size = avgclimsoil)) + 
  scale_color_manual(values = c('Permanently unsuitable' = "gray",
                                'Unsuitable' = 'red',
                                'Moderately suitable' = 'lightgreen',
                                'Suitable' = 'green',
                                'Highly suitable' = 'darkgreen')) +
  ggtitle("wheat suitability map") 

ggsave("wheat map.png", g, dpi = 300, scale = 2.2)  

# getting class percentages
nrow(cw[which(cw$Suitability == 'Permanently unsuitable'),])/nrow(cw)
#0.02272727
nrow(cw[which(cw$Suitability == 'Unsuitable'),])/nrow(cw)
#0.3423077
nrow(cw[which(cw$Suitability == 'Moderately suitable'),])/nrow(cw)
#0.4517483
nrow(cw[which(cw$Suitability == 'Suitable'),])/nrow(cw)
#0.1653846
nrow(cw[which(cw$Suitability ==  'Highly suitable'),])/nrow(cw)
#0.01783217

#wheat spatial writing out for qgis
wsp <- ldply(wheat)

# deprecated: for making the maxes no use
# wsp$tempMax = NULL
# wsp$aveMaxtempSoil = NULL
# for (i in 1:nrow(wsp)){
#   wsp$tempMax[i] = max(wsp[i,2:13], na.rm = T)
#   
# }
# 
# for (i in 1:nrow(wsp)){
#   wsp$aveMaxtempSoil[i] = mean(as.numeric(wsp[i, c("tempMax","averagesoilsuit")]))
#   
# }

wsp$Suitability <- cut(wsp$avgclimsoil,
                      breaks=c(0,20,40,60,80,100),
                      labels=c('Permanently unsuitable', 'Unsuitable', 'Moderately suitable', 'Suitable', 'Highly suitable'))
library(sp)
coordinates(wsp) <- c("lon", "lat")

wsp = st_as_sf(wsp, coords = c("lon", "lat"), crs = 4326)
write_sf(wsp, "./in/CROM/output.gpkg" )
write.csv(as.data.frame(wsp), "wheat_test.csv")

r <- rast(resolution = c(0.8,0.8))
wsp_rast <- rasterize(wsp, r, field = "avgclimsoil", fun = 'max')
spplot(wsp_rast)
writeRaster(wsp_rast, "./in/CROM/output.tif", overwrite=TRUE)

# trying with geom-sf ---> failed
# testing if we can solve the problem of non conformity
# cw_3857 <- st_transform(cw, 3857)
# 
# g =ggmap(gmap) + #geom_sf(data = ukbnd) + 
#   geom_sf(data = cw_3857, aes(color = Suitability))+
#   scale_color_manual(values = c('Permanently unsuitable' = "gray",
#                                 'Unsuitable' = 'red',
#                                 'Moderately suitable' = 'lightgreen',
#                                 'Suitable' = 'green',
#                                 'Highly suitable' = 'darkgreen')) +
#   ggtitle("wheat suitability map") 
# 
# g
# 
# plot(st_transform(cw, crs = 3857)[1], bgMap = gmap)
# 
# 
# g =ggplot() + geom_sf(data = ukbnd) + 
#     geom_sf(data = cw, aes(color = Suitability))+
#     scale_color_manual(values = c('Permanently unsuitable' = "gray",
#                                   'Unsuitable' = 'red',
#                                   'Moderately suitable' = 'lightgreen',
#                                   'Suitable' = 'green',
#                                   'Highly suitable' = 'darkgreen')) +
#   ggtitle("wheat suitability map") 
# 
# g
# ggsave("wheat map.png",g,dpi = 300, scale = 2)  

library(raster)
wheatprod <- raster("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/CROM/UK cereals industry map (wheat area 2012)_modified.tif")
w1 = mask(crop(wheatprod,extent(ukbnd)), ukbnd)
writeRaster(w1, "./in/CROM/test")





#------------------------
# making map for peas
#-----------------------
pea <- list()
#length(climsoil_ID_crops_gridpoints
for (i in 1:length(climsoil_ID_crops_gridpoints)){
  
  pea[[i]] <- 
    filter(climsoil_ID_crops_gridpoints[[i]], name == "Pigeon Pea" & avgclimsoil >= 40)
}

cp <- ldply(pea)

# qmplot(lon, lat, data = cw, maptype = "toner-lite") this works on a non-sf object


cp$Suitability <- cut(cp$avgclimsoil,
                      breaks=c(0,20,40,60,80,100),
                      labels=c('Permanently unsuitable', 'Unsuitable', 'Moderately suitable', 'Suitable', 'Highly suitable'))

options(bitmapType='cairo')
library(ggmap)
uk = c(-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085)
stamen = get_stamenmap(uk, zoom = 6, maptype = "toner-lite") #%>% ggmap() 
gmap = get_map(uk, maptype = "terrain") #, inherit.aes = FALSE

g = ggmap(stamen) +
  geom_point(data=cp, aes(x = lon,y = lat, color = Suitability, size = avgclimsoil)) + 
  scale_color_manual(values = c('Permanently unsuitable' = "gray",
                                'Unsuitable' = 'red',
                                'Moderately suitable' = 'lightgreen',
                                'Suitable' = 'green',
                                'Highly suitable' = 'darkgreen')) +
  ggtitle("Pea suitability map") 

ggsave("Pea map.png", g, dpi = 300, scale = 2.2)  



Cucumber <- list()
#length(climsoil_ID_crops_gridpoints
for (i in 1:length(climsoil_ID_crops_gridpoints)){
  
  Cucumber[[i]] <- 
    filter(climsoil_ID_crops_gridpoints[[i]], name == "Cucumber Tree" & avgclimsoil >= 40)
}

ct <- ldply(Cucumber)
summary(ct$avgclimsoil)
  



#dat <- dat[complete.cases(dat), ]

    #datasf = st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)

    #g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = datasf, aes(col = name))  

    #ggsave(paste("cropno_", " ,".png"),g)



# top data map
#topdata = climsoil_ID_crops_gridpoints_topcrop[complete.cases(climsoil_ID_crops_gridpoints_topcrop),]

#topdata = filter(topdata, avgclimsoil >= 70)
#topdata$name = as.factor(topdata$name)
# library(sp)
# print(topdata)
# coordinates(topdata) <- ~ lon + lat
# st_crs(topdata) = 4326



#ukbnd <- st_transform(datasf, st_crs(ukbnd))


