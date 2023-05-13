# making a grid across UK
library(sf)
library(ggplot2)

ukbnd <- st_read("./in/gadm41_GBR_shp/gadm41_GBR_0.shp")

# 
#  boundary  <-  st_as_sf(data, coords = c("Latitude", "Longitude"),
#                     crs = 4326)
# 
# https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points

# grid high 

 grid <- ukbnd %>%
   st_make_grid(cellsize = 0.1, what = "centers") #%>% # grid of points
#  st_intersection(DT_sf)
 
# grid low 
 gridlo <- ukbnd %>%
   st_make_grid(cellsize = 0.25, what = "centers") #%>% # grid of points
 
 grid
 ggplot() +
   geom_sf(data=gridlo)

 grid1 = st_intersection(gridlo, ukbnd)
 grid1

 ggplot() +
   geom_sf(data=grid1)

 st_write(grid1, "./in/grid/uk_gridlo543.shp")
 

#slk = st_read("./in/", "slk_grid")
#ggplot() +
#  geom_sf(data=slk)
