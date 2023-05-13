library(sf)
library(ggplot2)
ukbnd <- st_read("./in/gadm41_GBR_shp/gadm41_GBR_0.shp")
soildataongrid <- st_read("./soildataongridfromBSTWGS84.gpkg")
options(bitmapType='cairo')


g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = soildataongrid, aes(col = SOIL_TEX))#+
  ggtitle("Texture") #+ theme(legend.position = "none")

ggsave("UK_soil_texture_map.png", g, width = 5000, height = 2000, units = c("px"), dpi = 300, limitsize = F)  



g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = soildataongrid, aes(col = PH_07))#+
ggtitle("pH") #+ theme(legend.position = "none")

ggsave("UK_soil_pH_map.png", g, width = 5000, height = 2000, units = c("px"), dpi = 300, limitsize = F)  



g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = soildataongrid, aes(col = BULKD_07))#+
ggtitle("Bulk Density") #+ theme(legend.position = "none")

ggsave("UK_soil_BD_map.png", g, width = 5000, height = 2000, units = c("px"), dpi = 300, limitsize = F)   



g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = soildataongrid, aes(col = SOIL_DEPTH))#+
ggtitle("Soil Depth") #+ theme(legend.position = "none")

ggsave("UK_soil_Depth_map.png", g, width = 5000, height = 2000, units = c("px"), dpi = 300, limitsize = F)  




g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = soildataongrid, aes(col = SOIL_GROUP.x))#+
ggtitle("Soil group") #+ theme(legend.position = "none")

ggsave("UK_soil_group_map.png", g, width = 5000, height = 2000, units = c("px"), dpi = 300, limitsize = F)  



g = ggplot() + geom_sf(data = ukbnd) + geom_sf(data = soildataongrid, aes(col = CARB_CNTNT))#+
ggtitle("Soil Carbon Content") #+ theme(legend.position = "none")

ggsave("UK_soil_carbonC_map.png", g, width = 5000, height = 2000, units = c("px"), dpi = 300, limitsize = F)  
