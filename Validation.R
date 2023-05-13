library(tidyverse)
library(sp)
library(sf)
library(tmap)

pts <- points[c(-469802,-410728,-345044,-325272,-61035, -91622),]
tb.pts <- as_tibble(pts)
tb.pts$croplevles= as_factor(tb.pts$cropname)

tb.pts$numeric <- as.numeric(tb.pts$croplevles)

tb.pts<- tb.pts %>% filter(lon > 6 & lon < 18 & lat > 36.6 & lat < 47)

tb.pts.narm <- tb.pts %>% filter(complete.cases(.))

coordinates (tb.pts.narm) <- ~ lon + lat

tb.pts.narm.sf <- st_as_sf(tb.pts.narm)

# bam = tb.pts.narm.sf %>% filter(cropname == "Bambara Groundnut")
# st_as_text(bam)
# plot(bam)

# tmap_mode("plot")
# 
# tm_shape(tb.pts.narm.sf) +
#   tm_bubbles("numeric")

library(ggmap)

register_google(key = "AIzaSyAJhmRdeFyKwdt1JWEgMMQP35mRb5gb3JM")

map <- get_map(location = "Italy", source = "osm")
               
               ,zoom = 2,maptype = "terrain",
               color = "color",source = "google")
ggmap(map, 
      base_layer = ggplot(data = tb.pts.narm, aes(x = lon, y = lat))) + 
  geom_point(aes(fill = cropname, col = cropsuit), alpha = 0.7, size = 2)




