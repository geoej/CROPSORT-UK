nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
nc_3857 <- st_transform(nc, 3857)

map <- get_map("north carolina", maptype = "satellite", zoom = 6, source = "google")

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- ggmap_bbox(map)

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = nc_3857, aes(fill = AREA), inherit.aes = FALSE)
