library(terra)
library(sf)

phdata = stack()
sandata = stack()
claydata = stack()

phpath ="/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx/data/soil/"
otherpath = "/Users/ej/CFFRC/05-Projects/Bera/Activity 3/Tool/TTSR/soil/Soils/"
for (i in 1:7) phdata = addLayer(phdata, raster(paste(phpath,"PHIHOX_M_sl",  i, "_250m_ll.tif", sep = "")))
for (i in 1:7) claydata = addLayer(claydata, raster(paste(otherpath,"CLYPPT_M_sl",  i, "_250m_ll.tif", sep = "")))
for (i in 1:7) sandata = addLayer(sandata, raster(paste(otherpath,"SNDPPT_M_sl",  i, "_250m_ll.tif", sep = "")))
depthdata = raster(paste(phpath,"BDRICM_M_250m_ll.tif", sep = ""))


ukbnd <- st_read("./in/gadm41_GBR_shp/gadm41_GBR_0.shp")

phdata = mask(crop(phdata,extent(ukbnd)), ukbnd)
sandata = mask(crop(sandata,extent(ukbnd)), ukbnd)
claydata = mask(crop(claydata,extent(ukbnd)), ukbnd)
depthdata = mask(crop(depthdata,extent(ukbnd)), ukbnd)

outpath = "/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/soilgrids/"
for (i in c(1:12)){
  writeRaster(phdata[[i]], paste0(outpath, names(phdata[[i]]),"_uk"), format = "GTiff",overwrite=TRUE)
  writeRaster(sandata[[i]], paste0(outpath, names(phdata[[i]]),"_uk"), format = "GTiff",overwrite=TRUE)
  writeRaster(claydata[[i]], paste0(outpath, names(phdata[[i]]),"_uk"), format = "GTiff",overwrite=TRUE)
  writeRaster(depthdata, paste0(outpath, names(phdata[[i]]),"_uk"), format = "GTiff",overwrite=TRUE)
  
  
  
  }

