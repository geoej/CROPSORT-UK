# this code no longer is used

library(plyr)
library(sf)
library(raster)
library(revgeo)
library(ggplot2)

  #grid <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/grid/uk_gridlo543.shp")
  grid <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/grid/uk_grid.shp")
  
  # for validation now we extracted the grid point data during the 
  # execution of the code so we just need to add the address or 
  # because we have already validated the temp we do manually for rainfall
  
  
  
  #---------------------------------------------------
  #               data load
  #---------------------------------------------------
  
  
  # climate data
  tempdata = stack()
  
  ukmetpath = "/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/ukmet/"
  for (i in c("UKtasJan", "UKtasFeb", "UKtasMar", "UKtasApr", "UKtasMay", "UKtasJun", "UKtasJul",
              "UKtasAug", "UKtasSep", "UKtasOct", "UKtasNov", "UKtasDec")) {
    temprast = raster(paste(ukmetpath,i,".tif",sep=""));
    crs(temprast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
    tempdata = addLayer(tempdata, temprast);
  }
  names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  
  
  # for soil --- deprecated: we used UK data
  phdata = stack()
  sandata = stack()
  claydata = stack()
  
  phpath ="/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx/data/soil/"
  otherpath = "/Users/ej/CFFRC/05-Projects/Bera/Activity 3/Tool/TTSR/soil/Soils/"
  for (o in 1:7) phdata = addLayer(phdata, raster(paste(phpath,"PHIHOX_M_sl",  o, "_250m_ll.tif", sep = "")))
  for (o in 1:7) claydata = addLayer(claydata, raster(paste(otherpath,"CLYPPT_M_sl",  o, "_250m_ll.tif", sep = "")))
  for (o in 1:7) sandata = addLayer(sandata, raster(paste(otherpath,"SNDPPT_M_sl",  o, "_250m_ll.tif", sep = "")))
  depthdata = raster(paste(phpath,"BDRICM_M_250m_ll.tif", sep = ""))

  # getting a subset of the grid to check 
  gridsample <- st_sample(grid, 25, type = "random")
  gridsample <- st_cast(gridsample, "POINT")
  climsoil_gridpoints = list()

  #--------------
  # getting extracting points for each coordinate
  #--------------
  
  for (i in 1:length(gridsample)){
    print(i)
    coords = st_coordinates(gridsample[i])
    sp = SpatialPointsDataFrame(coords, data= as.data.frame(1))
    crs(sp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # Extract soil data
    
    ph = NULL
    clay = NULL
    sand = NULL
    
    for (l in 1:7){
      ph = cbind(ph, as.numeric(extract(phdata[[l]], sp)))
      clay = cbind(clay, as.numeric(extract(claydata[[l]], sp)))
      sand = cbind(sand, as.numeric(extract(sandata[[l]], sp)))
    }
    BDRICM.BDRICM_M = as.numeric(extract(depthdata, sp))
    
    colnames(ph) <- c("PHIHOX.M.sl1","PHIHOX.M.sl2","PHIHOX.M.sl3","PHIHOX.M.sl4","PHIHOX.M.sl5","PHIHOX.M.sl6", "PHIHOX.M.sl7")
    colnames(clay) <- c("CLYPPT.M.sl1","CLYPPT.M.sl2","CLYPPT.M.sl3","CLYPPT.M.sl4","CLYPPT.M.sl5","CLYPPT.M.sl6", "CLYPPT.M.sl7")
    colnames(sand) <- c("SNDPPT.M.sl1","SNDPPT.M.sl2","SNDPPT.M.sl3","SNDPPT.M.sl4","SNDPPT.M.sl5","SNDPPT.M.sl6", "SNDPPT.M.sl7")
    #names(BDRICM.M) <- "BDRICM.BDRICM_M"
    
    out <- cbind(ph, clay, sand, BDRICM.BDRICM_M)
    
    # extract climate data
    utemp <- as.numeric(data.frame(extract(tempdata, sp)))
    names(utemp) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")

    climsoil_gridpoints[[i]] = cbind (as.data.frame(out),as.data.frame(t(utemp)))
    
    climsoil_gridpoints[[i]]$lon = coords[,1]
    climsoil_gridpoints[[i]]$lat = coords[,2]
    climsoil_gridpoints[[i]]$address = revgeo(longitude=coords[,1], 
                                       latitude=coords[,2], 
                                       provider = 'google', 
                                       API = "AIzaSyB9TaclZkGh1FEXN-bH2uE_Dc4__op7IuE",
                                       output= NULL )[[1]][1]

  }
  
  results = ldply(climsoil_gridpoints)
  results = results[complete.cases(results), ]
  write.csv(results, "ClimSoilGridData.csv")
  
  # extracting data fro BGS soil layer
  
  # already made a shapefil with the above sample data
  gridsamples <- st_read("./in/grid/GridSampleOutput.shp")
  bgs <- st_read("./in/bgs/SPMM_1km/SoilParentMateriall_V1_portal1km.shp")
  gridsamples <- st_transform(gridsamples, crs = st_crs(bgs))

  ggplot() + geom_sf(data = gridsamples, aes(fill = field_1))
  
  library(stars)
  test <- st_intersects(bgs, gridsamples)
  
  
#------
