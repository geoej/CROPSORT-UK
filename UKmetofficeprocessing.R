library(ncdf4)
library(raster)
nc <- nc_open("./in/ukmet/tas_hadukgrid_uk_12km_mon-30y_199101-202012.nc")
#attributes(nc$var)
tas <- ncvar_get(nc, "tas")
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
t <- ncvar_get(nc, "time")
fillvalue <- ncatt_get(nc, "tas", "_FillValue")
tas[tas==fillvalue$value] <- NA
nc_close(nc)

mon = 0
for (i in c("UKtasJan", "UKtasFeb", "UKtasMar", "UKtasApr", "UKtasMay", "UKtasJun", "UKtasJul",
            "UKtasAug", "UKtasSep", "UKtasOct", "UKtasNov", "UKtasDec")){
  mon = mon + 1
  data <- tas[, , mon] 
  r <- raster(t(data), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction='y')
  writeRaster(r, paste0("./in/ukmet/",i), "GTiff", overwrite=TRUE)
}



#-----------
# for rainfall
#-----------
nc <- nc_open("./in/ukmet/rainfall_hadukgrid_uk_12km_mon-30y_199101-202012.nc")
#attributes(nc$var)
rainfall <- ncvar_get(nc, "rainfall")
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
t <- ncvar_get(nc, "time")
fillvalue <- ncatt_get(nc, "rainfall", "_FillValue")
rainfall[rainfall==fillvalue$value] <- NA
nc_close(nc)

mon = 0
for (i in c("UKrainfallJan", "UKrainfallFeb", "UKrainfallMar", "UKrainfallApr", "UKrainfallMay", "UKrainfallJun", "UKrainfallJul",
            "UKrainfallAug", "UKrainfallSep", "UKrainfallOct", "UKrainfallNov", "UKrainfallDec")){
  mon = mon + 1
  data <- rainfall[, , mon] 
  r <- raster(t(data), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction='y')
  writeRaster(r, paste0("./in/ukmet/",i), "GTiff", overwrite=TRUE)
}







#r <- rotate(r)
#plot(r)



