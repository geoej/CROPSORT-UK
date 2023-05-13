library(readr)
library(dplyr)
library(sf)
#library(raster)
library(ggplot2)
library(modeest)
library(tidyverse)
library(terra)

gbif <- read_delim("in/gbif/0124756-220831081235567.csv", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE)
GBIF_UK_Wheat <- read_delim("in/gbif/GBIF UK Wheat.csv", 
                                          delim = "\t", escape_double = FALSE, 
                                          trim_ws = TRUE)

gbifuk <- GBIF_UK_Wheat %>% dplyr::select("decimalLongitude", "decimalLatitude")%>% na.omit


#library(sf)
#epsg https://spatialreference.org/ref/epsg/wgs-84/
a = st_as_sf(gbifuk, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#st_write(a, "./in/gbif/shapegbifuk.shp")

# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 3 * IQR(x) | x > quantile(x, 0.75) + 3 * IQR(x))
# }

b = gbif[, c("elevation")]
b= b[complete.cases(b),]
# outliers = which(is_outlier(unlist(b)) == TRUE)
# b=b[-outliers,]
# hist(unlist(b))

#library(ggplot2)
#library(modeest)
mod = mlv(unlist(b),method = "parzen", kernel = "gaussian")

median(unlist(b))
p <- ggplot(b, aes(x = elevation)) + geom_density(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mod), # 432.8859
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=min(unlist(b))), #0  
             color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=quantile(unlist(b), 0.10)), #218     
             color="red", linetype="dashed", size=0.5) + 
  geom_vline(aes(xintercept=quantile(unlist(b), 0.90)), #700.9 
             color="red", linetype="dashed", size=0.5) + 
  geom_vline(aes(xintercept=max(unlist(b))), # 1693.5 
             color="red", linetype="dashed", size=1) +
  ylab("density of GBIF observations") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
p
ggsave(paste0('elvation_limits.png'))


# extracting data for other environmental variables
#------------------
# get the temp data
#-----------------

#tempdata = stack()

ukmetpath = "/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/ukmet/"
# for (i in c("UKtasJan", "UKtasFeb", "UKtasMar", "UKtasApr", "UKtasMay", "UKtasJun", "UKtasJul",
#             "UKtasAug", "UKtasSep", "UKtasOct", "UKtasNov", "UKtasDec")) {
#   temprast = raster(paste(ukmetpath,i,".tif",sep=""));
#   crs(temprast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
#   tempdata = addLayer(tempdata, temprast);
# }
# names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")


# teraa
temprast = character()
for (i in c("UKtasJan", "UKtasFeb", "UKtasMar", "UKtasApr", "UKtasMay", "UKtasJun", "UKtasJul",
            "UKtasAug", "UKtasSep", "UKtasOct", "UKtasNov", "UKtasDec")) {
  temprast = append(temprast, paste(ukmetpath,i,".tif",sep=""));
}
tempdata = rast(temprast)
names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")

a = st_read("./in/gbif/shapegbifuk.shp")
ukbnd <- st_read("./in/gadm41_GBR_shp/gadm41_GBR_0.shp")

options(bitmapType='cairo')
# plot(ukbnd["COUNTRY"])
# plot(a, add = T)



utemp <- list()
a1 <- vect(a)
for (i in 1:12){
  utemp[[i]] <- terra::extract(tempdata[[i]], a)
} 


ecology <- read.csv("./in/ecology/ecology Jul20.csv")
wheateco = filter(ecology, name == "Common Wheat")



#plot(a_transf["elevation"], add = T)
#st_crs(a) == crs(tempdata)

# b = gbif[, c("elevation")]
# b= b[complete.cases(b),]
# outliers = which(is_outlier(unlist(b)) == TRUE)
# b=b[-outliers,]
# hist(unlist(b))
for (i in 1:12) {
  
  b = utemp[[i]] %>% na.omit
  b = b[,2]
  mod = mlv(b, method = "parzen", kernel = "gaussian")
  median(b)
  b = as.data.frame(b)
  
  # density plot with the data statistics
  # p <- ggplot(b, aes(x = b)) + geom_density(color="darkblue", fill="lightblue") +
  #   geom_vline(aes(xintercept=mod), # 432.8859
  #              color="blue", linetype="dashed", size=1) +
  #   geom_vline(aes(xintercept=min(unlist(b))), #0  
  #              color="red", linetype="dashed", size=1) + 
  #   geom_vline(aes(xintercept=quantile(unlist(b), 0.10)), #218     
  #              color="red", linetype="dashed", size=0.5) + 
  #   geom_vline(aes(xintercept=quantile(unlist(b), 0.90)), #700.9 
  #              color="red", linetype="dashed", size=0.5) + 
  #   geom_vline(aes(xintercept=max(unlist(b))), # 1693.5 
  #              color="red", linetype="dashed", size=1) +
  #   ylab("density of GBIF observations") +
  #   xlab(names(utemp[[1]])[2]) +
  #   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
  # p
  # ggsave(paste0('elvation_limits.png'))
  
  # density plot with ecology data
  p <- ggplot(b, aes(x = b)) + 
    geom_density(color="darkblue", fill="lightblue") +
    #geom_vline(aes(xintercept=mod), # 432.8859
    #           color="blue", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=wheateco[,"temperature_absolute_min"]), #0  
               color="red", linetype="dashed", size=1) + 
    geom_vline(aes(xintercept=wheateco[,"temperature_optimal_min"]), #218     
               color="green", linetype="dashed", size=0.5) + 
    geom_vline(aes(xintercept=wheateco[,"temperature_optimal_max"]), #700.9 
               color="green", linetype="dashed", size=0.5) + 
    geom_vline(aes(xintercept=wheateco[,"temperature_absolute_max"]), # 1693.5 
               color="red", linetype="dashed", size=1) +
    ylab("GBIF Density") +
    xlab(names(utemp[[i]])[2]) +
    geom_text(x=10, y=0.75, label="Marginal", col = "red") +
    geom_text(x=19, y=0.75, label="Optimal", col = "green") +
    geom_text(x=25, y=0.75, label="Marginal", col = "red") +
    annotate("rect", xmin = 5, xmax = 15, ymin = 0, ymax = 1.25,
             alpha = .1,fill = "red") +
    annotate("rect", xmin = 15, xmax = wheateco[,"temperature_optimal_max"], ymin = 0, ymax = 1.25,
             alpha = .1,fill = "green") +
    annotate("rect", xmin = 23, xmax = wheateco[,"temperature_absolute_max"], ymin = 0, ymax = 1.25,
             alpha = .1,fill = "red") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
  #p
  ggsave(paste0('temperature_limits_', names(utemp[[i]])[2],'.png'), p, width = 1000, dpi = 300, units = c("px") )

}
