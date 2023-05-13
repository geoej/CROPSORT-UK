
#list.of.packages <- c("raster","rgdal", "maptools","RMySQL","rjson","sp","dplyr","jsonlite", "RCurl", "tibble  ")
#pckList <- lapply(list.of.packages, require, character.only = TRUE)
f <- function (ngrid, ncrop) {
    
  
  library(sf)
  #grid <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/grid/uk_gridlo543.shp")
  # grid <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/grid/uk_grid.shp")
  # 
  # topsoilbdph <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/bgs/topsoilpH_BD_metadata/data/CS_topsoil_pH_bulkDensity.shp")
  # parentmaterial <- st_read("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/bgs/SPMM_1km/SoilParentMateriall_V1_portal1km.shp")
  # 
  # grid_OSGB36 <- st_transform(grid, st_crs(topsoilbdph))
  # 
  # phbd <- st_intersection(topsoilbdph, grid_OSGB36)
  # 
  # parentm <- st_intersection(parentmaterial, grid_OSGB36)
  # 
  # soildataongrid <- st_join(phbd,parentm)
  
  #soildataongridWGS84 <- st_transform(soildataongrid, 4326)
  # 
  # st_write(soildataongridWGS84, "./soildataongridfromBSTWGS84.gpkg")
  
  
  #get grid
  # we are getting the grid from the soil data that was created BGS
  soildataongrid <- st_read("./soildataongridfromBSTWGS84.gpkg")
  
  
  #---------------------------------------------------
  #               data load
  #---------------------------------------------------
  library(raster)
  
  # climate data
  tempdata = stack()
  raindata = stack()
  
  ukmetpath = "/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/ukmet/"
  for (i in c("UKtasJan", "UKtasFeb", "UKtasMar", "UKtasApr", "UKtasMay", "UKtasJun", "UKtasJul",
              "UKtasAug", "UKtasSep", "UKtasOct", "UKtasNov", "UKtasDec")) {
    temprast = raster(paste(ukmetpath,i,".tif",sep=""));
    crs(temprast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
    tempdata = addLayer(tempdata, temprast);
  }
  names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  
  
  for (i in c("UKrainfallJan", "UKrainfallFeb", "UKrainfallMar", "UKrainfallApr", "UKrainfallMay", "UKrainfallJun", "UKrainfallJul",
              "UKrainfallAug", "UKrainfallSep", "UKrainfallOct", "UKrainfallNov", "UKrainfallDec")) {
    rainrast = raster(paste(ukmetpath,i,".tif",sep=""));
    crs(rainrast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
    raindata = addLayer(raindata, rainrast);
  }
  names(raindata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  
  # data from DB
  ecology <- read.csv("./in/ecology/ecology Jul20.csv")
  

  #---------------------------------------------------
  #               running suitability
  #---------------------------------------------------
    
  climSoil_ID_crops = data.frame(CROP_ID = NA,
                                 Jan= NA,
                                 Feb= NA, 
                                 Mar= NA, 
                                 Apr= NA, 
                                 May= NA, 
                                 Jun= NA, 
                                 Jul= NA,  
                                 Aug= NA, 
                                 Sep= NA, 
                                 Oct= NA,  
                                 Nov= NA, 
                                 Dec= NA,  
                                 MaxMonths= NA, 
                                 averagesoilsuit= NA, 
                                 name=NA, 
                                 avgclimsoil=NA, 
                                 lon=NA, 
                                 lat=NA)
  
  climdatum = data.frame(TJan= NA, TFeb= NA, TMar= NA, TApr= NA, TMay= NA, TJun= NA, TJul= NA, TAug= NA, TSep= NA, TOct= NA, TNov= NA, TDec= NA,
                        RJan= NA, RFeb= NA, RMar= NA, RApr= NA, RMay= NA, RJun= NA, RJul= NA, RAug= NA, RSep= NA, ROct= NA, RNov= NA, RDec= NA,
                        lon=NA, 
                        lat=NA)
  
  climsoil_ID_crops_gridpoints = list()
  climsoil_ID_crops_gridpoints_topcrop = data.frame()
  
  #--------------
  # getting extracting points for each coordinate
  #--------------
  
  for (i in 1:ngrid){
    print(i)
    coords = st_coordinates(soildataongrid$geom[i])
    sp = SpatialPointsDataFrame(coords, data= as.data.frame(1))
    crs(sp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
    # Extract soil data
    # soil data is already extracted and includes in the grid. 
    
    # extract climate data
    utemp <- as.numeric(data.frame(extract(tempdata, sp)))
    urain <- as.numeric(data.frame(extract(raindata, sp)))
    climdata <- cbind(utemp, urain)
    names(climdata) <- c("TJan", "TFeb", "TMar", "TApr", "TMay", "TJun", "TJul", "TAug", "TSep",  "TOct", "TNov", "TDec",
                         "RJan", "RFeb", "RMar", "RApr", "RMay", "RJun", "RJul", "RAug", "RSep",  "ROct", "RNov", "RDec")
    
    climdata$lon = coords[1,1]
    climdata$lat = coords[1,2]
    climdatum = rbind(climdatum, climdata)
    
    #print(utemp)
  
    
    # calculate the seasonal suitability for average seasonal temperature for n crops
    
    if (any(is.nan(utemp))){ # | any((is.na(out)))
      climsoil_ID_crops_gridpoints[[i]] <- NaN
      climsoil_ID_crops_gridpoints_topcrop  =  rbind(climsoil_ID_crops_gridpoints_topcrop , data.frame(CROP_ID = NA,
                                                                                                       Jan= NA,
                                                                                                       Feb= NA, 
                                                                                                       Mar= NA, 
                                                                                                       Apr= NA, 
                                                                                                       May= NA, 
                                                                                                       Jun= NA, 
                                                                                                       Jul= NA,  
                                                                                                       Aug= NA, 
                                                                                                       Sep= NA, 
                                                                                                       Oct= NA,  
                                                                                                       Nov= NA, 
                                                                                                       Dec= NA,  
                                                                                                       MaxMonths= NA, 
                                                                                                       averagesoilsuit= NA, 
                                                                                                       name=NA, 
                                                                                                       avgclimsoil=NA, 
                                                                                                       lon=NA, 
                                                                                                       lat=NA))
    }else{
      #tecocrop = numeric()
      #recocrop <- numeric()
  
      seasons = numeric()
      
      phsuits = numeric()
      depthsuits = numeric()
      txtursuits = numeric()
      
      #totalsoilsuit = numeric()
      averagesoilsuit = numeric()
      
      #TCS = data.frame()
      TCSave = data.frame()
      #TCnorains = data.frame()
      climSoil_ID_crops = data.frame()
      
      #-----------------
      # run for all crops 
      #----------------
      
      for (k in 1:ncrop){ #nrow(ecology)){
        
        # getting approximate season
        
        season = round ((ecology[k,"season_length_min"] + ecology[k,"season_length_max"])/60)
        seasons = append(seasons, season)
        
        #---------------------------------------------------
        #               climate suitability
        #---------------------------------------------------
        
        tmonsuit = numeric()
        tcropsuit = numeric()
        tcropsuitpren = numeric()
        
        raindataggreg = numeric()
        raindatapren = numeric()
        rainsuit = numeric()  
        
        phsuit = numeric()  
        depthsuit = numeric()
        texturesuit = numeric()
        
        #totalsuit = numeric()
        #totalsuitave = numeric()
        #totalsoilsuit = numeric()
        totalclimsuitave = numeric()
        
        for (j in 1:12){
          x = utemp[j]
          tabs_min=ecology[k,"temperature_absolute_min"] 
          topt_min=ecology[k,"temperature_optimal_min"] 
          topt_max=ecology[k,"temperature_optimal_max"] 
          tabs_max=ecology[k,"temperature_absolute_max"]
          #print(paste0("i = ", i,", k = ", k, " tabs_min = ", tabs_min))
          if (x < tabs_min) {y = 0}
          if ((x >= tabs_min) & (x < topt_min)) {y = ((x - tabs_min)/(topt_min - tabs_min))}
          if ((x>= topt_min) & (x <topt_max))   {y=1}
          if ((x >= topt_max) & (x <tabs_max))   {y  = 1-( (x - topt_max)/(tabs_max - topt_max))} 
          if (x >= tabs_max) {y=0}
          tmonsuit= append(tmonsuit, as.numeric(y))  #CHAN test2
        }
        #teco = cbind (tabs_min,topt_min,topt_max,tabs_max)
        #tecocrop = rbind(tecocrop, teco)
        names(tmonsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        #assign(paste("tmonsuit",i, sep="") , tmonsuit)
        #}
        
        # control the quality
        # tmonsuits=numeric()
        # for (i in 1:nrow(ecology)){
        #   tmonsuits = rbind(tmonsuits,get(paste("tmonsuit",i,sep="")))
        # }
        # round(tmonsuits, digits =1)
        
        # GETTING THE SEASONS RIGHT 
        # average the temp for that crop to be used for temp suitability
        #for (j in 1:nrow(ecology)){
        
        #tmonsuit <- get(paste("tmonsuit",j, sep=""))
        
        raindataggreg = numeric()
        raindatapren = numeric()
        
        if (season <= 1) {
          tcropsuit = tmonsuit
          raindataggreg = urain
        }
        
        if ((season < 12) & (season > 1)) {
          for (m in 1:(12-(season-1))){
            tcropsuit = append(tcropsuit, min(tmonsuit[m:(m+(season-1))]))
            raindataggreg = append(raindataggreg, sum(as.numeric(urain[m:(m+(season-1))])))
          }
          
          #creating a stack for the months that fall over dec
          tcropsuitpren = c(tmonsuit[(12-(season-2)):12], tmonsuit[1:(season-1)])
          raindatapren = c(urain[(12-(season-2)):12], urain[1:(season-1)])
  
          # adding the aggreages for the rest of the year
          for (n in 1:(length(tcropsuitpren)-(season-1))){
            tcropsuit = append(tcropsuit, min(tcropsuitpren[n:(n+(season-1))]))
            raindataggreg = append(raindataggreg, sum(as.numeric(raindatapren[n:(n+(season-1))])))
          }
        }
        
        if (season >= 12) {                           #CHAN test2
          # to calcuate for the prennials all layers are the same
          for (n in 1:12){
            tcropsuit = append(tcropsuit, min(tmonsuit))
            raindataggreg = append(raindataggreg, sum(as.numeric(urain[1:12])))
          }
        }
        tcropsuit = tcropsuit[1:12]
        names(tcropsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        names(raindataggreg) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        
        #assign(paste("tcropsuit",j, sep="") , tcropsuit)
        #}

        # control the quality
        # tcropsuits=numeric()
        # for (i in 1:nrow(ecology)){
        #   tcropsuits = rbind(tcropsuits,get(paste("tcropsuit",i,sep="")))
        #   #print(get(paste("tcropsuit",i,sep="")))
        # }
        # round(tcropsuits, digits = 1)

        # calcualte rain suitability

        for (j in 1:12){
           x <- raindataggreg[j]
           rabs_min=ecology[k,"rainfall_absolute_min"]
           ropt_min=ecology[k,"rainfall_optimal_min"]
           ropt_max=ecology[k,"rainfall_optimal_max"]
           rabs_max=ecology[k,"rainfall_absolute_max"]
           if (x < rabs_min) {y = 0}
           if ((x >= rabs_min) & (x < ropt_min)) {y = ((x - rabs_min)/(ropt_min - rabs_min))}
           if ((x >= ropt_min) & (x < ropt_max)) {y=1}
           if ((x >= ropt_max) & (x < rabs_max)) {y  = (1-( (x - ropt_max)/(rabs_max - ropt_max)))}
           if (x >= rabs_max) {y = 0}
           rainsuit= append(rainsuit, y)
         }
        
  
        #TCnorains = rbind(TCnorains, tcropsuit*100)
        
        
        #names(TCnorains) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        
        TCSave = rbind(TCSave, rowMeans(cbind(rainsuit, tcropsuit)*100))
        names(TCSave) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        
        
        #---------------------------------------------------
        #               soil suitability
        #---------------------------------------------------
        
        ########################################################
        # calcualte PH suitability
        ########################################################
        
        #for (k in 1:nrow(ecology)){
        #phsuit = numeric()
        phsuitfun = function(x) {
          y=0
          phabs_min=ecology[k,"soil_ph_absolute_min"] 
          phopt_min=ecology[k,"soil_ph_optimal_min"] 
          phopt_max=ecology[k,"soil_ph_optimal_max"] 
          phabs_max=ecology[k,"soil_ph_absolute_max"]
          if (any(x > phabs_min) & any(x < phopt_min))  {y = round(((x - phabs_min)/(phopt_min - phabs_min))*100)}
          if (any(x > phopt_min) & any(x < phopt_max))  {y  = 100}
          if (any(x > phopt_max) & any(x < phabs_max))  {y  = round((1-( (x - phopt_max)/(phabs_max - phopt_max)))*100)}
          if (any(x < phabs_min) | any(x > phabs_max))  {y = 0}
          return(y)
        }  
        

        phagg = soildataongrid$PH_07[i]
        phsuit <- lapply(phagg, phsuitfun )
        phsuits = append(phsuits, phsuit)
        
        ########################################################
        # calcualte depth suitability
        ########################################################
        
        depthsuitfun = function(x) {
          optz=10
          if (x == "SHALLOW") {
            if (ecology[k,"soil_depth_optimal_low"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          if (x == "INTERMEDIATE-SHALLOW" | x == "INTERMEDIATE" | x == "DEEP-INTERMEDIATE") {
            if (ecology[k,"soil_depth_optimal_medium"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          if (x == "DEEP" ) {
            if (ecology[k,"soil_depth_optimal_deep"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          
          if (x == "NA") {
            optz = 10
          }
          
          return(optz)
        }  
        
        depthsuit <- lapply(soildataongrid$SOIL_DEPTH[i], depthsuitfun )
        depthsuits = append(depthsuits, depthsuit)

        ########################################################
        # calculate the soil texture suitability for the selected crops
        ########################################################
        
        textsuitfun = function(x) {
          optz=10
          if (grepl("LIGHT", x)) {
            if (ecology[k,"soil_texture_optimal_light"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          if (grepl("MEDIUM", x)) {
            if (ecology[k,"soil_texture_optimal_medium"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          if (grepl("HEAVY", x)) {
            if (ecology[k,"soil_texture_optimal_heavy"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          
          if (x == "NA") {
            optz = 10
          }
          
          return(optz)
        }  
        
        txtursuit <- lapply(soildataongrid$SOIL_GROUP.y[i], textsuitfun )
        txtursuit <- as.numeric(txtursuit)
        txtursuits = append(txtursuits, txtursuit)
        
        
        
        #txtursuit <- as.numeric(texturesuit)
        #txtursuits <- append(txtursuits, txtursuit)
        #names(texturesuit) <- c(sprintf("texturesuit",seq(1:k)))
        
        #totalsoilsuit= append (totalsoilsuit,(as.numeric(phsuit) * as.numeric(depthsuit) * as.numeric(txtursuits))/10000)
        averagesoilsuit=append(averagesoilsuit, sum(0.6*as.numeric(phsuit) , 0.2*as.numeric(depthsuit) , 0.2*as.numeric(txtursuit)))
        
        
      #}
      
      ########################################################
      # calculate total  suitability 
      ########################################################
  
      clim_suit <- cbind(CROP_ID = ecology[k,2],TCSave[k,], 
                                MaxMonths = max(c(as.numeric(TCSave[k,]))))
      
      avgTotSoilSuit=cbind.data.frame( cropid = ecology$id[k] , averagesoilsuit=averagesoilsuit[k], name = ecology$name[k])
      
      # to enable the print of cropid as numeric
      options(scipen = 50)
      climSoil <- as.data.frame(cbind(clim_suit,avgTotSoilSuit))
      climSoil$avgclimsoil <- mean(c(climSoil$MaxMonths, climSoil$averagesoilsuit))
  
      # remove cropid and keep CROP_ID
      climSoil_ID <- subset(climSoil, select = -c(cropid) )
      # now aggragating the resuls for all the crops
      climSoil_ID$lon = coords[1,1]
      climSoil_ID$lat = coords[1,2]
      climSoil_ID_crops = rbind(climSoil_ID_crops, climSoil_ID)
      }
  
  }
  
    # now aggregating results for all points
    
    climsoil_ID_crops_gridpoints[[i]] <- climSoil_ID_crops
    
    #print(paste0("i = ", i, ", ncol(climsoil_ID_crops_gridpoints_topcrop) = ",ncol(climsoil_ID_crops_gridpoints_topcrop)))
    climsoil_ID_crops_gridpoints_topcrop = rbind(climsoil_ID_crops_gridpoints_topcrop,
                    climSoil_ID_crops[which(climSoil_ID_crops$avgclimsoil == max(climSoil_ID_crops$avgclimsoil,na.rm = T))[1],])
    
  #  print(paste("i =", i, ",k =", k, "lat = ", climsoil_ID_crops_gridpoints_topcrop$lat[i],
  #              "lon = ", climsoil_ID_crops_gridpoints_topcrop$lon[i]))
  }
  
  saveRDS(climsoil_ID_crops_gridpoints, "climsoil_ID_crops_gridpoints")
  write.csv(climsoil_ID_crops_gridpoints_topcrop, "climsoil_ID_crops_gridpoints_topcrop.csv")
  write.csv(climdatum, "climdatum.csv")
    

  # topdata = climsoil_ID_crops_gridpoints_topcrop[complete.cases(climsoil_ID_crops_gridpoints_topcrop),]
  # topdata$name = as.factor(topdata$name)
  # library(sp)
  # print(topdata)
  # coordinates(topdata) <- ~ lon + lat
  # datasf = st_as_sf(topdata, coords = c("lon", "lat"), crs = 4326)
  # 
  # library(ggplot2)
  # ggplot() + geom_sf(data = datasf, aes(col = name, size = avgclimsoil))
  # ggsave("preliminarymap.png")

}

f(ngrid = 2862, ncrop = 1842)
#f(ngrid = 15, ncrop = 10)
