library(plyr)
library(dplyr)

climsoil_ID_crops_gridpoints = readRDS("climsoil_ID_crops_gridpoints")
nona <- list()

#--------------
# cutting data to suitability classes
#--------------

for (i in 1:length(climsoil_ID_crops_gridpoints)){
  cw <- climsoil_ID_crops_gridpoints[[i]]
  if (!all(is.na(cw$avgclimsoil))) {
    cw$Suitability <- cut(cw$avgclimsoil,
                          breaks=c(0,20,40,60,80,100),
                          labels=c('Permanently unsuitable', 'Unsuitable', 'Moderately suitable', 'Suitable', 'Highly suitable'))
  }
  nona[[i]] <- cw
}

Permanently_unsuitable = list()
Unsuitable = list()
Moderately_suitable = list()
Suitable = list()
Highly_suitable = list()

for (i in 1:length(nona)){
  if (!is.null(nona[[i]]$Suitability)){
    Permanently_unsuitable[[i]] <- filter(nona[[i]], Suitability == 'Permanently unsuitable')
    Unsuitable[[i]] <- filter(nona[[i]], Suitability == 'Unsuitable')
    Moderately_suitable[[i]] <- filter(nona[[i]], Suitability == 'Moderately suitable')
    Suitable[[i]] <- filter(nona[[i]], Suitability == 'Suitable')
    Highly_suitable[[i]] <- filter(nona[[i]], Suitability == 'Highly suitable')
  }

}  

Permanently_unsuitable = ldply(Permanently_unsuitable)
Unsuitable = ldply(Unsuitable)
Moderately_suitable = ldply(Moderately_suitable)
Suitable = ldply(Suitable)
Highly_suitable = ldply(Highly_suitable)


write.csv(Permanently_unsuitable, "Permanently_unsuitable.csv", row.names = F)
write.csv(Unsuitable, "Unsuitable.csv", row.names = F)
write.csv(Moderately_suitable, "Moderately_suitable.csv", row.names = F)
write.csv(Suitable, "Suitable.csv", row.names = F)
write.csv(Highly_suitable, "Highly_suitable.csv", row.names = F)

allsuitable <- as.data.frame(qpcR:::cbind.na(
  levels(as.factor(Moderately_suitable$name)),
  levels(as.factor(Suitable$name)),
  levels(as.factor(Highly_suitable$name))
))

names(allsuitable) <- c("Moderately_suitable", "Suitable", "Highly_suitable")
write.csv(allsuitable, 'allsuitable.csv')

#--------------
# getting top 5
#-------------

top5 <- list()
#length(climsoil_ID_crops_gridpoints
for (i in 1:length(climsoil_ID_crops_gridpoints)){
  
  top5[[i]] <- climsoil_ID_crops_gridpoints[[i]] %>%
    arrange(desc(avgclimsoil)) %>%
    slice_head(n = 5)
}



topcrops = ldply(top5)


result = topcrops %>%
  filter(avgclimsoil >= 70) %>%
  group_by(name) %>%
  summarise(mean = mean(avgclimsoil), n = n(), percent = round(n/length(climsoil_ID_crops_gridpoints)*100,1)) 
  

# getting the scientific name
crecords <- read.csv("/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/ecology/Crop_Records.csv")

results <- left_join(result, crecords, by = c("name" = "Name"))
write.csv(results, "stats_top5.csv")

# adding the crop classification 

results <- read.csv("stats_top5.csv")
class <- read.csv("./in/ecology/Crop_Classification.csv")

resultss <- left_join(results, class, by = c("name" = "Crop.ID"))

library(dplyr)
a = resultss %>%
  group_by(name) %>%
  summarise(allclasses = paste(Crop.Classification, Notes.y, collapse=", "))

final_results <- left_join(results, a)
write.csv(final_results, "stats_top5_classes.csv")



#-----------------
# for validation of agroclimatic limits
#----------------

results <- read.csv("stats.csv")
ecology_Jul20 <- read_csv("in/ecology/ecology Jul20.csv")
res_plus_ecology <- left_join(results, ecology_Jul20, by = c("Crop.ID" = "id"))
library(readxl)
validation <- read_excel("in/ecology/validation.xlsx")
res_plus_ecology_wheatpea <- bind_rows(res_plus_ecology, validation)
write_csv(res_plus_ecology_wheatpea, "res_plus_ecology_validation.csv")
write_excel_csv(res_plus_ecology_wheatpea, "res_plus_ecology_validation.csv")


#---------------
# getting result for all crops
#--------------

moderatetohighall = ldply(climsoil_ID_crops_gridpoints)

result = moderatetohighall %>%
  filter(avgclimsoil >= 40) %>%
  group_by(name) %>%
  summarise(mean = mean(avgclimsoil), n = n(), percent = round(n/length(climsoil_ID_crops_gridpoints)*100,1)) 

result = distinct(result)


#-------------------
# For each crop what is the average suitabitliy?

