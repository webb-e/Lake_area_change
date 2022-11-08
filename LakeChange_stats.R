library(rgeos)
library(sp)
library(rgdal)
library(sf)
library(readxl)
library(tidyverse)
library(spatialEco)
library(stringr)
library(nnet)
library(car)
library(MASS)
library(coin)

#pc
setwd("...")


###############
##### DATA SET UP
###############
rain<-raster("ClimateTifs/Rain_1981-2020.tif")
snow<-raster("ClimateTifs/snowfall_1981-2020.tif")
temp<-raster("ClimateTifs/annualtemp_81-20.tif")
precip<-raster("ClimateTifs/Total_precip_81_21.tif")

permafrost<-st_read("Brownetal2002/permaice.shp")

data<-read_excel("LakeChangeData.xlsx", sheet = "Sorted by Location")


##### make variables factors
data<-data %>% drop_na(c("net")) 
data<-as.data.frame(data)

data$permafrost<-recode_factor(data$permafrost, continuous="Continuous", discontinuous="Discontinuous")
data$net<-as.factor(data$net)
data$net <- relevel(data$net, ref = "no trend")

#############################################
#####################
### CLIMATE VARIABLE SECTION
#####################
#############################################
xy<-data[,c("longitude", "latitude")]
points<-SpatialPointsDataFrame(coords=xy, data=data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

rstack<-stack(rain, snow, temp, precip)
values <-raster::extract(rstack,points)
values<-as.data.frame(values)
colnames(values)<-c("rain", "snow", "temp","precip")

data$rain<-values$rain 
data$snow<-values$snow
data$temp<-values$temp
data$precip<-values$precip

# percentage increasing/decreasing
nrow(data[data$snow<0,])/nrow(data)
nrow(data[data$rain>0,])/nrow(data)
nrow(data[data$precip<0,])/nrow(data)
nrow(data[data$precip>0,])/nrow(data)

## how many study sites with decreasing temp?
nrow(data[data$temp<0,])

################ CHI-SQUARED TESTS

### first have to turn precip variables into categorical
data$precipdirection<-ifelse(data$precip<0,"decreasing","increasing")
data$raindirection<-ifelse(data$rain<0,"decreasing","increasing")
data$snowdirection<-ifelse(data$snow<0,"decreasing","increasing")

#### are directional trends in precip related to trends in lake area change?

## annual precip
chisq_test(table(data$net, data$precipdirection))
#chi-squared = 1.8062, df = 2, p-value = 0.4053

## snow
chisq_test(table(data$net, data$snowdirection))
#chi-squared = 3.303, df = 2, p-value = 0.1918

## rain
chisq_test(table(data$net, data$raindirection))
#chi-squared = 1.4951, df = 2, p-value = 0.4735


#############################################
#####################
### GROUND ICE SECTION
#####################
#############################################

### first extract ground ice content of data

### assign CRS

pf<-st_transform(permafrost,sprintf('+proj=laea +lon_0=%f +lat_0=%f',
                                    150, 60))

# set up spatial dataframe from points
points<-data %>% drop_na(c("latitude", "longitude")) 
points$longitude<-as.numeric(points$longitude)
points$latitude<-as.numeric(points$latitude)
points<-st_as_sf(points,coords=c("longitude", "latitude"), remove=FALSE, crs=4326) 
points <- st_transform(points, crs = sprintf('+proj=laea +lon_0=%f +lat_0=%f',
                                             150, 60))
## extract ground ice content
groundice<-as.data.frame(point.in.poly(points,pf[,"CONTENT"]))
## Riordan et al., 2006 show NA for ground ice content in Talkeetna area
## checked and this is a low ground ice content region - add manually
groundice$CONTENT<-ifelse(is.na(groundice$CONTENT),"l",groundice$CONTENT )

#assign factors
groundice$net<-factor(groundice$net, levels=c('negative', 'positive', 'no trend'))
groundice$CONTENT<-factor(groundice$CONTENT, levels=c('h', 'm', 'l'))

#### does ground ice content affect lake direction?
Tabla<-table(groundice$net, groundice$CONTENT)
dimnames(Tabla)= list("net" = dimnames(Tabla)[[1]], "ice" = dimnames(Tabla)[[2]])

chisq_test(Tabla, scores = list("ice" = c(1,2,3)) )
###data:  ice (ordered) by net (negative, positive, no trend)
###chi-squared = 11.482, df = 2, p-value = 0.003211

#### percentage of increasing/decreasing reports by ground ice content
Tablb<-table(groundice$net, groundice$CONTENT)
Tablb[,1]<-Tablb[,1]/sum(Tablb[,1])
Tablb[,2]<-Tablb[,2]/sum(Tablb[,2])
Tablb[,3]<-Tablb[,3]/sum(Tablb[,3])


#############################################
#####################
### METHODS SECTION
#####################
#############################################


######
### are temporally rich data different from temporally dense?
######
Table_1 =table(data$net, data$points)
dimnames(Table_1)= list("net" = dimnames(Table_1)[[1]], "points" = dimnames(Table_1)[[2]])

chisq_test(Table_1, scores = list("points" = c(2,3,1)))

#chi-squared = 0.53612, df = 2, p-value = 0.7649

######
### do landsat data give you a different result?
######
landsatdata<-data[data$`Landsat/Highres`=="Landsat"| data$`Landsat/Highres`=="HighRes",]
landsat_table<-table(landsatdata$net, landsatdata$`Landsat/Highres`)
chisq_test(landsat_table)

#chi-squared = 1.2828, df = 2, p-value = 0.5266

