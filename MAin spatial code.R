library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(tmap)
library(terra)
library(spatstat)
library(geoelectrics)
library(ncdf4) 
library(lattice)
library(RColorBrewer)
library(readxl)
library(gstat)
library(osmdata)
library(rasterVis)
library(raster)


#Plot Existing Plants in R

indonesia = st_read("/Users/**/Desktop/spatial assignment/Spatialcode/data/indonesiaboundary/idn_admbnda_adm0_bps_20200401.shp" )


plant <- drop_na(plant)

plant <- plant %>% 
  mutate(Type = factor(Type))

plant <- plant %>% 
  mutate(Status = factor(Status))

plant <- st_as_sf(plant, coords = c("longitude", "latitude"), crs = 4326)

indonesia1 <- st_simplify(indonesia, dTolerance = 0.01)

z <- tm_shape(indonesia1)+
  tm_fill(col= "blue", alpha=.4)+
  tm_shape(plant)+
  tm_dots(col = "Type", size = "Capacity", shape = "Status") 


z +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,   # Adjust the legend text size
            legend.title.size = 0.9) +  # Remove the outer border
  tm_legend(title.size = 1, title.fontface = "bold", title.position = c("left", "top")) +
  tm_layout(
    main.title = "INDONESIA",
    main.title.position = c("center", "bottom"),  # set the position of the title
    main.title.size = 1,  # set the font size of the title
  )



raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
quick_mask_raster <- function(raster_data, masking_vector){
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)
}

#Constraints 
#water Constraints


water1 <-rast("/Users/**/Desktop/spatial assignment/Spatialcode/data/waterindonesia.tif")

water <- aggregate(water, fact = 10)

plot(water)

watersuit<- matrix(c(0, 1,  1,
                     1, 2, 0), ncol=3, byrow=TRUE)
waternew <- classify(water, watersuit, include.lowest=TRUE )



colors <- c("blue", "green") # Unsuitable in red, suitable in green

legend("bottomright", legend = c("Unsuitable", "Suitable"), fill = colors, title = "Suitability")
tm_scale_bar(breaks = c(0, 10000, 20000), position = c(0.05, 0.05), prefix = "km")
tm_compass(type = "arrow", position = c(0.95, 0.05), size = 1)

tm_shape(water)+ tm_raster(style = "cat", labels = c("Unsuitable", "Suitable"),palette = colors,alpha=.7) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)


# population constraints

population <-rast("/Users/**/Desktop/spatial assignment/Spatialcode/data/population.tif")
population <- quick_mask_raster(population, indonesia) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
population <- aggregate(population, fact = 10)

names(population)="Population_density"

populsuit<- matrix(c(0, 0.03,  1,
                     0.03, 1, 0), ncol=3, byrow=TRUE)
populnew <- classify(population, populsuit, include.lowest=TRUE )
plot(populnew)


colors <- c("blue", "green") # Unsuitable in red, suitable in green

# Create a plot of the classified raster

tm_shape(population)+ tm_raster(palette = "viridis",alpha=.7) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)





#altitude constraints 
altitude<-rast("/Users/**/Desktop/spatial assignment/Spatialcode/data/IDN_msk_alt/IDN_msk_alt.vrt")
crs(altitude) <- crs(population) 
altitude <- quick_mask_raster(altitude, indonesia) 

altsuit<- matrix(c(-30, 0, 0,
                   0, 1000, 1,
                   1000,4650, 0), ncol=3, byrow=TRUE)
altnew <- classify(altitude, altsuit, include.lowest=TRUE )
plot(altnew)


colors1 <- c("blue", "green") # Unsuitable in blue, suitable in green


#slope constraints

Slope = terrain(altitude,v='slope', unit='degrees') 
#  reclassify slope data, assign suitable area as 1, and unsuitable as 0.
slope_m<- matrix(c(0, 5,  1,
                   5, 46, 0), ncol=3, byrow=TRUE) 
slope_rc <- classify(Slope, slope_m, include.lowest=TRUE )
tm_shape(Slope)+ tm_raster(palette = c("viridis"),alpha=.7, style = "quantile") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9) +
  tm_legend(title.size = 1, title.fontface = "bold", title.position = c("left", "top")) +
  tm_layout(
    main.title = "LIVERPOOL",
    main.title.position = c("center", "bottom"),
    main.title.size = 1
  )

tm_shape(Slope)+ tm_raster(style = "cat", labels = c("Unsuitable", "Suitable"),palette = colors) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)
# Define breaks and colors for the plot
colors1 <- c("blue", "green") # Unsuitable in blue, suitable in green



#landcover constraints

landcover<-rast("/Users/**/Desktop/spatial assignment/Spatialcode/data/landcover.tif")
crs(landcover) <- crs(population) 
landcover <- mask(landcover, indonesia) 

landcover <- aggregate(landcover, fact = 2) 

tm_shape(landcover) +
  tm_raster(palette = "viridis", legend.show = TRUE) +tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)

lcsuit<- matrix(c(10, 100,0,
                  100,160,1,
                  160,190,0,
                  190,205, 1,
                  205,210,0), ncol=3, byrow=TRUE)
lcnew <- classify(landcover, lcsuit, include.lowest=TRUE )

plot(lcnew)
names(landcover)

# Define breaks and colors for the plot
colors1 <- c("blue", "green") # Unsuitable in blue, suitable in green


tm_shape(lcnew)+ tm_raster(style = "cat", labels = c("Unsuitable", "Suitable"),palette = colors) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)



#powerline constraints

coords<-matrix(c(95.01079,141.01940,-11.00762, 6.07693),byrow = TRUE,nrow =2,ncol = 2, dimnames = list(c("x","y"),c("min","max")))

location <- opq(coords, timeout = 180)

powerplant = add_osm_feature(location, key = "power", value = "line" )
powerplant.sf = osmdata_sf(powerplant)


lines <- powerplant.sf$osm_lines

library(geojsonio)

geojson_write(lines, file = "/Users/**/Desktop/spatial assignment/Spatialcode/data/lines.geojson")


transmissionlines = population

transmissionlines <- aggregate(transmissionlines, fact = 20)


powerlinedist= terra::distance(transmissionlines, lines)

writeRaster(powerlinedist, "/Users/**/Desktop/spatial assignment/Spatialcode/data/powerlinedistance.tif")

transuit<- matrix(c(0, 1000,  1,
                    1000, 1135118, 0), ncol=3, byrow=TRUE)
transline <- classify(powerlinedist, transuit, include.lowest=TRUE )

transline_indonesia <- mask(transline, indonesia)

tm_shape(transline_indonesia)+ tm_raster(style = "cat", labels = c("Unsuitable", "Suitable"),palette = colors,alpha=.7) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)

names(transline_indonesia)="transline"



#road constraints

roads = st_read("/Users/**/Desktop/spatial assignment/Spatialcode/data/IDN_rds/IDN_roads.shp" )

plot(roads)

dist= terra::distance(population, roads)

writeRaster(dist, "/Users/**/Desktop/spatial assignment/Spatialcode/data/roaddistance.tif")

names(dist)="Road_network"

rdsuit<- matrix(c(0, 10000,  1,
                  10000, 50000, 0,
                  50000, 1009000, 0), ncol=3, byrow=TRUE)
rdnew <- classify(dist, rdsuit, include.lowest=TRUE )

rdnew <- mask(rdnew, indonesia)

plot(rdnew)

colors <- c("blue", "green") # Unsuitable in red, suitable in green


tm_shape(roads)+ tm_raster(style = "cat", palette = colors,alpha=.7) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)


# Create a plot of the classified raster
plot(rdnew, col = colors, breaks = breaks, legend = FALSE, main = "Distance from roads Suitability")

legend("bottomright", legend = c("Unsuitable", "Suitable"), fill = colors, title = "Suitability")

tm_shape(rdnew)+ tm_raster(style = "cat", labels = c("Unsuitable", "Suitable"),palette = colors,alpha=.7) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)

#solar radiation constraint

era <- nc_open("/Users/**/Desktop/spatial assignment/Spatialcode/data/data.nc" )
era 
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array)

dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")

image(ssrd_array, col=rev(brewer.pal(10,"RdBu")) )

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template 

lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat)

ssrd_vec <- as.vector( ssrd_array) 
length(ssrd_vec)

ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 


ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

#Interopolation

coor = as.data.frame(st_coordinates(ssrd_sf))
ssrd_sf$x = coor$X
ssrd_sf$y = coor$Y
ssrd_nogeom = st_drop_geometry(ssrd_sf) #get rid of geometry but keep all other attributes
ssrd_nogeom=na.omit(ssrd_nogeom)

gs <- gstat(formula=ssrd~1, locations=~x+y, data=ssrd_nogeom, nmax=Inf, set=list(idp=4 )) #data should be in data frame format
gs


idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)

idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)

names(idw_mask) = c( "predicted","observed" )
tmap_mode("view") #tmap can also do raster plot


solarrad <- idw_mask[[1]]

#solar radiation constraint

solarsuit<- matrix(c(0, 14000000,  0,
                     14000000, 22090210, 1), ncol=3, byrow=TRUE)
solarnew <- classify(solarrad, solarsuit, include.lowest=TRUE )


tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE)

tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")

ncatt_get(era,"ssrd","units")


radiation_to_power <- function(radiation, area, yield_r=0.175, pr=0.6, hours=8760)
{ kWh <- radiation * area * yield_r * pr * hours * (1/3600000)
return(kWh) } 

rad=14000000 #scientific notation: equals to 9*10^6, you can also write as 9*10**6
area=1
power_kWh <- radiation_to_power(rad, area) 
power_kWh


ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value, area))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")

# check IDW decay rate through RMSE

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

n_idp = 8 #examine power ranging from 1 to 20
n_fold = 4

rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(7713)
kf <- sample(1:n_fold, nrow(ssrd_nogeom), replace=TRUE)
va = data.frame( c(1:10), NA)
colnames(va) =c("idp","rmse") 



for (j in 1:4) 
{
  for (i in 1:n_fold) {
    test <- ssrd_nogeom[kf == 1, ]
    train <- ssrd_nogeom[kf != 1, ]
    gs <- gstat(formula=ssrd~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$ssrd, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}

va[which(va$rmse==min(va)),]

#protected areas constraints


protectedareas = st_read("/Users/**/Desktop/spatial assignment/Spatialcode/data/Protectedareas/WDPA_WDOECM_Apr2023_Public_IDN_shp-points.shp" ))
protectedareas$present=1

empty_raster <- raster(nrows = 200, ncols = 200)
extent(empty_raster) <- c(95.01109, 141.0194, -11.00694, 6.076394)
crs(empty_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
empty_spat_raster <- rast(empty_raster)
prot_raster <- rasterize(protectedareas, empty_spat_raster, field = "present")
prot_raster[is.na(prot_raster)] <- 0

protectedareas_mask <- quick_mask_raster(prot_raster, indonesia)
protectedareas_mask <- mask(protectedareas_mask, indonesia)
plot(protectedareas_mask)


protsuit<- matrix(c(0, 1,
                    1, 2), ncol=2, byrow=TRUE)
protnew <- classify(protectedareas_mask, protsuit, include.lowest=TRUE )

# Define breaks and colors for the plot
breaks1 <- c(-Inf, 1.5, 2.5, Inf)
colors1 <- c("blue", "green") # Unsuitable in blue, suitable in green

# Create a plot of the classified raster
plot(protnew, col = colors1, breaks = breaks1, legend = FALSE, main = "Protected areas Suitability")

# Add a custom legend
legend("bottomright", legend = c("Unsuitable", "Suitable"), fill = colors1, title = "Suitability")


tm_shape(indonesia)+
  tm_fill(col= "blue", alpha=.4)+
  
  tm_shape(protectedareas) + tm_dots()


#Combination of Suitable Factors



rd= terra::resample(rdnew,solarrad,method = "bilinear")
lc= terra::resample(lcnew,solarrad,method = "bilinear")
wat= terra::resample(waternew,solarrad,method = "bilinear")
pop= terra::resample(populnew,solarrad,method = "bilinear")
slope= terra::resample(slope_rc,solarrad,method = "bilinear")
trans= terra::resample(transline_indonesia,solarrad,method = "bilinear")


combined <- c(rd,lc,wat,pop,slope,trans,solarrad)
combined_df <- as.data.frame(combined, xy=TRUE)



id6 = which( combined_df$water > 0
             & combined_df$Population_density > 0
             & combined_df$predicted > 14000000
             & combined_df$transline > 0
             & combined_df$landcover > 0
             & combined_df$Road_network > 0
             & combined_df$slope > 0
)

id5 = which( combined_df$water > 0
             & combined_df$predicted > 14000000
             & combined_df$transline > 0
             & combined_df$landcover > 0
             & combined_df$Road_network > 0
             & combined_df$slope > 0
)
id4 = which( combined_df$water > 0
             & combined_df$predicted > 14000000
             & combined_df$transline > 0
             & combined_df$landcover > 0
             & combined_df$slope > 0
)

id3 = which( combined_df$water > 0
             & combined_df$predicted > 14000000
             & combined_df$transline > 0
             & combined_df$slope > 0
)

id2 = which( combined_df$water > 0
             & combined_df$predicted > 14000000
             & combined_df$slope > 0
)

id1 = which( combined_df$water > 0
             & combined_df$predicted > 14000000
             
)



rm(suitability)

length (id)/ ncell(populnew)


suitability = solarrad
values(suitability)= 0
#add the suitablity layer onto newdata
combined=c(combined, suitability)
names(combined)[8] = "suitability"
#assign matched cells with 1 based on id generated in step 4
combined$suitability[id1] = 1
combined$suitability[id2] = 2
combined$suitability[id3] = 3
combined$suitability[id4] = 4
combined$suitability[id5] = 5
combined$suitability[id6] = 6
combined$suitability[combined$suitability == 0] <- NA
combined <- quick_mask_raster(combined, indonesia)
plot(combined$suitability)


names(combined[[8]]) = "SuitabilityCriteria"

tm_shape(combined$SuitabilityCriteria)+ tm_raster(style = "cat", labels = c("Constraint","UnSuitable", "Very low Suitability", "Low Suitable", "Suitable", "Very SUitable", "Most Suitable"),palette = "viridis" ,alpha=.7) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"),
            legend.text.size = 0.5,
            legend.title.size = 0.7)

suitable = combined[[8]]

writeRaster(suitable, "suitable.tif")





Financials

CapEx= (1160000 * 34504.167 )+ (590* 34504.167 * 6.854499 /1000000)
CapEx

annual_revenue = 103 * 34504.167 *8760/2
annual_revenue


calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CapEx, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CapEx
  return(round(NPV, 0))
}

npv= calc_NPV(annual_revenue=1694782750,lifetime_yrs=25, CapEx=4357733722)
npv
ifelse(npv>0, "Support","object" )


yearly_generation_kWH = 4573.333 *4380 * 1000


Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.08, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

LCOE <- function(npv,Life_span_generation){
  lcoe <- npv/Life_span_generation
  return(round(lcoe,2))
}

lsg = Life_span_generation_kWH(yearly_generation_kWH=yearly_generation_kWH)

lcoe <- LCOE(npv = 23773749709,lsg)

lcoe
