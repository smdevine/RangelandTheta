#migrated to Desktop/GITprojects/RangelandTheta on 5/8/17
#was originally saved in Desktop/rangeland project/r scripts
#based on analyis of DEM provided by Sean Hogan's 2017 flight
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEM_S_Hogan/Camatta_Mar_2017/Rasters'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r_v3'
sensorDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
library(rgdal)
library(raster)
#library(dynatopmodel)
options(digits = 10)
list.files(dem_fineres, pattern = glob2rx('*.tif'))
#read in S Hogan's raster file and upscale to different dimensions
dem_9cm <- raster(file.path(dem_fineres, 'DEM_9.4cm.tif'))
dem_30cm <- aggregate(dem_9cm, fact=3, fun=mean)
writeRaster(dem_30cm, file.path(terrainDir, 'dem30cm.tif'))
dem_1m <- aggregate(dem_9cm, fact=10, fun=mean)
writeRaster(dem_1m, file.path(terrainDir, 'dem1m.tif'))
dem_5m <- aggregate(dem_9cm, fact=53, fun=mean)
writeRaster(dem_5m, file.path(terrainDir, 'dem5m.tif'))
slope_30cm <- terrain(dem_30cm, opt = 'slope', unit = 'degrees', neighbors = 8) 
writeRaster(slope_30cm, file.path(terrainDir, 'slope_30cm.tif'))
slope_1m <- terrain(dem_1m, opt = 'slope', unit = 'degrees', neighbors = 8)
writeRaster(slope_1m, file.path(terrainDir, 'slope_1m.tif'))

aspect_30cm <- terrain(dem_30cm, opt = 'aspect', unit = 'degrees', neighbors = 8)
writeRaster(aspect_30cm, file.path(terrainDir, 'aspect_30cm.tif'))
aspect_1m <- terrain(dem_1m, opt = 'aspect', unit = 'degrees', neighbors = 8)
writeRaster(aspect_1m, file.path(terrainDir, 'aspect_1m.tif'))
tri_30cm <- terrain(dem_30cm, opt = 'TRI', neighbors = 8)
writeRaster(tri_30cm, file.path(terrainDir, 'terrain_ruggedness30cm.tif'), format='GTiff')
tri_1m <- terrain(dem_1m, opt = 'TRI', neighbors = 8)
writeRaster(tri_1m, file.path(terrainDir, 'terrain_ruggedness1m.tif'), format='GTiff')
tpi_30cm <- terrain(dem_30cm, opt='TPI', neighbors = 8)
writeRaster(tpi_30cm, file.path(terrainDir, 'topographic_position30cm.tif'), format='GTiff')
tpi_1m <- terrain(dem_1m, opt='TPI', neighbors = 8)
writeRaster(tpi_1m, file.path(terrainDir, 'topographic_position1m.tif'), format='GTiff')
roughness_30cm <- terrain(dem_30cm, opt = 'roughness', neighbors = 8)
writeRaster(roughness_30cm, file.path(terrainDir, 'roughness_30cm.tif'), format='GTiff')
roughness_1m <- terrain(dem_1m, opt = 'roughness', neighbors = 8)
writeRaster(roughness_1m, file.path(terrainDir, 'roughness_1m.tif'), format='GTiff')
plot(roughness_1m)
plot(roughness_30cm)

#merge with sensors
list.files(terrainDir)
stack_30cm <- stack(file.path(terrainDir, 'dem30cm.tif'), file.path(terrainDir, 'aspect_30cm.tif'), file.path(terrainDir, 'slope_30cm.tif'), file.path(terrainDir, 'roughness_30cm.tif'), file.path(terrainDir, 'terrain_ruggedness30cm.tif'), file.path(terrainDir, 'topographic_position30cm.tif'), file.path(terrainDir, 'curvature_mean30cm.tif'), file.path(terrainDir, 'compound_topo_index30cm.tif'))
names(stack_30cm) <- c('elevation', 'aspect', 'slope', 'roughness', 'TRI', 'TPI', 'curvature_mean', 'CTI')

setwd(sensorDir)
sensor_shp <- shapefile(file.path(sensorDir, "5TM_sensor_locations_Camatta.shp"))
plot(sensor_shp, col='blue')
sensor_terrain_summary <- extract(stack_30cm, sensor_shp, buffer=1.5, fun=mean)
sensor_terrain_summary <- as.data.frame(sensor_terrain_summary)
sensor_terrain_summary
sensor_terrain_summary$aspect_class <- ifelse(sensor_terrain_summary$aspect >= 45 & sensor_terrain_summary$aspect < 135, 'east', ifelse(sensor_terrain_summary$aspect >= 135 & sensor_terrain_summary$aspect < 225, 'south', ifelse(sensor_terrain_summary$aspect >= 225 & sensor_terrain_summary$aspect < 315, 'west', 'north')))
write.csv(sensor_terrain_summary, file.path(results, 'terrain_characteristics', 'sensor_Rterrain30cm_summary.csv')) #used 30 cm res data and 1.5 m buffer around points to calculate
sensor_terrain_summary_v1 <- read.csv(file.path(results, 'terrain_characteristics', 'sensor_Rterrain30cm_summary.csv'))

##compare with a filled DEM
list.files(file.path(terrainDir, '30cmfilled'))
dem30cm_filled <- raster(file.path(terrainDir, '30cmfilled', 'dem30cm_filled.tif'))
aspect_30cm <- terrain(dem30cm_filled, opt = 'aspect', unit = 'degrees', neighbors = 8)
writeRaster(aspect_30cm, file.path(terrainDir, '30cmfilled', 'aspect_30cm_filled.tif'))
slope_30cm <- terrain(dem30cm_filled, opt = 'slope', unit = 'degrees', neighbors = 8)
writeRaster(slope_30cm, file.path(terrainDir, '30cmfilled', 'slope_30cm_filled_degrees.tif'))
#tri_30cm <- terrain(dem_30cm, opt = 'TRI', neighbors = 8)
#writeRaster(tri_30cm, file.path(terrainDir, 'terrain_ruggedness30cm.tif'), format='GTiff')
#tpi_30cm <- terrain(dem_30cm, opt='TPI', neighbors = 8)
#writeRaster(tpi_30cm, file.path(terrainDir, 'topographic_position30cm.tif'), format='GTiff')
#roughness_30cm <- terrain(dem_30cm, opt = 'roughness', neighbors = 8)
#writeRaster(roughness_30cm, file.path(terrainDir, 'roughness_30cm.tif'), format='GTiff')
list.files(file.path(terrainDir, '30cmfilled'))
stack_30cm_filled <- stack(file.path(terrainDir, '30cmfilled', 'dem30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'aspect_30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'slope_30cm_filled_degrees.tif'), file.path(terrainDir, '30cmfilled', 'curvature_mean30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'CTI_30cm_filled.v2.tif'), file.path(terrainDir, '30cmfilled', 'curvature_profile30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'curvature_plan30cm_filled.tif')) #note that v2 of CTI used a revised formula
names(stack_30cm_filled) <- c('elevation', 'aspect', 'slope', 'curvature_mean', 'CTI', 'curvature_profile', 'curvature_plan')
sensor_shp <- shapefile('C:/Users/smdevine/Desktop/rangeland project/soil.moisture.sensors/5TM_sensor_locations_Camatta.shp')
plot(sensor_shp, col='blue')
sensor_terrain_summary <- extract(stack_30cm_filled, sensor_shp, buffer=1.5, fun=mean)
sensor_terrain_summary <- as.data.frame(sensor_terrain_summary)
sensor_terrain_summary
plot(sensor_terrain_summary_v1$elevation, sensor_terrain_summary$elevation)
abline(0, 1)
plot(sensor_terrain_summary_v1$aspect, sensor_terrain_summary$aspect)
abline(0, 1)
plot(sensor_terrain_summary_v1$slope, sensor_terrain_summary$slope)
abline(0, 1)
plot(sensor_terrain_summary_v1$curvature_mean, sensor_terrain_summary$curvature_mean)
abline(0, 1)
plot(sensor_terrain_summary_v1$CTI, sensor_terrain_summary$CTI) #this is only discrepancy: points 13 and 9 
abline(0, 1)
text(x=sensor_terrain_summary_v1$CTI, y=sensor_terrain_summary$CTI, pos=1, offset=0.3, labels=sensor_terrain_summary_v1$X)
sensor_terrain_summary$location <- row.names(sensor_terrain_summary)
write.csv(sensor_terrain_summary, file.path(results, 'terrain_characteristics', 'sensor_Rterrain30cmfilled_summary_CTI.v2.csv'))
write.csv(sensor_terrain_summary_1mbuffer, file.path(results, 'terrain_characteristics', 'sensor_Rterrain30cmfilled1mbuffer_summary.csv'))

##compare with a filled 1m DEM
list.files(file.path(terrainDir, '1mfilled'))
dem1m_filled <- raster(file.path(terrainDir, '1mfilled', 'dem1m_filled.tif'))
aspect_1m <- terrain(dem1m_filled, opt = 'aspect', unit = 'degrees', neighbors = 8)
writeRaster(aspect_1m, file.path(terrainDir, '1mfilled', 'aspect_1m_filled.tif'))
stack_1m_filled <- stack(file.path(terrainDir, '1mfilled', 'dem1m_filled.tif'), file.path(terrainDir, '1mfilled', 'aspect_1m_filled.tif'), file.path(terrainDir, '1mfilled', 'slope1m_filled_degrees.tif'), file.path(terrainDir, '1mfilled', 'curvature_mean1m_filled.tif'), file.path(terrainDir, '1mfilled', 'CTI_1mfilled.v2.tif'), file.path(terrainDir, '1mfilled', 'curvature_profile1m_filled.tif'), file.path(terrainDir, '1mfilled', 'curvature_plan1m_filled.tif')) #note that v2 of CTI used a revised formula
names(stack_1m_filled) <- c('elevation', 'aspect', 'slope', 'curvature_mean', 'CTI', 'curvature_profile', 'curvature_plan')
sensor_shp <- shapefile('C:/Users/smdevine/Desktop/rangeland project/soil.moisture.sensors/5TM_sensor_locations_Camatta.shp')
plot(sensor_shp, col='blue')
sensor_terrain_summary <- extract(stack_1m_filled, sensor_shp) #use buffer: buffer=1.5, fun=mean
sensor_terrain_summary <- as.data.frame(sensor_terrain_summary)
sensor_terrain_summary
sensor_terrain_summary$location <- row.names(sensor_terrain_summary)
write.csv(sensor_terrain_summary, file.path(results, 'terrain_characteristics', 'sensor_Rterrain1mfille.csv')) #both of these used the v2 definition of CTI


##compare with Nov 2016 1 m DEM from Grace, now excluding CTI as it is suspect
Nov2016_terrain <- stack(list.files(file.path(terrainDir, '1m_Nov2016'), full.names = TRUE))
Nov2016_terrain
names(Nov2016_terrain) <- c('aspect', 'curvature_mean', 'curvature_plan', 'curvature_profile', 'elevation', 'slope', 'TCI')
sensor_terrain_summary <- extract(Nov2016_terrain, sensor_shp) #use buffer: buffer=1.5, fun=mean
sensor_terrain_summary <- as.data.frame(sensor_terrain_summary)
sensor_terrain_summary
sensor_terrain_summary$location <- row.names(sensor_terrain_summary)
write.csv(sensor_terrain_summary, file.path(results, 'terrain_characteristics', 'sensor_terrain1mNov20161.5mbuff.csv')) #now includes TCI as opposed to 

##compare with Nov 2016 5 m DEM from Grace, also excluding CTI as it is suspect
Nov2016_terrain_5m <- stack(list.files(file.path(terrainDir, '5m_Nov2016'), full.names = TRUE))
Nov2016_terrain_5m
names(Nov2016_terrain_5m) <- c('aspect', 'elevation', 'curvature_mean', 'curvature_plan', 'curvature_profile', 'slope', 'TCI')
sensor_terrain_summary_5m <- extract(Nov2016_terrain_5m, sensor_shp) #use buffer: buffer=1.5, fun=mean
sensor_terrain_summary_5m <- as.data.frame(sensor_terrain_summary_5m)
sensor_terrain_summary_5m
sensor_terrain_summary_5m$location <- row.names(sensor_terrain_summary_5m)
write.csv(sensor_terrain_summary_5m, file.path(results, 'terrain_characteristics', 'sensor_terrain5mNov2016.csv')) #now includes TCI as opposed to 
plot(sensor_terrain_summary$aspect, sensor_terrain_summary_5m$aspect)
abline(0, 1)
plot(sensor_terrain_summary$curvature_mean, sensor_terrain_summary_5m$curvature_mean)
abline(0, 1)
plot(sensor_terrain_summary$curvature_plan, sensor_terrain_summary_5m$curvature_plan)
abline(0, 1)
plot(sensor_terrain_summary$curvature_profile, sensor_terrain_summary_5m$curvature_profile)
abline(0, 1)
plot(sensor_terrain_summary$elevation, sensor_terrain_summary_5m$elevation)
abline(0, 1)
plot(sensor_terrain_summary$slope, sensor_terrain_summary_5m$slope)
abline(0, 1)
plot(sensor_terrain_summary$TCI, sensor_terrain_summary_5m$TCI)
abline(0, 1)
sensor_terrain_summary$TCI - sensor_terrain_summary_5m$TCI

##compare with Nov 2016 3 m DEM from Grace, also excluding CTI as it is suspect
Nov2016_terrain_3m <- stack(list.files(file.path(terrainDir, '3m_Nov2016'), full.names = TRUE))
Nov2016_terrain_3m
names(Nov2016_terrain_3m) <- c('aspect', 'curvature_mean', 'curvature_plan', 'curvature_profile', 'elevation', 'slope', 'TCI')
sensor_terrain_summary_3m <- extract(Nov2016_terrain_3m, sensor_shp) #use buffer: buffer=1.5, fun=mean
sensor_terrain_summary_3m <- as.data.frame(sensor_terrain_summary_3m)
sensor_terrain_summary_3m
sensor_terrain_summary_3m$location <- row.names(sensor_terrain_summary_3m)
write.csv(sensor_terrain_summary_3m, file.path(results, 'terrain_characteristics', 'sensor_terrain3mNov2016.csv')) #now includes TCI as opposed to 
#compare with 1m analysis
plot(sensor_terrain_summary$aspect, sensor_terrain_summary_3m$aspect)
abline(0, 1)
plot(sensor_terrain_summary$curvature_mean, sensor_terrain_summary_3m$curvature_mean)
abline(0, 1)
plot(sensor_terrain_summary$curvature_plan, sensor_terrain_summary_3m$curvature_plan)
abline(0, 1)
plot(sensor_terrain_summary$curvature_profile, sensor_terrain_summary_3m$curvature_profile)
abline(0, 1)
plot(sensor_terrain_summary$elevation, sensor_terrain_summary_3m$elevation)
abline(0, 1)
plot(sensor_terrain_summary$slope, sensor_terrain_summary_3m$slope)
abline(0, 1)
plot(sensor_terrain_summary$TCI, sensor_terrain_summary_3m$TCI)
abline(0, 1)
sensor_terrain_summary$TCI - sensor_terrain_summary_3m$TCI
#compare with 5m analysis
plot(sensor_terrain_summary_5m$aspect, sensor_terrain_summary_3m$aspect)
abline(0, 1)
plot(sensor_terrain_summary_5m$curvature_mean, sensor_terrain_summary_3m$curvature_mean)
abline(0, 1)
plot(sensor_terrain_summary_5m$curvature_plan, sensor_terrain_summary_3m$curvature_plan)
abline(0, 1)
plot(sensor_terrain_summary_5m$curvature_profile, sensor_terrain_summary_3m$curvature_profile)
abline(0, 1)
plot(sensor_terrain_summary_5m$elevation, sensor_terrain_summary_3m$elevation)
abline(0, 1)
plot(sensor_terrain_summary_5m$slope, sensor_terrain_summary_3m$slope)
abline(0, 1)
plot(sensor_terrain_summary_5m$TCI, sensor_terrain_summary_3m$TCI)
abline(0, 1)
sensor_terrain_summary_5m$TCI - sensor_terrain_summary_3m$TCI
