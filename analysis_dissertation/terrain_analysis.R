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
stack_30cm_filled <- stack(file.path(terrainDir, '30cmfilled', 'dem30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'aspect_30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'slope_30cm_filled_degrees.tif'), file.path(terrainDir, '30cmfilled', 'curvature_mean30cm_filled.tif'), file.path(terrainDir, '30cmfilled', 'CTI_30cm_filled.tif'))
names(stack_30cm_filled) <- c('elevation', 'aspect', 'slope', 'curvature_mean', 'CTI')
sensor_shp <- shapefile('C:/Users/smdevine/Desktop/rangeland project/soil.moisture.sensors/5TM_sensor_locations_Camatta.shp')
plot(sensor_shp, col='blue')
sensor_terrain_summary_2mbuffer <- extract(stack_30cm_filled, sensor_shp, buffer=2, fun=mean)
sensor_terrain_summary_2mbuffer <- as.data.frame(sensor_terrain_summary_2mbuffer)
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
write.csv(sensor_terrain_summary, file.path(results, 'terrain_characteristics', 'sensor_Rterrain30cmfilled_summary.csv'))
