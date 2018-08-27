#migrated to Desktop/GITprojects/RangelandTheta on 5/8/17
#was originally saved in Desktop/rangeland project/r scripts
#based on analyis of DEM provided by Sean Hogan's 2017 flight
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEM_S_Hogan/Camatta_Mar_2017/Rasters'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r_v3'
sensorDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
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
setwd(sensorDir)
sensor_shp <- shapefile("5TM_sensor_locations_Camatta.shp")
plot(sensor_shp, add=T, col='blue')

