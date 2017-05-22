library(raster)
library(extrafont)
library(extrafontdb)
loadfonts()
spatialDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
#read-in coordinate data
plotDir <- 'C:/Users/smdevine/Desktop/rangeland project/study_plots'
elevDir <- 'C:/Users/smdevine/Desktop/rangeland project/elevation_NED10M_studysite/elevation'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017'
setwd(spatialDir)
sensor_coords <- read.csv('sensor_coords_UTM10N.csv', stringsAsFactors = FALSE)
print(sensor_coords, digits=10) #default is 7, which could be changed under options(digits=...)
longitude <- sensor_coords$Easting_utm10N #this is x coordinate
latitude <- sensor_coords$Northing_utm10N #this is y coordinate
lonlat <- cbind(longitude, latitude) #convention is to present as x, y
crs_sensors <- CRS('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #as defined by prj file sent by Grace to me for sensor locations
sensor_pts <- SpatialPoints(lonlat, proj4string=crs_sensors)
sensor_pts <- SpatialPointsDataFrame(sensor_pts, data=sensor_coords)

#write to file if so desired
shapefile(x=sensor_pts, filename='5TM_sensor_locations_Camatta.shp')

#read-in sensor pts file
setwd(spatialDir)
sensor_pts <- shapefile('5TM_sensor_locations_Camatta.shp')

#plot aspect of the catchment from 1 m DEM
setwd(dem_fineres)
dem_1m <- raster('camatta_Nov2016_1m_dsm.tif')
aspect_1m <- terrain(dem_1m, opt='aspect', unit = 'degrees')
brks <- c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5)
legend_key <- list(at=c(45, 90, 135, 180, 225, 270, 315, 355), labels=c('Northeast', 'East', 'Southeast', 'South', 'Southwest', 'West', 'Northwest', 'North'), line=2)
setwd(plot_results)
png(file = paste('Camatta_aspect_1mDEM.png', sep = ''), family = 'Book Antiqua', width = 700, height = 500, units = 'px', res=100)
par(mar=c(4, 4, 3, 2))
plot(aspect_1m, xlab='Longitude (UTM Zone 10N)', ylab='Latitude(UTM Zone 10N)', cex.axis=0.8, cex.lab=0.9, breaks=brks, legend.width = 1, legend.mar= 10, col=terrain.colors(8), axis.args=legend_key, legend.args=list(text='Aspect', side=4, line=0.5, cex=1)) #this is really calling the image.plot function; the par(mar..) is helping control where the legend bar is placed
title(main='Soil moisture sensor locations and aspect of Camatta catchment', adj=0) #adj=1 creates right-justified text; default is 0.5 for centering title
plot(sensor_pts, pch=17, col='blue', add=T)
text(x=sensor_pts, labels=sensor_pts$dtlggr_, pos=1, cex=1.1, halo=T)
dev.off()

#plot aspect of the catchment
setwd(plotDir)
camatta <- shapefile('camatta_catchment_rough.shp')
setwd(terrainDir)
aspect <- raster('aspect.tif') #expressed in degrees
aspect_clip <- crop(aspect, camatta)
brks <- c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5)
legend <- list(at=c(45, 90, 135, 180, 225, 270, 315, 355), labels=c('Northeast', 'East', 'Southeast', 'South', 'Southwest', 'West', 'Northwest', 'North'))
plot(aspect_clip, main='Soil moisture sensor locations and aspect of Camatta catchment', breaks=brks, col=terrain.colors(8), axis.args=legend)


#or, plot elevation
setwd(elevDir)
e <- raster('dem10m_combined.tif')
plot(e)
e_clip <- crop(e, camatta)
plot(e_clip, main='Soil moisture sensor locations and elevation of Camatta catchment', col=terrain.colors(5))


#and plot the sensor locations
plot(sensor_pts, pch=17, col='blue', add=T)
text(x=sensor_pts, labels=sensor_pts$datalogger_no, pos=1, cex=1.1, halo=T)

#read in terrain rasters from 10 m DEM analysis done in 'study_site_analysis.R" for slope, aspect, and TPI. CTI calc done in ArcGIS and exported to same folder as tif files.
setwd(terrainDir)
slope <- raster('slope.tif') #expressed in degrees
plot(slope)
aspect <- raster('aspect.tif') #expressed in degrees
plot(aspect)
cti <- raster('cti.tif') #compound topographic index done in R
cti <- projectRaster(cti, slope) #there is a slight difference in the crs arguments
mean_curv <- raster('mean_curv.tif')
mean_curv <- projectRaster(mean_curv, slope)
terrain_stack <- stack(slope, aspect, cti, mean_curv)
#project sensor points to terrain stack
sensor_pts <- spTransform(sensor_pts, crs(slope))
print(as.data.frame(sensor_pts), digits=8)
sensor_pts <- extract(terrain_stack, sensor_pts, sp=TRUE) #adds terrain characteristics to each point
sensor_pts$aspect_cardinal <- c('NW', "N", "N", "N", "NW", "NW", "NW", "SW", "NW", "S", "S", "S", "S", "S", "NW", "W") #this is in order of the datalogger numbers
sensor_pts_df <- as.data.frame(sensor_pts)
sensor_pts_df$longitude <- NULL
sensor_pts_df$latitude <- NULL
sensor_pts_df$coords.x1 <- NULL
sensor_pts_df$coords.x2 <- NULL

#write terrain characteristics for each sensor location to results
setwd(results)
write.csv(sensor_pts_df, "sensor_terrain_characteristics5_3_17.csv", row.names = FALSE)