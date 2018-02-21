spatial_data <- 'C:/Users/smdevine/Desktop/rangeland project/results'
sensor_coords <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017'
additional_waypoints <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots/coordinates_waypoints'
forage_data <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
soil_VWCdata <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location/VWC'
options(digits = 10)
options(stringsAsFactors = FALSE)
library(extrafont)
library(extrafontdb)
loadfonts()
library(raster)
setwd(forage_data)
fnames <- list.files(pattern = glob2rx('*.csv'))
fnames
sensorplot_data <- read.csv("CamattaBiomassSensorPlotsOnly2017.csv", stringsAsFactors = FALSE)
sensorplot_data$Location_sub <- paste(sensorplot_data$Location, sensorplot_data$Subsample, sep='')
by_subsample <- as.data.frame(tapply(sensorplot_data$Forage_kg_hectare, list(sensorplot_data$Location_sub, sensorplot_data$DateClipped), as.numeric))
by_subsample$location_sub <- rownames(by_subsample)
by_subsample$location <- as.integer(gsub('[A,B]', '', by_subsample$location_sub))
#left off here; combine with sensor coordinates
by_plot <- as.data.frame(tapply(sensorplot_data$Forage_kg_hectare, list(sensorplot_data$Location, sensorplot_data$DateClipped), mean))
by_plot$location <- as.integer(rownames(by_plot))
summary(by_plot)
setwd(spatial_data)
sensor_pts <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE)
sensor_pts <- sensor_pts[ ,c(1,3:4)] #reduce to columns that have location number and coordinates, because these terrain characteristics were derived from 10 m DEM
forage_summary <- merge(by_subsample, sensor_pts, by='location')
#colnames(forage_summary)[2:5] <- c("clp021517", "clp031417", "clp041017", "clp050117") #rename to match other waypoint clipping date column names for writing shapefile
coords <- forage_summary[ ,c('Est_10N', 'Nrt_10N')]
forage_summary <- forage_summary[ ,-6:-7]
forage_summary_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=forage_summary)
setwd(file.path(forage_data, 'results'))
shapefile(forage_summary_sp, 'sensor_forage2017.shp', overwrite=TRUE)
setwd(dem_fineres)
dem_1m <- raster('camatta_Nov2016_1m_dsm.tif')
hillshade_1m <- hillShade(terrain(dem_1m, opt='aspect'), terrain(dem_1m, opt='slope'), angle=45, direction=315)
i <- 4
reducer <- 900
setwd(file.path(plot_results, 'forage'))
for (i in 2:5) {
  png(file = paste(gsub('/', '_', names(forage_summary_sp)[i]), '_standing_biomass.png', sep = ''), family = 'Book Antiqua', width = 700, height = 500, units = 'px', res=100)
  par(mar=c(2, 2, 2, 2.5))
  plot(hillshade_1m, main=paste(names(forage_summary_sp)[i], 'standing biomass at Camatta at sensor plots'), col=gray(30:80/100), legend=FALSE, axes=F, ylim=c(3931300, 3931750))
  plot(dem_1m, col=terrain.colors(255, alpha=0.35), add=T)
  points(forage_summary_sp, cex=forage_summary[,i]/reducer, col='green', pch=17)
  legend(x=744875, y=3931455, legend=c('1,000', '2,000', '4,000'), col='green', pch=17, pt.cex=c(1000/reducer, 2000/reducer, 4000/reducer), x.intersp = 2, y.intersp = 1.9, bty="n")
  text(x=744905, y=3931458, labels='biomass (kg/ha)', font=2, offset=0)
  dev.off()
}

setwd('C:/Users/smdevine/Desktop/rangeland project/clip_plots')
waypoint_data <- read.csv("CamattaBiomassWaypointPlotsOnly2017.csv", stringsAsFactors = FALSE)
waypoint_forage <- as.data.frame(tapply(waypoint_data$Forage_kg_hectare, list(waypoint_data$WayPt, waypoint_data$DateClipped), as.numeric))
waypoint_forage$location <- rownames(waypoint_forage)
setwd(additional_waypoints)
waypoint_fnames <- list.files(pattern = glob2rx('*.shp'))
waypoint_fnames
feb_pts <- shapefile(waypoint_fnames[1])
mar_pts <- shapefile(waypoint_fnames[2])
apr_pts <- shapefile(waypoint_fnames[3])
may_pts <- shapefile(waypoint_fnames[4])
plot(feb_pts, pch=17, col='red')
plot(mar_pts, pch=17, col='green', add=T)
plot(apr_pts, pch=17, col='blue', add=T)
plot(may_pts, pch=17, col='orange', add=T)
plot(forage_summary_sp, pch=17, col='black', add=T)

feb_coords <- data.frame(feb_pts)
mar_coords <- data.frame(mar_pts)
apr_coords <- data.frame(apr_pts)
may_coords <- data.frame(may_pts)
cbind(feb_coords$Comment, mar_coords$Comment, apr_coords$Comment, may_coords$Comment)

#check the order of each data.frame
Nrt_10N <- rowMeans(cbind(feb_coords$coords.x2, mar_coords$coords.x2, apr_coords$coords.x2, may_coords$coords.x2))
Est_10N <- rowMeans(cbind(feb_coords$coords.x1, mar_coords$coords.x1, apr_coords$coords.x1, may_coords$coords.x1))
avg_coords <- data.frame(location=feb_coords$Comment, Nrt_10N=Nrt_10N, Est_10N=Est_10N)
waypoint_forage <- merge(waypoint_forage, avg_coords, by='location')
colnames(waypoint_forage)[2:5] <- c("clp021517", "clp031417", "clp041017", "clp050117")
waypoint_coords <- waypoint_forage[ ,c('Est_10N', 'Nrt_10N')]
waypoint_forage <- waypoint_forage[ ,-6:-7] #get rid of coordinates
waypoint_forage_sp <- SpatialPointsDataFrame(coords=waypoint_coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=waypoint_forage)
setwd(file.path(forage_data, 'results'))
shapefile(waypoint_forage_sp, 'waypoint_forage2017.shp', overwrite=TRUE)

#summarize waypoint clip plots
reducer <- 900
setwd(file.path(plot_results, 'forage/otherwaypoints'))
for (i in 2:5) {
  png(file = paste(gsub('/', '_', names(forage_summary_sp)[i]), '_standing_biomass.png', sep = ''), family = 'Book Antiqua', width = 700, height = 500, units = 'px', res=100)
  par(mar=c(2, 2, 2, 2.5))
  plot(hillshade_1m, main=paste(names(waypoint_forage_sp)[i], 'standing biomass at Camatta at other waypoints'), col=gray(30:80/100), legend=FALSE, axes=F, ylim=c(3931300, 3931750))
  plot(dem_1m, col=terrain.colors(255, alpha=0.35), add=T)
  points(waypoint_forage_sp, cex=waypoint_forage[,i]/reducer, col='green', pch=17)
  legend(x=744875, y=3931455, legend=c('1,000', '2,000', '4,000'), col='green', pch=17, pt.cex=c(1000/reducer, 2000/reducer, 4000/reducer), x.intersp = 2, y.intersp = 1.9, bty="n")
  text(x=744905, y=3931458, labels='biomass (kg/ha)', font=2, offset=0)
  dev.off()
}

#plot all points together
reducer <- 900
setwd(file.path(plot_results, 'forage/allpoints'))
for (i in 2:5) {
  png(file = paste(gsub('/', '_', names(forage_summary_sp)[i]), '_standing_biomass.png', sep = ''), family = 'Book Antiqua', width = 700, height = 500, units = 'px', res=100)
  par(mar=c(2, 2, 2, 2.5))
  plot(hillshade_1m, main=paste(names(waypoint_forage_sp)[i], 'standing biomass at Camatta at other waypoints'), col=gray(30:80/100), legend=FALSE, axes=F, ylim=c(3931300, 3931750))
  plot(dem_1m, col=terrain.colors(255, alpha=0.35), add=T)
  points(waypoint_forage_sp, cex=waypoint_forage[,i]/reducer, col='green', pch=17)
  points(forage_summary_sp, cex=forage_summary[,i]/reducer, col='green', pch=17)
  legend(x=744875, y=3931455, legend=c('1,000', '2,000', '4,000'), col='green', pch=17, pt.cex=c(1000/reducer, 2000/reducer, 4000/reducer), x.intersp = 2, y.intersp = 1.9, bty="n")
  text(x=744905, y=3931458, labels='biomass (kg/ha)', font=2, offset=0)
  dev.off()
}

#read in shapefiles for waypoint and sensor forage data
setwd(file.path(forage_data, 'results'))
list.files(pattern = glob2rx('*.shp'))
sensor_forage_sp <- shapefile("sensor_forage2017.shp")
waypoint_forage_sp <- shapefile("waypoint_forage2017.shp")  
all_forage_sp <- rbind(sensor_forage_sp, waypoint_forage_sp)

###analysis comparing forage growth and soil moisture drydown
#calculate kg/ha*day gains
sensor_forage_sp$gain_period1 <- (sensor_forage_sp$clp031417-sensor_forage_sp$clp021517)/27 #27 days between 3/14/17 and 2/14/17
sensor_forage_sp$gain_period2 <- (sensor_forage_sp$clp041017-sensor_forage_sp$clp031417)/27 #27 days between 4/10/17 and 3/14/17
sensor_forage_sp$gain_54days <- (sensor_forage_sp$clp041017-sensor_forage_sp$clp021517)/54
sensor_forage_sp$VWC7cmslope_period1 <- NA
sensor_forage_sp$VWC7cmslope_period2 <- NA
sensor_forage_sp$VWC22cmslope_period1 <- NA
sensor_forage_sp$VWC22cmslope_period2 <- NA

#read in 7 cm soil moisture data
setwd(soil_VWCdata)
list.files()
medianVWC_7cm <- read.csv("MedianVWC_7cm_dailymeans_by_location.csv")
end_data <- which(colnames(medianVWC_7cm)=='May_03_2017')
medianVWC_7cm <- medianVWC_7cm[,1:end_data]
medianVWC_7cm <- as.data.frame(t(medianVWC_7cm))
colnames(medianVWC_7cm) <- paste0('location_', as.character(as.integer(medianVWC_7cm[1,])))
medianVWC_7cm <- medianVWC_7cm[-1,]
medianVWC_7cm$date <- as.Date(rownames(medianVWC_7cm), '%b_%d_%Y')
feb20_2017row <- which(rownames(medianVWC_7cm)=='Feb_20_2017')
mar14_2017row <- which(rownames(medianVWC_7cm)=='Mar_14_2017')
mar09_2017row <- which(rownames(medianVWC_7cm)=='Mar_09_2017')
mar28_2017row <- which(rownames(medianVWC_7cm)=='Mar_28_2017')
apr10_2017row <- which(rownames(medianVWC_7cm)=='Apr_10_2017')
#i <- 1
for (i in 1:16) {
  if (i == 13) {
    sensor_forage_sp$VWC7cmslope_period1[i] <- NA # datalogger 13 appeared to go bad on March 10th, 2017 at 1 PM and all went to NA on March 16th, 2017
    next
  }
  drydown <- lm(medianVWC_7cm[,i][feb20_2017row:mar14_2017row] ~ medianVWC_7cm$date[feb20_2017row:mar14_2017row])
  drydown_slope <- drydown$coefficients[2]
  names(drydown_slope) <- colnames(medianVWC_7cm[i])
  print(drydown_slope)
  sensor_forage_sp$VWC7cmslope_period1[i] <- drydown_slope
}
for (i in 1:16) {
  if (i == 13) {
    sensor_forage_sp$VWC7cmslope_period2[i] <- NA # datalogger 13 appeared to go bad on March 10th, 2017 at 1 PM and all went to NA on March 16th, 2017
    next
  }
  drydown <- lm(medianVWC_7cm[,i][mar28_2017row:apr10_2017row] ~ medianVWC_7cm$date[mar28_2017row:apr10_2017row])
  drydown_slope <- drydown$coefficients[2]
  names(drydown_slope) <- colnames(medianVWC_7cm[i])
  print(drydown_slope)
  sensor_forage_sp$VWC7cmslope_period2[i] <- drydown_slope
}
#test relationship between forage growth and soil VWC drydown at 7 cm (period 1)
summary(lm(sensor_forage_sp$gain_period1 ~ sensor_forage_sp$VWC7cmslope_period1))
plot(sensor_forage_sp$gain_period1 ~ sensor_forage_sp$VWC7cmslope_period1, ylim=c(0,60))
#test relationship between forage growth and soil VWC drydown at 7 cm(period 2)
summary(lm(sensor_forage_sp$gain_period2 ~ sensor_forage_sp$VWC7cmslope_period2))
plot(sensor_forage_sp$gain_period2 ~ sensor_forage_sp$VWC7cmslope_period2)

#read in 22 cm soil moisture data
setwd(soil_VWCdata)
list.files()
medianVWC_22cm <- read.csv("MedianVWC_22cm_dailymeans_by_location.csv")
end_data <- which(colnames(medianVWC_22cm)=='May_03_2017')
medianVWC_22cm <- medianVWC_22cm[,1:end_data]
medianVWC_22cm <- as.data.frame(t(medianVWC_22cm))
colnames(medianVWC_22cm) <- paste0('location_', as.character(as.integer(medianVWC_22cm[1,])))
medianVWC_22cm <- medianVWC_22cm[-1,]
medianVWC_22cm$date <- as.Date(rownames(medianVWC_22cm), '%b_%d_%Y')
feb20_2017row <- which(rownames(medianVWC_22cm)=='Feb_20_2017')
feb23_2017row <- which(rownames(medianVWC_22cm)=='Feb_23_2017')
mar14_2017row <- which(rownames(medianVWC_22cm)=='Mar_14_2017')
mar09_2017row <- which(rownames(medianVWC_22cm)=='Mar_09_2017')
mar28_2017row <- which(rownames(medianVWC_22cm)=='Mar_28_2017')
mar31_2017row <- which(rownames(medianVWC_22cm)=='Mar_31_2017')
mar15_2017row <- which(rownames(medianVWC_22cm)=='Mar_15_2017')
apr10_2017row <- which(rownames(medianVWC_22cm)=='Apr_10_2017')
#i <- 1
for (i in 1:16) {
  if (i == 13) {
    sensor_forage_sp$VWC22cmslope_period1[i] <- NA # datalogger 13 appeared to go bad on March 10th, 2017 at 1 PM and all went to NA on March 16th, 2017
    next
  }
  drydown <- lm(medianVWC_22cm[,i][feb23_2017row:mar14_2017row] ~ medianVWC_22cm$date[feb23_2017row:mar14_2017row])
  drydown_slope <- drydown$coefficients[2]
  names(drydown_slope) <- colnames(medianVWC_22cm[i])
  print(drydown_slope)
  sensor_forage_sp$VWC22cmslope_period1[i] <- drydown_slope
}
for (i in 1:16) {
  if (i == 13) {
    sensor_forage_sp$VWC22cmslope_period2[i] <- NA # datalogger 13 appeared to go bad on March 10th, 2017 at 1 PM and all went to NA on March 16th, 2017
    next
  }
  drydown <- lm(medianVWC_22cm[,i][mar31_2017row:apr10_2017row] ~ medianVWC_22cm$date[mar31_2017row:apr10_2017row])
  drydown_slope <- drydown$coefficients[2]
  names(drydown_slope) <- colnames(medianVWC_22cm[i])
  print(drydown_slope)
  sensor_forage_sp$VWC22cmslope_period2[i] <- drydown_slope
}

#test relationship between forage growth and soil VWC drydown at 22 cm (period 1)
summary(lm(sensor_forage_sp$gain_period1 ~ sensor_forage_sp$VWC22cmslope_period1))
plot(sensor_forage_sp$gain_period1 ~ sensor_forage_sp$VWC22cmslope_period1, ylim=c(0,60))
#test relationship between forage growth and soil VWC drydown at 22 cm(period 2)
summary(lm(sensor_forage_sp$gain_period2 ~ sensor_forage_sp$VWC22cmslope_period2))
plot(sensor_forage_sp$gain_period2 ~ sensor_forage_sp$VWC22cmslope_period2)


#read in 1, 2, and 3 m DEM and calculate slope and aspect to compare deltas relative to scale
setwd(dem_fineres)
list.files(pattern = glob2rx('*.tif'))
dem_1m <- raster('camatta_Nov2016_1m_dsm.tif')
dem_2m <- raster("camatta_Nov2016_2m_dsm.tif")
dem_3m <- raster("camatta_Nov2016_3m_dsm.tif")
slope_1m <- terrain(dem_1m, opt='slope', unit='degrees')
slope_2m <- terrain(dem_2m, opt='slope', unit='degrees')
slope_3m <- terrain(dem_3m, opt='slope', unit='degrees')
aspect_1m <- terrain(dem_1m, opt='aspect', unit='degrees')
aspect_2m <- terrain(dem_2m, opt='aspect', unit='degrees')
aspect_3m <- terrain(dem_3m, opt='aspect', unit='degrees')
terrain_1m <- stack(dem_1m, slope_1m, aspect_1m)
names(terrain_1m) <- c('elev_1m', 'slope_deg_1m', 'aspect_deg_1m')
terrain_2m <- stack(dem_2m, slope_2m, aspect_2m)
names(terrain_2m) <- c('elev_2m', 'slope_deg_2m', 'aspect_deg_2m')
terrain_3m <- stack(dem_3m, slope_3m, aspect_3m)
names(terrain_3m) <- c('elev_3m', 'slope_deg_3m', 'aspect_deg_3m')
all_forage_sp <- extract(terrain_1m, all_forage_sp, method='simple', sp=TRUE) #default method is 'simple'; could set method='bilinear', which is the average of four nearest raster cells to point of interest
all_forage_sp <- extract(terrain_2m, all_forage_sp, method='simple', sp=TRUE)
all_forage_sp <- extract(terrain_3m, all_forage_sp, method='simple', sp=TRUE)

#examine differences in estimate of terrain characteristics depending on scale of DEM
all_forage <- data.frame(all_forage_sp)
summary(all_forage$elev_1m - all_forage$elev_2m)
summary(all_forage$elev_1m - all_forage$elev_3m)
summary(all_forage$elev_2m - all_forage$elev_3m)
summary(all_forage$slope_deg_1m - all_forage$slope_deg_2m)
summary(all_forage$slope_deg_1m - all_forage$slope_deg_3m)
summary(all_forage$slope_deg_2m - all_forage$slope_deg_3m)
summary(all_forage$aspect_deg_1m - all_forage$aspect_deg_2m)
summary(all_forage$aspect_deg_1m - all_forage$aspect_deg_3m)
summary(all_forage$aspect_deg_2m - all_forage$aspect_deg_3m)

#classify simple aspect in degrees
all_forage$aspect_cardinal_1m <- NA
all_forage$aspect_cardinal_1m[which(all_forage$aspect_deg_1m < 135 & all_forage$aspect_deg_1m > 45)] <- 'East facing'
all_forage$aspect_cardinal_1m[which(all_forage$aspect_deg_1m < 225 & all_forage$aspect_deg_1m > 135)] <- 'South facing'
all_forage$aspect_cardinal_1m[which(all_forage$aspect_deg_1m < 360 & all_forage$aspect_deg_1m > 315)] <- 'North facing'
all_forage$aspect_cardinal_1m[which(all_forage$aspect_deg_1m < 45 & all_forage$aspect_deg_1m > 0)] <- 'North facing'
all_forage$aspect_cardinal_1m[which(all_forage$aspect_deg_1m < 315 & all_forage$aspect_deg_1m > 225)] <- 'West facing'
summary(as.factor(all_forage$aspect_cardinal_1m)) #count the points in each class
tapply(all_forage$clp021517, all_forage$aspect_cardinal_1m, mean)
tapply(all_forage$clp031417, all_forage$aspect_cardinal_1m, mean)
tapply(all_forage$clp041017, all_forage$aspect_cardinal_1m, mean)
tapply(all_forage$clp050117, all_forage$aspect_cardinal_1m, mean)
tapply(all_forage$clp021517, all_forage$aspect_cardinal_1m, summary)
tapply(all_forage$clp031417, all_forage$aspect_cardinal_1m, summary)
tapply(all_forage$clp041017, all_forage$aspect_cardinal_1m, summary)
tapply(all_forage$clp050117, all_forage$aspect_cardinal_1m, summary)
all_forage[which(all_forage$aspect_cardinal=='South facing'), ]
all_forage[which(all_forage$aspect_cardinal=='West facing'), ]
all_forage[which(all_forage$aspect_cardinal=='North facing'), ]

#classify more complex aspect
all_forage$aspect_8cardinal_1m <- NA
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 112.5 & all_forage$aspect_deg_1m > 67.5)] <- 'E'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 157.5 & all_forage$aspect_deg_1m > 112.5)] <- 'SE'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 202.5 & all_forage$aspect_deg_1m > 157.5)] <- 'S'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 247.5 & all_forage$aspect_deg_1m > 202.5)] <- 'SW'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 292.5 & all_forage$aspect_deg_1m > 247.5)] <- 'W'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 337.5 & all_forage$aspect_deg_1m > 292.5)] <- 'NW'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 360 & all_forage$aspect_deg_1m > 337.5)] <- 'N'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 22.5 & all_forage$aspect_deg_1m > 0)] <- 'N'
all_forage$aspect_8cardinal_1m[which(all_forage$aspect_deg_1m < 67.5 & all_forage$aspect_deg_1m > 22.5)] <- 'NE'
summary(as.factor(all_forage$aspect_8cardinal_1m))
tapply(all_forage$clp021517, all_forage$aspect_8cardinal_1m, mean)
tapply(all_forage$clp031417, all_forage$aspect_8cardinal_1m, mean)
tapply(all_forage$clp041017, all_forage$aspect_8cardinal_1m, mean)
tapply(all_forage$clp050117, all_forage$aspect_8cardinal_1m, mean)
tapply(all_forage$clp021517, all_forage$aspect_8cardinal, summary)
tapply(all_forage$clp031417, all_forage$aspect_8cardinal, summary)
tapply(all_forage$clp041017, all_forage$aspect_8cardinal, summary)
tapply(all_forage$clp050117, all_forage$aspect_8cardinal, summary)

#test aspect classification of points based on 3m DEM; results do not differ from 1 m DEM
all_forage$aspect_cardinal_3m <- NA
all_forage$aspect_cardinal_3m[which(all_forage$aspect_deg_3m < 135 & all_forage$aspect_deg_3m > 45)] <- 'East facing'
all_forage$aspect_cardinal_3m[which(all_forage$aspect_deg_3m < 225 & all_forage$aspect_deg_3m > 135)] <- 'South facing'
all_forage$aspect_cardinal_3m[which(all_forage$aspect_deg_3m < 360 & all_forage$aspect_deg_3m > 315)] <- 'North facing'
all_forage$aspect_cardinal_3m[which(all_forage$aspect_deg_3m < 45 & all_forage$aspect_deg_3m > 0)] <- 'North facing'
all_forage$aspect_cardinal_3m[which(all_forage$aspect_deg_3m < 315 & all_forage$aspect_deg_3m > 225)] <- 'West facing'
summary(as.factor(all_forage$aspect_cardinal_3m))
tapply(all_forage$clp021517, all_forage$aspect_cardinal_3m, mean)
tapply(all_forage$clp031417, all_forage$aspect_cardinal_3m, mean)
tapply(all_forage$clp041017, all_forage$aspect_cardinal_3m, mean)
tapply(all_forage$clp050117, all_forage$aspect_cardinal_3m, mean)

