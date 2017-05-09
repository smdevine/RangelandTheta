#assume that sensor 7A and 22A crossed at location 15 in the datalogger ports (1 and 2); checked on 3/9/17, as labelled, sensors are in correct ports but could have been mislabeled. code to correct this is functional as of 5/3/17
#large divergence at Location 11 at 22 cm depth (creates problem for plotting)
#sensor A at location 14 at 7 cm depth was replaced on 1/16/17, because it was giving NA up to that point
#sensor B at location 9 at 22 cm depth has data gap in early March (569 NAs); sensor A closely tracks so not really a problem
#location 13 datalogger stopped working on 3/16/17; however on 3/10/17; both sensors at 22 cm depth suddenly reported a 0.1 VWC drop at 1 PM, which must be erroneous
#to-do 5/3/17 (1) add elevation to terrain characteristics; (2) check to see if higher resolution DEM is available; (3)solar radiation--beam radiance calculation; (4) elevation above a channel; (5) distance from a ridge; (6) temporal stability of soil water 

library(rgdal)
library(raster)
options(digits = 10)
#library(sp)
min_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {min(x, na.rm = TRUE)}
}
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}
mainDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/apr2017/csv_files'
spatialDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'

#read-in coordinate data; not necessary to rerun this now
plotDir <- 'C:/Users/smdevine/Desktop/rangeland project/study_plots'
elevDir <- 'C:/Users/smdevine/Desktop/rangeland project/elevation_NED10M_studysite/elevation'
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

#read in terrain char for each sensor location
setwd(results)
sensor_pts_df <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE)

#now, read-in and process the soil moisture data
setwd(mainDir)
soil_moisture_fnames <- list.files(pattern = glob2rx('*.csv'))
soil_moisture_dfs <- lapply(soil_moisture_fnames, read.csv, stringsAsFactors=FALSE, na.strings="***")
names(soil_moisture_dfs) <- soil_moisture_fnames
# test <- lapply(soil_moisture_dfs, function(x) {x[1, 1] <- ""}) #this didn't work0

#reformat each dataframe; need to check excel files exported from DataTrac that sensor names are included (e.g. 5TM_P16_A7)
for (i in 1:length(soil_moisture_dfs)) {
  soil_moisture_dfs[[i]][1, 1] <- ""
  colnames(soil_moisture_dfs[[i]]) <- paste(soil_moisture_dfs[[i]][1, ], soil_moisture_dfs[[i]][2, ])
  soil_moisture_dfs[[i]] <- soil_moisture_dfs[[i]][-1:-2, ]
  for (j in 2:9) {
    soil_moisture_dfs[[i]][ ,j] <- as.numeric(soil_moisture_dfs[[i]][ ,j])
  }
  soil_moisture_dfs[[i]]$location <- substr(colnames(soil_moisture_dfs[[i]])[2], 5, 7)
  soil_moisture_dfs[[i]]$location <- gsub('[P, _]', '', soil_moisture_dfs[[i]]$location)
  soil_moisture_dfs[[i]]$` Measurement Time` <- strptime(soil_moisture_dfs[[i]]$` Measurement Time`, "%m/%d/%Y %I:%M:%S %p")
  soil_moisture_dfs[[i]]$date <- format(soil_moisture_dfs[[i]]$` Measurement Time`, "%Y%m%d") #makes a specific time e.g. on 4/1/17 into just 20170401
  k <- which(soil_moisture_dfs[[i]]$date=='20161118') #find the indices of rows when the date was 11/18/16
  soil_moisture_dfs[[i]] <- soil_moisture_dfs[[i]][-k, ] #then remove them
  #change point 14, sensor A at 7 cm depth to NA on 1/16/2017, which is when it was replaced (column 2 in the data.frame) 
  if (soil_moisture_dfs[[i]]$location[1] == 14){
    k <- which(soil_moisture_dfs[[i]]$date == '20170116')
    soil_moisture_dfs[[i]][k, 2] <- NA
  } 
  print(paste("Location ", soil_moisture_dfs[[i]]$location[1], 'A at 7 cm depth has ', length(which(is.na(soil_moisture_dfs[[i]][ ,2]))), " NAs for VWC.", sep=''))
  print(paste("Location ", soil_moisture_dfs[[i]]$location[1], 'A at 22 cm depth has ', length(which(is.na(soil_moisture_dfs[[i]][ ,4]))), " NAs for VWC.", sep=''))
  print(paste("Location ", soil_moisture_dfs[[i]]$location[1], 'B at 7 cm depth has ', length(which(is.na(soil_moisture_dfs[[i]][ ,6]))), " NAs for VWC.", sep=''))
  print(paste("Location ", soil_moisture_dfs[[i]]$location[1], 'B at 22 cm depth has ', length(which(is.na(soil_moisture_dfs[[i]][ ,8]))), " NAs for VWC.", sep=''))
}
#switch A7 sensor with A22 sensor data for location 15 (updated for April 2017 download)
a22VWC <- soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A7 m³/m³ VWC`
a7VWC <- soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A22 m³/m³ VWC`
a22T <- soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A7 °C Temp`
a7T <- soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A22 °C Temp`
soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A7 m³/m³ VWC` <- a7VWC
soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A22 m³/m³ VWC` <- a22VWC
soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A7 °C Temp` <- a7T
soil_moisture_dfs$`Cam15-10Apr2017-1241.csv`$`5TM_P15_A22 °C Temp` <- a22T

#merge terrain characteristics with soil_moisture_dfs 
for (i in 1:length(soil_moisture_dfs)){
  location <- soil_moisture_dfs[[i]]$location[1]
  rownum <- match(location, sensor_pts_df$datalogger_no)
  land_position_data <- sensor_pts_df[rownum, ]
  soil_moisture_dfs[[i]] <- merge(soil_moisture_dfs[[i]], land_position_data)
}

#write merged soil_moisture_dfs & terrain characteristics to results
setwd(file.path(results, 'processed_soil_moisture/Apr2017'))
for (i in 1:length(soil_moisture_dfs)) {
  write.csv(soil_moisture_dfs[[i]], paste(sub('.csv', '', names(soil_moisture_dfs)[i]), 'processed', format(Sys.Date(), "%F"), '.csv', sep = ''), row.names = FALSE)
}

#read in merged data
setwd(file.path(results, 'processed_soil_moisture/Apr2017'))
soil_moisture_fnames <- list.files(pattern = glob2rx('*.csv'))
soil_moisture_dfs <- lapply(soil_moisture_fnames, read.csv, stringsAsFactors=FALSE)
names(soil_moisture_dfs) <- soil_moisture_fnames

#write function to plot raw data by sensor
#need to work on scaling because if scale for Sensor A differs from Sensor B, it will not plot correctly - problem with Point 11 at 22 cm depth
for (i in 1:length(soil_moisture_dfs)) {
  labDates <- seq(from=soil_moisture_dfs[[i]][ ,1][1], to=soil_moisture_dfs[[i]][ ,1][nrow(soil_moisture_dfs[[i]])], by='week', format='%m/%d/%Y')
  plot(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,2], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col='Blue', main=paste('Point', soil_moisture_dfs[[i]]$location[1], 'at 7 cm depth'))
  axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
  lines.default(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,6], type='l', col='Red')
  legend("bottomright", legend=(c('sensor A', 'sensor B')), lty=1, col = c( 'blue', 'red'))
  plot(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,4], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col='Blue', main=paste('Point', soil_moisture_dfs[[i]]$location[1], 'at 22 cm depth'))
  axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
  lines.default(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,8], type='l', col='Red')
  legend("bottomright", legend=(c('sensor A', 'sensor B')), lty=1, col = c( 'blue', 'red'))
}

#plot all on one graph at 7 cm depth 
plot(soil_moisture_dfs[[1]][ ,1], soil_moisture_dfs[[1]][ ,2], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='All sensors at 7 cm', ylim=c(0.1, 0.45))
axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
lines.default(soil_moisture_dfs[[1]][ ,1], soil_moisture_dfs[[1]][ ,6], type='l', col=1)
for (i in 2:length(soil_moisture_dfs)) {
  lines.default(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,2], col=i+1)
  lines.default(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,6], col=i+1)
}

#plot all on one graph at 22 cm depth 
plot(soil_moisture_dfs[[1]][ ,1], soil_moisture_dfs[[1]][ ,4], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='All sensors at 22 cm', ylim=c(0.1, 0.45))
axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
lines.default(soil_moisture_dfs[[1]][ ,1], soil_moisture_dfs[[1]][ ,8], type='l', col=1)
for (i in 2:length(soil_moisture_dfs)) {
  lines.default(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,4], col=i+1)
  lines.default(soil_moisture_dfs[[i]][ ,1], soil_moisture_dfs[[i]][ ,8], col=i+1)
}

#plot means by location at 22 cm depth. 
plot(soil_moisture_dfs[[1]][ ,1], rowMeans(soil_moisture_dfs[[1]][ ,c(4,8)], na.rm = TRUE), type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='VWC means by location at 22 cm depth', ylim=c(0.1, 0.45))
axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
for (i in 2:length(soil_moisture_dfs)) {
  lines.default(soil_moisture_dfs[[i]][ ,1], rowMeans(soil_moisture_dfs[[i]][ ,c(4,8)], na.rm = TRUE), col=i+1)
}
#plot means by location at 7 cm depth. 
plot(soil_moisture_dfs[[1]][ ,1], rowMeans(soil_moisture_dfs[[1]][ ,c(2,6)], na.rm = TRUE), type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='VWC means by location at 7 cm depth', ylim=c(0.1, 0.45))
axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
for (i in 2:length(soil_moisture_dfs)) {
  lines.default(soil_moisture_dfs[[i]][ ,1], rowMeans(soil_moisture_dfs[[i]][ ,c(2,6)], na.rm = TRUE), col=i+1)
}

#write function to take raw data and produce daily statistics for each sensor
for (i in 1:length(soil_moisture_dfs)) {
  for (j in c(2, 4, 6, 8)) { #this refers to columns with VWC data
    VWC <- as.data.frame(tapply(soil_moisture_dfs[[i]][ ,j], soil_moisture_dfs[[i]]$date, mean, na.rm=TRUE))
    colnames(VWC) <- 'MeanVWC'
    VWC$MedianVWC <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j], soil_moisture_dfs[[i]]$date, median, na.rm=TRUE))
    VWC$MaxVWC <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j], soil_moisture_dfs[[i]]$date, max_modified))
    VWC$MinVWC <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j], soil_moisture_dfs[[i]]$date, min_modified))
    VWC$MeanT <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j+1], soil_moisture_dfs[[i]]$date, mean, na.rm=TRUE))
    VWC$MedianT <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j+1], soil_moisture_dfs[[i]]$date, median, na.rm=TRUE))
    VWC$MinT <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j+1], soil_moisture_dfs[[i]]$date, min_modified))
    VWC$MaxT <- as.numeric(tapply(soil_moisture_dfs[[i]][ ,j+1], soil_moisture_dfs[[i]]$date, max_modified))
    VWC$Date <- rownames(VWC)
    rownames(VWC) <- NULL
    VWC$Location <- soil_moisture_dfs[[i]]$location[1]
    VWC$Depth <- substr(colnames(soil_moisture_dfs[[i]])[j], 10, 12)
    VWC$Depth <- gsub('[A, B, m, .]', '', VWC$Depth)
    VWC$Depth <- as.integer(gsub(' ', '', VWC$Depth))
    VWC$SubsampleID <-substr(colnames(soil_moisture_dfs[[i]])[j], 9, 10)
    VWC$SubsampleID <- gsub('[7, 2, _, .]', '', VWC$Subsample)
    VWC$Nrt_10N <- soil_moisture_dfs[[i]]$Nrt_10N[1]
    VWC$Est_10N <- soil_moisture_dfs[[i]]$Est_10N[1]
    VWC$slope <- soil_moisture_dfs[[i]]$slope[1]
    VWC$aspect_deg <- soil_moisture_dfs[[i]]$aspect[1]
    VWC$cti <- soil_moisture_dfs[[i]]$cti[1] #see raster package, terrain function for definition
    VWC$mean_curv <- soil_moisture_dfs[[i]]$mean_curv[1]
    VWC$aspect_cardinal <- soil_moisture_dfs[[i]]$aspect_cardinal[1]
    if (i==1 & j==2) {
      daily_dataVWC <- VWC
      next
    }
    daily_dataVWC <- rbind(daily_dataVWC, VWC)
  }
}
#add sensor code column
daily_dataVWC$sensor_code <- paste(daily_dataVWC$Location, '-', daily_dataVWC$Depth, '-', daily_dataVWC$SubsampleID, sep='') #as Location(i.e. datalogger no)-Depth-Subsample ID
#add time column
daily_dataVWC$Date_Calendar <- as.Date(daily_dataVWC$Date, format='%Y%m%d')
#order this by location, depth, subsampleID, and date
daily_dataVWC <- daily_dataVWC[with(daily_dataVWC, order(Location, Depth, SubsampleID, Date_Calendar)), ]

#save daily summary for each sensor
setwd(file.path(results, 'processed_soil_moisture/Apr2017/daily_by_sensor'))
write.csv(daily_dataVWC, paste('daily_by_sensor_summary', 'processed', format(Sys.Date(), "%F"), '.csv', sep = ''), row.names=FALSE) #9052 rows for April 2017 data

#read-in daily summary for each location summary
setwd(file.path(results, 'processed_soil_moisture/Apr2017/daily_by_sensor'))
daily_fnames <- list.files()
daily_fnames
daily_dataVWC <- read.csv(daily_fnames[2], stringsAsFactors = FALSE)
head(daily_dataVWC)
colnames(daily_dataVWC)

#function to produce daily means by depth for each pair of sensors for median, mean, max, and min daily data by sensor and then merged with terrain characteristics by location (i.e. datalogger #)
daily_by_location <- function(depth, df, varname, subdir) {
  a <- which(df$Depth==depth)
  specific_depth <- df[a,]
  depth_aggregated <- as.data.frame(tapply(specific_depth[[varname]], list(specific_depth$Location, specific_depth$Date_Calendar), mean, na.rm=TRUE))
  depth_aggregated$location <- as.integer(row.names(depth_aggregated))
  row.names(depth_aggregated) <- NULL
  depth_aggregated <- depth_aggregated[ ,c(ncol(depth_aggregated), 1:(ncol(depth_aggregated)-1))]
  setwd(results)
  sensor_pts_df <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE) #this needs to be updated for additional terrain characteristics with higher res DEM
  depth_aggregated <- merge.data.frame(depth_aggregated, sensor_pts_df, by='location')
  setwd(file.path(results, 'processed_soil_moisture/Apr2017/daily_by_location', subdir))
  write.csv(depth_aggregated, paste(varname, '_', as.character(depth), 'cm_dailymeans_by_location.csv', sep = ''), row.names = FALSE)
}

daily_by_location(7, daily_dataVWC, 'MedianVWC', 'VWC')
daily_by_location(22, daily_dataVWC, 'MedianVWC', 'VWC')
daily_by_location(7, daily_dataVWC, 'MeanVWC', 'VWC')
daily_by_location(22, daily_dataVWC, 'MeanVWC', 'VWC')
daily_by_location(7, daily_dataVWC, 'MaxVWC', 'VWC')
daily_by_location(22, daily_dataVWC, 'MaxVWC', 'VWC')
daily_by_location(7, daily_dataVWC, 'MedianT', 'Temperature')
daily_by_location(22, daily_dataVWC, 'MedianT', 'Temperature')
daily_by_location(7, daily_dataVWC, 'MeanT', 'Temperature')
daily_by_location(22, daily_dataVWC, 'MeanT', 'Temperature')
daily_by_location(7, daily_dataVWC, 'MaxT', 'Temperature')
daily_by_location(22, daily_dataVWC, 'MaxT', 'Temperature')

#plotting daily data (needs to be revised now)
sensor_codes <- unique(daily_dataVWC$sensor_code)
for (i in 1:length(sensor_codes)) {
  by_sensor <- daily_dataVWC[which(daily_dataVWC$sensor_code==sensor_codes[i]), ]
  labDates <- seq(from=by_sensor$Date_Calendar[1], to=by_sensor$Date_Calendar[nrow(by_sensor)], by='week', format='%m/%d/%Y')
  plot(by_sensor$Date_Calendar, by_sensor$MeanVWC, type='b', main=paste('sensor', sensor_codes[i]), xlab='Date', ylab='Mean VWC', xaxt='n')
  axis.Date(side = 1, labDates, at=labDates, format = '%m/%d')
  #plot(by_sensor$Date_Calendar, by_sensor$MaxVWC, type='b')
}



#extra code
#to determine change from beginning to end of each day
for (k in 1:length(dates)) {
  t <- which(soil_moisture_dfs[[i]]$date==dates[k])
  VWC_init <- soil_moisture_dfs[[i]][t[1], j]
  VWC_final <- soil_moisture_dfs[[i]][t[length(t)], j]
  VWC$DeltaVWC_day[k] <- VWC_final - VWC_init
}


#made spatial points object from scratch as above
#coordinate data from Grace 2/2/17 (from 1/16/17 trip). these are the coordinates for the mid-point between each pair of sensors at each location (datalogger)
setwd(spatialDir)
coordinates <- shapefile('Point_ge.shp')
coordinates_df <- data.frame(coordinates)
print(coordinates_df, digits=10) #otherwise, not all of the coordinates will show up
crs(coordinates)
geom(coordinates)

head(soil_moisture_dfs$`Cam1-16Jan2017-1508.csv`)
class(soil_moisture_dfs$`Cam1-16Jan2017-1508.csv`$`5TM_P1_A7 m³/m³ VWC`)

