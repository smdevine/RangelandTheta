#assume that sensor 7A and 22A crossed at location 15 in the datalogger ports (1 and 2); checked on 3/9/17, as labelled, sensors are in correct ports but could have been mislabeled. code to correct this is functional as of 5/3/17
#large divergence at Location 11 at 22 cm depth (creates problem for plotting)
#sensor A at location 14 at 7 cm depth was replaced on 1/16/17, because it was giving NA up to that point
#sensor B at location 9 at 22 cm depth has data gap in early March (569 NAs); sensor A closely tracks so not really a problem
#location 13 datalogger stopped working on 3/16/17; however on 3/10/17; both sensors at 22 cm depth suddenly reported a 0.1 VWC drop at 1 PM, which must be erroneous; on 3/10/17, both sensors at 7 cm reported a sudden drop of 0.02 VWC 
#to-do 5/3/17 (1) add elevation to terrain characteristics; (2) check to see if higher resolution DEM is available; (3)solar radiation--beam radiance calculation; (4) elevation above a channel; (5) distance from a ridge; (6) temporal stability of soil water 
#moved mapping code to 'general_mapping.R' on 5/22/17
#library(readxl)
library(extrafont)
library(extrafontdb)
loadfonts()
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
mainDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/may2017/csv_files'
spatialDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017_normalized'
forage_data <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'

#read in terrain char for each sensor location
setwd(results)
sensor_pts_df <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE)

#now, read-in and process the soil moisture data
setwd(mainDir)
soil_moisture_fnames <- list.files(pattern = glob2rx('*.csv'))
soil_moisture_dfs <- lapply(soil_moisture_fnames, read.csv, stringsAsFactors=FALSE, na.strings="***")
names(soil_moisture_dfs) <- soil_moisture_fnames
# test <- lapply(soil_moisture_dfs, function(x) {x[1, 1] <- ""}) #this didn't work

#reformat each dataframe; need to check excel files exported from DataTrac that sensor names are included (e.g. 5TM_P16_A7)
for (i in 1:length(soil_moisture_dfs)) {
  soil_moisture_dfs[[i]][1, 1] <- "" #gets rid of string 'n records' in 1,1 position
  colnames(soil_moisture_dfs[[i]]) <- paste(soil_moisture_dfs[[i]][1, ], soil_moisture_dfs[[i]][2, ]) #create new column names by concatenating rows 1 and 2
  colnames(soil_moisture_dfs[[i]])[1] <- "Measurement.Time" #make a proper column name 
  soil_moisture_dfs[[i]] <- soil_moisture_dfs[[i]][-1:-2, ]
  for (j in 2:9) {
    soil_moisture_dfs[[i]][ ,j] <- as.numeric(soil_moisture_dfs[[i]][ ,j])
  }
  soil_moisture_dfs[[i]]$location <- substr(colnames(soil_moisture_dfs[[i]])[2], 5, 7)
  soil_moisture_dfs[[i]]$location <- gsub('[P, _]', '', soil_moisture_dfs[[i]]$location)
  soil_moisture_dfs[[i]]$Measurement.Time <- strptime(soil_moisture_dfs[[i]]$Measurement.Time, "%m/%d/%Y %I:%M:%S %p")
  soil_moisture_dfs[[i]]$date <- format(soil_moisture_dfs[[i]]$Measurement.Time, "%Y%m%d") #makes a specific time e.g. on 4/1/17 into just 20170401
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

#switch A7 sensor with A22 sensor data for location 15 (updated for May 1-3, 2017 download)
a22VWC <- soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A7 m³/m³ VWC`
a7VWC <- soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A22 m³/m³ VWC`
a22T <- soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A7 °C Temp`
a7T <- soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A22 °C Temp`
soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A7 m³/m³ VWC` <- a7VWC
soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A22 m³/m³ VWC` <- a22VWC
soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A7 °C Temp` <- a7T
soil_moisture_dfs$`Cam15-01May2017-1507.csv`$`5TM_P15_A22 °C Temp` <- a22T

#merge terrain characteristics with soil_moisture_dfs 
for (i in 1:length(soil_moisture_dfs)){
  location <- as.integer(soil_moisture_dfs[[i]]$location[1])
  rownum <- match(location, sensor_pts_df$location) #column name was previously datalogger_no
  land_position_data <- sensor_pts_df[rownum, ]
  soil_moisture_dfs[[i]] <- merge(soil_moisture_dfs[[i]], land_position_data)
}

#write merged soil_moisture_dfs & terrain characteristics to results
setwd(file.path(results, 'processed_soil_moisture/May2017'))
for (i in 1:length(soil_moisture_dfs)) {
  write.csv(soil_moisture_dfs[[i]], paste(sub('.csv', '', names(soil_moisture_dfs)[i]), 'processed', format(Sys.Date(), "%F"), '.csv', sep = ''), row.names = FALSE)
}

#read in merged data
setwd(file.path(results, 'processed_soil_moisture/May2017'))
soil_moisture_fnames <- list.files(pattern = glob2rx('*.csv'))
soil_moisture_dfs <- lapply(soil_moisture_fnames, read.csv, stringsAsFactors=FALSE)
names(soil_moisture_dfs) <- soil_moisture_fnames

#check column names
for (i in 1:16) {
  print(colnames(soil_moisture_dfs[[i]]))
}


#write function to plot raw data by sensor
#need to work on scaling because if scale for Sensor A differs from Sensor B, it will not plot correctly - problem with Point 11 at 22 cm depth
#had to update column indexing for May 2017 files because location is now inserted into column 1
for (i in 1:length(soil_moisture_dfs)) {
  labDates <- seq(from=soil_moisture_dfs[[i]]$Measurement.Time[1], to=soil_moisture_dfs[[i]]$Measurement.Time[nrow(soil_moisture_dfs[[i]])], by='week', format='%m/%d/%Y')
  plot(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,3], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col='Blue', main=paste('Point', soil_moisture_dfs[[i]]$location[1], 'at 7 cm depth'))
  axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
  lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,7], type='l', col='Red')
  legend("bottomright", legend=(c('sensor A', 'sensor B')), lty=1, col = c( 'blue', 'red'))
  plot(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,5], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col='Blue', main=paste('Point', soil_moisture_dfs[[i]]$location[1], 'at 22 cm depth'))
  axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
  lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,9], type='l', col='Red')
  legend("bottomright", legend=(c('sensor A', 'sensor B')), lty=1, col = c( 'blue', 'red'))
}

#plot all on one graph at 7 cm depth 
plot(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,3], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='All sensors at 7 cm', ylim=c(0.1, 0.45))
axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
lines.default(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,7], type='l', col=1)
for (i in 2:length(soil_moisture_dfs)) {
  lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,3], col=i+1)
  lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,7], col=i+1)
}

#plot all on one graph at 22 cm depth 
plot(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,5], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='All sensors at 22 cm', ylim=c(0.1, 0.45))
axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
lines.default(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,9], type='l', col=1)
for (i in 2:length(soil_moisture_dfs)) {
  lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,5], col=i+1)
  lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,9], col=i+1)
}

#write function to take raw data and produce daily statistics for each sensor
#will only work after reading in processed soil moisture files from this directory -- file.path(results, 'processed_soil_moisture/May2017'), line 159 above
for (i in 1:length(soil_moisture_dfs)) {
  for (j in c(3, 5, 7, 9)) { #this refers to columns with VWC data (updated for May2017 processed data)
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
    if (i==1 & j==3) {
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

#save daily summary for each sensor (updated for May 2017 results)
setwd(file.path(results, 'processed_soil_moisture/May2017/daily_by_sensor'))
write.csv(daily_dataVWC, paste('daily_by_sensor_summary', 'processed', format(Sys.Date(), "%F"), '.csv', sep = ''), row.names=FALSE) #9052 rows for April 2017 data

#read-in daily summary for each location summary
setwd(file.path(results, 'processed_soil_moisture/May2017/daily_by_sensor'))
daily_fnames <- list.files()
daily_fnames
daily_dataVWC <- read.csv(daily_fnames[1], stringsAsFactors = FALSE)
head(daily_dataVWC)
colnames(daily_dataVWC)

#function to produce daily means by depth for each pair of sensors for median, mean, max, and min daily data by sensor and then merged with terrain characteristics by location (i.e. datalogger #)
# df <- daily_dataVWC
# depth <- 7
# varname <- 'MedianVWC'
daily_by_location <- function(depth, df, varname, subdir) {
  a <- which(df$Depth==depth)
  specific_depth <- df[a,]
  depth_aggregated <- as.data.frame(tapply(specific_depth[[varname]], list(specific_depth$Location, specific_depth$Date_Calendar), mean, na.rm=TRUE))
  colnames(depth_aggregated) <- format.Date(as.Date(gsub('[-]', '', colnames(depth_aggregated)), format = '%Y%m%d'), '%b_%d_%Y') #this takes the original date (eg. "2016-11-19") and strips out the '/' (eg. "20161119") and then converts it to a column name ("Nov_19_2016") that would be coerced to "X2016.11.19" when reading the file back in as a data.frame
  depth_aggregated$location <- as.integer(row.names(depth_aggregated))
  row.names(depth_aggregated) <- NULL
  depth_aggregated <- depth_aggregated[ ,c(ncol(depth_aggregated), 1:(ncol(depth_aggregated)-1))]
  setwd(results)
  sensor_pts_df <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE) #this needs to be updated for additional terrain characteristics with higher res DEM
  depth_aggregated <- merge.data.frame(depth_aggregated, sensor_pts_df, by='location')
  setwd(file.path(results, 'processed_soil_moisture/May2017/daily_by_location', subdir))
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

#plotting daily data by location x depth location from summaries produced above
data_name <- 'VWC'
depth <- '7'
vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location', data_name, paste0('MedianVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
endcol <- which(colnames(vwc_data)=='May_03_2017')
dates <- seq.Date(as.Date('2016/11/19'), as.Date('2017/5/3'), by='day')
weeks <- seq.Date(as.Date('2016/11/19'), as.Date('2017/5/3'), by='week')
for (i in 1:nrow(vwc_data)) {
  plot(dates, vwc_data[i, 2:endcol], type='b', xlab='Date', ylab='Daily Median VWC at 7 cm (mean of 2 sensors)', xaxt='n', main=paste('Location', vwc_data$location[i], ',', vwc_data$aspect_cardinal[i], 'aspect'))
  axis.Date(side = 1, dates, at=weeks, format = '%m/%d')
  text(x=dates[75], y=0.18, labels=paste(round(vwc_data$mean_curv[i], 2), 'mean curvature', ',', round(vwc_data$cti[i], 2), 'Compound Topographic Index'))
}


for (i in 1:nrow(vwc_data)) {
  if (i == 1) {
    plot(dates, vwc_data[i, 2:endcol], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data[,2:endcol], na.rm=TRUE), max(vwc_data[,2:endcol], na.rm=TRUE)))
    text(x=dates[length(dates)-50], y=vwc_data[i, endcol-50], labels=vwc_data[i, 1], cex=0.7)
  } else {
      lines.default(dates, vwc_data[i, 2:endcol], xaxt='n', col=i+1)
      text(x=dates[length(dates)- 50], y=vwc_data[i, (endcol- 50)], labels=vwc_data[i, 1], cex=0.7)
    }
}
axis.Date(side = 1, dates, at=weeks, format = '%m/%d')

#normalize vwc_data
vwc_data_normalized <- vwc_data
vwc_data_normalized[ ,2:endcol] <- (vwc_data_normalized[ ,2:endcol] - rowMeans(vwc_data_normalized[ ,2:endcol], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:endcol], 1, sd, na.rm=TRUE)

#and then plot as above
for (i in 1:nrow(vwc_data_normalized)) {
  if (i == 1) {
    plot(dates[5:length(dates)], vwc_data_normalized[i, 6:endcol], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_normalized[,6:endcol], na.rm=TRUE), max(vwc_data_normalized[,6:endcol], na.rm=TRUE)), xlab = 'Dates', ylab='Std Deviations by Location')
    text(x=dates[25], y=vwc_data_normalized[i, 25], labels=vwc_data_normalized[i, 1], cex=0.7)
  } else {
    lines.default(dates[5:length(dates)], vwc_data_normalized[i, 6:endcol], xaxt='n', col=i+1)
    text(x=dates[25], y=vwc_data_normalized[i, 25], labels=vwc_data_normalized[i, 1], cex=0.7)
  }
}
axis.Date(side = 1, dates, at=weeks, format = '%m/%d')

#now plot normalized data as points on map [modified from mapping_soil_moisture_temperature.R]
magfactor <- function(x) {((x + 4) / 4)}
dem_1m <- raster(file.path(dem_fineres, 'camatta_Nov2016_1m_dsm.tif'))
hillshade_1m <- hillShade(terrain(dem_1m, opt='aspect'), terrain(dem_1m, opt='slope'), angle=45, direction=315)
coords <- coords <- vwc_data_normalized[ ,c('Est_10N', 'Nrt_10N')]
vwc_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data_normalized)

for (i in 2:which(colnames(vwc_data_normalized)=='May_01_2017')) {
  png(file = file.path(plot_results, '7cm_depth', paste(format(as.Date(names(vwc_data_sp)[i], format = '%b_%d_%Y'), '%Y%m%d'), '_daily_normalized_7cm.png', sep = '')), family = 'Book Antiqua', width = 700, height = 500, units = 'px', res=100)
  par(mar=c(2, 2, 2, 3)) #does not affect right side I think because plotting a raster creates a space on the right even if if legend=FALSE
  plot(hillshade_1m, main=paste(gsub('_', ' ', names(vwc_data_sp)[i]), 'normalized soil moisture at 7 cm'), col=gray(30:80/100), legend=FALSE, axes=FALSE)
  plot(dem_1m, col=terrain.colors(255, alpha=0.35), add=T)
  #mtext(text='Elevation (m)', side=1, line=1, at=744800)
  points(vwc_data_sp, cex=magfactor(vwc_data_normalized[,i]), col='blue', pch=19)
  #text(vwc_data_sp, labels=vwc_data_sp$location, pos=1, cex=1.1, halo=T)
  legend(x=744875, y=3931450, legend=c('-2 sd', 'mean', '+2 sd'), col='blue', pch=19, pt.cex=c(magfactor(-2), magfactor(0), magfactor(2)), x.intersp = 2, y.intersp = 1.9, bty="n")
  text(x=744895, y=3931452, labels='soil VWC', font=2, offset=0)
  dev.off()
}

#read in forage data
setwd(file.path(forage_data, 'results'))
list.files(pattern = glob2rx('*.shp'))
sensor_forage_sp <- shapefile("sensor_forage2017.shp")
#and then add normalized moisture data for clipping dates
sensor_forage <- as.data.frame(sensor_forage_sp)
colnames(sensor_forage)
sensor_forage_comp <- cbind(sensor_forage, vwc_data_normalized[ ,c(which(colnames(vwc_data_normalized)=='Feb_15_2017'), which(colnames(vwc_data_normalized)=='Mar_14_2017'), which(colnames(vwc_data_normalized)=='Apr_10_2017'), which(colnames(vwc_data_normalized)=='May_01_2017'))])
plot(sensor_forage_comp$Feb_15_2017, sensor_forage_comp$clp021517)
summary(lm(clp021517 ~ Feb_15_2017, data = sensor_forage_comp))
plot(sensor_forage_comp$Mar_14_2017, sensor_forage_comp$clp031417)
summary(lm(clp031417 ~ Mar_14_2017, data = sensor_forage_comp))
plot(sensor_forage_comp$Apr_10_2017, sensor_forage_comp$clp041017)
plot(sensor_forage_comp$May_01_2017, sensor_forage_comp$clp050117)

#relative soil moisture minima occurs Jan 18, 2017, Feb 3, 2017 and Mar 23, 2017
plot(vwc_data_normalized$Feb_03_2017, sensor_forage_comp$clp041017)
test <- summary(lm(sensor_forage_comp$clp041017 ~ vwc_data_normalized$Nov_19_2016))
plot(vwc_data_normalized$Nov_19_2016, sensor_forage_comp$clp041017)
summary(lm(sensor_forage_comp$clp041017 ~ vwc_data_normalized$Mar_23_2017))


#work through dates and find correlation between normalized soil moisture and April 10, 2017 biomass
#note that some of these variables are defined above
SM_vs_biomass_analysis <- data.frame(dates=dates, SMnorm.mean=apply(vwc_data_normalized[2:endcol], 2, mean, na.rm=TRUE), SMnorm.range=apply(vwc_data_normalized[2:endcol], 2, max, na.rm=TRUE) - apply(vwc_data_normalized[2:endcol], 2, min, na.rm=TRUE), slope=NA, p.value=NA, r2=NA)
for (i in 2:endcol) {
  lm.summary <- summary(lm(sensor_forage_comp$clp031417 ~ vwc_data_normalized[,i]))
  SM_vs_biomass_analysis[i-1, 'slope'] <- lm.summary$coefficients[2, 1]
  SM_vs_biomass_analysis[i-1, 'p.value'] <- lm.summary$coefficients[2, 4]
  SM_vs_biomass_analysis[i-1, 'r2'] <- lm.summary$r.squared
}
#this shows very strong negative correlation Jan 31-Feb 5, ending Feb 6 with April 10th data.  Most variability explained Jan 17-18 and Feb 1-2 with some structure in data apparent as early as Dec 24-30.  For March 14 clippings, best correlation from mid-Dec to Jan 8 (peaks Dec 29)
SM_vs_biomass_analysis
plot(dates, SM_vs_biomass_analysis$r2)
plot(dates, SM_vs_biomass_analysis$p.value)
plot(SM_vs_biomass_analysis$r2, SM_vs_biomass_analysis$p.value)
dates[SM_vs_biomass_analysis$p.value < 0.05]
plot(vwc_data_normalized$Jan_18_2017, sensor_forage_comp$clp041017)
text(x=vwc_data_normalized$Jan_18_2017, y=sensor_forage_comp$clp041017, labels=vwc_data_normalized$location, pos=1)
plot(vwc_data_normalized$Feb_02_2017, sensor_forage_comp$clp041017)
plot(vwc_data_normalized$Jan_18_2017, vwc_data_normalized$Feb_02_2017)
plot(vwc_data_normalized$Jan_18_2017, sensor_forage_comp$clp031417)
plot(vwc_data_normalized$Feb_02_2017, sensor_forage_comp$clp031417)
plot(vwc_data_normalized$Dec_29_2016, sensor_forage_comp$clp031417)
text(x=vwc_data_normalized$Dec_29_2016, y=sensor_forage_comp$clp031417, labels=vwc_data_normalized$location, pos=1, col=i+1)
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(c('2017/02/15', '2017/03/14', '2017/04/10', '2017/05/01')), sensor_forage[i,2:5], type='b', col=i+1, ylim=c(min(sensor_forage[,2:5]), max(sensor_forage[,2:5])))
  }
  else {
    lines.default(as.Date(c('2017/02/15', '2017/03/14', '2017/04/10', '2017/05/01')), sensor_forage[i,2:5], type='b', col=i+1)
  }
}

#check correlations with non-normalized data
#work through dates and find correlation between normalized soil moisture and April 10, 2017 biomass
#note that some of these variables are defined above
SMraw_vs_biomass_analysis <- data.frame(dates=dates, SM.mean=apply(vwc_data[2:endcol], 2, mean, na.rm=TRUE), SM.range=apply(vwc_data[2:endcol], 2, max, na.rm=TRUE) - apply(vwc_data[2:endcol], 2, min, na.rm=TRUE), slope=NA, p.value=NA, r2=NA)
for (i in 2:endcol) {
  lm.summary <- summary(lm(sensor_forage_comp$clp041017 ~ vwc_data[,i]))
  SMraw_vs_biomass_analysis[i-1, 'slope'] <- lm.summary$coefficients[2, 1]
  SMraw_vs_biomass_analysis[i-1, 'p.value'] <- lm.summary$coefficients[2, 4]
  SMraw_vs_biomass_analysis[i-1, 'r2'] <- lm.summary$r.squared
}
SMraw_vs_biomass_analysis
plot(dates, SMraw_vs_biomass_analysis$SM.mean)
plot(dates, SMraw_vs_biomass_analysis$r2)
plot(dates, SMraw_vs_biomass_analysis$slope)
plot(SMraw_vs_biomass_analysis$p.value, SMraw_vs_biomass_analysis$r2)
plot(vwc_data$Jan_18_2017, sensor_forage_comp$clp041017)
plot(vwc_data$Feb_02_2017, sensor_forage_comp$clp041017)
plot(vwc_data$Jan_18_2017, vwc_data$Feb_02_2017)
#not sure about this code below [8/17/18]

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

