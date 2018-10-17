#assume that sensor 7A and 22A crossed at location 15 in the datalogger ports (1 and 2); checked on 3/9/17, as labelled, sensors are in correct ports but could have been mislabeled. code to correct this is functional as of 5/3/17
#large divergence at Location 11 at 22 cm depth (creates problem for plotting)
#sensor A at location 14 at 7 cm depth was replaced on 1/16/17, because it was giving NA up to that point
#sensor B at location 9 at 22 cm depth has data gap in early March (569 NAs); sensor A closely tracks so not really a problem
#location 13 datalogger stopped working on 3/16/17; however on 3/10/17; both sensors at 22 cm depth suddenly reported a 0.1 VWC drop at 1 PM, which must be erroneous; on 3/10/17, both sensors at 7 cm reported a sudden drop of 0.02 VWC 
#to-do 5/3/17 (1) add elevation to terrain characteristics; (2) check to see if higher resolution DEM is available; (3)solar radiation--beam radiance calculation; (4) elevation above a channel; (5) distance from a ridge; (6) temporal stability of soil water 
#moved mapping code to 'general_mapping.R' on 5/22/17
#updating script to handle WY2018 data on 8/17/18 along with general clean-up of ole' practices
#library(readxl)
library(extrafont)
library(extrafontdb)
loadfonts()
#library(rgdal)
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
mainDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/jul2018/csv_files'
spatialDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017_normalized'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'

#read in terrain char for each sensor location
#to be updated
list.files(file.path(results, 'terrain_characteristics'))
terrain_chars <- read.csv(file.path(results, 'terrain_characteristics', "sensor_terrain5mNov2016.csv"), stringsAsFactors = FALSE) #this was made with a 1.5 m buffer with the raster 'extract' function in terrain_analysis.R
colnames(terrain_chars)[1] <- 'location'
terrain_chars

#now, read-in and process the soil moisture data
soil_moisture_fnames <- list.files(mainDir, pattern = glob2rx('*.csv'))
soil_moisture_fnames
soil_moisture_dfs <- lapply(soil_moisture_fnames, function(x) {read.csv(file.path(mainDir, x), stringsAsFactors=FALSE, na.strings="***")})
names(soil_moisture_dfs) <- soil_moisture_fnames
lapply(soil_moisture_dfs, class)
lapply(soil_moisture_dfs, dim)
lapply(soil_moisture_dfs, function(x) {print(x[3,2])})
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

#switch A7 sensor with A22 sensor data for location 15 (updated for last download when sensors removed Jul 2018)
a22VWC <- soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A7 m³/m³ VWC`
a7VWC <- soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A22 m³/m³ VWC`
a22T <- soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A7 °C Temp`
a7T <- soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A22 °C Temp`
soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A7 m³/m³ VWC` <- a7VWC
soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A22 m³/m³ VWC` <- a22VWC
soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A7 °C Temp` <- a7T
soil_moisture_dfs$`Cam15-17Aug2018-2205.csv`$`5TM_P15_A22 °C Temp` <- a22T

#check column names
for (i in seq_along(soil_moisture_dfs)) {
  print(names(soil_moisture_dfs)[i])
  print(colnames(soil_moisture_dfs[[i]]))
}

#re-order columns to match structure that results from merging with terrain characteristics
for (i in seq_along(soil_moisture_dfs)) {
  soil_moisture_dfs[[i]] <- soil_moisture_dfs[[i]][ ,c((ncol(soil_moisture_dfs[[i]]) - 1), 1:(ncol(soil_moisture_dfs[[i]]) - 2), ncol(soil_moisture_dfs[[i]]))]
}

#merge terrain characteristics with soil_moisture_dfs 
#needs updating still
# for (i in 1:length(soil_moisture_dfs)){
#   location <- as.integer(soil_moisture_dfs[[i]]$location[1])
#   rownum <- match(location, sensor_pts_df$location) #column name was previously datalogger_no
#   land_position_data <- sensor_pts_df[rownum, ]
#   soil_moisture_dfs[[i]] <- merge(soil_moisture_dfs[[i]], land_position_data)
# }

#write merged soil_moisture_dfs & terrain characteristics to results
for (i in 1:length(soil_moisture_dfs)) {
  write.csv(soil_moisture_dfs[[i]], file.path(results, 'processed_soil_moisture/Jul2018', paste(sub('.csv', '', names(soil_moisture_dfs)[i]), 'processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names = FALSE)
}

#read in merged data
soil_moisture_fnames <- list.files(file.path(results, 'processed_soil_moisture/Jul2018'), pattern = glob2rx('*.csv'), full.names = TRUE)
fnames_short <- list.files(file.path(results, 'processed_soil_moisture/Jul2018'), pattern = glob2rx('*.csv'))
soil_moisture_dfs <- lapply(soil_moisture_fnames, read.csv, stringsAsFactors=FALSE)
names(soil_moisture_dfs) <- fnames_short

#do some QC checking before aggregation
#note that points 3, 6, 9, and 14 had an extra port active in WY2018 due to sensors added
lapply(soil_moisture_dfs, function(x) print(colnames(x)))
#check for negative vwc
for (i in seq_along(soil_moisture_dfs)) {
  print(names(soil_moisture_dfs)[i])
  for (j in c(3, 5, 7, 9, if(grepl('X5TM', colnames(soil_moisture_dfs[[i]])[11])){11})) { #this refers to columns with VWC data
    print(paste(sum(soil_moisture_dfs[[i]][j] < 0, na.rm = TRUE), 'in column', j))
  }
}
for (i in seq_along(soil_moisture_dfs)) {
  print(names(soil_moisture_dfs)[i])
  for (j in c(4, 6, 8, 10, if(grepl('X5TM', colnames(soil_moisture_dfs[[i]])[12])){12})) { #this refers to columns with temperature data
    print(paste(sum(soil_moisture_dfs[[i]][j] < 0, na.rm = TRUE), 'in column', j))
  }
}
for (i in seq_along(soil_moisture_dfs)) {
  print(names(soil_moisture_dfs)[i])
  for (j in c(3, 5, 7, 9, if(grepl('X5TM', colnames(soil_moisture_dfs[[i]])[11])){11})) { #this refers to columns with VWC data
    print(paste(sum(soil_moisture_dfs[[i]][j] > 0.5, na.rm = TRUE), 'in column', j))
  }
} #one value greater than 0.5 at point 14
#summaries by sensor for soil temperature
for (i in seq_along(soil_moisture_dfs)) {
  print(names(soil_moisture_dfs)[i])
  print(summary(soil_moisture_dfs[[i]][c(4, 6, 8, 10, if(grepl('X5TM', colnames(soil_moisture_dfs[[i]])[12])){12})]))
}
which(soil_moisture_dfs$`Cam14-17Aug2018-2159processed2018-08-23.csv`$X5TM_P14_A22..C.Temp < 0)
soil_moisture_dfs$`Cam14-17Aug2018-2159processed2018-08-23.csv`$date[39831] #this sensor is removed below
#summaries by sensor for soil moisture
for (i in seq_along(soil_moisture_dfs)) {
  print(names(soil_moisture_dfs)[i])
  print(summary(soil_moisture_dfs[[i]][c(3, 5, 7, 9, if(grepl('X5TM', colnames(soil_moisture_dfs[[i]])[11])){11})]))
}
#no obvious issues except for point 14-A22


#write function to plot raw data by sensor
#need to work on scaling because if scale for Sensor A differs from Sensor B, it will not plot correctly - problem with Point 11 at 22 cm depth
#had to update column indexing for May 2017 files because location is now inserted into column 1
# for (i in 1:length(soil_moisture_dfs)) {
#   labDates <- seq(from=soil_moisture_dfs[[i]]$Measurement.Time[1], to=soil_moisture_dfs[[i]]$Measurement.Time[nrow(soil_moisture_dfs[[i]])], by='week', format='%m/%d/%Y')
#   plot(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,3], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col='Blue', main=paste('Point', soil_moisture_dfs[[i]]$location[1], 'at 7 cm depth'))
#   axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
#   lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,7], type='l', col='Red')
#   legend("bottomright", legend=(c('sensor A', 'sensor B')), lty=1, col = c( 'blue', 'red'))
#   plot(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,5], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col='Blue', main=paste('Point', soil_moisture_dfs[[i]]$location[1], 'at 22 cm depth'))
#   axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
#   lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,9], type='l', col='Red')
#   legend("bottomright", legend=(c('sensor A', 'sensor B')), lty=1, col = c( 'blue', 'red'))
# }

#plot all on one graph at 7 cm depth 
# plot(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,3], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='All sensors at 7 cm', ylim=c(0.1, 0.45))
# axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
# lines.default(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,7], type='l', col=1)
# for (i in 2:length(soil_moisture_dfs)) {
#   lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,3], col=i+1)
#   lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,7], col=i+1)
# }

#plot all on one graph at 22 cm depth 
# plot(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,5], type='l', xlab='Date', ylab='soil VWC', xaxt='n', col=1, main='All sensors at 22 cm', ylim=c(0.1, 0.45))
# axis.POSIXct(side = 1, labDates, at=labDates, format = '%m/%d')
# lines.default(soil_moisture_dfs[[1]]$Measurement.Time, soil_moisture_dfs[[1]][ ,9], type='l', col=1)
# for (i in 2:length(soil_moisture_dfs)) {
#   lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,5], col=i+1)
#   lines.default(soil_moisture_dfs[[i]]$Measurement.Time, soil_moisture_dfs[[i]][ ,9], col=i+1)
# }

#write function to take raw data and produce daily statistics for each sensor
#will only work after reading in processed soil moisture files from this directory -- file.path(results, 'processed_soil_moisture/Jul2018'), line 159 above
for (i in 1:length(soil_moisture_dfs)) {
  for (j in c(3, 5, 7, 9, if(grepl('X5TM', colnames(soil_moisture_dfs[[i]])[11])){11})) { #this refers to columns with VWC data (updated for May2017 processed data)
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
    VWC$Depth <- gsub('[A, B, C, m, .]', '', VWC$Depth)
    VWC$Depth <- as.integer(gsub(' ', '', VWC$Depth))
    VWC$SubsampleID <-substr(colnames(soil_moisture_dfs[[i]])[j], 9, 10)
    VWC$SubsampleID <- gsub('[7, 2, _, .]', '', VWC$Subsample)
    #VWC$Nrt_10N <- soil_moisture_dfs[[i]]$Nrt_10N[1]
    #VWC$Est_10N <- soil_moisture_dfs[[i]]$Est_10N[1]
    #VWC$slope <- soil_moisture_dfs[[i]]$slope[1]
    #VWC$aspect_deg <- soil_moisture_dfs[[i]]$aspect[1]
    #VWC$cti <- soil_moisture_dfs[[i]]$cti[1] #see raster package, terrain function for definition
    #VWC$mean_curv <- soil_moisture_dfs[[i]]$mean_curv[1]
    #VWC$aspect_cardinal <- soil_moisture_dfs[[i]]$aspect_cardinal[1]
    if (i==1 & j==3) {
      daily_dataVWC <- VWC
      next
    }
    daily_dataVWC <- rbind(daily_dataVWC, VWC)
  }
}

#add sensor code column
daily_dataVWC$sensor_code <- paste(daily_dataVWC$Location, '-', daily_dataVWC$Depth, '-', daily_dataVWC$SubsampleID, sep='') #as Location(i.e. datalogger no)-Depth-Subsample ID
unique(daily_dataVWC$sensor_code) #fixed problem with
#add time column
daily_dataVWC$Date_Calendar <- as.Date(daily_dataVWC$Date, format='%Y%m%d')
#order this by location, depth, subsampleID, and date
daily_dataVWC <- daily_dataVWC[with(daily_dataVWC, order(Location, Depth, SubsampleID, Date_Calendar)), ]
dim(daily_dataVWC) #41150 x 14
#save daily summary for each sensor (updated for May 2017 results)
write.csv(daily_dataVWC, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_summary', 'processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)

#read-in daily by sensor summary
# daily_fnames <- list.files(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor'))
# daily_fnames
daily_dataVWC <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor/daily_by_sensor_summaryprocessed2018-09-27.csv'), stringsAsFactors = FALSE)
head(daily_dataVWC)
colnames(daily_dataVWC)

#explore missing data
tapply(daily_dataVWC$MeanVWC, daily_dataVWC$sensor_code, function(x) sum(is.na(x)))
sum(tapply(daily_dataVWC$MeanVWC, daily_dataVWC$sensor_code, function(x) sum(is.na(x))) > 128) #16 are missing more than 128 days
#14-22-A, 14-22-C, 14-7-A, 15-22-A, 15-7-A, 3-22-B, 3-22-A, 3-22-C, 4-7-B, 6-7-B, 6-7-C, 9-22-A, 9-22-B, 9-22-C, 9-7-A, 9-7-B are missing more than 128 days; all C's are missing more than 128 days because they were installed after the initial installation as back-ups
daily_dataVWCdfs <- split(daily_dataVWC, daily_dataVWC$sensor_code)
for(i in seq_along(daily_dataVWCdfs)) {
  if(sum(is.na(daily_dataVWCdfs[[i]]$MeanVWC)) > 128) {
    print(names(daily_dataVWCdfs)[i])
    print(daily_dataVWCdfs[[i]]$Date[is.na(daily_dataVWCdfs[[i]]$MeanVWC)])
  }
}
for(i in seq_along(daily_dataVWCdfs)) {
  print(names(daily_dataVWCdfs)[i])
  print(nrow(daily_dataVWCdfs[[i]]))
}
#produce daily data by sensor for each water year
#get rid of replacement sensor data
#get rid of sensors with missing data that cannot be gap filled (see paper notes)
#note that point 13 datalogger stopped recording for 40 days in spring 2017; 6 days before going offline, data appeared to be problematic
daily_dataVWC_2017 <- daily_dataVWC[daily_dataVWC$Date_Calendar < '2017-07-01', ]
daily_dataVWC_2018 <- daily_dataVWC[daily_dataVWC$Date_Calendar > '2017-11-28' & daily_dataVWC$Date_Calendar < '2018-07-01', ]
daily_dataVWC_2017_clean <- daily_dataVWC_2017[-which(daily_dataVWC_2017$sensor_code %in% c('14-7-A', '14-22-C', '3-22-C', '6-7-C', '9-22-C')), ]
daily_dataVWC_2017_clean[daily_dataVWC_2017_clean$sensor_code=='13-7-B', c('Date_Calendar', 'MeanVWC')]
daily_dataVWC_2017_clean$Date_Calendar <- as.Date(daily_dataVWC_2017_clean$Date_Calendar)
plot(daily_dataVWC_2017_clean$Date_Calendar[daily_dataVWC_2017_clean$sensor_code=='13-7-A'], daily_dataVWC_2017_clean$MeanVWC[daily_dataVWC_2017_clean$sensor_code=='13-7-A'])
daily_dataVWC_2017_clean <- daily_dataVWC_2017_clean[-which(daily_dataVWC_2017_clean$Location==13 & daily_dataVWC_2017_clean$Date_Calendar %in% seq.Date(as.Date('2017-03-10'), as.Date('2017-03-16'), by=1)), ] #7 days have corrupt data in March 2017 at point 13, so remove 3/10-3/16/17 before datalogger 13 went offline
plot(daily_dataVWC_2017_clean$Date_Calendar[daily_dataVWC_2017_clean$sensor_code=='12-7-A'], daily_dataVWC_2017_clean$MeanVWC[daily_dataVWC_2017_clean$sensor_code=='12-7-A'])
#2018 data row removals
daily_dataVWC_2018_clean <- daily_dataVWC_2018[-which(daily_dataVWC_2018$sensor_code %in% c('14-22-A', '3-22-A', '3-22-B', '4-7-B', '6-7-B', '9-22-B', '14-22-C', '3-22-C', '6-7-C', '9-22-C')), ] #fixes for 2/12-2/14/18 for 15-22-A and 15-7-A and 1/30-2/14/18 for point 9-22-A and 9-7-A & B, which were periods with no rain when sensors became unplugged.
daily_dataVWC_2018_clean$Date_Calendar <- as.Date(daily_dataVWC_2018_clean$Date_Calendar)
plot(daily_dataVWC_2018_clean$Date_Calendar[daily_dataVWC_2018_clean$sensor_code=='9-7-A'], daily_dataVWC_2018_clean$MeanVWC[daily_dataVWC_2018_clean$sensor_code=='9-7-A'])

#sensor 4-7-A problematic from 2/11 to 2/12/18
daily_dataVWC_clean <- daily_dataVWC[-which(daily_dataVWC$sensor_code %in% c('14-7-A', '14-22-A', '3-22-A', '3-22-B', '4-7-B', '6-7-B', '9-22-B', '14-22-C', '3-22-C', '6-7-C', '9-22-C')), ]
daily_dataVWC_clean <- daily_dataVWC_clean[-which(daily_dataVWC_clean$Location==13 & as.Date(daily_dataVWC_clean$Date_Calendar) %in% seq.Date(as.Date('2017-03-10'), as.Date('2017-03-16'), by=1)), ]
daily_dataVWC_clean$Date_Calendar <- as.Date(daily_dataVWC_clean$Date_Calendar)
#daily_dataVWC_clean[which(as.Date(daily_dataVWC_clean$Date_Calendar) %in% seq.Date(as.Date('2017-07-15'), as.Date('2017-11-28'), by=1)), 1:8] <- NA
dim(daily_dataVWC_clean) #34438 x 14
clean_it_VWC <- function(df, sensor_code, varname, start_date, end_date) {
  df[[varname]][df$sensor_code==sensor_code & df$Date_Calendar > as.Date(start_date) & df$Date_Calendar < as.Date(end_date)] <- df[[varname]][df$Date_Calendar==start_date & df$sensor_code==sensor_code] - as.integer(df$Date_Calendar[df$sensor_code==sensor_code & df$Date_Calendar > as.Date(start_date) & df$Date_Calendar < as.Date(end_date)] - as.Date(start_date)) * ((df[[varname]][df$Date_Calendar==start_date & df$sensor_code==sensor_code] - df[[varname]][df$Date_Calendar==end_date & df$sensor_code==sensor_code]) / as.numeric(as.Date(end_date) - as.Date(start_date)))
  df
}
#both year fixes
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-A', 'MeanVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-A', 'MedianVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-A', 'MaxVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-A', 'MinVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-22-A', 'MeanVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-22-A', 'MedianVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-22-A', 'MaxVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-22-A', 'MinVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-B', 'MeanVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-B', 'MedianVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-B', 'MaxVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '9-7-B', 'MinVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-7-A', 'MeanVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-7-A', 'MedianVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-7-A', 'MaxVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-7-A', 'MinVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-22-A', 'MeanVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-22-A', 'MedianVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-22-A', 'MaxVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_clean <- clean_it_VWC(daily_dataVWC_clean, '15-22-A', 'MinVWC', '2018-02-11', '2018-02-15')

#2017 fixes
daily_dataVWC_2017_clean <- clean_it_VWC(daily_dataVWC_2017_clean, '9-22-B', 'MeanVWC', '2017-03-09', '2017-03-12')
daily_dataVWC_2017_clean <- clean_it_VWC(daily_dataVWC_2017_clean, '9-22-B', 'MedianVWC', '2017-03-09', '2017-03-12')
daily_dataVWC_2017_clean <- clean_it_VWC(daily_dataVWC_2017_clean, '9-22-B', 'MaxVWC', '2017-03-09', '2017-03-12')
daily_dataVWC_2017_clean <- clean_it_VWC(daily_dataVWC_2017_clean, '9-22-B', 'MinVWC', '2017-03-09', '2017-03-12')

#2018 fixes
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-A', 'MeanVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-A', 'MedianVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-A', 'MaxVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-A', 'MinVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-22-A', 'MeanVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-22-A', 'MedianVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-22-A', 'MaxVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-22-A', 'MinVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-B', 'MeanVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-B', 'MedianVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-B', 'MaxVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '9-7-B', 'MinVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-7-A', 'MeanVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-7-A', 'MedianVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-7-A', 'MaxVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-7-A', 'MinVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-22-A', 'MeanVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-22-A', 'MedianVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-22-A', 'MaxVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it_VWC(daily_dataVWC_2018_clean, '15-22-A', 'MinVWC', '2018-02-11', '2018-02-15')
plot(daily_dataVWC_2018_clean$Date_Calendar[daily_dataVWC_2018_clean$sensor_code=='15-22-A'], daily_dataVWC_2018_clean$MeanVWC[daily_dataVWC_2018_clean$sensor_code=='15-22-A'])

#check soil T relationships to fix 2018 data gaps at point 9 (16-19 days) and 15 (3 days)
#find an approach for point 9
unique(daily_dataVWC_2018_clean$sensor_code[daily_dataVWC_2018_clean$Location==9]) #"9-7-A"  "9-7-B"  "9-22-A" available
unique(daily_dataVWC_2018_clean$sensor_code[daily_dataVWC_2018_clean$Location==1]) #all sensors in dataset for point 1
unique(daily_dataVWC_2018_clean$sensor_code[daily_dataVWC_2018_clean$Location==13])

summary(lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-7-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-B']))
lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-7-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-B'])

summary(lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-7-B'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-B']))
lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-7-B'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-7-B'])

summary(lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-22-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-22-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-22-B']))
lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-22-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-22-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-22-B'])

summary(lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='13-7-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='4-7-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='4-7-B']))
lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='9-22-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-22-A'] + daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='1-22-B'])

#find an approach for gap-filling point 15
unique(daily_dataVWC_2018_clean$sensor_code[daily_dataVWC_2018_clean$Location==15])  #all sensors in dataset for point 15, so can use other sensors 

summary(lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-7-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-7-B']))
lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-7-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-7-B'])

summary(lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-22-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-22-B']))
lm(daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-22-A'] ~ daily_dataVWC_2018_clean$MeanT[daily_dataVWC_2018_clean$sensor_code=='15-22-B'])

#now put this approach into a function
# df <- daily_dataVWC_clean
# varname <- 'MeanT'
# sensor_code_fix <- '15-7-A'
# sensor_code_use <- '15-7-B'
# qc <- 0.99
gap_fill_soilTv1 <- function(df, varname, sensor_code_fix, sensor_code_use, qc) {
  lm.result <- lm(df[[varname]][df$sensor_code == sensor_code_fix] ~ df[[varname]][df$sensor_code == sensor_code_use])
  if (summary(lm.result)$r.squared < qc) {
    stop(print('Regression results are unacceptable.'))
  }
  fix_indices <- which(is.na(df[[varname]]) & df$sensor_code == sensor_code_fix)
  fixer_indices <- which(df$Date_Calendar %in% df$Date_Calendar[fix_indices] & df$sensor_code == sensor_code_use)
  df[[varname]][fix_indices] <- lm.result$coefficients[1] + lm.result$coefficients[2] * df[[varname]][fixer_indices]
  df
}
#former version
# gap_fill_soilTv1 <- function(df, varname, sensor_code_fix, sensor_code_use, qc) {
#   lm.result <- lm(df[[varname]][df$sensor_code == sensor_code_fix] ~ df[[varname]][df$sensor_code == sensor_code_use])
#   if (summary(lm.result)$r.squared < qc) {
#     stop(print('Regression results are unacceptable.'))
#   }
#   df[[varname]][df$sensor_code == sensor_code_fix & is.na(df[[varname]][df$sensor_code == sensor_code_fix])] <- lm.result$coefficients[1] + lm.result$coefficients[2] * df[[varname]][df$sensor_code == sensor_code_use & is.na(df[[varname]][df$sensor_code == sensor_code_fix])]
#   df
# }
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MeanT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MedianT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MaxT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MinT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MeanT', '15-22-A', '15-22-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MedianT', '15-22-A', '15-22-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MaxT', '15-22-A', '15-22-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv1(daily_dataVWC_2018_clean, 'MinT', '15-22-A', '15-22-B', 0.99)
#and for 2017
daily_dataVWC_2017_clean <- gap_fill_soilTv1(daily_dataVWC_2017_clean, 'MeanT', '9-22-B', '9-22-A', 0.99)
daily_dataVWC_2017_clean <- gap_fill_soilTv1(daily_dataVWC_2017_clean, 'MedianT', '9-22-B', '9-22-A', 0.99)
daily_dataVWC_2017_clean <- gap_fill_soilTv1(daily_dataVWC_2017_clean, 'MaxT', '9-22-B', '9-22-A', 0.99)
daily_dataVWC_2017_clean <- gap_fill_soilTv1(daily_dataVWC_2017_clean, 'MinT', '9-22-B', '9-22-A', 0.99)

#now for 2017-2018
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MeanT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MedianT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MaxT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MinT', '15-7-A', '15-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MeanT', '15-22-A', '15-22-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MedianT', '15-22-A', '15-22-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MaxT', '15-22-A', '15-22-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv1(daily_dataVWC_clean, 'MinT', '15-22-A', '15-22-B', 0.99)
#don't need to fix 9-22-B for daily_dataVWC_clean because it was removed on account of missing 2017 data


#v2 function for using 2 sensors to help gap fill
# df <- daily_dataVWC_clean
# varname <- 'MeanT'
# sensor_code_fix <- '9-7-A'
# sensor_code_use <- '1-7-A'
# sensor_code_use2 <- '1-7-B'
# qc <- 0.99
gap_fill_soilTv2 <- function(df, varname, sensor_code_fix, sensor_code_use, sensor_code_use2, qc) {
  lm.result <- lm(df[[varname]][df$sensor_code == sensor_code_fix] ~ df[[varname]][df$sensor_code == sensor_code_use] + df[[varname]][df$sensor_code == sensor_code_use2])
  if (summary(lm.result)$r.squared < qc) {
    stop(print('Regression results are unacceptable.'))
  }
  fix_indices <- which(is.na(df[[varname]]) & df$sensor_code == sensor_code_fix)
  fixer_indices <- which(df$Date_Calendar %in% df$Date_Calendar[fix_indices] & df$sensor_code == sensor_code_use)
  fixer_indices2 <- which(df$Date_Calendar %in% df$Date_Calendar[fix_indices] & df$sensor_code == sensor_code_use2)
  df[[varname]][fix_indices] <- lm.result$coefficients[1] + lm.result$coefficients[2] * df[[varname]][fixer_indices] + lm.result$coefficients[3] * df[[varname]][fixer_indices2]
  df
}

daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MeanT', '9-7-A', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MedianT', '9-7-A', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MaxT', '9-7-A', '1-7-A', '1-7-B', 0.98)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MinT', '9-7-A', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MeanT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MedianT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MaxT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MinT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MeanT', '9-22-A', '1-22-A', '1-22-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MedianT', '9-22-A', '1-22-A', '1-22-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MaxT', '9-22-A', '1-22-A', '1-22-B', 0.99)
daily_dataVWC_2018_clean <- gap_fill_soilTv2(daily_dataVWC_2018_clean, 'MinT', '9-22-A', '1-22-A', '1-22-B', 0.99)

#fix 2017-2018 data
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MeanT', '9-7-A', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MedianT', '9-7-A', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MaxT', '9-7-A', '1-7-A', '1-7-B', 0.98)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MinT', '9-7-A', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MeanT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MedianT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MaxT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MinT', '9-7-B', '1-7-A', '1-7-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MeanT', '9-22-A', '1-22-A', '1-22-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MedianT', '9-22-A', '1-22-A', '1-22-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MaxT', '9-22-A', '1-22-A', '1-22-B', 0.99)
daily_dataVWC_clean <- gap_fill_soilTv2(daily_dataVWC_clean, 'MinT', '9-22-A', '1-22-A', '1-22-B', 0.99)

#check goodness of 2018 data
tapply(daily_dataVWC_2018_clean$MeanVWC, daily_dataVWC_2018_clean$sensor_code, length) #214 days per sensor
tapply(daily_dataVWC_2018_clean$MeanVWC, daily_dataVWC_2018_clean$sensor_code, function(x) sum(is.na(x)))
tapply(daily_dataVWC_2018_clean$MeanT, daily_dataVWC_2018_clean$sensor_code, function(x) sum(is.na(x)))

#check goodness of 2017 data
tapply(daily_dataVWC_2017_clean$MeanVWC, daily_dataVWC_2017_clean$sensor_code, length) #224 per sensor except for point 13
tapply(daily_dataVWC_2017_clean$MeanVWC, daily_dataVWC_2017_clean$sensor_code, function(x) sum(is.na(x)))
tapply(daily_dataVWC_2017_clean$MeanT, daily_dataVWC_2017_clean$sensor_code, function(x) sum(is.na(x)))

#check goodness of both year data[combined both year data not used in further analysis]
tapply(daily_dataVWC_clean$MeanVWC, daily_dataVWC_clean$sensor_code, length)
tapply(daily_dataVWC_clean$MeanVWC, daily_dataVWC_clean$sensor_code, function(x) sum(is.na(x)))
tapply(daily_dataVWC_clean$MeanT, daily_dataVWC_clean$sensor_code, function(x) sum(is.na(x)))

#write these files to disk
write.csv(daily_dataVWC_2017_clean, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_2017_processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)
write.csv(daily_dataVWC_2018_clean, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_2018_processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)
write.csv(daily_dataVWC_clean, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_bothyrs_processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)

#function to produce daily means by depth for each pair of sensors for median, mean, max, and min daily data by sensor and then merged with terrain characteristics by location (i.e. datalogger #)
# df <- daily_dataVWC
# depth <- 7
# varname <- 'MedianVWC'
daily_dataVWC_2017_clean <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', 'daily_by_sensor_2017_processed2018-09-27.csv'), stringsAsFactors = FALSE)
daily_dataVWC_2018_clean <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', 'daily_by_sensor_2018_processed2018-09-27.csv'), stringsAsFactors = FALSE)
daily_dataVWC_clean <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', 'daily_by_sensor_bothyrs_processed2018-09-27.csv'), stringsAsFactors = FALSE)

daily_by_location <- function(depth, df, varname, subdir, yr) {
  a <- which(df$Depth==depth)
  specific_depth <- df[a,]
  depth_aggregated <- as.data.frame(tapply(specific_depth[[varname]], list(specific_depth$Location, specific_depth$Date_Calendar), mean, na.rm=TRUE))
  colnames(depth_aggregated) <- format.Date(as.Date(gsub('[-]', '', colnames(depth_aggregated)), format = '%Y%m%d'), '%b_%d_%Y') #this takes the original date (eg. "2016-11-19") and strips out the '/' (eg. "20161119") and then converts it to a column name ("Nov_19_2016") that would be coerced to "X2016.11.19" when reading the file back in as a data.frame
  depth_aggregated$location <- as.integer(row.names(depth_aggregated))
  row.names(depth_aggregated) <- NULL
  depth_aggregated <- depth_aggregated[ ,c(ncol(depth_aggregated), 1:(ncol(depth_aggregated)-1))]
  #setwd(results)
  #sensor_pts_df <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE) #this needs to be updated for additional terrain characteristics with higher res DEM
  #depth_aggregated <- merge.data.frame(depth_aggregated, sensor_pts_df, by='location')
  if (!dir.exists(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_location', yr, subdir))) {
    dir.create(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_location', yr, subdir))
  }
  write.csv(depth_aggregated, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_location', yr, subdir, paste(varname, '_', as.character(depth), 'cm_dailymeans_by_location.csv', sep = '')), row.names = FALSE)
}

#produce summaries for 2017
daily_by_location(7, daily_dataVWC_2017_clean, 'MedianVWC', 'VWC', '2017')
daily_by_location(22, daily_dataVWC_2017_clean, 'MedianVWC', 'VWC', '2017')
daily_by_location(7, daily_dataVWC_2017_clean, 'MeanVWC', 'VWC', '2017')
daily_by_location(22, daily_dataVWC_2017_clean, 'MeanVWC', 'VWC', '2017')
daily_by_location(7, daily_dataVWC_2017_clean, 'MaxVWC', 'VWC', '2017')
daily_by_location(22, daily_dataVWC_2017_clean, 'MaxVWC', 'VWC', '2017')
daily_by_location(7, daily_dataVWC_2017_clean, 'MedianT', 'Temperature', '2017')
daily_by_location(22, daily_dataVWC_2017_clean, 'MedianT', 'Temperature', '2017')
daily_by_location(7, daily_dataVWC_2017_clean, 'MeanT', 'Temperature', '2017')
daily_by_location(22, daily_dataVWC_2017_clean, 'MeanT', 'Temperature', '2017')
daily_by_location(7, daily_dataVWC_2017_clean, 'MaxT', 'Temperature', '2017')
daily_by_location(22, daily_dataVWC_2017_clean, 'MaxT', 'Temperature', '2017')

#produce summaries for 2018
daily_by_location(7, daily_dataVWC_2018_clean, 'MedianVWC', 'VWC', '2018')
daily_by_location(22, daily_dataVWC_2018_clean, 'MedianVWC', 'VWC', '2018')
daily_by_location(7, daily_dataVWC_2018_clean, 'MeanVWC', 'VWC', '2018')
daily_by_location(22, daily_dataVWC_2018_clean, 'MeanVWC', 'VWC', '2018')
daily_by_location(7, daily_dataVWC_2018_clean, 'MaxVWC', 'VWC', '2018')
daily_by_location(22, daily_dataVWC_2018_clean, 'MaxVWC', 'VWC', '2018')
daily_by_location(7, daily_dataVWC_2018_clean, 'MedianT', 'Temperature', '2018')
daily_by_location(22, daily_dataVWC_2018_clean, 'MedianT', 'Temperature', '2018')
daily_by_location(7, daily_dataVWC_2018_clean, 'MeanT', 'Temperature', '2018')
daily_by_location(22, daily_dataVWC_2018_clean, 'MeanT', 'Temperature', '2018')
daily_by_location(7, daily_dataVWC_2018_clean, 'MaxT', 'Temperature', '2018')
daily_by_location(22, daily_dataVWC_2018_clean, 'MaxT', 'Temperature', '2018')

#produce summaries for both years
daily_by_location(7, daily_dataVWC_clean, 'MedianVWC', 'VWC', '2017_2018')
daily_by_location(22, daily_dataVWC_clean, 'MedianVWC', 'VWC', '2017_2018')
daily_by_location(7, daily_dataVWC_clean, 'MeanVWC', 'VWC', '2017_2018')
daily_by_location(22, daily_dataVWC_clean, 'MeanVWC', 'VWC', '2017_2018')
daily_by_location(7, daily_dataVWC_clean, 'MaxVWC', 'VWC', '2017_2018')
daily_by_location(22, daily_dataVWC_clean, 'MaxVWC', 'VWC', '2017_2018')
daily_by_location(7, daily_dataVWC_clean, 'MedianT', 'Temperature', '2017_2018')
daily_by_location(22, daily_dataVWC_clean, 'MedianT', 'Temperature', '2017_2018')
daily_by_location(7, daily_dataVWC_clean, 'MeanT', 'Temperature', '2017_2018')
daily_by_location(22, daily_dataVWC_clean, 'MeanT', 'Temperature', '2017_2018')
daily_by_location(7, daily_dataVWC_clean, 'MaxT', 'Temperature', '2017_2018')
daily_by_location(22, daily_dataVWC_clean, 'MaxT', 'Temperature', '2017_2018')
