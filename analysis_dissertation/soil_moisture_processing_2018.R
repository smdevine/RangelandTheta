#assume that sensor 7A and 22A crossed at location 15 in the datalogger ports (1 and 2); checked on 3/9/17, as labelled, sensors are in correct ports but could have been mislabeled. code to correct this is functional as of 5/3/17
#large divergence at Location 11 at 22 cm depth (creates problem for plotting)
#sensor A at location 14 at 7 cm depth was replaced on 1/16/17, because it was giving NA up to that point
#sensor B at location 9 at 22 cm depth has data gap in early March (569 NAs); sensor A closely tracks so not really a problem
#location 13 datalogger stopped working on 3/16/17; however on 3/10/17; both sensors at 22 cm depth suddenly reported a 0.1 VWC drop at 1 PM, which must be erroneous; on 3/10/17, both sensors at 7 cm reported a sudden drop of 0.02 VWC 
#to-do 5/3/17 (1) add elevation to terrain characteristics; (2) check to see if higher resolution DEM is available; (3)solar radiation--beam radiance calculation; (4) elevation above a channel; (5) distance from a ridge; (6) temporal stability of soil water 
#moved mapping code to 'general_mapping.R' on 5/22/17
#updating script to handle WY2018 data on 8/17/18 along with general clean-up of ole' practices
#library(readxl)
#library(extrafont)
#library(extrafontdb)
#loadfonts()
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
#setwd(results)
#sensor_pts_df <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE)

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

#note that points 3, 6, 9, and 14 had an extra port active in WY2018 due to sensors added
#investigate dates of blanks
for (i in seq_along(soil_moisture_dfs)) {
  i <- which(soil_moisture_dfs[[i]])
  print(soil_moisture_dfs[[i]]date)
}
is.na(soil_moisture_dfs$`Cam1-17Aug2018-2144processed2018-08-23.csv`[,3])
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
    VWC$Depth <- gsub('[A, B, m, .]', '', VWC$Depth)
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
#add time column
daily_dataVWC$Date_Calendar <- as.Date(daily_dataVWC$Date, format='%Y%m%d')
#order this by location, depth, subsampleID, and date
daily_dataVWC <- daily_dataVWC[with(daily_dataVWC, order(Location, Depth, SubsampleID, Date_Calendar)), ]
dim(daily_dataVWC)
#save daily summary for each sensor (updated for May 2017 results)
write.csv(daily_dataVWC, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_summary', 'processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)

#read-in daily summary for each location summary
daily_fnames <- list.files(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor'))
daily_fnames
daily_dataVWC <- read.csv(daily_fnames[1], stringsAsFactors = FALSE)
head(daily_dataVWC)
colnames(daily_dataVWC)

#explore the data some
tapply(daily_dataVWC$MeanVWC, daily_dataVWC$sensor_code, function(x) sum(is.na(x)))
#14-22-A, , 14-7-A, 15-22-A, 15-7-A, 3-22-B, 3-22-A, 4-7-B, 6-7-B, 6-7-C, 9-22-A, 9-22-B, 9-7-A, 9-7-B are missing more than normal
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
daily_dataVWC_2017_clean <- daily_dataVWC_2017[-which(daily_dataVWC_2017$sensor_code %in% c('14-7-A', '14-NA-C', '3-22-C', '6-7-C', '9-22-C')), ]
daily_dataVWC_2017_clean[daily_dataVWC_2017_clean$sensor_code=='13-7-B', c('Date_Calendar', 'MeanVWC')]
plot(daily_dataVWC_2017_clean$Date_Calendar[daily_dataVWC_2017_clean$sensor_code=='13-7-A'], daily_dataVWC_2017_clean$MeanVWC[daily_dataVWC_2017_clean$sensor_code=='13-7-A']) #7 days have corrupt data in March 2017 at point 13, so remove 3/10-3/16/17 before datalogger 13 went offline
daily_dataVWC_2017_clean <- daily_dataVWC_2017_clean[-which(daily_dataVWC_2017_clean$Location==13 & daily_dataVWC_2017_clean$Date_Calendar %in% seq.Date(as.Date('2017-03-10'), as.Date('2017-03-16'), by=1)), ]
plot(daily_dataVWC_2017_clean$Date_Calendar[daily_dataVWC_2017_clean$sensor_code=='12-7-A'], daily_dataVWC_2017_clean$MeanVWC[daily_dataVWC_2017_clean$sensor_code=='12-7-A'])
daily_dataVWC_2018_clean <- daily_dataVWC_2018[-which(daily_dataVWC_2018$sensor_code %in% c('14-22-A', '3-22-A', '3-22-B', '4-7-B', '6-7-B', '9-22-B', '14-NA-C', '3-22-C', '6-7-C', '9-22-C')), ] #still need to fix 2/12-2/14/18 for 15-22-A and 15-7-A and 1/30-2/14/18 for point 9-22-A and 9-7-A & B.
plot(daily_dataVWC_2018_clean$Date_Calendar[daily_dataVWC_2018_clean$sensor_code=='9-7-A'], daily_dataVWC_2018_clean$MeanVWC[daily_dataVWC_2018_clean$sensor_code=='9-7-A'])
clean_it <- function(df, sensor_code, varname, start_date, end_date) {
  df[[varname]][df$sensor_code==sensor_code & is.na(df[[varname]])] <- df[[varname]][df$Date_Calendar==start_date & df$sensor_code==sensor_code] - as.integer(df$Date_Calendar[df$sensor_code==sensor_code & is.na(df[[varname]])] - as.Date(start_date)) * ((df[[varname]][df$Date_Calendar==start_date & df$sensor_code==sensor_code] - df[[varname]][df$Date_Calendar==end_date & df$sensor_code==sensor_code]) / as.numeric(as.Date(end_date) - as.Date(start_date)))
  df
}
#2017 fixes
daily_dataVWC_2017_clean <- clean_it(daily_dataVWC_2017_clean, '9-22-B', 'MeanVWC', '2017-03-09', '2017-03-12')
daily_dataVWC_2017_clean <- clean_it(daily_dataVWC_2017_clean, '9-22-B', 'MedianVWC', '2017-03-09', '2017-03-12')
daily_dataVWC_2017_clean <- clean_it(daily_dataVWC_2017_clean, '9-22-B', 'MaxVWC', '2017-03-09', '2017-03-12')
daily_dataVWC_2017_clean <- clean_it(daily_dataVWC_2017_clean, '9-22-B', 'MinVWC', '2017-03-09', '2017-03-12')
#2018 fixes
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-A', 'MeanVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-A', 'MedianVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-A', 'MaxVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-A', 'MinVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-22-A', 'MeanVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-22-A', 'MedianVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-22-A', 'MaxVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-22-A', 'MinVWC', '2018-01-29', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-B', 'MeanVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-B', 'MedianVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-B', 'MaxVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '9-7-B', 'MinVWC', '2018-01-29', '2018-02-19')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-7-A', 'MeanVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-7-A', 'MedianVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-7-A', 'MaxVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-7-A', 'MinVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-22-A', 'MeanVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-22-A', 'MedianVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-22-A', 'MaxVWC', '2018-02-11', '2018-02-15')
daily_dataVWC_2018_clean <- clean_it(daily_dataVWC_2018_clean, '15-22-A', 'MinVWC', '2018-02-11', '2018-02-15')
plot(daily_dataVWC_2018_clean$Date_Calendar[daily_dataVWC_2018_clean$sensor_code=='15-22-A'], daily_dataVWC_2018_clean$MeanVWC[daily_dataVWC_2018_clean$sensor_code=='15-22-A'])
#check goodness of 2018 data
tapply(daily_dataVWC_2018_clean$MeanVWC, daily_dataVWC_2018_clean$sensor_code, length)
tapply(daily_dataVWC_2018_clean$MeanVWC, daily_dataVWC_2018_clean$sensor_code, function(x) sum(is.na(x)))

#check goodness of 2017 data
tapply(daily_dataVWC_2017_clean$MeanVWC, daily_dataVWC_2017_clean$sensor_code, length)
tapply(daily_dataVWC_2017_clean$MeanVWC, daily_dataVWC_2017_clean$sensor_code, function(x) sum(is.na(x)))

#write these files to disk
write.csv(daily_dataVWC_2017_clean, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_2017_processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)
write.csv(daily_dataVWC_2018_clean, file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor', paste('daily_by_sensor_2018_processed', format(Sys.Date(), "%F"), '.csv', sep = '')), row.names=FALSE)


#function to produce daily means by depth for each pair of sensors for median, mean, max, and min daily data by sensor and then merged with terrain characteristics by location (i.e. datalogger #)
# df <- daily_dataVWC
# depth <- 7
# varname <- 'MedianVWC'
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

#plotting 2017 daily data by location x depth location from summaries produced above
data_name <- 'VWC'
depth <- '7'
yr <- '2017'
#vwc_data_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location', data_name, paste0('MedianVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
#vwc_data_2017 <- vwc_data_2017[,c(1:165)]
vwc_data_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, data_name, paste0('MedianVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
colnames(vwc_data_2017)
#endcol <- ncol(vwc_data_2017)
dates2017 <- seq.Date(as.Date(colnames(vwc_data_2017)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_2017)[ncol(vwc_data_2017)], '%b_%d_%Y'), by='day')
weeks2017 <- seq.Date(as.Date(colnames(vwc_data_2017)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_2017)[ncol(vwc_data_2017)], '%b_%d_%Y'), by='week')
for (i in 1:nrow(vwc_data_2017)) {
  plot(dates2017, vwc_data_2017[i, 2:ncol(vwc_data_2017)], type='b', xlab='Date', ylab='Daily Median VWC at 7 cm (mean of 2 sensors)', xaxt='n', main=paste('Location', vwc_data_2017$location[i])) #, ',', vwc_data_2017$aspect_cardinal[i], 'aspect'))
  axis.Date(side = 1, dates2017, at=weeks2017, format = '%m/%d')
  #text(x=dates2017[75], y=0.18, labels=paste(round(vwc_data_2017$mean_curv[i], 2), 'mean curvature', ',', round(vwc_data_2017$cti[i], 2), 'Compound Topographic Index'))
}

#plot all on one graph
for (i in 1:nrow(vwc_data_2017)) {
  if (i == 1) {
    plot(dates2017, vwc_data_2017[i, 2:ncol(vwc_data_2017)], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_2017[,2:ncol(vwc_data_2017)], na.rm=TRUE), max(vwc_data_2017[,2:ncol(vwc_data_2017)], na.rm=TRUE)))
    text(x=dates2017[length(dates2017)-50], y=vwc_data_2017[i, ncol(vwc_data_2017)-50], labels=vwc_data_2017[i, 1], cex=0.7)
  } else {
      lines.default(dates2017, vwc_data_2017[i, 2:ncol(vwc_data_2017)], xaxt='n', col=i+1)
      text(x=dates2017[length(dates2017)- 50], y=vwc_data_2017[i, (ncol(vwc_data_2017)- 50)], labels=vwc_data_2017[i, 1], cex=0.7)
    }
}
axis.Date(side = 1, dates2017, at=weeks2017, format = '%m/%d/%y')

#normalize 2017 vwc_data
#vwc_data_normalized_2017 <- vwc_data_2017[,1:165] #end May 1
vwc_data_normalized_2017 <- vwc_data_2017
vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)] <- (vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)] - rowMeans(vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)], na.rm = TRUE)) / apply(vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)], 1, sd, na.rm=TRUE)

#and then plot normalized data as above
for (i in 1:nrow(vwc_data_normalized_2017)) {
  if (i == 1) {
    plot(dates2017[5:length(dates2017)], vwc_data_normalized_2017[i, 6:ncol(vwc_data_normalized_2017)], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_normalized_2017[,6:ncol(vwc_data_normalized_2017)], na.rm=TRUE), max(vwc_data_normalized_2017[,6:ncol(vwc_data_normalized_2017)], na.rm=TRUE)), xlab = dates, ylab='Std Deviations by Location')
    text(x=dates2017[25], y=vwc_data_normalized_2017[i, 25], labels=vwc_data_normalized_2017[i, 1], cex=0.7)
  } else {
    lines.default(dates2017[5:length(dates2017)], vwc_data_normalized_2017[i, 6:ncol(vwc_data_normalized_2017)], xaxt='n', col=i+1)
    text(x=dates2017[25], y=vwc_data_normalized_2017[i, 25], labels=vwc_data_normalized_2017[i, 1], cex=0.7)
  }
}
axis.Date(side = 1, dates2017, at=weeks2017, format = '%m/%d/%y')

#plotting 2018 daily data by location x depth location from summaries produced above
data_name <- 'VWC'
depth <- '7'
yr <- '2018'
vwc_data_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, data_name, paste0('MedianVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
colnames(vwc_data_2018)
#endcol <- ncol(vwc_data_2018)
dates2018 <- seq.Date(as.Date(colnames(vwc_data_2018)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_2018)[ncol(vwc_data_2018)], '%b_%d_%Y'), by='day')
weeks2018 <- seq.Date(as.Date(colnames(vwc_data_2018)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_2018)[ncol(vwc_data_2018)], '%b_%d_%Y'), by='week')
for (i in 1:nrow(vwc_data_2018)) {
  plot(dates2018, vwc_data_2018[i, 2:ncol(vwc_data_2018)], type='b', xlab='Date', ylab='Daily Median VWC at 7 cm (mean of 2 sensors)', xaxt='n', main=paste('Location', vwc_data_2018$location[i])) #, ',', vwc_data_2018$aspect_cardinal[i], 'aspect'))
  axis.Date(side = 1, dates2018, at=weeks2018, format = '%m/%d')
  #text(x=dates2018[75], y=0.18, labels=paste(round(vwc_data_2018$mean_curv[i], 2), 'mean curvature', ',', round(vwc_data_2018$cti[i], 2), 'Compound Topographic Index'))
}

for (i in 1:nrow(vwc_data_2018)) {
  if (i == 1) {
    plot(dates2018, vwc_data_2018[i, 2:ncol(vwc_data_2018)], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_2018[,2:ncol(vwc_data_2018)], na.rm=TRUE), max(vwc_data_2018[,2:ncol(vwc_data_2018)], na.rm=TRUE)))
    text(x=dates2018[length(dates2018)-50], y=vwc_data_2018[i, ncol(vwc_data_2018)-50], labels=vwc_data_2018[i, 1], cex=0.7)
  } else {
    lines.default(dates2018, vwc_data_2018[i, 2:ncol(vwc_data_2018)], xaxt='n', col=i+1)
    text(x=dates2018[length(dates2018)- 50], y=vwc_data_2018[i, (ncol(vwc_data_2018)- 50)], labels=vwc_data_2018[i, 1], cex=0.7)
  }
}
axis.Date(side = 1, dates2018, at=weeks2018, format = '%m/%d/%y')

#normalize 2018 vwc_data
vwc_data_normalized_2018 <- vwc_data_2018
vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)] <- (vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)] - rowMeans(vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)], na.rm = TRUE)) / apply(vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)], 1, sd, na.rm=TRUE)

#and then plot as above
for (i in 1:nrow(vwc_data_normalized_2018)) {
  if (i == 1) {
    plot(dates2018[5:length(dates2018)], vwc_data_normalized_2018[i, 6:ncol(vwc_data_normalized_2018)], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_normalized_2018[,6:ncol(vwc_data_normalized_2018)], na.rm=TRUE), max(vwc_data_normalized_2018[,6:ncol(vwc_data_normalized_2018)], na.rm=TRUE)), xlab = 'Dates', ylab='Std Deviations by Location')
    text(x=dates2018[25], y=vwc_data_normalized_2018[i, 25], labels=vwc_data_normalized_2018[i, 1], cex=0.7)
  } else {
    lines.default(dates2018[5:length(dates2018)], vwc_data_normalized_2018[i, 6:ncol(vwc_data_normalized_2018)], xaxt='n', col=i+1)
    text(x=dates2018[25], y=vwc_data_normalized_2018[i, 25], labels=vwc_data_normalized_2018[i, 1], cex=0.7)
  }
}
axis.Date(side = 1, dates2018, at=weeks2018, format = '%m/%d/%y')

#read in forage data
forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018.by.sensor.csv'), stringsAsFactors=FALSE)

#work through dates and find correlation between normalized soil moisture and April 10, 2017 biomass
#note that some of these variables are defined above
SM_vs_biomass_analysis2017 <- data.frame(dates=dates2017, SMnorm.mean=apply(vwc_data_normalized_2017[2:ncol(vwc_data_normalized_2017)], 2, mean, na.rm=TRUE), SMnorm.range=apply(vwc_data_normalized_2017[2:ncol(vwc_data_normalized_2017)], 2, max, na.rm=TRUE) - apply(vwc_data_normalized_2017[2:ncol(vwc_data_normalized_2017)], 2, min, na.rm=TRUE), slope=NA, p.value=NA, r2=NA)
for (i in 2:ncol(vwc_data_normalized_2017)) {
  lm.summary <- summary(lm(forage_data$clp031417 ~ vwc_data_normalized_2017[,i]))
  SM_vs_biomass_analysis2017[i-1, 'slope'] <- lm.summary$coefficients[2, 1]
  SM_vs_biomass_analysis2017[i-1, 'p.value'] <- lm.summary$coefficients[2, 4]
  SM_vs_biomass_analysis2017[i-1, 'r2'] <- lm.summary$r.squared
}
# Most variability explained March 10 to cutting date with some structure in data apparent as early as Dec 24- Jan 8 with more negative relative soil moisture explaining more biomass.  For March 14 clippings, best correlation from Dec 2 to Jan 8 (peaks Dec 24)
SM_vs_biomass_analysis2017
plot(dates2017, SM_vs_biomass_analysis2017$slope)
plot(dates2017, SM_vs_biomass_analysis2017$r2)
plot(dates2017, SM_vs_biomass_analysis2017$p.value)
plot(SM_vs_biomass_analysis2017$r2, SM_vs_biomass_analysis2017$p.value)
dates2017[SM_vs_biomass_analysis2017$p.value < 0.05]
dates2017[which.max(SM_vs_biomass_analysis2017$r2)]
#positive slopes mid-March negative slopes late Dec-early Jan
plot(vwc_data_normalized_2017$Mar_23_2017, forage_data$clp041017)
plot(vwc_data_normalized_2017$Jan_08_2017, forage_data$clp031417)
plot(vwc_data_normalized_2017$Jan_08_2017, forage_data$clp041017)
plot(forage_data$clp021517, forage_data$clp041017)
plot(forage_data$clp021517, forage_data$clp031417)
plot(forage_data$clp031417, forage_data$clp041017)


#work through dates and find correlation between normalized soil moisture and April 15, 2018 biomass
#note that some of these variables are defined above
SM_vs_biomass_analysis2018 <- data.frame(dates=dates2018, SMnorm.mean=apply(vwc_data_normalized_2018[2:ncol(vwc_data_normalized_2018)], 2, mean, na.rm=TRUE), SMnorm.range=apply(vwc_data_normalized_2018[2:ncol(vwc_data_normalized_2018)], 2, max, na.rm=TRUE) - apply(vwc_data_normalized_2018[2:ncol(vwc_data_normalized_2018)], 2, min, na.rm=TRUE), slope=NA, p.value=NA, r2=NA)
for (i in 2:ncol(vwc_data_normalized_2018)) {
  lm.summary <- summary(lm(forage_data$clp032218 ~ vwc_data_normalized_2018[,i]))
  SM_vs_biomass_analysis2018[i-1, 'slope'] <- lm.summary$coefficients[2, 1]
  SM_vs_biomass_analysis2018[i-1, 'p.value'] <- lm.summary$coefficients[2, 4]
  SM_vs_biomass_analysis2018[i-1, 'r2'] <- lm.summary$r.squared
}
SM_vs_biomass_analysis2018
plot(dates2018, SM_vs_biomass_analysis2018$slope)
plot(dates2018, SM_vs_biomass_analysis2018$r2)
plot(dates2018, SM_vs_biomass_analysis2018$p.value)
plot(SM_vs_biomass_analysis2018$r2, SM_vs_biomass_analysis2018$p.value)
dates[which.max(SM_vs_biomass_analysis2018$r2)]
dates[SM_vs_biomass_analysis2018$p.value < 0.05] #strongest correlation 1/20/18 - 1/23/18 for 4/15/18 clipping, with r^2=0.54 on 1/21/18; when correlating to 3/22/18 clipping, best r^2 extends from 1/20-2/3/18 (peaks at r^2=0.63 on 1/26/2018)
plot(vwc_data_normalized_2018$Jan_26_2018, forage_data$clp032218) #relationship is positive, less drawdown means more productivity later
plot(vwc_data_normalized_2018$Jan_21_2018, forage_data$clp041518)
#both of these are positive

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

