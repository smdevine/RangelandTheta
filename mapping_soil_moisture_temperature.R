library(rgdal)
library(raster)
library(gstat)
library(spdep)
#define data summary directory (summaries produced by soil_moisture_processing.R)
soil_VWCdata <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location/VWC'
soil_temperatureData <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location/Temperature'
#test for autocorrelation using spdep package (create function out of this later)

data_dir <- soil_VWCdata #define working directory manually, either soil_VWCdata or soil_temperatureData
setwd(data_dir)
vwc_files <- list.files(pattern = glob2rx('*.csv')) #vwc_files can be taken to mean
#set j manually from 1 to 6
for (j in 1:6) {
  setwd(data_dir)
  vwc_data <- read.csv(vwc_files[j], stringsAsFactors = FALSE)
  coords <- vwc_data[ ,c('Est_10N', 'Nrt_10N')]
  vwc_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data)
#delete point 13 since missing data after 3/17/16; could be gap filled based on new data
  vwc_data_sp <- vwc_data_sp[-13, ]
  vwc_data <- vwc_data[-13, ]
#then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(vwc_data_sp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(vwc_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  dates <- seq.Date(as.Date('2016/11/19'), as.Date('2017/5/1'), by='day')
  dates <- format.Date(as.Date(dates, format = '%Y%m%d'), '%b_%d_%Y')
  for (i in 1:length(dates)) {
    result <- moran.mc(vwc_data[[dates[i]]], idw_list, nsim = 99)
    if (i==1) {
      results <- matrix(c(dates[i], result$statistic, result$p.value), nrow=1, ncol=3, byrow=TRUE)
      next
    }
    results <- rbind(results, c(dates[i], result$statistic, result$p.value))
  }
  results <- as.data.frame(results)
  colnames(results) <- c('date', 'Moran I statistic', 'p_value')
  setwd(file.path(data_dir, 'autocorrelation_test'))
  write.csv(results, paste('autocorr_test', vwc_files[j], sep = ''), row.names = FALSE)
}

#make daily point plots of VWC and temperature data to put into a simple animation


#playing around with semivariograms without invoking gstat, plotting all the pairs (120 total), as guided by lab 14 from Dr. Hijmans GEO200CN Spring 2016 course
distance_pairs <- pointDistance(medianVWC7cm_sp, latlon=FALSE) #produces 2x + 16 the unique number of pairs
diag(distance_pairs) <- NA #make the diagnol of the matrix equal to NA
distance_pairs <- as.dist(distance_pairs) #this essentially gets rid of the duplicate pairs
d_VWC <- dist(medianVWC7cm$X2016.11.21)
semivar <- d_VWC^2/2
plot(distance_pairs, semivar, xlab=c('Distance between locations'), ylab=c('Semivariance'), pch=20, xaxs="i")


#using the gstat package to plot a semivariogram (there are 120 pairs)
day <- gstat(formula= X2017.02.19~1, data=medianVWC7cm_sp)
test_variogram <- variogram(day, width=20, cutoff=260)
test_variogram
plot(test_variogram)


