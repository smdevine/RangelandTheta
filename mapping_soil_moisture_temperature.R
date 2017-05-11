library(rgdal)
library(raster)
library(gstat)
soil_VWCdata <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Apr2017/daily_by_location/VWC'
setwd(soil_VWCdata)
list.files()
medianVWC7cm <- read.csv("MedianVWC_7cm_dailymeans_by_location.csv", stringsAsFactors = FALSE)
coords <- medianVWC7cm[ ,c('Est_10N', 'Nrt_10N')]
medianVWC7cm_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=medianVWC7cm)

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
