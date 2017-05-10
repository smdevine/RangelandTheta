library(rgdal)
library(raster)
library(gstat)
soil_VWCdata <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Apr2017/daily_by_location/VWC'
setwd(soil_VWCdata)
list.files()
medianVWC7cm <- read.csv("MedianVWC_7cm_dailymeans_by_location.csv", stringsAsFactors = FALSE)
coords <- medianVWC7cm[ ,c('Est_10N', 'Nrt_10N')]
medianVWC7cm_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=medianVWC7cm)

#playing around with semivariograms, plotting all the pairs
distance_pairs <- pointDistance(medianVWC7cm_sp, latlon=FALSE)
diag(distance_pairs) <- NA
distance_pairs <- as.dist(distance_pairs)
d_VWC <- dist(medianVWC7cm$X2016.11.19)
semivar <- d_VWC^2/2
plot(distance_pairs, semivar, xlab=c('Distance between locations'), ylab=c('Semivariance'), pch=20, xaxs="i")


#using the gstat package
day <- gstat(formula= X2016.11.23~1, data=medianVWC7cm_sp)
test_variogram <- variogram(day, width=15)
test_variogram
plot(test_variogram)
