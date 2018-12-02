library(rgdal)
library(raster)
library(gstat)
library(spdep)
library(extrafont)
library(extrafontdb)
loadfonts()
library(animation)
#define data summary directory (summaries produced by soil_moisture_processing.R)
dataDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location'
DepletiondataDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/depletion_vwc'
soil_temperatureData <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location/Temperature'
spatialDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017'
tablesDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/tables'
options(digits = 10)

#make daily point plots of VWC and temperature data to put into a simple animation
setwd(soil_VWCdata)
vwc_files <- list.files(pattern = glob2rx('*.csv'))
vwc_files #6 is median value at 7 cm depth; 5 is median value at 22 cm depth
j <- 6
vwc_data <- read.csv(vwc_files[j], stringsAsFactors = FALSE)
coords <- vwc_data[ ,c('Est_10N', 'Nrt_10N')]
vwc_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data)
setwd(dem_fineres)
dem_1m <- raster('camatta_Nov2016_1m_dsm.tif')
dem_2m <- raster('camatta_Nov2016_2m_dsm.tif')
dem_3m <- raster('camatta_Nov2016_3m_dsm.tif')
hillshade_3m <- hillShade(terrain(dem_3m, opt='aspect'), terrain(dem_3m, opt='slope'), angle=45, direction=315)
hillshade_2m <- hillShade(terrain(dem_2m, opt='aspect'), terrain(dem_2m, opt='slope'), angle=45, direction=315)
hillshade_1m <- hillShade(terrain(dem_1m, opt='aspect'), terrain(dem_1m, opt='slope'), angle=45, direction=315)

magfactor <- 15
i <- 2

for (i in 2:which(colnames(vwc_data)=='May_01_2017')) {
  setwd(file.path(plot_results, '22cmdepth'))
  png(file = paste(format(as.Date(names(vwc_data_sp)[i], format = '%b_%d_%Y'), '%Y%m%d'), '_daily_median_22cm.png', sep = ''), family = 'Book Antiqua', width = 700, height = 500, units = 'px', res=100)
  par(mar=c(2, 2, 2, 3)) #does not affect right side I think because plotting a raster creates a space on the right even if if legend=FALSE
  plot(hillshade_1m, main=paste(gsub('_', ' ', names(vwc_data_sp)[i]), 'soil moisture at 22 cm depth'), col=gray(30:80/100), legend=FALSE, axes=FALSE)
  plot(dem_1m, col=terrain.colors(255, alpha=0.35), add=T)
  #mtext(text='Elevation (m)', side=1, line=1, at=744800)
  points(vwc_data_sp, cex=magfactor*vwc_data[,i], col='blue', pch=19)
#text(vwc_data_sp, labels=vwc_data_sp$location, pos=1, cex=1.1, halo=T)
  legend(x=744875, y=3931450, legend=c('0.10', '0.25', '0.40'), col='blue', pch=19, pt.cex=c(0.1*magfactor, 0.25*magfactor, 0.4*magfactor), x.intersp = 2, y.intersp = 1.9, bty="n")
  text(x=744895, y=3931452, labels='soil VWC', font=2, offset=0)
  dev.off()
}

#make an animation out of the soil moisture daily plots and forage data 
setwd(file.path(plot_results, 'forage'))
png_fnames <- list.files(pattern = glob2rx('*.png'))
ani.options(convert="C:/PROGRA~1/ImageMagick/convert.exe", loop=1, interval=3, ani.width=700, ani.height=500) #see http://stackoverflow.com/questions/24904945/command-prompt-error-c-program-is-not-recognized-as-an-internal-or-external-c for why this path does not work -- "C:/Program Files/ImageMagick/convert.exe"
im.convert(png_fnames, output = "Camatta_forage2017.gif")

#test for autocorrelation on normalized soil moisture for 2017 and 2018 data [previously done on absolute values] using spdep package (create function out of this later)

# year <- '2017'
# varname <- 'VWC'
# stat <- 'Median'
# depth <- '7'
autocorr_test_norm <- function(year, varname, stat, depth, nsim) {
  #MedianVWC_7cm_dailymeans_by_location.csv
  set.seed(19801976)
  sensor_pts <- shapefile(file.path(spatialDir, '5TM_sensor_locations_Camatta.shp'))
  names(sensor_pts)[1] <- 'location'
  vwc_data_normalized <- read.csv(file.path(dataDir, year, varname,  paste0(stat, varname, '_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  if(sum(is.na(vwc_data_normalized)) > 0) {
    vwc_data_normalized <- vwc_data_normalized[-which(apply(vwc_data_normalized, 1, anyNA)), ]
  }
  vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE)
  coords <- sensor_pts[which(sensor_pts$location %in% vwc_data_normalized$location), c('Est_10N', 'Nrt_10N')]
  vwc_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data_normalized)
  #vwc_data_sp <- vwc_data_sp[-13, ]
  #vwc_data_normalized <- vwc_data_normalized[-13, ]
  #then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(vwc_data_sp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(vwc_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  dates <- as.Date(colnames(vwc_data_normalized)[2:ncol(vwc_data_normalized)], '%b_%d_%Y')
  dates <- format.Date(dates, '%b_%d_%Y')
  for (i in 1:length(dates)) {
    result <- moran.mc(vwc_data_normalized[[dates[i]]], idw_list, nsim = nsim)
    if (i==1) {
      results <- matrix(c(dates[i], result$statistic, result$p.value), nrow=1, ncol=3, byrow=TRUE)
      next
    }
    results <- rbind(results, c(dates[i], result$statistic, result$p.value))
  }
  results <- as.data.frame(results)
  colnames(results) <- c('date', 'Moran I statistic', 'p_value')
  results$n_pts <- nrow(vwc_data_normalized)
  if (!dir.exists(file.path(dataDir, 'autocorrelation_test_normalized'))) {
    dir.create(file.path(dataDir, 'autocorrelation_test_normalized'))
  }
  write.csv(results, file.path(dataDir, 'autocorrelation_test_normalized', paste0(stat, year, '_', varname, depth, 'cm_autocorr_normtest.csv')), row.names = FALSE)
}
#(year, varname, stat, depth)
autocorr_test_norm('2017', 'VWC', 'Mean', '7', 999)
autocorr_test_norm('2017', 'VWC', 'Mean', '22', 999)
autocorr_test_norm('2018', 'VWC', 'Mean', '7', 999)
autocorr_test_norm('2018', 'VWC', 'Mean', '22', 999)

#autocorr test of absolute moisture and temperature
autocorr_test_abs <- function(year, varname, varname2, stat, depth, nsim) {
  set.seed(19801976)
  sensor_pts <- shapefile(file.path(spatialDir, '5TM_sensor_locations_Camatta.shp'))
  names(sensor_pts)[1] <- 'location'
  vwc_data <- read.csv(file.path(dataDir, year, varname,  paste0(stat, varname2, '_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  if(sum(is.na(vwc_data)) > 0) {
    vwc_data <- vwc_data[-which(apply(vwc_data, 1, anyNA)), ]
  } #have to remove rows for entire time frame if data missing
  coords <- sensor_pts[which(sensor_pts$location %in% vwc_data$location), c('Est_10N', 'Nrt_10N')]
  vwc_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data)
  #vwc_data_sp <- vwc_data_sp[-13, ]
  #vwc_data <- vwc_data[-13, ]
  #then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(vwc_data_sp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(vwc_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  dates <- as.Date(colnames(vwc_data)[2:ncol(vwc_data)], '%b_%d_%Y')
  dates <- format.Date(dates, '%b_%d_%Y')
  for (i in 1:length(dates)) {
    result <- moran.mc(vwc_data[[dates[i]]], idw_list, nsim = nsim)
    if (i==1) {
      results <- matrix(c(dates[i], result$statistic, result$p.value), nrow=1, ncol=3, byrow=TRUE)
      next
    }
    results <- rbind(results, c(dates[i], result$statistic, result$p.value))
  }
  results <- as.data.frame(results)
  colnames(results) <- c('date', 'Moran I statistic', 'p_value')
  results$n_pts <- nrow(vwc_data)
  if (!dir.exists(file.path(dataDir, 'autocorrelation_test_abs'))) {
    dir.create(file.path(dataDir, 'autocorrelation_test_abs'))
  }
  write.csv(results, file.path(dataDir, 'autocorrelation_test_abs', paste0(stat, year, '_', varname2, depth, 'cm_autocorrtest.csv')), row.names = FALSE)
}
#run the function
autocorr_test_abs('2017', 'VWC', 'VWC', 'Mean', '7', 999)
autocorr_test_abs('2017', 'VWC', 'VWC', 'Mean', '22', 999)
autocorr_test_abs('2018', 'VWC', 'VWC', 'Mean', '7', 999)
autocorr_test_abs('2018', 'VWC', 'VWC', 'Mean', '22', 999)
autocorr_test_abs('2017', 'Temperature', 'T', 'Mean', '7', 999)
autocorr_test_abs('2017', 'Temperature', 'T', 'Mean', '22', 999)
autocorr_test_abs('2018', 'Temperature', 'T', 'Mean', '7', 999)
autocorr_test_abs('2018', 'Temperature', 'T', 'Mean', '22', 999)

#autocorrelation of depletion data
autocorr_test_depletionVWC <- function(year, depth, nsim, varname2) {
  set.seed(19801976)
  sensor_pts <- shapefile(file.path(spatialDir, '5TM_sensor_locations_Camatta.shp'))
  names(sensor_pts)[1] <- 'location'
  vwc_data <- read.csv(file.path(DepletiondataDir, paste0('depletion_vwc_', depth, 'cm_', year, '.csv')), stringsAsFactors = FALSE) #depletion_vwc_7cm_2017.csv
  if(sum(is.na(vwc_data)) > 0) {
    vwc_data <- vwc_data[-which(apply(vwc_data, 1, anyNA)), ]
  } #have to remove rows for entire time frame if data missing
  coords <- sensor_pts[which(sensor_pts$location %in% vwc_data$location), c('Est_10N', 'Nrt_10N')]
  vwc_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data)
  print(as.data.frame(vwc_data_sp))
  #vwc_data_sp <- vwc_data_sp[-13, ]
  #vwc_data <- vwc_data[-13, ]
  #then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(vwc_data_sp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(vwc_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  dates <- as.Date(colnames(vwc_data)[2:ncol(vwc_data)], '%b_%d_%Y')
  dates <- format.Date(dates, '%b_%d_%Y')
  for (i in 1:length(dates)) {
    result <- moran.mc(vwc_data[[dates[i]]], idw_list, nsim = nsim)
    if (i==1) {
      results <- matrix(c(dates[i], result$statistic, result$p.value), nrow=1, ncol=3, byrow=TRUE)
      next
    }
    results <- rbind(results, c(dates[i], result$statistic, result$p.value))
  }
  results <- as.data.frame(results)
  colnames(results) <- c('date', 'Moran I statistic', 'p_value')
  results$n_pts <- nrow(vwc_data)
  if (!dir.exists(file.path(dataDir, 'autocorrelation_test_depletionvwc'))) {
    dir.create(file.path(dataDir, 'autocorrelation_test_depletionvwc'))
  }
  write.csv(results, file.path(dataDir, 'autocorrelation_test_depletionvwc', paste0(year, '_', varname2, depth, 'cm_autocorrtest.csv')), row.names = FALSE)
}
autocorr_test_depletionVWC(2017, 7, 999, 'depletionVWC')
autocorr_test_depletionVWC(2017, 22, 999, 'depletionVWC')
autocorr_test_depletionVWC(2018, 7, 999, 'depletionVWC')
autocorr_test_depletionVWC(2018, 22, 999, 'depletionVWC')
#count up the significant days
# test <- 'autocorrelation_test_abs'
# varname <- 'T'
# depth <- '7cm'
# yr <- '2017'
# start_date <- 'Dec_01_2016'
# end_date <- 'May_01_2017'
# p_val <- 0.05
days_sig <- function(test, varname, depth, yr, start_date, end_date, p_val) {
  autocorr_result <- read.csv(list.files(file.path(dataDir, test), pattern = glob2rx(paste0('*', yr, '*', varname, depth, '*.csv')), full.names = TRUE), stringsAsFactors = FALSE)
  day_count <- sum(autocorr_result[which(autocorr_result$date==start_date):which(autocorr_result$date==end_date), 'p_value'] < p_val)
  data.frame(test, varname, yr, depth, day_count, total_days=length(which(autocorr_result$date==start_date):which(autocorr_result$date==end_date)))
}
yr2017_7cm_depletionVWC <- days_sig('autocorrelation_test_depletionvwc', 'depletionVWC', '7cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2017_22cm_depletionVWC <- days_sig('autocorrelation_test_depletionvwc', 'depletionVWC', '22cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2018_7cm_depletionVWC <- days_sig('autocorrelation_test_depletionvwc', 'depletionVWC', '7cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2018_22cm_depletionVWC <- days_sig('autocorrelation_test_depletionvwc', 'depletionVWC', '22cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2017_7cm_normVWC <- days_sig('autocorrelation_test_normalized', 'VWC', '7cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2017_22cm_normVWC <- days_sig('autocorrelation_test_normalized', 'VWC', '22cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2018_7cm_normVWC <- days_sig('autocorrelation_test_normalized', 'VWC', '7cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2018_22cm_normVWC <- days_sig('autocorrelation_test_normalized', 'VWC', '22cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2017_7cm_absVWC <- days_sig('autocorrelation_test_abs', 'VWC', '7cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2017_22cm_absVWC <- days_sig('autocorrelation_test_abs', 'VWC', '22cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2018_7cm_absVWC <- days_sig('autocorrelation_test_abs', 'VWC', '7cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2018_22cm_absVWC <- days_sig('autocorrelation_test_abs', 'VWC', '22cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2017_7cm_absT <- days_sig('autocorrelation_test_abs', 'T', '7cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2017_22cm_absT <- days_sig('autocorrelation_test_abs', 'T', '22cm', '2017', 'Dec_01_2016', 'May_01_2017', 0.05)
yr2018_7cm_absT <- days_sig('autocorrelation_test_abs', 'T', '7cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
yr2018_22cm_absT <- days_sig('autocorrelation_test_abs', 'T', '22cm', '2018', 'Dec_01_2017', 'May_01_2018', 0.05)
autocorr_count_final <- rbind(yr2017_7cm_absT, yr2017_7cm_absVWC, yr2017_7cm_normVWC, yr2017_7cm_depletionVWC, yr2017_22cm_absT, yr2017_22cm_absVWC, yr2017_22cm_normVWC, yr2017_22cm_depletionVWC, yr2018_7cm_absT, yr2018_7cm_absVWC, yr2018_7cm_normVWC, yr2018_7cm_depletionVWC, yr2018_22cm_absT, yr2018_22cm_absVWC, yr2018_22cm_normVWC, yr2018_22cm_depletionVWC)
write.csv(autocorr_count_final, file.path(tablesDir, 'autocorr_results_final.csv'), row.names = FALSE)

#now do kriging interpolation of some of the significant ones
read_data <- function(year, varname, stat, depth) {
  read.csv(file.path(dataDir, year, varname,  paste0(stat, varname, '_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
}
normalize_data <- function(df) {
  df[ ,2:ncol(df)] <- (df[ ,2:ncol(df)] - rowMeans(df[ ,2:ncol(df)], na.rm = TRUE)) / apply(df[ ,2:ncol(df)], 1, sd, na.rm=TRUE)
  df
}
vwc_data <- read_data('2017', 'VWC', 'Mean', '7')
vwc_data_normalized <- normalize_data(vwc_data)
#read-in points
sensor_pts <- shapefile(file.path(spatialDir, '5TM_sensor_locations_Camatta.shp'))
names(sensor_pts)[1] <- 'location'
coords <- sensor_pts[which(sensor_pts$location %in% vwc_data_normalized$location), c('Est_10N', 'Nrt_10N')]
vwc_data_norm_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=vwc_data_normalized)

range(vwc_data_norm_sp$Jan_07_2017)
range(vwc_data_norm_sp$Jan_08_2017)
range(vwc_data_norm_sp$Jan_09_2017)
magfactor <- 2
plotSM <- function(sp_obj, date, magfactor, color.pts, symbol.type) {
  plot(sp_obj, cex=magfactor * sp_obj[[date]], col = color.pts, pch = symbol.type, main=date)
}
dates <- format.Date(seq.Date(as.Date('Nov_20_2016', '%b_%d_%Y'), as.Date('Apr_15_2017', '%b_%d_%Y'), by='day'), format = '%b_%d_%Y')
for (i in seq_along(dates)) {
  plotSM(vwc_data_norm_sp, dates[i], 2, 'blue', 19)
  #text()
}


plot(vwc_data_norm_sp, cex=2*vwc_data_norm_sp$Jan_08_2017, col='blue', pch=19)
plot(vwc_data_norm_sp, cex=2*vwc_data_norm_sp$Jan_09_2017, col='blue', pch=19)
plot(vwc_data_norm_sp, cex=2*vwc_data_norm_sp$Jan_10_2017, col='blue', pch=19)
plot(vwc_data_norm_sp, cex=2*vwc_data_norm_sp$Jan_11_2017, col='blue', pch=19)
plot(vwc_data_norm_sp, cex=2*vwc_data_norm_sp$Jan_12_2017, col='blue', pch=19)
gs <- gstat(formula=Jan_07_2017 ~ 1, locations=vwc_data_norm_sp)
v <- variogram(gs, width=20, cutoff=300)
v
plot(v)
#now fit a model variogram
fve <- fit.variogram(v, vgm(model = "Exp"))
fve
plot(variogramLine(fve, 200), type='l')
points(v[,2:3], pch=20, col='red')
#Try a different type (spherical in stead of exponential)
fvs <- fit.variogram(v, vgm(model="Sph"))
fvs
plot(variogramLine(fvs, 200), type='l', col='blue', lwd=2)
points(v[,2:3], pch=20, col='red')
#Another way to plot the variogram and the model
plot(v, fve)
plot(v, fvs)
#Use variogram fve in a kriging interpolation
k <- gstat(formula=Jan_07_2017~1, locations=vwc_data_norm_sp, model=fve)
# predicted values
r <- raster(vwc_data_norm_sp)
res(r) <- 1  # 10 km if your CRS's units are in km
g <- as(r, 'SpatialGrid')
kp <- predict(k, g)
class(kp)
spplot(kp)
kp_raster <- raster(kp)
plot(kp_raster)
plot(vwc_data_norm_sp, add=TRUE, cex=vwc_data_normalized$Jan_07_2017, pch=19, col='blue')

# variance
ok <- brick(kp)
ok <- mask(ok, ca)
names(ok) <- c('prediction', 'variance')
plot(ok)


#playing around with semivariograms without invoking gstat, plotting all the pairs (120 total), as guided by lab 14 from Dr. Hijmans GEO200CN Spring 2016 course
distance_pairs <- pointDistance(medianVWC7cm_sp, latlon=FALSE) #produces 2x + 16 the unique number of pairs
diag(distance_pairs) <- NA #make the diagnol of the matrix equal to NA
distance_pairs <- as.dist(distance_pairs) #this essentially gets rid of the duplicate pairs
d_VWC <- dist(medianVWC7cm$X2016.11.21)
semivar <- d_VWC^2/2
plot(distance_pairs, semivar, xlab=c('Distance between locations'), ylab=c('Semivariance'), pch=20, xaxs="i")




