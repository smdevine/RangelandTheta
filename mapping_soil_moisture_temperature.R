library(rgdal)
library(raster)
library(gstat)
library(spdep)
library(extrafont)
library(extrafontdb)
loadfonts()
library(animation)
#define data summary directory (summaries produced by soil_moisture_processing.R)
soil_VWCdata <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location/VWC'
soil_temperatureData <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location/Temperature'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017'
options(digits = 10)

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
setwd(soil_VWCdata)
vwc_files <- list.files(pattern = glob2rx('*.csv'))
vwc_files #6 is median value at 7 cm depth; 5 is median value at 22 cm depth
j <- 5
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


