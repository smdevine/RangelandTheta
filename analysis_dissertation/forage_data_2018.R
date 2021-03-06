spatial_data <- 'C:/Users/smdevine/Desktop/rangeland project/results'
sensor_coords <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017'
additional_waypoints <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots/coordinates_waypoints'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots' #previously called forage_data
soil_VWCdata <- 'C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/May2017/daily_by_location/VWC'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
library(extrafont)
library(extrafontdb)
loadfonts()
library(raster)
library(spdep)

list.files(forageDir, pattern = glob2rx('*.csv'))
sensorplot_data <- read.csv(file.path(forageDir, "CamattaBiomassSensorPlotsOnly2017.csv"), stringsAsFactors = FALSE)
sensorplot_data$Location_sub <- paste(sensorplot_data$Location, sensorplot_data$Subsample, sep='')
by_subsample <- as.data.frame(tapply(sensorplot_data$Forage_kg_hectare, list(sensorplot_data$Location_sub, sensorplot_data$DateClipped), as.numeric))
by_subsample$location_sub <- rownames(by_subsample)
by_subsample$location <- as.integer(gsub('[A,B]', '', by_subsample$location_sub))
#left off here; combine with sensor coordinates
by_plot <- as.data.frame(tapply(sensorplot_data$Forage_kg_hectare, list(sensorplot_data$Location, sensorplot_data$DateClipped), mean))
by_plot$location <- as.integer(rownames(by_plot))
by_plot <- by_plot[ ,c(5, 1:4)]
by_plot
summary(by_plot[ ,2:5])
boxplot(by_plot[,2:5])
write.csv(by_plot, file.path(forageDir, 'summaries', 'forage2017.by.sensor.csv'), row.names = FALSE)

#get standard deviations by plot for 2017
by_plot_stdev <- as.data.frame(tapply(sensorplot_data$Forage_kg_hectare, list(sensorplot_data$Location, sensorplot_data$DateClipped), sd))
by_plot_stdev$location <- as.integer(rownames(by_plot_stdev))
by_plot_stdev <- by_plot_stdev[ ,c(5, 1:4)]
by_plot_stdev
summary(by_plot_stdev[ ,2:5])
boxplot(by_plot_stdev[,2:5])
write.csv(by_plot_stdev, file.path(forageDir, 'summaries', 'forage2017.by.sensor_stdevs.csv'), row.names = FALSE)

#read-in 2018 data and merge with 2017 data
forage2018 <- read.csv(file.path(forageDir, "Camatta BioMass Production 2018 all dates.csv"), stringsAsFactors = FALSE)
colnames(forage2018)[2] <- 'biomass.kg.ha'
forage2018$location <- as.integer(gsub('A|B', '', forage2018$ID))
by_plot2018 <- as.data.frame(tapply(forage2018$biomass.kg.ha, list(forage2018$location, forage2018$Date), mean))
by_plot2018$location <- as.integer(rownames(by_plot2018))
by_plot2018 <- by_plot2018[ ,c(5, 1:4)]
by_plot2018
summary(by_plot2018[ ,2:5])
write.csv(by_plot2018, file.path(forageDir, 'summaries', 'forage2018.by.sensor.csv'), row.names = FALSE)
#get stdev for 2018
by_plot2018_stdev <- as.data.frame(tapply(forage2018$biomass.kg.ha, list(forage2018$location, forage2018$Date), sd))
by_plot2018_stdev$location <- as.integer(rownames(by_plot2018_stdev))
by_plot2018_stdev <- by_plot2018_stdev[ ,c(5, 1:4)]
by_plot2018_stdev
summary(by_plot2018_stdev[ ,2:5])
write.csv(by_plot2018_stdev, file.path(forageDir, 'summaries', 'forage2018.by.sensor_stdevs.csv'), row.names = FALSE)

#merge 2017 and 2018
by_plot_all <- cbind(by_plot, by_plot2018[,2:5])
colnames(by_plot_all)
colnames(by_plot_all)[2:9] <- c('clp021517', 'clp031417', 'clp041017', 'clp050117', 'clp011618', 'clp021518', 'clp032218', 'clp041518')
boxplot(by_plot_all[,2:9])
plot(by_plot_all$clp041017, by_plot_all$clp041518)
#plot Feb 2017 vs. 2018
lm_Mar_forage <- summary(lm(clp021518 ~ clp021517, data = by_plot_all))
png(file = file.path(results, 'figures', 'WY2017Feb.forage.vs.WY2018Feb.forage.png', sep = ''), family = 'Book Antiqua', width = 800, height = 600, units = 'px', res=100)
par(mar=c(4.5, 4.5, 2, 2))
plot(by_plot_all$clp021517, by_plot_all$clp021517, xlab='Feb 15, 2017 biomass (kg / ha)', ylab='Feb 15, 2018 biomass (kg / ha)', main='Relationship between 2017 and 2018 standing forage in March, Camatta catchment', cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.1)
abline(a=lm_Feb_forage$coefficients[1], b=lm_Feb_forage$coefficients[2], lty = 2)
text(x=by_plot_all$clp031417, y=by_plot_all$clp032218, labels = by_plot_all$location, pos = 1, offset = 0.3)
dev.off()

#plot Mar 2017 vs. 2018
lm_Mar_forage <- summary(lm(clp032218 ~ clp031417, data = by_plot_all))
png(file = file.path(results, 'figures', 'WY2017Mar.forage.vs.WY2018Mar.forage.png', sep = ''), family = 'Book Antiqua', width = 800, height = 600, units = 'px', res=100)
par(mar=c(4.5, 4.5, 2, 2))
plot(by_plot_all$clp031417, by_plot_all$clp032218, xlab='Mar 14, 2017 biomass (kg / ha)', ylab='Mar 22, 2018 biomass (kg / ha)', main='Relationship between 2017 and 2018 standing forage in March, Camatta catchment', cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.1)
abline(a=lm_Mar_forage$coefficients[1], b=lm_Mar_forage$coefficients[2], lty = 2)
text(x=by_plot_all$clp031417, y=by_plot_all$clp032218, labels = by_plot_all$location, pos = 1, offset = 0.3)
dev.off()
#plot Apr 2017 vs. 2018
lm_Apr_forage <- summary(lm(clp041518 ~ clp041017, data = by_plot_all))
png(file = file.path(results, 'figures', 'WY2017Apr.forage.vs.WY2018Apr.forage.png', sep = ''), family = 'Book Antiqua', width = 800, height = 600, units = 'px', res=100)
par(mar=c(4.5, 4.5, 2, 2))
plot(by_plot_all$clp041017, by_plot_all$clp041518, xlab = 'Apr 10, 2017 biomass (kg / ha)', ylab ='Apr 15, 2018 biomass (kg / ha)', main='Relationship between 2017 and 2018 standing forage in April, Camatta catchment', cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.1, ylim = c(0, 4800), xlim=c(0, 4800))
abline(a=lm_Apr_forage$coefficients[1], lm_Apr_forage$coefficients[2], lty = 2)
text(x=by_plot_all$clp041017, y=by_plot_all$clp041518, labels = by_plot_all$location, pos = 1, offset = 0.3)
legend("topleft", legend=(c("< 1254", '1254-1341', "1341-1458", '>1458')), lty=c(1, 2, 1, 2, 1, 2), col=c('blue', '', '', '', '', ''), inset = 0.005, cex=0.9)
dev.off()

plot(by_plot_all$clp031417, by_plot_all$clp041518)
summary(lm(clp041518 ~ clp041017, data = by_plot_all))
summary(lm(clp041518 ~ clp031417, data = by_plot_all))
summary(lm(clp041518 ~ clp021517, data = by_plot_all))
write.csv(by_plot_all, file.path(forageDir, 'summaries', 'peakforage_2017_2018.csv'), row.names = FALSE)

by_plot_all <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018.by.sensor.csv'), stringsAsFactors = FALSE)

#read-in terrain chars
terrain_energy <- read.csv(file.path(results, 'tables', "forage_terrain_energy_3m_Hogan.csv"), stringsAsFactors = FALSE)

terrain_energy$energy_colors <- ifelse(terrain_energy$annual_kwh.m2 < summary(terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(terrain_energy$annual_kwh.m2 > summary(terrain_energy$annual_kwh.m2)[2] & terrain_energy$annual_kwh.m2 < summary(terrain_energy$annual_kwh.m2)[3], 'lightblue2', ifelse(terrain_energy$annual_kwh.m2 < summary(terrain_energy$annual_kwh.m2)[5], 'darkorange', 'red3'))) #classifies annual energy into 4 groups


test <- data.frame(terrain_energy$annual_kwh.m2, energy_colors)
test[order(test$terrain_energy.annual_kwh.m2),]

#look at peak forage differences between 2017 and 2018
peak2017 <- as.numeric(round(apply(by_plot_all[,c(2:5)], 1, max), 1))
peak2017_dates <- apply(by_plot_all[,c(2:5)], 1, function(x) names(x)[which(x == max(x))])
peak2018 <- as.numeric(round(apply(by_plot_all[,c(6:9)], 1, max), 1))
peak2018_dates <- apply(by_plot_all[,c(6:9)], 1, function(x) names(x)[which(x == max(x))])
peak_df <- data.frame(location=1:16, peak2017 = peak2017, peak2017_dates = peak2017_dates, peak2018 = peak2018, peak2018_dates = peak2018_dates)

plot(peak_df$peak2017, peak_df$peak2018)
lm_peak_forage <- summary(lm(peak2018 ~ peak2017, data = peak_df))
plot(lm_peak_forage$residuals, terrain_energy$annual_kwh.m2) #residuals tightly related to aspect in non-linear way
non.lm_model <- summary(glm(peak_df$peak2018 ~ peak_df$peak2017 + poly(terrain_energy$annual_kwh.m2, 2)))
non.lm_model$coefficients

#plot peak2018 vs. peak2017
plot(peak_df$peak2018, peak_df$peak2017)
lm_peak_forage_v2 <- summary(lm(peak2017 ~ peak2018, data = peak_df))
plot(lm_peak_forage_v2$residuals, terrain_energy$annual_kwh.m2)

#write peak forage to file
write.csv(peak_df, file.path(forageDir, 'summaries', 'forage2017_2018.peak.csv'), row.names=FALSE)

#merge back with forage by month and write to file
forage_all <- merge(by_plot_all, peak_df[,c('location', 'peak2017', 'peak2018')], by='location')
write.csv(forage_all, file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), row.names=FALSE)

#make a plot of peak2017 vs. peak2018
png(file = file.path(results, 'figures', 'finals', 'peak2017.vs.2018.forage.png', sep = ''), family = 'Book Antiqua', width = 800, height = 600, units = 'px', res=100)
par(mar=c(4.5, 4.5, 2, 2))
plot(peak_df$peak2017, peak_df$peak2018, xlab=expression(paste('2017 peak forage (kg', ' ', ha^-1, ')')), ylab=expression(paste('2018 peak forage (kg', ' ', ha^-1, ')')), main='Relationship between dry and wet year standing forage', pch=19, cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.1, ylim = c(0, 1800), xlim=c(0, 4800), col=energy_colors)
abline(a=lm_peak_forage$coefficients[1], b=lm_peak_forage$coefficients[2], lty = 2)
text(x=peak_df$peak2017, y=peak_df$peak2018, labels = peak_df$location, pos = 1, offset = 0.3)
legend("topleft", legend=(c("< 1254", '1254-1341', "1341-1458", '>1458')), pch=19, col=c('blue', 'gold', 'darkorange', 'red2'), inset = 0.005, title = expression(paste('annual kWh ', m^2)))
dev.off()
summary((peak_df$peak2017 - peak_df$peak2018) / peak_df$peak2017)

#make a box plot of all dates
png(file = file.path(results, 'figures', 'finals', 'WY2017_WY2018.forage.png', sep = ''), family = 'Book Antiqua', width = 800, height = 480, units = 'px', res=100)
par(mar=c(3, 4.5, 2, 2))
boxplot(by_plot_all[,c(2:5,7:9)], names = c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '2/15/18', '3/22/18', '4/15/18'), ylab=expression(paste('kg forage ', ha^-1)), main='Annual forage growth, Camatta catchment, 2017-2018', boxwex = 0.5)
abline(v=4.5, lty=2)
text(1.5, 3000, 'wet year')
text(5.5, 3000, 'dry year')
dev.off()

#autocorr test of forage results
autocorr_test_forage <- function(nsim) {
  set.seed(19801976)
  #MedianVWC_7cm_dailymeans_by_location.csv
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors = FALSE)
  forage_data$Mar2017growth <- forage_data$clp031417 - forage_data$clp021517
  forage_data$Apr2017growth <- forage_data$clp041017 - forage_data$clp031417
  forage_data$May2017growth <- forage_data$clp050117 - forage_data$clp041017
  forage_data$Mar2018growth <- forage_data$clp032218 - forage_data$clp021518
  forage_data$Apr2018growth <- forage_data$clp041518 - forage_data$clp032218
  sensor_pts <- shapefile(file.path(sensor_coords, '5TM_sensor_locations_Camatta.shp'))
  names(sensor_pts)[1] <- 'location'
  coords <- sensor_pts[which(sensor_pts$location %in% forage_data$location), c('Est_10N', 'Nrt_10N')]
  forage_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=forage_data)
  #then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(forage_data_sp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(forage_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  dates <- colnames(forage_data)[2:ncol(forage_data)]
  for (i in 1:length(dates)) {
    result <- moran.mc(forage_data[[dates[i]]], idw_list, nsim = nsim)
    if (i==1) {
      results <- matrix(c(dates[i], result$statistic, result$p.value), nrow=1, ncol=3, byrow=TRUE)
      next
    }
    results <- rbind(results, c(dates[i], result$statistic, result$p.value))
  }
  results <- as.data.frame(results)
  colnames(results) <- c('date', 'Moran I statistic', 'p_value')
  results$n_pts <- nrow(forage_data)
  if (!dir.exists(file.path(forageDir, 'summaries', 'autocorrelation_test_abs'))) {
    dir.create(file.path(forageDir, 'summaries', 'autocorrelation_test_abs'))
  }
  write.csv(results, file.path(forageDir, 'summaries', 'autocorrelation_test_abs', 'forage_autocorrtest_final.csv'), row.names = FALSE)
}
autocorr_test_forage(nsim=999)

#old code
#merge with spatial coordinates
sensor_pts <- read.csv(file.path(spatial_data, "sensor_terrain_characteristics5_3_17.csv"), stringsAsFactors = FALSE) #how were terrain characteristics derived?
sensor_pts <- sensor_pts[ ,c(1,3:4)] #reduce to columns that have location number and coordinates, because these terrain characteristics were derived from 10 m DEM
forage_summary <- merge(by_subsample, sensor_pts, by='location')
#colnames(forage_summary)[2:5] <- c("clp021517", "clp031417", "clp041017", "clp050117") #rename to match other waypoint clipping date column names for writing shapefile
coords <- forage_summary[ ,c('Est_10N', 'Nrt_10N')]
forage_summary <- forage_summary[ ,-6:-7]
forage_summary_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=forage_summary)
setwd(file.path(forageDir, 'results'))
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
setwd(file.path(forageDir, 'results'))
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
setwd(file.path(forageDir, 'results'))
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

