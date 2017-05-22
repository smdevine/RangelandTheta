spatial_data <- 'C:/Users/smdevine/Desktop/rangeland project/results'
dem_fineres <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
plot_results <- 'C:/Users/smdevine/Desktop/rangeland project/results/plots/May2017'
additional_waypoints <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots/coordinates_waypoints'
options(digits = 10)
options(stringsAsFactors = FALSE)
library(extrafont)
library(extrafontdb)
loadfonts()
library(raster)
setwd('C:/Users/smdevine/Desktop/rangeland project/clip_plots')
fnames <- list.files(pattern = glob2rx('*.csv'))
fnames
sensorplot_data <- read.csv("CamattaBiomassSensorPlotsOnly2017.csv", stringsAsFactors = FALSE)
by_plot <- as.data.frame(tapply(sensorplot_data$Forage_kg_hectare, list(sensorplot_data$Location, sensorplot_data$DateClipped), mean))
by_plot$location <- as.integer(rownames(by_plot))
summary(by_plot)
setwd(spatial_data)
sensor_pts <- read.csv("sensor_terrain_characteristics5_3_17.csv", stringsAsFactors = FALSE)
forage_summary <- merge(by_plot, sensor_pts, by='location')
coords <- forage_summary[ ,c('Est_10N', 'Nrt_10N')]
forage_summary_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=forage_summary) 
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
cbind(feb_coords$Comment, mar_coords$Comment, apr_coords$Comment, may_coords$Comment) #check the order of each data.frame
Nrt_10N <- rowMeans(cbind(feb_coords$coords.x2, mar_coords$coords.x2, apr_coords$coords.x2, may_coords$coords.x2))
Est_10N <- rowMeans(cbind(feb_coords$coords.x1, mar_coords$coords.x1, apr_coords$coords.x1, may_coords$coords.x1))
avg_coords <- data.frame(location=feb_coords$Comment, Nrt_10N=Nrt_10N, Est_10N=Est_10N)
waypoint_forage <- merge(waypoint_forage, avg_coords, by='location')
waypoint_coords <- waypoint_forage[ ,c('Est_10N', 'Nrt_10N')]
waypoint_forage_sp <- SpatialPointsDataFrame(coords=waypoint_coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=waypoint_forage)
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

head(forage_summary)
head(waypoint_forage)
  