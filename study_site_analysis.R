#migrated to Desktop/GITprojects/RangelandTheta on 5/8/17
#was originally saved in Desktop/rangeland project/r scripts
#mean curvature and topographic position index calculated in ArcGIS 
dems <- 'C:/Users/smdevine/Desktop/rangeland project/elevation_NED10M_studysite/elevation'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r_v2'
library(rgdal)
library(raster)
options(digits = 10)
setwd(dems)
fnames <- list.files(dems, pattern = glob2rx('*.tif'))
fnames
#read in raster files (only need dem_1 for study area)
dem_1 <- raster(fnames[2]) #verify "ned10m35120d3.tif"
crs(dem_1)
dem_2 <- raster(fnames[3]) #verify "ned10m35120e3.tif"
crs(dem_2)

#re-do R terrain analysis based on a cropped version of "ned10m35120d3.tif" where soil moisture sensors are located
dem_1
new_ex <- extent(743682.5, 745682.5, 3930560, ymax(dem_1))
dem_cropped <- crop(dem_1, new_ex)
writeRaster(dem_cropped, 'dem_cropped.tif', format='GTiff', options='TFW=yes', overwrite=TRUE)
setwd(terrainDir)
slope_neighbors4 <- terrain(dem_cropped, opt = 'slope', unit = 'degrees', neighbors = 4)
writeRaster(slope_neighbors4, 'slope_neighbors4.tif', format='GTiff', overwrite=TRUE) #options='TFW=YES' creates a tfw file also; useful for ArcGIS
slope_neighbors8 <- terrain(dem_cropped, opt = 'slope', unit = 'degrees', neighbors = 8)
writeRaster(slope_neighbors8, 'slope_neighbors8.tif', format='GTiff', overwrite=TRUE)
plot(slope_neighbors4 - slope_neighbors8)
aspect_neighbors4 <- terrain(dem_cropped, opt = 'aspect', unit = 'degrees', neighbors = 4)
writeRaster(aspect_neighbors4, 'aspect_neighbors4.tif', format='GTiff', overwrite=TRUE) #options='TFW=YES' creates a tfw file also; useful for ArcGIS
aspect_neighbors8 <- terrain(dem_cropped, opt = 'aspect', unit = 'degrees', neighbors = 8)
writeRaster(aspect_neighbors8, 'aspect_neighbors8.tif', format='GTiff', overwrite=TRUE)
plot(aspect_neighbors4 - aspect_neighbors8)
tri <- terrain(dem_cropped, opt = 'TRI')
writeRaster(tri, 'terrain_ruggedness.tif', format='GTiff')
tpi <- terrain(dem_cropped, opt='TPI')
writeRaster(tpi, 'topographic_position.tif', format='GTiff')
roughness <- terrain(dem_cropped, opt = 'roughness')
writeRaster(roughness, 'roughness.tif', format='GTiff')

#old code based on investigating whether or not a larger catchment (ie. larger area of interest) would affect sampling of terrain characteristics.  ultimately, sensor locations were only in dem_1 and not in overlapping area between dem_1 and dem_2
#inspect origins. they need to be the same to perform a mosaic
origin(dem_1)
origin(dem_2)
#origin(dem_2) <- origin(dem_1)  #is this an appropriate solution?
dem_combined <- mosaic(dem_1, dem_2, fun=mean, tolerance=0.06) #the default tolerance is 0.1 (see RasterOptions()) but fails unless tolerance is specified within mosaic function as >= 0.06
#write the raster to a file
writeRaster(dem_combined, 'dem10m_combined.tif', overwrite=TRUE)

#read in combined dem raster
dem_combined <- raster('dem10m_combined.tif')
plot(dem_combined)
#read in plot shapefiles
study_plots <- 'C:/Users/smdevine/Desktop/rangeland project/study_plots'
setwd(study_plots)
list.files(study_plots)
tenha_plot <- shapefile('study_area.shp')
plot(tenha_plot, add=T)
fourtyha_plot <- shapefile('study_area40ha.shp')
plot(fourtyha_plot, add=T)

#calc slope raster
setwd(terrainDir)
slope_all <- terrain(dem_combined, opt = 'slope', unit = 'degrees')
writeRaster(slope_all, 'slope.tif')
plot(slope_all)
slope_10ha <- mask(slope_all, tenha_plot)
slope_40ha <- mask(slope_all, fourtyha_plot)
slope <- stack(slope_all, slope_40ha, slope_10ha)
boxplot(slope, names=c('34,756 ha DEM', '40 ha plot', '10 ha plot'), main= 'Terrain sampling bias analysis', ylab='slope (deg)')

aspect_all <- terrain(dem_combined, opt='aspect', unit = 'degrees')
writeRaster(aspect_all, 'aspect.tif')
aspect_10ha <- mask(aspect_all, tenha_plot)
aspect_40ha <- mask(aspect_all, fourtyha_plot)
aspect <- stack(aspect_all, aspect_40ha, aspect_10ha)
boxplot(aspect, names=c('34,756 ha DEM', '40 ha plot', '10 ha plot'), main= 'Terrain sampling bias analysis', ylab='aspect (deg)')

TPI_all <- terrain(dem_combined, opt = 'TPI') #this is the topographic position index, 
TPI_10ha <- mask(TPI_all, tenha_plot)
TPI_40ha <- mask(TPI_all, fourtyha_plot)
TPI <- stack(TPI_all, TPI_40ha, TPI_10ha)
boxplot(TPI, names=c('34,756 ha DEM', '40 ha plot', '10 ha plot'), main= 'Terrain sampling bias analysis', ylab='topographic position index (TPI)')

TRI_all <- terrain(dem_combined, opt = 'TRI')
TRI_10ha <- mask(TRI_all, tenha_plot)
TRI_40ha <- mask(TRI_all, fourtyha_plot)
TRI <- stack(TRI_all, TRI_40ha, TRI_10ha)
boxplot(TPI, names=c('34,756 ha DEM', '40 ha plot', '10 ha plot'), main= 'Terrain sampling bias analysis', ylab='topographic roughness index (TRI)')

terrain_calcs <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
setwd(terrain_calcs)
list.files(terrain_calcs)

boxplot_rasters <- function(wd, raster_name, var_name) {
  setwd('C:/Users/smdevine/Desktop/rangeland project/study_plots')
  tenha_plot <- shapefile('study_area.shp')
  fourtyha_plot <- shapefile('study_area40ha.shp')
  twoKha_plot <- shapefile('study_area2000ha.shp')
  setwd(wd)
  ras_all <- raster(raster_name)
  ras_10ha <- mask(ras_all, tenha_plot)
  ras_40ha <- mask(ras_all, fourtyha_plot)
  ras_2000ha <- mask(ras_all, twoKha_plot)
  ras_stack <- stack(ras_all, ras_2000ha, ras_40ha, ras_10ha)
  boxplot(ras_stack, names=c('34,756 ha DEM', '2000 ha plot', '40 ha plot', '10 ha plot'), main= 'Terrain sampling bias analysis', ylab=var_name, boxwex=0.5)
}
#hist function for cti
breaks <- c(0,2,4,6,8,10,12,14)
hist_rasters <- function(wd, raster_name, var_name, breaks) {
  setwd('C:/Users/smdevine/Desktop/rangeland project/study_plots')
  tenha_plot <- shapefile('study_area.shp')
  fourtyha_plot <- shapefile('study_area40ha.shp')
  twoKha_plot <- shapefile('study_area2000ha.shp')
  setwd('C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r')
  ras_all <- raster('cti.tif')
  ras_10ha <- mask(ras_all, tenha_plot)
  ras_40ha <- mask(ras_all, fourtyha_plot)
  ras_2000ha <- mask(ras_all, twoKha_plot)
  hist(ras_all[ras_all<14], breaks=breaks, col='green', xlab=var_name, main='34,756 ha 10m DEM in San Joaquin County, CA')
  hist(ras_2000ha[ras_2000ha<14], breaks=breaks, col='yellow', xlab=var_name, main='2000 ha plot in DEM')
  hist(ras_40ha, breaks=breaks, col='red', xlab=var_name, main='40 ha plot in DEM')
  hist(ras_10ha, breaks=breaks, col='blue', xlab=var_name, main='10 ha plot in DEM in San Joaquin County, CA')
}
hist('C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r', 'cti.tif', 'Compound Topographic Index', c(0,2,4,6,8,10,12,14))

#run code outside of function
wd <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
raster_name <- 'mean_curv.tif'
var_name <- 'Mean Curvature'
setwd('C:/Users/smdevine/Desktop/rangeland project/study_plots')
tenha_plot <- shapefile('study_area.shp')
fourtyha_plot <- shapefile('study_area40ha.shp')
twoKha_plot <- shapefile('study_area2000ha.shp')
setwd(wd)
ras_all <- raster(raster_name)
ras_all <- ras_all/1000 #divided by 1000 to put into units of m^-1 because arcgis jacks the output units
ras_10ha <- mask(ras_all, tenha_plot)
ras_40ha <- mask(ras_all, fourtyha_plot)
ras_2000ha <- mask(ras_all, twoKha_plot)
ras_stack <- stack(ras_all, ras_2000ha, ras_40ha, ras_10ha)
breaks <- c(0,2,4,6,8,10,12,14)
hist(ras_all[ras_all<1 & ras_all>-1], col='green', xlab=var_name, main='34,756 ha 10m DEM in San Joaquin County, CA')
hist(ras_2000ha, col='yellow', xlab=var_name, main='2000 ha plot in DEM')
hist(ras_40ha, col='red', xlab=var_name, main='40 ha plot in DEM')
hist(ras_10ha, col='blue', xlab=var_name, main='10 ha plot in DEM in San Joaquin County, CA')
summary(ras_all)
cellcount <- ncell(ras_all)
cellcount_2 <- ncell(ras_all[ras_all<0.02 & ras_all>-0.02])
cellcount_2/cellcount
summary(ras_2000ha)
summary(ras_40ha)
summary(ras_10ha)
