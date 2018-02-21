library(raster)
#DEM from Sean Hogan
setwd('C:/Users/smdevine/Desktop/rangeland project/DEM_S_Hogan/Camatta_Mar_2017/Rasters')
list.files()
dem <- stack("RGB_DEM_SlpPc_Asp_Hill.tif")
crs(dem) <- '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
names(dem) <- c('Red', 'Green', 'Blue', 'Elevation', 'Slope', 'Aspect', 'Hillshade')
plot(dem$Elevation)
elev <- dem$Elevation
writeRaster(dem$Elevation, 'DEM_1.9cm.tif', format='GTiff', options='TFW=yes')
aggregate(x=dem$Elevation, fact=5, fun=mean, filename='DEM_9.4cm.tif', format='GTiff', options='TFW=yes')
elev_9.4cm <- raster("DEM_9.4cm.tif")
plot(elev_9.4cm)

#DEM from Grace, Nov 2017
dem_Grace <- 'C:/Users/smdevine/Desktop/rangeland project/DEMs_10cm'
setwd(dem_Grace)
list.files(pattern = glob2rx('*.tif'))
elev_10cm <- raster("camatta_11112016_dsm.tif")
resample(x=elev_9.4cm, y=elev_10cm, method='bilinear', 'camatta_Nov2016_10cm_resample.tif', format='GTiff', options='TFW=yes', overwrite=TRUE)
elev_10cm_resample <- raster('camatta_Nov2016_10cm_resample.tif')
diff_10cm_dems <- elev_10cm - elev_10cm_resample
summary(diff_10cm_dems)
plot(diff_10cm_dems)
writeRaster(diff_10cm_dems, 'Nov2016dem_minus_HoganDEM.tif', format='GTiff', options='TFW=yes')

