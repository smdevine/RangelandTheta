rasterDir <- 'C:/Users/smdevine/Desktop/rangeland project/sampling strategy April 2018'
BiomassRaster <- raster(file.path(rasterDir, 'Biomass_2017-04-10_APAR.tif'))
samplingClassRaster <- raster(file.path(rasterDir, 'ISO_5Class5Inter_Classification.tif'))
cellNumberRaster <- samplingClassRaster
values(cellNumberRaster) <- 1:ncell(cellNumberRaster)
df <- data.frame(class = values(samplingClassRaster), cellNumber = values(cellNumberRaster))
dim(df)
df$class[df$class==0] <- NA
class_summary <- as.data.frame(table(df$class))
class_summary$proportion <- class_summary$Freq/sum(class_summary$Freq)
class_summary$sample_counts <- round(105*class_summary$proportion, 0)
class1_sampling <- sample(df$cellNumber[which(df$class==1)], class_summary$sample_counts[1])
class2_sampling <- sample(df$cellNumber[which(df$class==2)], class_summary$sample_counts[2])
class3_sampling <- sample(df$cellNumber[which(df$class==3)], class_summary$sample_counts[3])
class4_sampling <- sample(df$cellNumber[which(df$class==4)], class_summary$sample_counts[4])
class5_sampling <- sample(df$cellNumber[which(df$class==5)], class_summary$sample_counts[5])
sampling_cellnumbers <- c(class1_sampling, class2_sampling, class3_sampling, class4_sampling, class5_sampling)
sampling_df <- df[match(sampling_cellnumbers, df$cellNumber),]
samplingClassRaster[sampling_df$cellNumber]
coordinates(cellNumberRaster)[sampling_df$cellNumber, 1]
sampling_df$longitude_UTM10 <- coordinates(cellNumberRaster)[sampling_df$cellNumber, 1]
sampling_df$latitude_UTM10 <- coordinates(cellNumberRaster)[sampling_df$cellNumber, 2]
sampling_sp_df <- SpatialPointsDataFrame(coords = sampling_df[ ,c('longitude_UTM10', 'latitude_UTM10')], data = sampling_df)
plot(sampling_sp_df)
shapefile(sampling_sp_df, file.path(rasterDir, 'sampling_pts_150_byClass.shp'))

#approximate sampling area is 5.46 ha
sqrt(5.46*10000/120)
5.46*10000/(22^2)

#sampling points by grid (originally 116 but deleted points within 4 m of road)
sampling_plan_shp <- shapefile(file.path(rasterDir, 'sampling_point_plan.shp'))
sampling_plan_shp$Id <- 1:nrow(sampling_plan_shp)
sampling_plan_df <- as.data.frame(sampling_plan_shp)
colnames(sampling_plan_df) <- c('ID', 'x', 'y')
write.csv(sampling_plan_df, file.path(rasterDir, 'sampling_plan_105points.csv'), row.names = FALSE)
sampling_plan_shp$class <- extract(samplingClassRaster, sampling_plan_shp)
table(sampling_plan_shp$class)
sampling_plan_shp$biomest_4_17 <- extract(BiomassRaster, sampling_plan_shp)
shapefile(sampling_plan_shp, file.path(rasterDir, 'sampling_point_plan_final.shp'))
