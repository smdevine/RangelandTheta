#TO-DO correct for bag weights, 
#double-check entered data [DONE]
#working up of bulk density data
vol_0_10cm <- 179.450914
vol_10_30cm <- 2*vol_0_10cm
BD_data <- read.csv(file.path('C:/Users/smdevine/Desktop/RangelandProject/soils_data', 'soils_datasheet.7.23.18.csv'), stringsAsFactors = FALSE)
summary(BD_data$frags_g[BD_data$frags_g > 0 & !is.na(BD_data$frags_mL)] / BD_data$frags_mL[BD_data$frags_g > 0 & !is.na(BD_data$frags_mL)])
avgBD_frags <- mean(BD_data$frags_g[BD_data$frags_g > 0] / BD_data$frags_mL[BD_data$frags_g > 0], na.rm = TRUE)
BD_data$frags_mL[is.na(BD_data$frags_mL) & BD_data$frags_g > 0] <- BD_data$frags_g[is.na(BD_data$frags_mL) & BD_data$frags_g > 0] / avgBD_frags
BD_data$bag_weight <- ifelse(BD_data$bag_type==1, 4.8, ifelse(BD_data$bag_type==2, 3.9, ifelse(BD_data$bag_type==3, 6.2 , 6.4))) #bag type 4 was 6.4 g
BD_data$fines_g <- BD_data$dry_wt_g - BD_data$frags_g - BD_data$bag_weight #subtract out frag and bag mass
BD_data$fines_mL <- ifelse(BD_data$depth_code==1, vol_0_10cm, vol_10_30cm) - BD_data$frags_mL
#need to correct samples 2-1 and 3-1 because they were only 0-5 cm samples
BD_data$fines_mL[BD_data$sample_code=='2_1'] <- BD_data$fines_mL[BD_data$sample_code=='2_1'] - vol_0_10cm * 0.5
BD_data$fines_mL[BD_data$sample_code=='3_1'] <- BD_data$fines_mL[BD_data$sample_code=='3_1'] - vol_0_10cm * 0.5
BD_data$bulk_density_g_cm3 <- BD_data$fines_g / BD_data$fines_mL
summary(BD_data$bulk_density_g_cm3)
hist(BD_data$bulk_density_g_cm3)
BD_data$frags_vol_perc <- 100 * BD_data$frags_mL / ifelse(BD_data$depth_code==1, vol_0_10cm, vol_10_30cm)
BD_data$frags_vol_perc[BD_data$sample_code=='2_1'] <- 100 * BD_data$frags_mL[BD_data$sample_code=='2_1'] / (vol_0_10cm * 0.5)
BD_data$frags_vol_perc[BD_data$sample_code=='3_1'] <- 100 * BD_data$frags_mL[BD_data$sample_code=='3_1'] / (vol_0_10cm * 0.5)
hist(BD_data$frags_vol_perc)
summary(BD_data$frags_vol_perc)
BD_data$g_H2O <- BD_data$moist_wt_g - BD_data$dry_wt_g 
summary(BD_data$g_H2O)
hist(BD_data$g_H2O)
BD_data$H2O_gravimetric <- (BD_data$g_H2O + 0.025 * BD_data$fines_g) / (BD_data$dry_wt_g - BD_data$bag_weight) #assume 2.5% air-dry moisture for all samples now
BD_data$H2O_volumetric <- (BD_data$g_H2O + 0.025 * BD_data$fines_g) / BD_data$fines_mL
summary(BD_data$H2O_gravimetric)
summary(BD_data$H2O_volumetric)
hist(BD_data$H2O_volumetric)
BD_data_0_10cm <- BD_data[BD_data$depth_code==1,]
BD_data_10_30cm <- BD_data[BD_data$depth_code==2,]

#read in shapefile of points and merge with two dataframes above
library(raster)
sampling_pts <- shapefile(file.path('C:/Users/smdevine/Desktop/RangelandProject/sampling points 2018', 'soil_sampling_points.shp'))
biomass_Apr2017 <- raster('C:/Users/smdevine/Desktop/RangelandProject/sampling strategy April 2018/Biomass_2017-04-10_APAR.tif')
plot(biomass_Apr2017)
sampling_pts$point_no <- as.integer(gsub('point', '', sampling_pts$Comment))
sampling_pts$point_no
plot(sampling_pts, pch=16, col = 'brown')
text(sampling_pts, sampling_pts$point_no)
sampling_pts$Apr2017biomass <- extract(biomass_Apr2017, coordinates(sampling_pts)[,1:2], buffer=1, fun=mean) #this calculates a mean of all 30 x 30 cm cells within 1 m of soil sampling point
point_data_0_10cm <- merge(sampling_pts, BD_data_0_10cm, 'point_no')
plot(point_data_0_10cm, cex=point_data_0_10cm$Apr2017biomass/1000, pch=1)
plot(point_data_0_10cm, cex=1.5 * point_data_0_10cm$bulk_density_g_cm3, pch=1)
plot(point_data_0_10cm, cex=point_data_0_10cm$frags_vol_perc / 3, pch=1)
plot(point_data_0_10cm, cex=point_data_0_10cm$H2O_volumetric * 20, pch=1)
plot(point_data_0_10cm$bulk_density_g_cm3, point_data_0_10cm$Apr2017biomass)
plot(point_data_0_10cm$frags_vol_perc, point_data_0_10cm$Apr2017biomass)
plot(point_data_0_10cm$H2O_volumetric, point_data_0_10cm$Apr2017biomass) #should be done against 2018 biomass
summary(lm(Apr2017biomass ~ bulk_density_g_cm3, data = point_data_0_10cm))
summary(lm(Apr2017biomass ~ H2O_volumetric, data = point_data_0_10cm)) #should be done against 2018 biomass
summary(lm(Apr2017biomass ~ frags_vol_perc, data = point_data_0_10cm))

#now the 10-30 cm data
point_data_10_30cm <- merge(sampling_pts, BD_data_10_30cm, 'point_no')
#plot(point_data_10_30cm, cex=point_data_10_30cm$Apr2017biomass/1000, pch=1)
plot(point_data_10_30cm, cex=1.5 * point_data_10_30cm$bulk_density_g_cm3, pch=1)
plot(point_data_10_30cm, cex=point_data_10_30cm$frags_vol_perc / 3, pch=1)
plot(point_data_10_30cm, cex=point_data_10_30cm$H2O_volumetric * 20, pch=1)
plot(point_data_10_30cm$bulk_density_g_cm3, point_data_10_30cm$Apr2017biomass)
plot(point_data_10_30cm$frags_vol_perc, point_data_10_30cm$Apr2017biomass)
plot(point_data_10_30cm$H2O_volumetric, point_data_10_30cm$Apr2017biomass) #should be done against 2018 biomass
summary(lm(Apr2017biomass ~ bulk_density_g_cm3, data = point_data_10_30cm))
summary(lm(Apr2017biomass ~ frags_vol_perc, data = point_data_10_30cm))
summary(lm(Apr2017biomass ~ H2O_volumetric, data = point_data_10_30cm)) #should be done against 2018 biomass

#multiple linear regression
summary(lm(Apr2017biomass ~ bulk_density_g_cm3 + frags_vol_perc + H2O_volumetric, data = point_data_10_30cm))
summary(lm(Apr2017biomass ~ bulk_density_g_cm3 + frags_vol_perc + H2O_volumetric, data = point_data_0_10cm))

#write to shapefiles
fname <- file.path('C:/Users/smdevine/Desktop/RangelandProject/sampling points 2018', 'soil_data_0_10cm.shp')
fname <- gsub("\\\\", "/", fname)
shapefile(point_data_0_10cm, fname, overwrite=TRUE)

#read in sensor locations
sensor.locations <- shapefile(file.path('C:/Users/smdevine/Desktop/RangelandProject/soil.moisture.sensors', 'sensor_forage2017.shp'))
plot(sensor.locations, add=TRUE)
plot(sensor.locations, cex=sensor.locations$clp041017/1000, pch=1)

#see data at point 88 and 56
as.data.frame(point_data_0_10cm[point_data_0_10cm$sample_code=='88_1',])
100*(6.3+247*0.042)/179.45 #9.29% vol H2O
as.data.frame(point_data_0_10cm[point_data_0_10cm$sample_code=='56_1',])
100*(5.5+256.8*0.032)/179.45 #7.64% vol H2O
