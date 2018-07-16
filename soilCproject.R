#working up of bulk density data
vol_0_10cm <- 179.450914
vol_10_30cm <- 2*vol_0_10cm
BD_data <- read.csv(file.path('C:/Users/smdevine/Desktop/RangelandProject/soils_data', 'soils_datasheet.5.9.18.csv'), stringsAsFactors = FALSE)
avgBD <- mean(BD_data$frags_g / BD_data$frags_mL, na.rm = TRUE)
BD_data$frags_mL[is.na(BD_data$frags_mL) & BD_data$frags_g > 0] <- BD_data$frags_g[is.na(BD_data$frags_mL) & BD_data$frags_g > 0] / avgBD
BD_data$fines_g <- BD_data$dry_wt_g - BD_data$frags_g
BD_data$fines_mL <- ifelse(BD_data$depth_code==1, vol_0_10cm, vol_10_30cm) - BD_data$frags_mL
#need to correct samples 2-1 and 3-1 because they were only 0-5 cm samples
BD_data$fines_mL[BD_data$sample_code=='2_1'] <- BD_data$fines_mL[BD_data$sample_code=='2_1'] - vol_0_10cm * 0.5
BD_data$fines_mL[BD_data$sample_code=='3_1'] <- BD_data$fines_mL[BD_data$sample_code=='3_1'] - vol_0_10cm * 0.5
BD_data$bulk_density_g_cm3 <- BD_data$fines_g / BD_data$fines_mL
hist(BD_data$bulk_density_g_cm3)
BD_data$frags_vol_perc <- 100 * BD_data$frags_mL / ifelse(BD_data$depth_code==1, vol_0_10cm, vol_10_30cm)
BD_data$frags_vol_perc[BD_data$sample_code=='2_1'] <- 100 * BD_data$frags_mL[BD_data$sample_code=='2_1'] / (vol_0_10cm * 0.5)
BD_data$frags_vol_perc[BD_data$sample_code=='3_1'] <- 100 * BD_data$frags_mL[BD_data$sample_code=='3_1'] / (vol_0_10cm * 0.5)
hist(BD_data$frags_vol_perc)
summary(BD_data$frags_vol_perc)
BD_data_0_10cm <- BD_data[BD_data$depth_code==1,]
BD_data_10_30cm <- BD_data[BD_data$depth_code==2,]
#read in shapefile of points
library(raster)
sampling_pts <- shapefile(file.path('C:/Users/smdevine/Desktop/RangelandProject/sampling points 2018', 'soil_sampling_points.shp'))
biomass_Apr2017 <- raster('C:/Users/smdevine/Desktop/RangelandProject/sampling strategy April 2018/Biomass_2017-04-10_APAR.tif')
sampling_pts$point_no <- as.integer(gsub('point', '', sampling_pts$Comment))
sampling_pts$point_no
plot(sampling_pts, pch=16, col = 'brown')
text(sampling_pts, sasmpling_pts$point_no)
sampling_pts$Apr2017biomass <- extract(biomass_Apr2017, coordinates(sampling_pts)[,1:2], buffer=1, fun=mean) #this calculates a mean of all 30 x 30 cm cells within 1 m of soil sampling point
point_data_0_10cm <- merge(sampling_pts, BD_data_0_10cm, 'point_no')
plot(biomass_Apr2017)
plot(point_data_0_10cm, cex=point_data_0_10cm$Apr2017biomass/1000, pch=1, add=T)
plot(point_data_0_10cm, cex=0.5 * point_data_0_10cm$bulk_density_g_cm3, pch=1, add=T)
plot(point_data_0_10cm$bulk_density_g_cm3, point_data_0_10cm$Apr2017biomass)
point_data_10_30cm <- merge(sampling_pts, BD_data_10_30cm, 'point_no')
plot(point_data_10_30cm$bulk_density_g_cm3, point_data_10_30cm$Apr2017biomass)
summary(lm(Apr2017biomass ~ bulk_density_g_cm3, data = point_data_0_10cm))
summary(lm(Apr2017biomass ~ bulk_density_g_cm3, data = point_data_10_30cm))
plot(point_data_0_10cm$frags_vol_perc, point_data_0_10cm$Apr2017biomass)
plot(point_data_10_30cm$frags_vol_perc, point_data_10_30cm$Apr2017biomass)
