library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SMnorm_T_model_results'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
forage_terrain_energy <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, 'orange2', 'red3'))
forage_terrain_energy$May2017growth <- forage_terrain_energy$clp050117 - forage_terrain_energy$clp041017
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
forage_terrain_energy$Mar2017growth <- forage_terrain_energy$clp031417 - forage_terrain_energy$clp021517
forage_terrain_energy$Mar2018growth <- forage_terrain_energy$clp032218 - forage_terrain_energy$clp021518
forage_terrain_energy$Apr2018growth <- forage_terrain_energy$clp041518 - forage_terrain_energy$clp032218
forage_terrain_energy$peak2018growth <- forage_terrain_energy$clp041518 - forage_terrain_energy$clp021518
# soilT_data_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
# soilT_data_7cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
# soilT_data_7cm_2018_max <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MaxT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
# soilT_data_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
# soilT_data_22cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
min_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {min(x, na.rm = TRUE)}
}
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}
normalize_VWCdata <- function(df) {
  df[ ,2:ncol(df)] <- (df[ ,2:ncol(df)] - rowMeans(df[ ,2:ncol(df)], na.rm = TRUE)) / apply(df[ ,2:ncol(df)], 1, sd, na.rm=TRUE)
  df
}

#standardize VWC data by field capacity and wilting point
#yr <- 2018
depth <- 22
#vwc_data<- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
vwc_data<- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017_2018', 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
#check location 3
plot(seq(from=1, by=1, length.out = ncol(vwc_data) -1), vwc_data[3,2:ncol(vwc_data)])
plot(seq(from=1, by=1, length.out = 150), vwc_data[3,2:151])
depth <- 7
soilT_data<- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017_2018', 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
if (depth == 7) {
  soilT_data_7cm_2018 <- soilT_data[,which(colnames(soilT_data)=='Nov_30_2017'):which(colnames(soilT_data)=='Jun_30_2018')]
  soilT_data_7cm_2017 <- soilT_data[,2:which(colnames(soilT_data)=='Jun_30_2017')]
} else {
  soilT_data_22cm_2017 <- soilT_data[,2:which(colnames(soilT_data)=='Jun_30_2017')]
  soilT_data_22cm_2018 <- soilT_data[ ,which(colnames(soilT_data)=='Nov_30_2017'):which(colnames(soilT_data)=='Jun_30_2018')]
}
gap_fill_soilTv3 <- function(df, location_fix, location_use, location_use2, qc) {
  lm.result <- lm(as.numeric(df[location_fix, 2:ncol(df)]) ~ as.numeric(df[location_use, 2:ncol(df)]) + as.numeric(df[location_use2, 2:ncol(df)]))
  if (summary(lm.result)$r.squared < qc) {
    stop(print('Regression results are unacceptable.'))
  }
  fix_indices <- which(is.na(df[location_fix, ]))
  #print(fix_indices)
  df[location_fix, fix_indices] <- lm.result$coefficients[1] + lm.result$coefficients[2] * df[location_use, fix_indices] + lm.result$coefficients[3] * df[location_use2, fix_indices]
  df
}
# soilT_data_7cm_2017 <- gap_fill_soilTv3(soilT_data_7cm_2017, 13, 4, 14, 0.99)
# soilT_data_22cm_2017 <- gap_fill_soilTv3(soilT_data_22cm_2017, 13, 4, 14, 0.99)
#no longer needed because location 3 now included at 22 cm
# if (depth == 22) {
#   vwc_data[16,] <- c(3,rep(NA, (ncol(vwc_data)-1)))
#   vwc_data <- vwc_data[order(vwc_data$location), ]
# }
# if (depth == 22) {
#   soilT_data[16,] <- c(3,rep(NA, (ncol(soilT_data)-1)))
#   soilT_data <- soilT_data[order(soilT_data$location), ]
# }
#standardize by "field capacity"
#22 cm data to get location 3 for 2017
#vwc_22cm.2017_by.sensor <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor/daily_by_sensor_2017_processed2018-09-27.csv'), stringsAsFactors = FALSE)
#vwc_22cm_loc3_WP <- mean(vwc_22cm.2017_by.sensor$MeanVWC[vwc_22cm.2017_by.sensor$Date==20170630 & vwc_22cm.2017_by.sensor$Location==3]) #because data was suspect and removed from 2018
#vwc_22cm.2017_by.sensor <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor/daily_by_sensor_2018_processed2018-09-27.csv'), stringsAsFactors = FALSE)
#vwc_22cm_loc3_WP <- mean(vwc_22cm.2017_by.sensor$MeanVWC[vwc_22cm.2017_by.sensor$Date==20171129 & vwc_22cm.2017_by.sensor$Location==3])
apply(vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Dec_31_2017')], 1, mean, na.rm=TRUE)/ vwc_data$Jul_20_2017
FC.by.location <- if (depth==7) {rowMeans(cbind(vwc_data$Jan_27_2017, vwc_data$Jan_17_2017))} else{rowMeans(cbind(vwc_data$Jan_29_2017, vwc_data$Jan_18_2017))} #Jan 16 is 3 days after no rainfall; Jan 27 is 4 days after no rainfall for 22 cm; these dates were chosen as times when flux reached 0.003 cm / day on average
#wp.date <- 'Jun_15_2017' #last analysis done with Jun_15_2017
wilting.point.by.location <- if (depth == 7) {
  2 * apply(vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Dec_20_2017')], 1, mean) #used Jun_20_2017 for latest version of figures
  #vwc_data[[wp.date]]
} else {
    1.35 * apply(vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Dec_31_2017')], 1, mean, na.rm=TRUE)
}
paw.by.location <- FC.by.location - wilting.point.by.location
SM_chars <- data.frame(location=1:16, FC=FC.by.location, WP=wilting.point.by.location, PAW=paw.by.location)
write.csv(SM_chars, file.path(results, 'tables', paste0('SM_characteristics_', depth, 'cm.11.2.18.csv')), row.names = FALSE)
mean(paw.by.location, na.rm = TRUE)
mean(FC.by.location, na.rm = TRUE)
mean(wilting.point.by.location, na.rm = TRUE)
plot(wilting.point.by.location, FC.by.location)
abline(lm(FC.by.location ~ wilting.point.by.location))
text(wilting.point.by.location, FC.by.location, label=forage_terrain_energy$location, pos = 1, offset = 0.1)
summary(lm(wilting.point.by.location ~ FC.by.location))
summary(lm(FC.by.location ~ forage_terrain_energy$annual_kwh.m2))
summary(lm(FC.by.location ~ if (depth==7) {rowMeans(cbind(soilT_data$Jan_27_2017, soilT_data$Jan_17_2017))} else{rowMeans(cbind(soilT_data$Jan_29_2017, soilT_data$Jan_18_2017))})) #NS
plot(if (depth==7) {rowMeans(cbind(soilT_data$Jan_27_2017, soilT_data$Jan_17_2017))} else{rowMeans(cbind(soilT_data$Jan_29_2017, soilT_data$Jan_18_2017))}, FC.by.location)
text(if (depth==7) {rowMeans(cbind(soilT_data$Jan_27_2017, soilT_data$Jan_17_2017))} else{rowMeans(cbind(soilT_data$Jan_29_2017, soilT_data$Jan_18_2017))}, FC.by.location, pos = 1, offset = 0.1)
#deltaT_Jan27 <- min(soilT_data$Jan_27_2017) - soilT_data$Jan_27_2017
#deltaT_Jan27 * lm(FC.by.location ~ soilT_data$Jan_27_2017)$coefficients[2]
#FC.by.location_BC <- FC.by.location + deltaT_Jan27 * lm(FC.by.location ~ soilT_data$Jan_27_2017)$coefficients[2]
#summary(lm(FC.by.location_BC ~ soilT_data$Jan_27_2017))
#plot(forage_terrain_energy$annual_kwh.m2, FC.by.location_BC)
#abline(lm(FC.by.location_BC ~ forage_terrain_energy$annual_kwh.m2))
#text(forage_terrain_energy$annual_kwh.m2, FC.by.location_BC, pos = 1, offset = 0.5)
summary(lm(wilting.point.by.location ~ forage_terrain_energy$annual_kwh.m2))
plot(forage_terrain_energy$annual_kwh.m2, wilting.point.by.location)
abline(lm(wilting.point.by.location ~ forage_terrain_energy$annual_kwh.m2))
text(forage_terrain_energy$annual_kwh.m2, wilting.point.by.location, labels = forage_terrain_energy$location, offset = 0.5, pos=1)
#paw.by.location <- FC.by.location_BC - wilting.point.by.location
summary(lm(paw.by.location ~ forage_terrain_energy$annual_kwh.m2))
plot(forage_terrain_energy$annual_kwh.m2, paw.by.location)
text(forage_terrain_energy$annual_kwh.m2, paw.by.location, labels = forage_terrain_energy$location, pos = 1, offset = 0.2)
abline(lm(paw.by.location ~ forage_terrain_energy$annual_kwh.m2))
if (depth == 7) {
  depletion_vwc_2017 <-  vwc_data[,2:which(colnames(vwc_data)=='Jun_30_2017')]
  depletion_vwc_2017 <- data.frame(location=1:16, lapply(depletion_vwc_2017[,2:ncol(depletion_vwc_2017)], function(x) { 1 - ((FC.by.location - x) / (FC.by.location - wilting.point.by.location))}))
  depletion_vwc_2018 <- vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Jun_30_2018')]
  depletion_vwc_2018 <- data.frame(location=1:16, lapply(depletion_vwc_2018[,2:ncol(depletion_vwc_2018)], function(x) { 1 - ((FC.by.location - x) / (FC.by.location - wilting.point.by.location))}))
} else {
  depletion_vwc_2017_22 <-  vwc_data[,2:which(colnames(vwc_data)=='Jun_30_2017')]
  depletion_vwc_2017_22 <- data.frame(location=1:16, lapply(depletion_vwc_2017_22[,2:ncol(depletion_vwc_2017_22)], function(x) { 1 - ((FC.by.location - x) / (FC.by.location - wilting.point.by.location))}))
  depletion_vwc_2018_22 <- vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Jun_30_2018')]
  depletion_vwc_2018_22 <- data.frame(location=1:16, lapply(depletion_vwc_2018_22[,2:ncol(depletion_vwc_2018_22)], function(x) { 1 - ((FC.by.location - x) / (FC.by.location - wilting.point.by.location))}))
}

#write em to disk
if (depth==7) {
  write.csv(depletion_vwc_2017, file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc', paste0('depletion_vwc_', depth, 'cm_2017.csv')), row.names=FALSE)
  write.csv(depletion_vwc_2018, file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc', paste0('depletion_vwc_', depth, 'cm_2018.csv')), row.names=FALSE)
} else {
  write.csv(depletion_vwc_2017_22, file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc', paste0('depletion_vwc_', depth, 'cm_2017.csv')), row.names=FALSE)
  write.csv(depletion_vwc_2018_22, file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc', paste0('depletion_vwc_', depth, 'cm_2018.csv')), row.names=FALSE)
}
# read em in
depletion_vwc_2017 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2018.csv'), stringsAsFactors=FALSE)
depletion_vwc_2017_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2018.csv'), stringsAsFactors=FALSE)

#make 2017 Mar 14-Apr 10 plot
Mar_growth_dates <- which(colnames(depletion_vwc_2017)=='Dec_01_2016'):which(colnames(depletion_vwc_2017)=='Mar_15_2017')
Mar_growth_dates_v2 <- which(colnames(depletion_vwc_2017)=='Feb_15_2017'):which(colnames(depletion_vwc_2017)=='Mar_14_2017')
Apr_growth_dates <- which(colnames(depletion_vwc_2017)=='Mar_15_2017'):which(colnames(depletion_vwc_2017)=='Apr_10_2017')
spring_growth_dates <- which(colnames(depletion_vwc_2017)=='Feb_28_2017'):which(colnames(depletion_vwc_2017)=='Apr_10_2017')
mean_depletion_Mar_growth <- apply(depletion_vwc_2017[,Mar_growth_dates], 1, mean)
mean_depletion_Mar_growth_v2 <- apply(depletion_vwc_2017[,Mar_growth_dates_v2], 1, mean)
mean_soilT_Mar_growth <- apply(soilT_data_7cm_2017[,Mar_growth_dates], 1, mean)
mean_soilT_Mar_growth_v2 <- apply(soilT_data_7cm_2017[,Mar_growth_dates_v2], 1, mean)
mean_depletion22_Mar_growth <- apply(depletion_vwc_2017_22[,Mar_growth_dates], 1, mean)
mean_depletion22_Mar_growth_v2 <- apply(depletion_vwc_2017_22[,Mar_growth_dates_v2], 1, mean)
mean_depletion_Apr_growth <- apply(depletion_vwc_2017[,Apr_growth_dates], 1, mean)
mean_soilT_Apr_growth <- apply(soilT_data_7cm_2017[,Apr_growth_dates], 1, mean)
mean_depletion_spring_growth <- apply(depletion_vwc_2017[,spring_growth_dates], 1, mean)
mean_depletion22_spring_growth <- apply(depletion_vwc_2017_22[,spring_growth_dates], 1, mean)
mean_depletion22_Apr_growth <- apply(depletion_vwc_2017_22[,Apr_growth_dates], 1, mean)
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2017 drawdown', 'Apr2017.growth.vs.PAW.final.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4, 0.5, 0.5))
plot(rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)), forage_terrain_energy$Apr2017growth, col=forage_terrain_energy$energy_colors, ylab=expression('Mar 15 - Apr 10, 2017 growth (kg '~~ha^-1*')'), xaxt='n', xlab='Mar 15 - Apr 10, 2017, mean 0-30 cm plant available water (%)', cex.axis=1, cex.lab=1, pch=19, mgp=c(2.5, 1, 0), cex=1.3) #  cex=forage_terrain_energy$clp031417/750
axis(side = 1, at=c(0.3, 0.4, 0.5, 0.6, 0.7), labels = c('30', '40', '50', '60', '70'), mgp=c(2.5, 1, 0))
#text(rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)), forage_terrain_energy$Apr2017growth, labels=round(forage_terrain_energy$elevation, 0), pos=1, offset = 0.5)
#abline(v=0.5, lty=2, col='grey')
abline(lm(forage_terrain_energy$Apr2017growth ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth))), lty=2)
#legend('topleft', legend=(c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), 'linear fit')), lty=c(NA, NA, NA, 2), pch = c(19,19,19,1), pt.cex = c(1.5, 1.5, 1.5, NA), col=c('blue', 'orange2', 'red3', 'black'), inset=0.01, cex = 1, y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
text(x=0.45, y=700, label='linear model results', cex=1, adj=c(0,0))
text(x=0.45, y=500, label=expression(paste(r^2, ' = 0.44, p.val = 0.006')), cex = 1, adj = c(0, 0))
text(x=0.45, y=300, label=expression(paste('RMSE = 594 kg ', ha^-1)), cex = 1, adj = c(0, 0))
dev.off()

summary(lm(forage_terrain_energy$Apr2017growth ~ mean_soilT_Apr_growth))
summary(lm(forage_terrain_energy$Apr2017growth ~ mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$Apr2017growth ~ mean_depletion_Apr_growth + mean_soilT_Apr_growth))
summary(lm(forage_terrain_energy$Apr2017growth ~ mean_depletion_Apr_growth * mean_soilT_Apr_growth))
summary(lm(forage_terrain_energy$Apr2017growth ~ mean_depletion22_Apr_growth))
summary(lm(forage_terrain_energy$Apr2017growth ~ mean_depletion_Apr_growth + mean_depletion22_Apr_growth))
summary(lm(forage_terrain_energy$Apr2017growth ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth))))
plot(lm(forage_terrain_energy$Apr2017growth ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)))$fitted.values, lm(forage_terrain_energy$Apr2017growth ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)))$residuals)
summary(lm(forage_terrain_energy$Apr2017growth ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)) + mean_soilT_Apr_growth))
plot(mean_depletion_Apr_growth / mean_depletion22_Apr_growth, forage_terrain_energy$Apr2017growth)
summary(lm(mean_depletion_Apr_growth ~ mean_soilT_Apr_growth))
#abline(lm(forage_terrain_energy$Apr2017growth[-8] ~ mean_depletion_Apr_growth[-8]), lty=2)
summary(lm(forage_terrain_energy$clp041017 ~ forage_terrain_energy$clp031417))
summary(lm(forage_terrain_energy$clp041017 ~ forage_terrain_energy$clp021517))
summary(lm(forage_terrain_energy$clp041518 ~ forage_terrain_energy$clp021518))
summary(lm(forage_terrain_energy$clp041017 ~ forage_terrain_energy$clp031417 + mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ forage_terrain_energy$clp021517 + mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ forage_terrain_energy$curvature_mean + mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ rowMeans(cbind(mean_depletion_spring_growth, mean_depletion22_spring_growth))))
summary(lm(forage_terrain_energy$clp041017 ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)) + forage_terrain_energy$clp031417))
summary(lm(forage_terrain_energy$clp041017 ~ rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)) + forage_terrain_energy$clp021517))
summary(lm(forage_terrain_energy$clp041017 ~ rowMeans(cbind(mean_depletion_spring_growth, mean_depletion22_spring_growth)) + forage_terrain_energy$clp021517))
summary(lm(forage_terrain_energy$clp041017 ~ rowMeans(cbind(mean_depletion_spring_growth, mean_depletion22_spring_growth)) + forage_terrain_energy$clp021517 + FC.by.location.7))
summary(lm(rowMeans(cbind(mean_depletion_spring_growth, mean_depletion22_spring_growth)) ~ forage_terrain_energy$elevation))
summary(lm(rowMeans(cbind(mean_depletion_spring_growth, mean_depletion22_spring_growth)) ~ wilting.point.by.location))
plot(mean_depletion_Mar_growth)
plot(mean_soilT_Mar_growth, mean_depletion_Mar_growth)
plot(depletion_vwc_2017$Mar_15_2017, forage_terrain_energy$Apr2017growth, col=forage_terrain_energy$energy_colors)
plot(lm(forage_terrain_energy$Apr2017growth ~ mean_depletion_Apr_growth))
plot(mean_depletion_Apr_growth, forage_terrain_energy$clp041017)
abline(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ mean_soilT_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth))
summary(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth * mean_soilT_Mar_growth))
summary(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth + I(mean_soilT_Mar_growth^2)))
summary(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth + forage_terrain_energy$elevation))
lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth)$residuals
plot(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth)) #location 13 is missing VWC data; location 14 and 16 are nearly outliers
plot(forage_terrain_energy$clp041017[-13], lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth)$residuals)
plot(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth)$fitted.values, lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth)$residuals)
summary(lm(forage_terrain_energy$clp041017[-c(14,16)] ~ mean_depletion_Apr_growth[-c(14,16)] + mean_soilT_Mar_growth[-c(14,16)]))
plot(forage_terrain_energy$clp041017[-c(13:14,16)], lm(forage_terrain_energy$clp041017[-c(14,16)] ~ mean_depletion_Apr_growth[-c(14,16)] + mean_soilT_Mar_growth[-c(14,16)])$residuals)
plot(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth + mean_soilT_Mar_growth)$fitted.values, forage_terrain_energy$Apr2017growth[-13])


summary(lm(forage_terrain_energy$clp031417 ~ mean_soilT_Mar_growth))

plot(lm(forage_terrain_energy$clp041017 ~ mean_depletion_Apr_growth))
summary(lm(forage_terrain_energy$clp041017 ~ mean_depletion22_Apr_growth))

summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth_v2))
summary(lm(forage_terrain_energy$Mar2017growth ~ rowMeans(cbind(mean_depletion_Mar_growth, mean_depletion22_Mar_growth))))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth + mean_soilT_Mar_growth))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_soilT_Mar_growth_v2))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_soilT_Mar_growth))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth + mean_soilT_Mar_growth + forage_terrain_energy$curvature_mean))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth + mean_soilT_Mar_growth + forage_terrain_energy$curvature_mean))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth + mean_soilT_Mar_growth + forage_terrain_energy$curvature_mean))
summary(lm(forage_terrain_energy$Mar2017growth ~  mean_soilT_Mar_growth + forage_terrain_energy$curvature_mean + forage_terrain_energy$slope))
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_depletion_Mar_growth + mean_soilT_Mar_growth + forage_terrain_energy$elevation))

#Mar 2017 growth fig
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2017 drawdown', 'Mar2017.growth.vs.soilT.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4, 0.5, 0.5))
plot(mean_soilT_Mar_growth_v2, forage_terrain_energy$Mar2017growth, col=forage_terrain_energy$energy_colors, ylab=expression('Feb 15 - Mar 14, 2017 growth (kg '~~ha^-1*')'), xlab=expression('Feb 15 - Mar 14, 2017, mean 0-30 cm soil temperature ('~degree*'C)'), ylim=c(200, 1500), cex.axis=1, cex.lab=1, pch=19, cex=1.3, mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
#axis(side = 1, at=c(0.3, 0.4, 0.5, 0.6, 0.7), labels = c('30', '40', '50', '60', '70'), mgp=c(2.5, 1, 0))
#text(rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)), forage_terrain_energy$Apr2017growth, labels=round(forage_terrain_energy$elevation, 0), pos=1, offset = 0.5)
#abline(v=0.5, lty=2, col='grey')
abline(lm(forage_terrain_energy$Mar2017growth ~ mean_soilT_Mar_growth_v2), lty=2)
legend('topleft', legend=(c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), 'linear fit')), lty=c(NA, NA, NA, 2), pch = c(19,19,19,1), pt.cex = c(1.3, 1.3, 1.3, NA), col=c('blue', 'orange2', 'red3', 'black'), inset=0.01, cex = 1, y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
text(x=11.3, y=675, label='linear model results', cex=1, adj=c(0,0))
text(x=10.1, y=575, label=expression(r^2~'= 0.48, p.val = 0.004, RMSE = 237 kg'~ha^-1), cex = 1, adj = c(0, 0))
#text(x=8, y=1500, label=expression(paste('RMSE = 232 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=10.1, y=475, label=expression('slope = 6.6 \u00B1 1.9  kg forage'~ha^-1~degree*C^-1~day^-1), adj = c(0, 0), cex = 1)
dev.off()
summary(lm(forage_terrain_energy$Mar2017growth ~ mean_soilT_Mar_growth_v2))
#now 2018 growth
winter_growth_dates <- which(colnames(depletion_vwc_2018)=='Jan_10_2018'):which(colnames(depletion_vwc_2018)=='Feb_01_2018')
late_winter_growth_dates <- which(colnames(depletion_vwc_2018)=='Feb_15_2018'):which(colnames(depletion_vwc_2018)=='Mar_22_2018')
Feb2018_growth_dates <- which(colnames(depletion_vwc_2018)=='Feb_01_2018'):which(colnames(depletion_vwc_2018)=='Feb_28_2018')
Mar2018_growth_dates <- which(colnames(depletion_vwc_2018)=='Feb_16_2018'):which(colnames(depletion_vwc_2018)=='Mar_22_2018')
Apr2018_growth_dates <- which(colnames(depletion_vwc_2018)=='Mar_22_2018'):which(colnames(depletion_vwc_2018)=='Apr_10_2018')
mean_depletion_winter_growth <- apply(depletion_vwc_2018[,winter_growth_dates], 1, mean)
mean_depletion22_winter_growth <- apply(depletion_vwc_2018_22[,winter_growth_dates], 1, mean)
mean_depletion_latewinter_growth <- apply(depletion_vwc_2018[,late_winter_growth_dates], 1, mean)
mean_depletion22_latewinter_growth <- apply(depletion_vwc_2018_22[,late_winter_growth_dates], 1, mean)
mean_depletion_mar2018_growth <- apply(depletion_vwc_2018[,Mar2018_growth_dates], 1, mean)
mean_depletion22_mar2018_growth <- apply(depletion_vwc_2018_22[,Mar2018_growth_dates], 1, mean)
mean_depletion_feb2018_growth <- apply(depletion_vwc_2018[,Feb2018_growth_dates], 1, mean)
mean_depletion22_feb2018_growth <- apply(depletion_vwc_2018_22[,Feb2018_growth_dates], 1, mean)
mean_depletion_apr2018_growth <- apply(depletion_vwc_2018[,Apr2018_growth_dates], 1, mean)
mean_depletion22_apr2018_growth <- apply(depletion_vwc_2018_22[,Apr2018_growth_dates], 1, mean)
mean_soilT_apr2018_growth <- apply(soilT_data_7cm_2018[,Apr2018_growth_dates], 1, mean)
mean_soilT_mar2018_growth <- apply(soilT_data_7cm_2018[,Mar2018_growth_dates], 1, mean)
mean_soilT_feb2018_growth <- apply(soilT_data_7cm_2018[,Feb2018_growth_dates], 1, mean)
mean_soilT_winter_growth <- apply(soilT_data_7cm_2018[,winter_growth_dates], 1, mean)
mean_soilT_latewinter_growth <- apply(soilT_data_7cm_2018[,late_winter_growth_dates], 1, mean)
plot(mean_depletion_winter_growth, forage_terrain_energy$clp021518)

summary(lm(forage_terrain_energy$clp021518 ~ mean_depletion_winter_growth))
summary(lm(forage_terrain_energy$clp021518 ~ mean_depletion22_winter_growth))
summary(lm(forage_terrain_energy$clp021518 ~ mean_soilT_winter_growth))
summary(lm(forage_terrain_energy$clp021518 ~ forage_terrain_energy$curvature_mean))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_winter_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion22_winter_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_soilT_winter_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_winter_growth + mean_soilT_winter_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion22_winter_growth + mean_soilT_winter_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion22_winter_growth + mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion22_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion22_mar2018_growth+ mean_soilT_mar2018_growth + mean_depletion22_winter_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion22_mar2018_growth+ mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_mar2018_growth+ mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_mar2018_growth * mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ rowMeans(cbind(mean_depletion_mar2018_growth, mean_depletion22_mar2018_growth)) + mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_mar2018_growth + mean_depletion22_mar2018_growth + mean_soilT_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth + mean_soilT_feb2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth * mean_soilT_feb2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth * mean_soilT_feb2018_growth + mean_depletion22_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth * mean_soilT_feb2018_growth + mean_depletion_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth + mean_soilT_feb2018_growth + mean_depletion_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_feb2018_growth + mean_soilT_feb2018_growth + mean_depletion22_mar2018_growth))
summary(lm(forage_terrain_energy$Mar2018growth ~ mean_depletion_latewinter_growth * mean_soilT_latewinter_growth))
depletion_soilT_feb2018 <- mean_depletion_feb2018_growth * mean_soilT_feb2018_growth
summary(lm(forage_terrain_energy$Mar2018growth ~ depletion_soilT_feb2018))
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2017 drawdown', 'Mar2017.growth.vs.soilT.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4, 0.5, 0.5))
plot(mean_soilT_mar2018_growth, mean_depletion22_mar2018_growth, pch=19, cex=pmax(forage_terrain_energy$Mar2018growth/168, 0.8), col=forage_terrain_energy$energy_colors, ylab=('Feb 15 - Mar 22, 2018 mean % PAW 15-30 cm'), xlab=expression('Mar 2 - Mar 22, 2018 mean 7 cm soil temperature ('~degree*'C)'), cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
#axis(side = 1, at=c(0.3, 0.4, 0.5, 0.6, 0.7), labels = c('30', '40', '50', '60', '70'), mgp=c(2.5, 1, 0))
#text(rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)), forage_terrain_energy$Apr2017growth, labels=round(forage_terrain_energy$elevation, 0), pos=1, offset = 0.5)
#abline(v=0.5, lty=2, col='grey')
#abline(lm(forage_terrain_energy$Mar2017growth ~ mean_soilT_Mar_growth), lty=2)
legend('bottomleft', legend=(c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), 'linear fit')), lty=c(NA, NA, NA, 2), pch = c(19,19,19,1), pt.cex = c(1.5, 1.5, 1.5, NA), col=c('blue', 'orange2', 'red3', 'black'), inset=0.01, cex = 1, y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
text(x=9.5, y=700, label='linear model results', cex=1, adj=c(0,0))
text(x=8.6, y=600, label=expression(r^2~'= 0.50, p.val = 0.003, RMSE = 232 kg'~~ha^-1), cex = 1, adj = c(0, 0))
#text(x=8, y=1500, label=expression(paste('RMSE = 232 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=8.6, y=500, label=expression('slope = 7.6 \u00B1 2.1  kg forage'~ha^-1~degree*C^-1~day^-1), adj = c(0, 0), cex = 1)
dev.off()
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_winter_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_winter_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion22_apr2018_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion22_apr2018_growth+ mean_soilT_apr2018_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_apr2018_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion22_apr2018_growth* mean_soilT_apr2018_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_feb2018_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_feb2018_growth* mean_soilT_feb2018_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_apr2018_growth* mean_soilT_apr2018_growth + mean_depletion22_Apr_growth))
summary(lm(forage_terrain_energy$Apr2018growth ~ mean_depletion_feb2018_growth* mean_soilT_feb2018_growth+mean_depletion22_apr2018_growth))

summary(lm(mean_depletion_winter_growth ~ mean_soilT_winter_growth))
summary(lm(mean_depletion22_winter_growth ~ mean_soilT_winter_growth))
#plot vwc data
for(i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(vwc_data)[2:ncol(vwc_data)], '%b_%d_%Y'), vwc_data[i,2:ncol(vwc_data)], type='l', col=i, ylim=c(0.06,0.42))
  } else {lines(as.Date(colnames(vwc_data)[2:ncol(vwc_data)], '%b_%d_%Y'), vwc_data[i,2:ncol(vwc_data)], col=i)}
}
vwc_data_normalized <- normalize_VWCdata(vwc_data)
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)

#maxVWC_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MaxVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
#maxVWC_by.location <- apply(maxVWC_2017[,2:ncol(maxVWC_2017)], 1, max_modified)
WF_porosity <- data.frame(location=1:16, lapply(vwc_data[,2:ncol(vwc_data)], function(x) x / maxVWC_by.location)) #used 2017 maxes for both 2017 and 2018; location 3 missing for 22 cm in 2018
write.csv(WF_porosity, file.path(results, 'processed_soil_moisture/Jul2018/WF_porosity', paste0('WF_porosity_', depth, 'cm_', yr, '.csv')), row.names=FALSE)
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(WF_porosity)[2:ncol(WF_porosity)], '%b_%d_%Y'), WF_porosity[i,2:ncol(WF_porosity)], type='l', col=i, ylim=c(0,1))
  } else {lines(as.Date(colnames(WF_porosity)[2:ncol(WF_porosity)], '%b_%d_%Y'), WF_porosity[i,2:ncol(WF_porosity)], col=i)}
}
summary(lm(forage_terrain_energy$peak2017 ~ apply(WF_porosity[,45:103], 1, mean) + apply(soilT_data[,45:103], 1, mean)))


#add energy colors
#definition based on aspect south vs. north aspect classes
forage_terrain_energy$aspect_class <- ifelse(forage_terrain_energy$aspect >= 45 & forage_terrain_energy$aspect < 135, 'east', ifelse(forage_terrain_energy$aspect >= 135 & forage_terrain_energy$aspect < 225, 'south', ifelse(forage_terrain_energy$aspect >= 225 & forage_terrain_energy$aspect < 315, 'west', 'north')))
forage_terrain_energy$aspect_class_sp <- ifelse(forage_terrain_energy$aspect >= 67.5 & forage_terrain_energy$aspect < 112.5, 'east', ifelse(forage_terrain_energy$aspect >= 112.5 & forage_terrain_energy$aspect < 157.5, 'southeast', ifelse(forage_terrain_energy$aspect >= 157.5 & forage_terrain_energy$aspect < 202.5, 'south', ifelse(forage_terrain_energy$aspect >= 202.5 & forage_terrain_energy$aspect < 247.5, 'southwest', ifelse(forage_terrain_energy$aspect >= 247.5 & forage_terrain_energy$aspect < 292.5, 'west', ifelse(forage_terrain_energy$aspect >= 292.5 & forage_terrain_energy$aspect < 337.5, 'northwest', ifelse(forage_terrain_energy$aspect >= 337.5 & forage_terrain_energy$aspect < 22.5, 'north', 'northeast')))))))
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, 'orange2', 'red3')) #tres colores #tres colores
forage_terrain_energy[order(forage_terrain_energy$annual_kwh.m2) ,c('location', 'annual_kwh.m2', 'aspect_class', 'aspect_class_sp', 'energy_colors')]
#old def
#forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > summary(forage_terrain_energy$annual_kwh.m2)[2] & forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[5], 'orange2', 'red3'))
######
#7cm VWC depletion
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '7 cm plots', paste0('WY2017.SM.7cm.WP.2xFD.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.6, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'fraction of plant available water, 0-15 cm', xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(-0.5, 0, 0.5, 1, 1.5))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/01'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
#abline(v=as.Date('2017/4/10'))
legend("bottom", legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.01, ncol = 3, lwd = 1.3)
text(x=as.Date('2016/12/07'), y= 1.2, label='field\ncapacity', cex=1)
text(x=as.Date('2017/2/07'), y= 0.7, label='easily available \nwater', cex=1)
text(x=as.Date('2016/12/07'), y= -0.1, label='wilting point', cex=1)
#text(x=as.Date('2016/12/15'), y=1.4, label='WY2017', cex = 1)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='lightblue', cex=0.5)
dev.off()

#7 cm soil T 2017
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '7 cm plots', paste0('WY2017.soilT.7cm.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_data_7cm_2017)[2:ncol(soilT_data_7cm_2017)], '%b_%d_%Y'), soilT_data_7cm_2017[i,2:ncol(soilT_data_7cm_2017)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('7 cm depth soil temperature ('~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(soilT_data_7cm_2017)[2:ncol(soilT_data_7cm_2017)], '%b_%d_%Y'), soilT_data_7cm_2017[i,2:ncol(soilT_data_7cm_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
#text(x=as.Date('2016/12/15'), y=30, label='WY2017', cex = 1.5)
#abline(h=30, lty=2)
#abline(h=10, lty=2)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.01, lwd = 1.3)
dev.off()

#now 2018 7 cm
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '7 cm plots', paste0('WY2018.SM.7cm.WP2xFD.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.6, 1.5), xaxt='n', xlab='', ylab = 'fraction of plant available water, 0-15 cm', xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], col=forage_terrain_energy$energy_colors[i])}
}
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
#abline(v=as.Date('2018/4/15'))
#legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, ncol = 3, lwd = 1.3)
text(x=as.Date('2017/12/15'), y= 1.1, label='field capacity', cex=1)
text(x=as.Date('2017/12/15'), y= 0.75, label='easily available \nwater', cex=1)
text(x=as.Date('2017/12/15'), y= -0.1, label='wilting point', cex=1)
#text(x=as.Date('2017/12/15'), y=1.4, label='WY2018', cex = 1)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='lightblue', cex=0.5)
dev.off()

#7 cm soil T 2018
depth <- 7
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '7 cm plots', paste0('WY2018.soilT.7cm.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_data_7cm_2018)[2:ncol(soilT_data_7cm_2018)], '%b_%d_%Y'), soilT_data_7cm_2018[i,2:ncol(soilT_data_7cm_2018)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('7 cm depth soil temperature ('~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(soilT_data_7cm_2018)[2:ncol(soilT_data_7cm_2018)], '%b_%d_%Y'), soilT_data_7cm_2018[i,2:ncol(soilT_data_7cm_2018)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
#abline(h=10, lty=2)
#text(x=as.Date('2017/12/15'), y=30, label='WY2018', cex = 1.5)
#legend('bottomright', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
dev.off()

#now 22 cm graphs
#2017
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '22 cm plots', paste0('WY2017.SM.22cm.WP1.35xFD.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2017_22)[2:ncol(depletion_vwc_2017_22)], '%b_%d_%Y'), depletion_vwc_2017_22[i,2:ncol(depletion_vwc_2017_22)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.55, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'fraction of plant available water, 15-30 cm', xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2017_22)[2:ncol(depletion_vwc_2017_22)], '%b_%d_%Y'), depletion_vwc_2017_22[i,2:ncol(depletion_vwc_2017_22)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(-0.5, 0, 0.5, 1.0, 1.5))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
legend(x=as.Date('2017/1/10'), y=-0.05, legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.01, ncol=3, lwd = 1.3)
text(x=as.Date('2016/12/07'), y= 1.1, label='field capacity', cex=1)
text(x=as.Date('2017/2/07'), y= 0.75, label='easily available \nwater', cex=1)
text(x=as.Date('2017/4/25'), y= -0.1, label='wilting point', cex=1)
#text(x=as.Date('2016/12/15'), y=1.4, label='WY2017', cex = 1)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='lightblue', cex=0.5)
dev.off()

#now 2018 22 cm
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '22 cm plots', paste0('WY2018.SM.22cm.WP1.35xFD.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2018_22)[2:ncol(depletion_vwc_2018_22)], '%b_%d_%Y'), depletion_vwc_2018_22[i,2:ncol(depletion_vwc_2018_22)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.55, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'fraction of plant available water, 15-30 cm', xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2018_22)[2:ncol(depletion_vwc_2018_22)], '%b_%d_%Y'), depletion_vwc_2018_22[i,2:ncol(depletion_vwc_2018_22)], col=forage_terrain_energy$energy_colors[i])}
}
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
axis(side = 2, at=c(-0.5, 0, 0.5, 1.0, 1.5))
abline(h=0.5, lty=2)
abline(h=1, lty=2)
#legend('topright', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
text(x=as.Date('2017/12/15'), y= 1.1, label='field capacity', cex=1.1)
text(x=as.Date('2017/12/15'), y= 0.75, label='easily available \nwater', cex=1.1)
text(x=as.Date('2017/12/15'), y= -0.1, label='wilting point', cex=1.1)
#text(x=as.Date('2017/12/15'), y=1.4, label='WY2018', cex = 1.5)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='lightblue', cex=0.5)
dev.off()

#22 cm soil T 2018
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '22 cm plots', paste0('WY2018.soilT.22cm.tif')), family = 'Times New Roman', width = 9, height = 3, units = 'in', res=150, pointsize = 11)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_data_22cm_2018)[2:ncol(soilT_data_22cm_2018)], '%b_%d_%Y'), soilT_data_22cm_2018[i,2:ncol(soilT_data_22cm_2018)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('22 cm depth soil temperature (' ~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(soilT_data_22cm_2018)[2:ncol(soilT_data_22cm_2018)], '%b_%d_%Y'), soilT_data_22cm_2018[i,2:ncol(soilT_data_22cm_2018)], col=forage_terrain_energy$energy_colors[i])}
}
#text(x=as.Date('2017/12/15'), y=30, label='WY2018', cex = 1.5)
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
#abline(h=10, lty=2)
#legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, lwd = 1.3)
dev.off()

#22 cm 2017 soil T
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'aspect scheme', '22 cm plots', paste0('WY2017.soilT.22cm.tif')), family = 'Times New Roman', width = 9, height = 3, units = 'in', res=150, pointsize = 11)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_data_22cm_2017)[2:ncol(soilT_data_22cm_2017)], '%b_%d_%Y'), soilT_data_22cm_2017[i,2:ncol(soilT_data_22cm_2017)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('22 cm depth soil temperature ('~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(soilT_data_22cm_2017)[2:ncol(soilT_data_22cm_2017)], '%b_%d_%Y'), soilT_data_22cm_2017[i,2:ncol(soilT_data_22cm_2017)], col=forage_terrain_energy$energy_colors[i])}
}
#text(x=as.Date('2016/12/15'), y=30, label='WY2017', cex = 1.5)
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
#abline(h=10, lty=2)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.01, lwd=1.3)
dev.off()

#count days below AD
date1_2017 <- 'Dec_01_2016'
date2_2017 <- 'Apr_15_2017'
date1_2018 <- 'Dec_01_2017'
date2_2018 <- 'Apr_15_2018'
days.below.AD2017 <- apply(depletion_vwc_2017[,which(colnames(depletion_vwc_2017) ==date1_2017):which(colnames(depletion_vwc_2017)==date2_2017)], 1, function(x) sum(x < 0.5))
days.below.AD2017 #missing location 13
days.below.AD2018 <- apply(depletion_vwc_2018[,which(colnames(depletion_vwc_2018)==date1_2018):which(colnames(depletion_vwc_2018)==date2_2018)], 1, function(x) sum(x < 0.5))
days.below.AD2018 #not missing any
days.below.AD2017_22 <- apply(depletion_vwc_2017_22[,which(colnames(depletion_vwc_2017_22) ==date1_2017):which(colnames(depletion_vwc_2017_22)==date2_2017)], 1, function(x) sum(x < 0.5))
days.below.AD2017_22 #missing location 13
days.below.AD2018_22 <- apply(depletion_vwc_2018_22[,which(colnames(depletion_vwc_2018_22) ==date1_2018):which(colnames(depletion_vwc_2018_22)==date2_2018)], 1, function(x) sum(x < 0.5))
days.below.AD2018_22 #missing location 3
#plot(seq(from=0.4, by=.005, length.out = 16), days.below.AD2018_22, xlim = c(0,1), cex=1.5, col=forage_terrain_energy$energy_colors)
days.below.50AD.summary <- data.frame(WY=c(2017, 2018), depth7cm_mean=c(mean(days.below.AD2017, na.rm = TRUE), mean(days.below.AD2018, na.rm = TRUE)), depth7cm_sd=c(sd(days.below.AD2017, na.rm = TRUE), sd(days.below.AD2018, na.rm = TRUE)), depth22cm_mean=c(mean(days.below.AD2017_22, na.rm = TRUE), mean(days.below.AD2018_22, na.rm = TRUE)), depth22cm_sd=c(sd(days.below.AD2017_22, na.rm = TRUE), sd(days.below.AD2018_22, na.rm = TRUE)), depth7cm_max = c(max(days.below.AD2017, na.rm = TRUE), max(days.below.AD2018, na.rm = TRUE)), depth7cm_min=c(min(days.below.AD2017, na.rm = TRUE), min(days.below.AD2018, na.rm = TRUE)), depth22cm_max=c(max(days.below.AD2017_22, na.rm = TRUE), max(days.below.AD2018_22, na.rm = TRUE)), depth22cm_min=c(min(days.below.AD2017_22, na.rm = TRUE), min(days.below.AD2018_22, na.rm = TRUE)))
days.below.50AD.summary
write.csv(days.below.50AD.summary, file.path(results, 'tables', 'days.below50percentPAW.Jan10.Apr15.csv'), row.names = FALSE)
barplot(as.matrix(days.below.50AD.summary[ ,c(2,4)]), beside = TRUE, col=c('blue', 'red3'), ylab='Days below 50% plant available water', names.arg = c('0-15 cm', '15-30 cm'), legend.text = c('WY2017 (wet)', 'WY2018 (dry)'), cex.axis = 1.3, cex.names = 1.3, cex.lab = 1.3, args.legend = list(x="topleft", inset=0.005, cex=1.4))


tiff(file = file.path(results, 'figures', 'finals', 'days below AD', 'days.below.50percentPAW.Dec1_Apr15.tif'), family = 'Times New Roman', width = 6.5, height = 4.5, units = 'in', res=150)
par(mar=c(3.5, 4.5, 1, 1))
plot(x=seq(from=0.08, by=.0025, length.out = 16), days.below.AD2017, pch=1, col=forage_terrain_energy$energy_colors, xlim = c(0.05, 0.5), ylim=c(0, length(seq.Date(as.Date(date1_2018, '%b_%d_%Y'), as.Date(date2_2018, '%b_%d_%Y'), by='day'))), xaxt='n', ylab = 'days below 50% plant available water (Jan 10 - Apr 15)', xlab = '', cex=1.2, cex.axis=1, cex.lab=1)
points(x=seq(from=0.18, by=.0025, length.out = 16), days.below.AD2017_22, pch=1, col=forage_terrain_energy$energy_colors, cex=1.2)
#abline(h=96, lty=1)
abline(v=0.275, lty=2)
points(x=seq(from=0.33, by=.0025, length.out = 16), days.below.AD2018, pch=1, col=forage_terrain_energy$energy_colors, cex=1.2)
points(x=seq(from=0.43, by=.0025, length.out = 16), days.below.AD2018_22, pch=1, col=forage_terrain_energy$energy_colors, cex=1.2)
axis(side = 1, at = c(0.1, 0.2, 0.35, 0.45), labels = FALSE, tick = TRUE)
mtext(text = c('WY2017', 'WY2018'), side = 1, at=c(0.15, 0.4), line=2.5, cex=1)
mtext(text = c('0-15 cm', '15-30 cm', '0-15 cm', '15-30 cm'), side = 1, at = c(0.1, 0.2, 0.35, 0.45), line=1, cex = 1)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), pch = 1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.1, pt.cex = 1.2)
dev.off()

plot(days.below.AD2017, forage_terrain_energy$Apr2017growth)
summary(lm(forage_terrain_energy$Apr2017growth ~ days.below.AD2017)) #explains 45% of variance in Mar 14 - Apr 10 growth
days.below.AD2017_end <- apply(depletion_vwc_2017[,which(colnames(depletion_vwc_2017) =="Mar_01_2017"):which(colnames(depletion_vwc_2017)=='Apr_15_2017')], 1, function(x) sum(x < 0.5))
first.day.below.50AD <- apply(depletion_vwc_2017[,which(colnames(depletion_vwc_2017) =="Mar_01_2017"):which(colnames(depletion_vwc_2017)=='Apr_15_2017')], 1, function(x) which(x < 0.5)[1])
summary(lm(forage_terrain_energy$Apr2017growth ~ days.below.AD2017_end))
plot(days.below.AD2017_end, forage_terrain_energy$Apr2017growth, col=forage_terrain_energy$energy_colors)
plot(first.day.below.50AD, forage_terrain_energy$Apr2017growth, col=forage_terrain_energy$energy_colors)
plot(depletion_vwc_2017$Mar_18_2017, forage_terrain_energy$Apr2017growth, col=forage_terrain_energy$energy_colors)

plot(days.below.AD2017, forage_terrain_energy$peak2017)
abline(lm(forage_terrain_energy$peak2017 ~ days.below.AD2017))
summary(lm(forage_terrain_energy$peak2017 ~ days.below.AD2017))
plot(lm(forage_terrain_energy$peak2017 ~ days.below.AD2017))
plot(days.below.AD2017_22, forage_terrain_energy$peak2017)
summary(lm(forage_terrain_energy$peak2017 ~ days.below.AD2017_22))
plot(days.below.AD2018, forage_terrain_energy$peak2018)
summary(lm(forage_terrain_energy$peak2018 ~ days.below.AD2018))
plot(days.below.AD2018_22, forage_terrain_energy$peak2018)
abline(lm(forage_terrain_energy$peak2018 ~ days.below.AD2018_22))
summary(lm(forage_terrain_energy$peak2018 ~ days.below.AD2018_22))

#days below WP



avg.soilT.2018.growing.seson <- apply(soilT_data[,which(colnames(soilT_data) =="Jan_08_2018"):which(colnames(soilT_data)=='Apr_15_2018')], 1, mean)
mean.depletion <- apply(depletion_vwc_2018[,which(colnames(depletion_vwc_2018) =="Jan_08_2018"):which(colnames(depletion_vwc_2018)=='Apr_15_2018')], 1, mean)
summary(lm(forage_terrain_energy$peak2018 ~ days.below.AD2018 * avg.soilT.2018.growing.seson))
summary(lm(forage_terrain_energy$peak2018 ~ mean.depletion * avg.soilT.2018.growing.seson))
summary(lm(forage_terrain_energy$peak2018 ~ days.below.AD2018 + poly(avg.soilT.2018.growing.seson, 2)))
summary(lm(forage_terrain_energy$peak2018 ~ mean.depletion * avg.soilT.2018.growing.seson))
summary(lm(forage_terrain_energy$peak2018 ~ poly(avg.soilT.2018.growing.seson, 2)))
plot(avg.soilT.2018.growing.seson, forage_terrain_energy$peak2018)

summary(lm(forage_terrain_energy$peak2017 ~ apply(depletion_vwc_2017[,45:103], 1, mean) + apply(soilT_data[,45:103], 1, mean)))
summary(lm(forage_terrain_energy$peak2017 ~ apply(soilT_data[,45:103], 1, mean)))
plot(apply(soilT_data[,45:103], 1, mean), forage_terrain_energy$peak2017)

#get some stats for wet period 2017
summary(apply(soilT_data[,which(colnames(soilT_data)=='Jan_08_2017'):which(colnames(soilT_data)=='Mar_01_2017')], 2, function(x) max(x) - min(x)))
summary(apply(soilT_data[,which(colnames(soilT_data)=='Mar_01_2017'):which(colnames(soilT_data)=='Apr_15_2017')], 2, function(x) max(x) - min(x)))
wet_2017_meanT <- apply(soilT_data[,which(colnames(soilT_data)=='Jan_08_2017'):which(colnames(soilT_data)=='Mar_01_2017')], 1, mean)
summary(lm(forage_terrain_energy$peak2017 ~ wet_2017_meanT))


SMnorm_T2model_peak2017 <- read.csv(list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), stringsAsFactors=FALSE)
sigdates <- format.Date(SMnorm_T2model_peak2017$dates[SMnorm_T2model_peak2017$p.value.model < 0.01 & 'Jan' == format.Date(SMnorm_T2model_peak2017$dates, '%b')], '%b_%d_%Y')
sigdates
soilT_data_sig <- soilT_data[ ,c(1, which(colnames(soilT_data) %in% sigdates))]
soilT_data_sig_mean <- apply(soilT_data_sig, 1, mean)
soilT_data_sig_sd <- apply(soilT_data_sig, 1, sd)
VWC_data_sig <- vwc_data_normalized[ ,c(1, which(colnames(vwc_data_normalized) %in% sigdates))]
VWC_data_sig_mean <- apply(VWC_data_sig, 1, mean)
VWC_data_sig_sd <- apply(VWC_data_sig, 1, sd)
plot(soilT_data_sig_mean, forage_terrain_energy$annual_kwh.m2)
plot(soilT_data_sig_mean, forage_terrain_energy$peak2017)
plot(VWC_data_sig_mean, forage_terrain_energy$peak2017)
nlm_soilT <- lm(forage_terrain_energy$peak2017 ~ poly(soilT_data_sig_mean, 2))
summary(nlm_soilT)
#curve(nlm_soilT$coefficients[1] + nlm_soilT$coefficients[2] * x + nlm_soilT$coefficients[3] * x^2, from = 8.5, to = 12, add=TRUE)
plot(VWC_data_sig_mean, nlm_soilT$residuals)
summary(lm(soilT_data_sig_mean ~ VWC_data_sig_mean))
plot(VWC_data_sig_mean, soilT_data_sig_mean, cex=forage_terrain_energy$peak2017/1500)


vwcnorm_T2_agg <- lm(forage_terrain_energy$peak2017 ~ VWC_data_sig_mean + poly(soilT_data_sig_mean, 2))
summary(vwcnorm_T2_agg)
plot(vwcnorm_T2_agg)
vif(vwcnorm_T2_agg)
vwcnorm_T_agg <- lm(forage_terrain_energy$peak2017[-c(2,15)] ~ VWC_data_sig_mean[-c(2,15)] + soilT_data_sig_mean[-c(2,15)])
summary(vwcnorm_T_agg)
plot(vwcnorm_T_agg)
vif(vwcnorm_T_agg) #11.4 VIF!


#function to plot linear daily effects
sensor.date <- 'Feb_19_2017'
clip.date <- 'peak2017'
lm_results <- lm(forage_terrain_energy[[clip.date]] ~ vwc_data_normalized[[sensor.date]] + soilT_data[[sensor.date]])
summary(lm_results)
plot(lm_results)
vif(lm_results)

SMnorm_effect <- function(x) {
  y <- lm_results$coefficients[[2]] * x - mean(lm_results$coefficients[[2]] * x, na.rm=TRUE)}
SMnorm_effect(vwc_data_normalized[[sensor.date]])
T_effect <- function(x) {lm_results$coefficients[[3]] * x - mean(lm_results$coefficients[[3]] * x, na.rm=TRUE)} #T data missing from point 9 from 1/31-2/13/18
T_effect(soilT_data[[sensor.date]])
plot(SMnorm_effect(vwc_data_normalized[[sensor.date]]), T_effect(soilT_data[[sensor.date]]), xlab='soil moisture effect (kg /ha)', ylab='soil temperature effect (kg /ha)', main = paste(sensor.date, 'vs.', clip.date))#, cex = forage_data[[clip.date]] / mag.factor)

#make plot to visualize associations
mag.factor <- 1500
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('peak2017.SM.T', depth, 'cm.associations.', sensor.date, '.png')), family = 'Book Antiqua', width = 800, height = 800, units = 'px', res=100)
par(mar=c(4, 4, 1, 1))
plot(vwc_data_normalized[[sensor.date]], soilT_data[[sensor.date]], ylab=paste('soil temperature (deg C) on', format.Date(as.Date(sensor.date, format = '%b_%d_%Y'), '%b %d, %Y')), xlab=paste('normalized soil moisture on', format.Date(as.Date(sensor.date, format = '%b_%d_%Y'), '%b %d, %Y'), ' (std. dev. by location)'), cex=forage_terrain_energy[[clip.date]] / mag.factor, pch=21, bg='grey', cex.lab=1.1) #main = paste(sensor.date, 'vs.', clip.date)
legend("bottomright", legend=c(as.expression(bquote('4000 kg ha'^-1)), bquote('2000 kg ha'^-1), bquote('1000 kg ha'^-1)), pch=c(21,21,21), pt.bg=c('grey','grey','grey'), inset = 0.1, pt.cex = c(4000/mag.factor, 2000/mag.factor, 1000/mag.factor), title = 'peak forage, 2017', cex = 1.1)
dev.off()

lm_results$SMnorm_residuals <- lm_results$residuals + SMnorm_effect(vwc_data_normalized[[sensor.date]])
lm_results$T_residuals <- lm_results$residuals + T_effect(soilT_data[[sensor.date]])
plot(lm_results$fitted.values, forage_terrain_energy[[clip.date]], type='p', xlab="fitted value (kg / ha)", ylab="actual (kg / ha)", main = paste(sensor.date, 'vs.', clip.date))
abline(0, 1, col='green', lty=2, lwd=2)
plot(vwc_data_normalized[[sensor.date]], lm_results$SMnorm_residuals, type='p', xlab="normalized soil moisture", ylab="biomass association with 7 cm normalized soil moisture (kg / ha)", main = paste(sensor.date, 'vs.', clip.date))
curve(SMnorm_effect, from = min(vwc_data_normalized[[sensor.date]]), to = max(vwc_data_normalized[[sensor.date]]), add=TRUE, type='l', col='red', lty=2, lwd=2)
text(vwc_data_normalized[[sensor.date]], lm_results$SMnorm_residuals, labels = round(forage_terrain_energy$aspect, 0), pos = 1, offset = 0.5, cex = 0.7)
plot(soilT_data[[sensor.date]], lm_results$T_residuals, type='p', xlab="soil temperature at 7 cm", ylab="biomass association with 7 cm soil temperature (kg / ha)", main = paste(sensor.date, 'vs.', clip.date))
curve(T_effect, from = min(soilT_data[[sensor.date]]), to = max(soilT_data[[sensor.date]]), add=TRUE, type='l', col='red', lty=2, lwd=2)
text(soilT_data[[sensor.date]], lm_results$T_residuals, labels = round(forage_terrain_energy$aspect, 0), pos = 1, offset = 0.5, cex = 0.7)

#make plots for specified time periods (only done for 2018 thus far)
data_name <- 'VWC'
depth <- '7'
yr <- '2017'
vwc_data_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, data_name, paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
colnames(vwc_data_2017)
#endcol <- ncol(vwc_data_2017)
dates2017 <- seq.Date(as.Date(colnames(vwc_data_2017)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_2017)[ncol(vwc_data_2017)], '%b_%d_%Y'), by='day')
#normalize 2017 vwc_data
#vwc_data_normalized_2017 <- vwc_data_2017[,1:165] #end May 1
vwc_data_normalized_2017 <- vwc_data_2017
vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)] <- (vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)] - rowMeans(vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)], na.rm = TRUE)) / apply(vwc_data_normalized_2017[ ,2:ncol(vwc_data_normalized_2017)], 1, sd, na.rm=TRUE)

#plotting 2018 daily data by location x depth location from summaries produced above
data_name <- 'VWC'
depth <- '7'
yr <- '2018'
vwc_data_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, data_name, paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
colnames(vwc_data_2018)
#endcol <- ncol(vwc_data_2018)
dates2018 <- seq.Date(as.Date(colnames(vwc_data_2018)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_2018)[ncol(vwc_data_2018)], '%b_%d_%Y'), by='day')
#normalize 2018 vwc_data
vwc_data_normalized_2018 <- vwc_data_2018
vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)] <- (vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)] - rowMeans(vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)], na.rm = TRUE)) / apply(vwc_data_normalized_2018[ ,2:ncol(vwc_data_normalized_2018)], 1, sd, na.rm=TRUE)

#plot 2018 significant association from 1/18-1/27/18
#png(file = file.path(results, 'figures', paste0('WY2018soilmoisture.norm.', depth, 'cm.v2.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
#par(mar=c(2, 4, 2, 2))
date1 <- which(colnames(vwc_data_normalized_2018)=='Jan_01_2018')
date2 <- which(colnames(vwc_data_normalized_2018)=='Jan_27_2018')
clip.date <- 'clp032218'
xlab.date <- '3/22/18'
for (i in 1:nrow(vwc_data_normalized_2018)) {
  if (i == 1) {
    plot(dates2018[(date1-1):(date2-1)], vwc_data_normalized_2018[i, date1:date2], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_normalized_2018[,date1:date2], na.rm=TRUE), max(vwc_data_normalized_2018[,date1:date2], na.rm=TRUE)), xlim = c(dates2018[date1-1], dates2018[date2-1]), xlab = "", ylab='Std Deviations by Location', main = paste('WY2018 normalized soil moisture,', depth, 'cm depth, Camatta catchment'))
    #text(x=dates2018[plotpos + i], y=vwc_data_normalized_2018[i, plotpos+ 1 + i ], labels=vwc_data_normalized_2018[i, 1], cex=0.6, pos = 1, offset = 0.1)
  } else {
    lines.default(dates2018[(date1-1):(date2-1)], vwc_data_normalized_2018[i, date1:date2], xaxt='n', col=i+1)
    #text(x=dates2018[plotpos + i], y=vwc_data_normalized_2018[i, plotpos + 1 + i], labels=vwc_data_normalized_2018[i, 1], cex=0.6, pos = 1, offset = 0.1)
  }
}
#abline(0, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date(colnames(vwc_data_normalized_2018)[date1], format='%b_%d_%Y'), to = as.Date(colnames(vwc_data_normalized_2018)[date2], format='%b_%d_%Y'), by='day'), format = '%m/%d/%y')
#dev.off()
plot(vwc_data_normalized_2018[,date1], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+1], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+1], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+2], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+2], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+3], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+3], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+4], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+4], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+5], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+5], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+6], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+6], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+7], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+7], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+8], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+8], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+9], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+9], xlim=c(0.8, 1.8), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
summary(lm(forage_data[[clip.date]] ~ vwc_data_normalized_2018[,date1+4])) #with soil T in model, r^2=0.6 on this date but soil is non-sig

#plot 2018 significant association from 1/18-1/27/18
#png(file = file.path(results, 'figures', paste0('WY2018soilmoisture.norm.', depth, 'cm.v2.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
#par(mar=c(2, 4, 2, 2))
date1 <- which(colnames(vwc_data_normalized_2018)=='Feb_03_2018')
date2 <- which(colnames(vwc_data_normalized_2018)=='Mar_15_2018')
clip.date <- 'clp041518'
xlab.date <- '4/15/18'
for (i in 1:nrow(vwc_data_normalized_2018)) {
  if (i == 1) {
    plot(dates2018[(date1-1):(date2-1)], vwc_data_normalized_2018[i, date1:date2], type='l', xaxt='n', col=i+1, ylim=c(min(vwc_data_normalized_2018[,date1:date2], na.rm=TRUE), max(vwc_data_normalized_2018[,date1:date2], na.rm=TRUE)), xlim = c(dates2018[date1-1], dates2018[date2-1]), xlab = "", ylab='Std Deviations by Location', main = paste('WY2018 normalized soil moisture,', depth, 'cm depth, Camatta catchment'))
    #text(x=dates2018[plotpos + i], y=vwc_data_normalized_2018[i, plotpos+ 1 + i ], labels=vwc_data_normalized_2018[i, 1], cex=0.6, pos = 1, offset = 0.1)
  } else {
    lines.default(dates2018[(date1-1):(date2-1)], vwc_data_normalized_2018[i, date1:date2], xaxt='n', col=i+1)
    #text(x=dates2018[plotpos + i], y=vwc_data_normalized_2018[i, plotpos + 1 + i], labels=vwc_data_normalized_2018[i, 1], cex=0.6, pos = 1, offset = 0.1)
  }
}
#abline(0, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date(colnames(vwc_data_normalized_2018)[date1], format='%b_%d_%Y'), to = as.Date(colnames(vwc_data_normalized_2018)[date2], format='%b_%d_%Y'), by='day'), format = '%m/%d/%y')
#dev.off()
plot(vwc_data_normalized_2018[,date1], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+1], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+1], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+2], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+2], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+3], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+3], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+4], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+4], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+5], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+5], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+6], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+6], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+7], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+7], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+8], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+8], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
plot(vwc_data_normalized_2018[,date1+9], forage_data[[clip.date]], main=colnames(vwc_data_normalized)[date1+9], xlim=c(0, 1.2), ylab=paste('biomass on', xlab.date, '(kg / ha)'), xlab='normalized soil moisture (std. dev.)')
summary(lm(forage_data[[clip.date]] ~ vwc_data_normalized_2018[,date1+4])) #with soil T in model, r^2=0.6 on this date but soil is non-sig

