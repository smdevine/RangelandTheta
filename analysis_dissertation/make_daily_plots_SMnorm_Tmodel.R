library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SMnorm_T_model_results'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
forage_terrain_energy <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
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
depth <- 7
#vwc_data<- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
vwc_data<- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017_2018', 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_data<- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017_2018', 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
if (depth == 22) {
  vwc_data[16,] <- c(3,rep(NA, (ncol(vwc_data)-1)))
  vwc_data <- vwc_data[order(vwc_data$location), ]
}
if (depth == 22) {
  soilT_data[16,] <- c(3,rep(NA, (ncol(soilT_data)-1)))
  soilT_data <- soilT_data[order(soilT_data$location), ]
}
#standardize by "field capacity"
#22 cm data to get location 3 for 2017
#vwc_22cm.2017_by.sensor <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor/daily_by_sensor_2017_processed2018-09-27.csv'), stringsAsFactors = FALSE)
#vwc_22cm_loc3_WP <- mean(vwc_22cm.2017_by.sensor$MeanVWC[vwc_22cm.2017_by.sensor$Date==20170630 & vwc_22cm.2017_by.sensor$Location==3]) #because data was suspect and removed from 2018
vwc_22cm.2017_by.sensor <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/daily_by_sensor/daily_by_sensor_2018_processed2018-09-27.csv'), stringsAsFactors = FALSE)
vwc_22cm_loc3_WP <- mean(vwc_22cm.2017_by.sensor$MeanVWC[vwc_22cm.2017_by.sensor$Date==20171129 & vwc_22cm.2017_by.sensor$Location==3])

FC.by.location <- if (depth==7) {vwc_data$Jan_27_2017} else{vwc_data$Jan_31_2017} #Jan 16 is 3 days after no rainfall; Jan 27 is 4 days after no rainfall for 22 cm
#wp.date <- 'Jun_15_2017' #last analysis done with Jun_15_2017
wilting.point.by.location <- if (depth == 7) {
  2 * apply(vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Dec_20_2017')], 1, mean) #used Jun_20_2017 for latest version of figures
  #vwc_data[[wp.date]]
} else {
    1.35 * apply(vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Dec_31_2017')], 1, mean)
}
paw.by.location <- FC.by.location - wilting.point.by.location
mean(paw.by.location, na.rm = TRUE)
mean(FC.by.location, na.rm = TRUE)
mean(wilting.point.by.location, na.rm = TRUE)
plot(wilting.point.by.location, FC.by.location)
abline(lm(FC.by.location ~ wilting.point.by.location))
text(wilting.point.by.location, FC.by.location, label=forage_terrain_energy$location, pos = 1, offset = 0.1)
summary(lm(wilting.point.by.location ~ FC.by.location))
summary(lm(FC.by.location ~ forage_terrain_energy$annual_kwh.m2))
summary(lm(FC.by.location ~ soilT_data$Jan_30_2017)) #NS
plot(soilT_data$Jan_27_2017, FC.by.location)
text(soilT_data$Jan_27_2017, FC.by.location, pos = 1, offset = 0.1)
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
#paw.by.location <- FC.by.location_BC - wilting.point.by.location
summary(lm(paw.by.location ~ forage_terrain_energy$annual_kwh.m2))
plot(forage_terrain_energy$annual_kwh.m2, paw.by.location)
text(forage_terrain_energy$annual_kwh.m2, paw.by.location, labels = forage_terrain_energy$location, pos = 1, offset = 0.5)
abline(lm(paw.by.location ~ forage_terrain_energy$annual_kwh.m2))
depletion_vwc_2017 <-  vwc_data[,2:which(colnames(vwc_data)=='Jun_30_2017')]
depletion_vwc_2017 <- data.frame(location=1:16, lapply(depletion_vwc_2017[,2:ncol(depletion_vwc_2017)], function(x) { 1 - ((FC.by.location - x) / (FC.by.location - wilting.point.by.location))}))
depletion_vwc_2018 <- vwc_data[,which(colnames(vwc_data)=='Nov_30_2017'):which(colnames(vwc_data)=='Jun_30_2018')]
depletion_vwc_2018 <- data.frame(location=1:16, lapply(depletion_vwc_2018[,2:ncol(depletion_vwc_2018)], function(x) { 1 - ((FC.by.location - x) / (FC.by.location - wilting.point.by.location))}))


#read em in

write.csv(depletion_vwc_2017, file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc', paste0('depletion_vwc_', depth, 'cm_2017.csv')), row.names=FALSE)
write.csv(depletion_vwc_2018, file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc', paste0('depletion_vwc_', depth, 'cm_2018.csv')), row.names=FALSE)
depletion_vwc_2017 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2018.csv'), stringsAsFactors=FALSE)
depletion_vwc_2017_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2018.csv'), stringsAsFactors=FALSE)

#get drawdown slopes for March 2017
Mar2017_avgsoilT <- apply(soilT_data[,which(colnames(soilT_data)=='Mar_04_2017'):which(colnames(soilT_data)=='Mar_20_2017')], 1, mean)
Mar2017_drydown <- as.data.frame(t(apply(depletion_vwc_2017[,which(colnames(depletion_vwc_2017)=='Mar_04_2017'):which(colnames(depletion_vwc_2017)=='Mar_20_2017')], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
# Mar2017_drydown <- as.data.frame(t(apply(vwc_data[,which(colnames(vwc_data)=='Feb_25_2017'):which(colnames(vwc_data)=='Mar_20_2017')], 1, function(x) {
#   lm.summary <- summary(lm(x ~ seq_along(x)))
#   c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
# })))
colnames(Mar2017_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_drydown$drydown.slopes <- -Mar2017_drydown$drydown.slopes
plot(Mar2017_drydown$drydown.slopes, forage_terrain_energy$clp041017)
plot(Mar2017_avgsoilT, Mar2017_drydown$drydown.slopes)
plot(forage_terrain_energy$annual_kwh.m2, Mar2017_avgsoilT)
summary(lm(Mar2017_drydown$drydown.slopes ~ Mar2017_avgsoilT))
summary(lm(Mar2017_drydown$drydown.slopes ~ forage_terrain_energy$seasonal_kwh.m2))
mult.lm.result <- lm(forage_terrain_energy$clp041017 ~ Mar2017_avgsoilT + Mar2017_drydown$drydown.slopes)
summary(mult.lm.result)
plot(mult.lm.result)
lm.result <- lm(forage_terrain_energy$clp041017 ~ Mar2017_drydown$drydown.slopes)
summary(lm.result)
plot(lm.result)
summary(lm(forage_terrain_energy$peak2017 ~ Mar2017_avgsoilT))

Mar2017_drydown_22 <- as.data.frame(t(apply(depletion_vwc_2017_22[-3,which(colnames(depletion_vwc_2017_22)=='Mar_04_2017'):which(colnames(depletion_vwc_2017_22)=='Mar_20_2017')], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_drydown_22) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_drydown_22$drydown.slopes <- -Mar2017_drydown_22$drydown.slopes
Mar2017_drydown_22
plot(Mar2017_drydown_22$drydown.slopes, forage_terrain_energy$clp041017[-3])
summary(lm(forage_terrain_energy$clp041017[-3] ~ Mar2017_drydown_22$drydown.slopes))


for(i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(vwc_data)[2:ncol(vwc_data)], '%b_%d_%Y'), vwc_data[i,2:ncol(vwc_data)], type='l', col=i, ylim=c(0.06,0.42))
  } else {lines(as.Date(colnames(vwc_data)[2:ncol(vwc_data)], '%b_%d_%Y'), vwc_data[i,2:ncol(vwc_data)], col=i)}
}
vwc_data_normalized <- normalize_VWCdata(vwc_data)
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)

soilT_data_7cm_2018 <- soilT_data[,which(colnames(soilT_data)=='Nov_30_2017'):which(colnames(soilT_data)=='Jun_30_2018')]
soilT_data_7cm_2017 <- soilT_data[,2:which(colnames(soilT_data)=='Jun_30_2017')]

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
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > summary(forage_terrain_energy$annual_kwh.m2)[2] & forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[5], 'orange2', 'red3'))
######
#7cm VWC depletion
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('WY2017.SM.', depth, 'cm.WP.2xFD.png')), family = 'Book Antiqua', width = 1200, height = 500, units = 'px', res=100)
par(mar=c(2.5, 4.5, 1, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.6, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'fraction of plant available water, 7 cm depth', xlim = as.Date(c('2016-12-01', '2017-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(-0.5, 0, 0.5, 1, 1.5))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
#abline(v=as.Date('2017/4/10'))
legend('bottomleft', legend=(c("< 1254", '1254-1458', '>1458')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
text(x=as.Date('2016/12/07'), y= 1.1, label='field\ncapacity', cex=1.1)
text(x=as.Date('2017/2/07'), y= 0.75, label='easily available \nwater', cex=1.1)
text(x=as.Date('2017/4/28'), y= 0.05, label='wilting point', cex=1.1)
#text(x=as.Date('2017/4/1'), y=1.25, label='peak forage', cex = 1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.01, type='s', col='lightblue', cex=0.5)
dev.off()

#7 cm soil T 2017
depth <- 7
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('WY2017.soilT.', depth, 'cm.png')), family = 'Book Antiqua', width = 1200, height = 500, units = 'px', res=100)
par(mar=c(2.5, 4.5, 1, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_data_7cm_2017)[2:ncol(soilT_data_7cm_2017)], '%b_%d_%Y'), soilT_data_7cm_2017[i,2:ncol(soilT_data_7cm_2017)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = '7 cm depth soil temperature (deg C)', ylim = c(0,32), xlim = as.Date(c('2016-12-01', '2017-05-03')))
  } else {lines(as.Date(colnames(soilT_data_7cm_2017)[2:ncol(soilT_data_7cm_2017)], '%b_%d_%Y'), soilT_data_7cm_2017[i,2:ncol(soilT_data_7cm_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
abline(h=10, lty=2)
legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
dev.off()

#now 2018 7 cm
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('WY2018.SM.', depth, 'cm.WP2xFD.png')), family = 'Book Antiqua', width = 1200, height = 500, units = 'px', res=100)
par(mar=c(2.5, 4.5, 1, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.6, 1.5), xaxt='n', xlab='', ylab = 'fraction of plant available water, 7 cm depth', xlim = as.Date(c('2017-12-01', '2018-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], col=forage_terrain_energy$energy_colors[i])}
}
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
#abline(v=as.Date('2018/4/15'))
legend('topright', legend=(c("< 1254", '1254-1458', '>1458')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
text(x=as.Date('2017/12/15'), y= 1.05, label='field capacity', cex=1.1)
text(x=as.Date('2017/12/15'), y= 0.75, label='easily available \nwater', cex=1.1)
text(x=as.Date('2017/12/15'), y= 0.05, label='wilting point', cex=1.1)
#text(x=as.Date('2018/4/7'), y=1.25, label='peak forage', cex=1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.01, type='s', col='lightblue', cex=0.5)
dev.off()

#7 cm soil T 2018
depth <- 7
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('WY2018.soilT.', depth, 'cm.png')), family = 'Book Antiqua', width = 1200, height = 500, units = 'px', res=100)
par(mar=c(2.5, 4.5, 1, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_data_7cm_2018)[2:ncol(soilT_data_7cm_2018)], '%b_%d_%Y'), soilT_data_7cm_2018[i,2:ncol(soilT_data_7cm_2018)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = '7 cm depth soil temperature (deg C)', ylim = c(0,32), xlim = as.Date(c('2017-12-01', '2018-05-03')))
  } else {lines(as.Date(colnames(soilT_data_7cm_2018)[2:ncol(soilT_data_7cm_2018)], '%b_%d_%Y'), soilT_data_7cm_2018[i,2:ncol(soilT_data_7cm_2018)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
abline(h=10, lty=2)
legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
dev.off()



#now 22 cm graphs
#2017
depth <- 22
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('WY2017.SM.', depth, 'cm.WP1.35xFD.png')), family = 'Book Antiqua', width = 1200, height = 500, units = 'px', res=100)
par(mar=c(2.5, 4.5, 1, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.55, 1.4), xaxt='n', xlab='', yaxt = 'n', ylab = 'fraction of plant available water, 22 cm depth', xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(-0.5, 0, 0.5, 1))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
legend('topright', legend=(c("< 1254", '1254-1458', '>1458')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
text(x=as.Date('2016/12/07'), y= 1.05, label='field capacity', cex=1.1)
text(x=as.Date('2017/2/07'), y= 0.75, label='easily available \nwater', cex=1.1)
text(x=as.Date('2017/4/28'), y= 0.05, label='wilting point', cex=1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.01, type='s', col='lightblue', cex=0.5)
dev.off()

#now 2018 22 cm
png(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', paste0('WY2018.SM.', depth, 'cm.WP1.35xFD.png')), family = 'Book Antiqua', width = 1200, height = 500, units = 'px', res=100)
par(mar=c(2.5, 4.5, 1, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.55, 1.4), xaxt='n', xlab='', yaxt = 'n', ylab = 'fraction of plant available water, 22 cm depth', xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], col=forage_terrain_energy$energy_colors[i])}
}
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
axis(side = 2, at=c(-0.5, 0, 0.5, 1))
abline(h=0.5, lty=2)
abline(h=1, lty=2)
legend('topright', legend=(c("< 1254", '1254-1458', '>1458')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
text(x=as.Date('2017/12/15'), y= 1.05, label='field capacity', cex=1.1)
text(x=as.Date('2017/12/15'), y= 0.75, label='easily available \nwater', cex=1.1)
text(x=as.Date('2017/12/15'), y= 0.05, label='wilting point', cex=1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.01, type='s', col='lightblue', cex=0.5)
dev.off()

belowAD2017 <- apply(depletion_vwc_2017, 1, function(x) which(x < 0.5))
names(belowAD2017) <- 1:16
belowAD2018 <- apply(depletion_vwc_2018, 1, function(x) which(x < 0.5))
names(belowAD2018) <- 1:16
days.below.AD2018 <- apply(depletion_vwc_2018[,which(colnames(depletion_vwc_2018) =="Jan_08_2018"):which(colnames(depletion_vwc_2018)=='Apr_15_2018')], 1, function(x) sum(x < 0.5))
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

