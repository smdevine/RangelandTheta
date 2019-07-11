#this can be run in entirety to reproduce all 14 Figures for publication (Figs1-10 and FigsS1-S4)
#remake of Figure 9a-b, 10a-b, 12a-b, and 13a-b from dissertation
#condensing 4 into 2: Figure 7a-d (was Figure 9a-b and 10a-b) and Figure 9a-b (was 12a-b and 13a-b)
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
climateDir <- 'C:/Users/smdevine/Desktop/rangeland project/climate_data'
dataDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/'
#results <- 'C:/Users/smdevine/Desktop/rangeland project/dissertation Chp2/for publication/figures'
results <- 'C:/Users/smdevine/Desktop/rangeland project/dissertation Chp2/for publication/submission/figures'
#get precip data in order
precip_data <- read.csv(file.path(climateDir, 'Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
precip_data$Date <- as.Date(precip_data$Date, format = '%m/%e/%Y')
precip_data$Month <- format.Date(precip_data$Date, '%b')
precip_data$Year <- format.Date(precip_data$Date, '%Y')
precip_data$WY <- ifelse(precip_data$Date < as.Date('2017-07-01'), 2017, 2018)
precip_data_v2 <- read.csv(file.path(climateDir, 'Camatta_precip_2001_2018by.month.csv'), stringsAsFactors = FALSE) #sent via email from Grace 11/21/18; 3 years of Camatta data plus data from nearby gauge 2000-2014
colnames(precip_data_v2)[3:ncol(precip_data_v2)] <- paste0('WY.', 2001:2018)
precip_summary <- as.data.frame(t(data.frame(WY.2017=tapply(precip_data_v2$WY.2017, precip_data_v2$Month, function(x) 25.4 * mean(x)), WY.2018=tapply(precip_data_v2$WY.2018, precip_data_v2$Month, function(x) 25.4 * mean(x))))) #c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')
precip_summary_GS <- precip_summary[ ,c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')]
precip_summary_GS$TOTAL <- apply(precip_summary_GS, 1, sum)
precip_error_summary <- as.data.frame(t(data.frame(WY.2017=tapply(25.4*precip_data_v2$WY.2017, precip_data_v2$Month, function(x) sd(x)), WY.2018=tapply(25.4*precip_data_v2$WY.2018, precip_data_v2$Month, function(x) sd(x))))) #there is a slight difference if you re-scale stdevs after the calc
precip_error_summary <- precip_error_summary[,c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')]
precip_error_summary$TOTAL <- c(24.63, 12.97) #Camatta_precip_2001_2018by.month.xlsx

vwc_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'VWC', 'MeanVWC_7cm_dailymeans_by_location.csv'), stringsAsFactors = FALSE)
vwc_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'VWC', 'MeanVWC_22cm_dailymeans_by_location.csv'), stringsAsFactors = FALSE)
soilT_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', 'MeanT_7cm_dailymeans_by_location.csv'), stringsAsFactors = FALSE)
soilT_7cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', 'MeanT_7cm_dailymeans_by_location.csv'), stringsAsFactors = FALSE)
soilT_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_22cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', 'MeanT_22cm_dailymeans_by_location.csv'), stringsAsFactors = FALSE)
depletion_vwc_2017 <- read.csv(file.path(dataDir, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018 <- read.csv(file.path(dataDir, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2018.csv'), stringsAsFactors=FALSE)
depletion_vwc_2017_22 <- read.csv(file.path(dataDir, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018_22 <- read.csv(file.path(dataDir, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2018.csv'), stringsAsFactors=FALSE)
forage_terrain_energy <- read.csv(file.path(dataDir, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)

nlm_2018 <- function(x, date, df) {
  nlm_obj <- lm(df[,date][] ~ df$avg_soilT[] + I(df$avg_soilT[]^2))
  nlm_obj$coefficients[1] + nlm_obj$coefficients[2] * x + nlm_obj$coefficients[3] * x^2
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
soilT_7cm_2017 <- gap_fill_soilTv3(soilT_7cm_2017, 13, 4, 14, 0.99)
soilT_22cm_2017 <- gap_fill_soilTv3(soilT_22cm_2017, 13, 4, 14, 0.99)

#add energy colors
#colors <- c('blue', 'orange2', 'red3')
colors <- c('blue', 'orange2', 'red3')
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, colors[1], ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, colors[2], colors[3])) #tres colores #tres colores
forage_terrain_energy$May2017growth <- forage_terrain_energy$clp050117 - forage_terrain_energy$clp041017
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
forage_terrain_energy$Mar2017growth <- forage_terrain_energy$clp031417 - forage_terrain_energy$clp021517
forage_terrain_energy$Mar2018growth <- forage_terrain_energy$clp032218 - forage_terrain_energy$clp021518
forage_terrain_energy$Apr2018growth <- forage_terrain_energy$clp041518 - forage_terrain_energy$clp032218

#Fig 2: forage growth across two growing seasons
#produce a cumulative precipitation vector first
precip_data_2017 <- precip_data[precip_data$WY==2017,]
precip_data_2017$Rainfall.mm.cumulative <- cumsum(precip_data_2017$Rainfall..mm.)
precip_data_2018  <- precip_data[precip_data$WY==2018,]
precip_data_2018$Rainfall.mm.cumulative <- cumsum(precip_data_2018$Rainfall..mm.)
#as.Date('2017-05-01') - as.Date('2017-02-05')
#Time difference of 85 days + 1
#as.Date('2017-04-15') - as.Date('2017-02-05')
#nrow(precip_data_2017[102:nrow(precip_data_2017),])
#nrow(precip_data_2018[128:197,])
tiff(file = file.path(results, 'Fig2.tif'), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 11, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(3, 4.5, 1, 4.5))
plot(x=rep(0.25,16), forage_terrain_energy$clp021517, type = 'p', col=forage_terrain_energy$energy_colors, pch=1, ylim=c(0, 4600), xlim=c(0,5), xaxt='n', xlab='', ylab=expression(paste('Standing forage (kg', ' ', ha^-1, ')')), cex.axis=1, cex.lab=1, cex=1.2) #cex=forage_terrain_energy$curvature_cex_v2
points(x=rep(1,16), forage_terrain_energy$clp031417, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) #cex=forage_terrain_energy$slope_cex_v2
points(x=rep(1.75,16), forage_terrain_energy$clp041017, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) #cex=forage_terrain_energy$elevation_cex_v2) this is non-significant but notable
points(x=rep(2.5,16), forage_terrain_energy$clp050117, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) # cex=forage_terrain_energy$elevation_cex_v2) non-significant but notable
abline(v=2.875, lty=2)
points(x=rep(3.25,16), forage_terrain_energy$clp021518, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2)# cex=forage_terrain_energy$curvature_cex_v2)
points(x=rep(4,16), forage_terrain_energy$clp032218, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) #cex=forage_terrain_energy$aspect_cex_v2) non-significant
points(x=rep(4.75,16), forage_terrain_energy$clp041518, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2)# cex=forage_terrain_energy$slope_cex_v2)
lines(x=seq(from=-0.2, to=2.5, length.out = 85), y=10*287/303.276*c(precip_data_2017$Rainfall.mm.cumulative[102:nrow(precip_data_2017)], rep(precip_data_2017$Rainfall.mm.cumulative[nrow(precip_data_2017)], 10)))
lines(x=seq(from=2.875, to=4.75, length.out = 70), y=10*123/136*precip_data_2018$Rainfall.mm.cumulative[128:197])
#text(x=0.25, y= 2500, label = 'mean curvature', srt = 90, cex=1.1)
#text(x=1, y= 3300, label = 'aspect & slope', srt = 90, cex = 1.1)
#text(x=1.75, y= 500, label = 'NS', srt = 90, cex = 1.1)
#text(x=2.5, y= 500, label = 'NS', srt = 90, cex = 1.1)
#text(x=3.25, y = 1300, label = 'mean curvature', srt=90, cex = 1.1)
#text(x=4, y = 1600, label = 'non-linear aspect', srt=90, cex=1.1)
#text(x=4.75, y=2600, label='slope & non-linear aspect', srt=90, cex = 1.1)
axis(side = 1, at = c(0.25, 1, 1.75, 2.5, 3.25, 4, 4.75), labels = c('Feb 15', 'Mar 14', 'Apr 10', 'May 1', 'Feb 15', 'Mar 22', 'Apr 15'), cex=1)
axis(side = 4, at = c(0, 1000, 2000, 3000), labels = c('0', '100', '200', '300'))
mtext("Cumulative precipitation (mm)", side=4, line=2.5, at=1500)
legend('topright', legend=(c("< 1200", '1200-1410', '>1410')), pch=1, pt.cex = 1.2, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.01, cex = 1)
text(x=0.375, y=4400, label='wet 2016-17', cex=1)
text(x=3.375, y=4400, label='dry 2017-18', cex=1)
dev.off()

#Fig 3a
#0-15cm VWC depletion
tiff(file = file.path(results, 'Fig3a.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.6, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'Fraction of plant available water, 0-15 cm', xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2017)[2:ncol(depletion_vwc_2017)], '%b_%d_%Y'), depletion_vwc_2017[i,2:ncol(depletion_vwc_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(-0.5, 0, 0.5, 1, 1.5))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/01'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
#abline(v=as.Date('2017/4/10'))
legend("bottom", legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=colors, title = expression(paste('annual kWh ', m^-2)), inset=0.01, ncol = 3, lwd = 1.3)
text(x=as.Date('2016/12/07'), y= 1.2, label='field\ncapacity', cex=1)
text(x=as.Date('2017/2/07'), y= 0.7, label='easily available \nwater', cex=1)
text(x=as.Date('2016/12/07'), y= -0.1, label='wilting point', cex=1)
text(x=as.Date('2017/5/01'), y= 1.4, label='a', cex=1)
#text(x=as.Date('2016/12/15'), y=1.4, label='WY2017', cex = 1)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("Precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='black', cex=0.5)
dev.off()

#0-15 cm soil T 2017
#Fig 5a
tiff(file = file.path(results, 'Fig5a.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_7cm_2017)[2:ncol(soilT_7cm_2017)], '%b_%d_%Y'), soilT_7cm_2017[i,2:ncol(soilT_7cm_2017)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('Soil temperature, 0-15 cm ('~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(soilT_7cm_2017)[2:ncol(soilT_7cm_2017)], '%b_%d_%Y'), soilT_7cm_2017[i,2:ncol(soilT_7cm_2017)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
text(x=as.Date('2017/5/01'), y=2.5, label='a', cex = 1)
#text(x=as.Date('2016/12/15'), y=30, label='WY2017', cex = 1)
#abline(h=30, lty=2)
abline(h=5, lty=2)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=colors, title = expression(paste('annual kWh ', m^-2)), inset=0.01, lwd = 1.3)
dev.off()

#now 2018 0-15 cm soil moisture depletion
#Fig 3b
tiff(file = file.path(results, 'Fig3b.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2018)[2:ncol(depletion_vwc_2018)], '%b_%d_%Y'), depletion_vwc_2018[i,2:ncol(depletion_vwc_2018)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.6, 1.5), xaxt='n', xlab='', ylab = 'Fraction of plant available water, 0-15 cm', xlim = as.Date(c('2017-11-25', '2018-05-03')))
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
text(x=as.Date('2018/5/01'), y= 1.4, label='b', cex=1)
#text(x=as.Date('2017/12/15'), y=1.4, label='WY2018', cex = 1)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("Precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='black', cex=0.5)
dev.off()

#0-15 cm soil T 2018
#Fig 5b
tiff(file = file.path(results, 'Fig5b.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_7cm_2018)[2:ncol(soilT_7cm_2018)], '%b_%d_%Y'), soilT_7cm_2018[i,2:ncol(soilT_7cm_2018)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('Soil temperature, 0-15 cm ('~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(soilT_7cm_2018)[2:ncol(soilT_7cm_2018)], '%b_%d_%Y'), soilT_7cm_2018[i,2:ncol(soilT_7cm_2018)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
abline(h=5, lty=2)
text(x=as.Date('2018/5/01'), y=2.5, label='b', cex = 1)
#text(x=as.Date('2017/12/15'), y=30, label='WY2018', cex = 1.5)
#legend('bottomright', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
dev.off()

#2017 15-30 cm soil moisture depletion
#Fig 4a
tiff(file = file.path(results, 'Fig4a.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2017_22)[2:ncol(depletion_vwc_2017_22)], '%b_%d_%Y'), depletion_vwc_2017_22[i,2:ncol(depletion_vwc_2017_22)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.55, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'Fraction of plant available water, 15-30 cm', xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(depletion_vwc_2017_22)[2:ncol(depletion_vwc_2017_22)], '%b_%d_%Y'), depletion_vwc_2017_22[i,2:ncol(depletion_vwc_2017_22)], col=forage_terrain_energy$energy_colors[i])}
}
axis(side = 2, at=c(-0.5, 0, 0.5, 1.0, 1.5))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
abline(h=0.5, lty=2)
abline(h=1, lty=2)
legend(x=as.Date('2017/1/10'), y=-0.05, legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=colors, title = expression(paste('annual kWh ', m^-2)), inset=0.01, ncol=3, lwd = 1.3)
text(x=as.Date('2016/12/07'), y= 1.1, label='field capacity', cex=1)
text(x=as.Date('2017/2/07'), y= 0.75, label='easily available \nwater', cex=1)
text(x=as.Date('2017/4/25'), y= -0.1, label='wilting point', cex=1)
text(x=as.Date('2017/5/01'), y= 1.4, label='a', cex=1)
#text(x=as.Date('2016/12/15'), y=1.4, label='WY2017', cex = 1)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("Precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='black', cex=0.5)
dev.off()

#2018 15-30 cm soil moisture depletion
#Fig 4b
tiff(file = file.path(results, 'Fig4b.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(depletion_vwc_2018_22)[2:ncol(depletion_vwc_2018_22)], '%b_%d_%Y'), depletion_vwc_2018_22[i,2:ncol(depletion_vwc_2018_22)], type='l', col=forage_terrain_energy$energy_colors[i], ylim=c(-0.55, 1.5), xaxt='n', xlab='', yaxt = 'n', ylab = 'Fraction of plant available water, 15-30 cm', xlim = as.Date(c('2017-11-25', '2018-05-03')))
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
text(x=as.Date('2018/5/01'), y= 1.4, label='b', cex=1)
#text(x=as.Date('2017/12/15'), y=1.4, label='WY2018', cex = 1.5)
axis(side = 4, at = c(0, 0.15, 0.3, 0.45, 0.6), labels = c('0', '10', '20', '30', '40'))
mtext("Precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.015, type='s', col='black', cex=0.5)
dev.off()

#15-30 cm soil T 2018
#Fig 6b
tiff(file = file.path(results, 'Fig6b.tif'), family = 'Times New Roman', width = 9, height = 3, units = 'in', res=res_plots, pointsize = 11, compression = 'lzw')
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_22cm_2018)[2:ncol(soilT_22cm_2018)], '%b_%d_%Y'), soilT_22cm_2018[i,2:ncol(soilT_22cm_2018)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('Soil temperature, 15-30 cm (' ~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2017-11-25', '2018-05-03')))
  } else {lines(as.Date(colnames(soilT_22cm_2018)[2:ncol(soilT_22cm_2018)], '%b_%d_%Y'), soilT_22cm_2018[i,2:ncol(soilT_22cm_2018)], col=forage_terrain_energy$energy_colors[i])}
}
#text(x=as.Date('2017/12/15'), y=30, label='WY2018', cex = 1.5)
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
abline(h=5, lty=2) #biologic zero line
text(x=as.Date('2018/5/01'), y=2.5, label='b', cex = 1)
#legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, lwd = 1.3)
dev.off()

#15-30 cm 2017 soil T
#Fig6a
tiff(file = file.path(results, 'Fig6a.tif'), family = 'Times New Roman', width = 9, height = 3, units = 'in', res=res_plots, pointsize = 11)
par(mar=c(2.25, 4.5, 0.5, 4.5))
for (i in 1:16) {
  if (i == 1) {
    plot(as.Date(colnames(soilT_22cm_2017)[2:ncol(soilT_22cm_2017)], '%b_%d_%Y'), soilT_22cm_2017[i,2:ncol(soilT_22cm_2017)], type='l', col=forage_terrain_energy$energy_colors[i], xaxt='n', xlab='', yaxt = 'n', ylab = expression('Soil temperature, 15-30 cm ('~degree*'C)'), ylim = c(0,32), xlim = as.Date(c('2016-11-25', '2017-05-03')))
  } else {lines(as.Date(colnames(soilT_22cm_2017)[2:ncol(soilT_22cm_2017)], '%b_%d_%Y'), soilT_22cm_2017[i,2:ncol(soilT_22cm_2017)], col=forage_terrain_energy$energy_colors[i])}
}
#text(x=as.Date('2016/12/15'), y=30, label='WY2017', cex = 1.5)
axis(side = 2, at=c(0, 10, 20, 30))
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/03'), by='months'), format = '%m/%d/%y')
#abline(h=30, lty=2)
abline(h=5, lty=2) #biologic zero
text(x=as.Date('2017/5/01'), y=2.5, label='a', cex = 1)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=colors, title = expression(paste('annual kWh ', m^-2)), inset=0.01, lwd=1.3)
dev.off()

#Figure 7a and b
list.files('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/')
depletion_T7_r2_2017 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2017biomass_7cm.csv', stringsAsFactors = FALSE)
depletion_T22_r2_2017 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2017biomass_22cm.csv', stringsAsFactors = FALSE)
depletion_T7_r2_2018 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2018biomass_7cm.csv', stringsAsFactors = FALSE)
depletion_T22_r2_2018 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2018biomass_22cm.csv', stringsAsFactors = FALSE)
depletion_T_r2_2017_2018 <- data.frame(dates=c(depletion_T7_r2_2017$dates, '2017-7-30', depletion_T7_r2_2018$dates), r2.depletion.vs.T7=c(depletion_T7_r2_2017$r2.SM.vs.T, NA, depletion_T7_r2_2018$r2.SM.vs.T), p.val.depletion.vs.T7=c(depletion_T7_r2_2017$p.value.SM.vs.T, NA, depletion_T7_r2_2018$p.value.SM.vs.T), slope.depletion.vs.T7=c(depletion_T7_r2_2017$slope.SM.vs.T, NA, depletion_T7_r2_2018$slope.SM.vs.T), r2.depletion.vs.T22=c(depletion_T22_r2_2017$r2.SM.vs.T, NA, depletion_T22_r2_2018$r2.SM.vs.T), p.val.depletion.vs.T22=c(depletion_T22_r2_2017$p.value.SM.vs.T, NA, depletion_T22_r2_2018$p.value.SM.vs.T), slope.depletion.vs.T22=c(depletion_T22_r2_2017$slope.SM.vs.T, NA, depletion_T22_r2_2018$slope.SM.vs.T))

#7 cm plot (Fig 7a in publication)
tiff(file = file.path(results, 'Fig7a.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots)
par(mar=c(1, 4.5, 0.5, 4.5))
plot(as.Date(depletion_T_r2_2017_2018$dates[depletion_T_r2_2017_2018$p.val.depletion.vs.T7 < 0.05], format='%Y-%m-%d'), depletion_T_r2_2017_2018$r2.depletion.vs.T7[depletion_T_r2_2017_2018$p.val.depletion.vs.T7 < 0.05], type = 'p', col=ifelse(depletion_T_r2_2017_2018$slope.depletion.vs.T7[depletion_T_r2_2017_2018$p.val.depletion.vs.T7 < 0.05] < 0, 'red', 'blue'), xaxt='n', xlab = "", ylim=c(0,1), ylab = '')
#axis(side=1, at=as.Date(c('2016-12-01', '2017-02-01', '2017-04-01', '2017-06-01', '2017-12-01', '2018-02-01', '2018-04-01', '2018-06-01')), labels=c('Dec 2016', 'Feb 2017', 'Apr 2017', 'Jun 2017', 'Dec 2017', 'Feb 2018', 'Apr 2018', 'June 2018'))
axis(side = 4, at = c(0, 0.0625, 0.125, 0.1875, 0.25), labels = c('0', '10', '20', '30', '40'))
mtext("Precipitation per day (mm)", side=4, line=2.5, at=0.3)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.00625, type='s', col='lightblue', cex=0.5)
legend('topright', legend = c('warmer = drier (p < 0.05)', 'colder = drier (p < 0.05)'), pch = 1, col=c('red', 'blue'), inset = 0.01)
mtext(text='Daily 0-15 cm PAW fraction variance', side=2, line=3.5)
mtext(text=expression('explained by soil temperature ('~R^2*')'), side=2, line=2.25)
text(x=as.Date('2016/12/01'), y=0.9, label='a', adj=c(0,0))
dev.off()

#22 cm plot (Fig 7b in publication)
tiff(file = file.path(results, 'Fig7b.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots)
par(mar=c(2.25, 4.5, 0.5, 4.5))
plot(as.Date(depletion_T_r2_2017_2018$dates[depletion_T_r2_2017_2018$p.val.depletion.vs.T22 < 0.05], format='%Y-%m-%d'), depletion_T_r2_2017_2018$r2.depletion.vs.T22[depletion_T_r2_2017_2018$p.val.depletion.vs.T22 < 0.05], type = 'p', col=ifelse(depletion_T_r2_2017_2018$slope.depletion.vs.T22[depletion_T_r2_2017_2018$p.val.depletion.vs.T22 < 0.05] < 0, 'red', 'blue'), xaxt='n', xlab = "", ylim=c(0,1), ylab = '')
axis(side=1, at=as.Date(c('2016-12-01', '2017-02-01', '2017-04-01', '2017-06-01', '2017-12-01', '2018-02-01', '2018-04-01', '2018-06-01')), labels=c('Dec 2016', 'Feb 2017', 'Apr 2017', 'Jun 2017', 'Dec 2017', 'Feb 2018', 'Apr 2018', 'June 2018'))
axis(side = 4, at = c(0, 0.0625, 0.125, 0.1875, 0.25), labels = c('0', '10', '20', '30', '40'))
mtext("Precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.00625, type='s', col='lightblue', cex=0.5)
legend('topright', legend = c('warmer = drier (p < 0.05)', 'colder = drier (p < 0.05)'), pch = 1, col=c('red', 'blue'), inset = 0.01)
mtext(text='Daily 15-30 cm PAW fraction variance', side=2, line=3.5)
mtext(text=expression('explained by soil temperature ('~R^2*')'), side=2, line=2.25)
text(x=as.Date('2016/12/01'), y=0.9, label='b', adj=c(0,0))
dev.off()

#revised Figure 8a (was Figure 9a in dissertation)
#avgsoilT_dates_2017 <- which(colnames(soilT_7cm_2017)=='Dec_01_2016'):which(colnames(soilT_7cm_2017)=='Mar_15_2017') #because location 13 lost on Mar 10th
#avgsoilT_2017 <- apply(soilT_7cm_2017[ ,avgsoilT_dates_2017], 1, mean)
start_date <- 'Dec_01_2016'
end_date <- 'Mar_14_2017'
normalizeVars <- FALSE
forage_growth <- 'clp031417'
dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_7cm_2017[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_22cm_2017[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results,'Fig8a.tif'), family = 'Times New Roman', width = 4.5, pointsize=11, height = 2.75, units = 'in', res=res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT[-13], df_0_30cm$growth_period[-13], col=forage_terrain_energy$energy_colors[-13], xlab = "", ylab = "", pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
mtext(text=expression(paste('Dec 1, 2016 - Mar 14, 2017, 0-30 cm soil temperature ('*degree*'C)')), side = 1, line=2.4, at=9.75)
mtext(text=expression('Mar 14, 2017 standing forage (kg '~~ha^-1*')'), side = 2, line = 2.2, at=1500)
abline(lm(df_0_30cm$growth_period[-13] ~ df_0_30cm$avg_soilT[-13]), lty=2)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), pch = 19, col=c('blue', 'orange2', 'red3'), pt.cex = 1.3, title = expression(paste('annual kWh ', m^-2)), inset=0.01, cex = 1)
#text(x=9.2, y=2600, label='linear model results', cex=1, adj=c(0,0))
text(x=9.2, y=2475, label=expression(R^2~'= 0.57, p value = 0.001'), cex = 1, adj = c(0, 0))
text(x=9.2, y=2275, label=expression('slope = 352 \u00B1 85 kg forage'~ha^-1~degree*C^-1), adj = c(0, 0), cex = 1)
#text(x=8.95, y=2125, label=expression('RMSE = 340 kg forage'~ha^-1), cex = 1, adj = c(0, 0))
text(x=11.15, y=800, label='a', adj=c(0, 0))
#text(avgsoilT_2017[-13], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[-13])
dev.off()
summary(lm(df_0_30cm$growth_period[-13] ~ df_0_30cm$avg_soilT[-13])) #verified matches plot on 4/17/19

#revised Figure 8b (was Figure 9b in dissertation)
start_date <- 'Dec_01_2016'
end_date <- 'Apr_10_2017'
normalizeVars <- FALSE
forage_growth <- 'clp041017'
dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_7cm_2017[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_22cm_2017[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig8b.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 2.75, units = 'in', res=res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, col=forage_terrain_energy$energy_colors[], xlab = "", ylab = "", pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
mtext(text = expression('Dec 1, 2016 - Apr 10, 2017, 0-30 cm soil temperature ('*degree*'C)'), side = 1, line=2.4, at=11)
mtext(text = expression('Apr 10, 2017 standing forage (kg '~~ha^-1*')'), side = 2, line=2.2, at=2550)
abline(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT), lty=2)
#legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=9.4, y=4400, label='27 days later', cex=1, adj=c(0,0))
text(x=9.4, y=4100, label='linear relationship non-significant', cex = 1, adj=c(0,0))
text(x=9.4, y=3800, label='p value = 0.75', cex = 1, adj=c(0,0))
text(x=12.5, y=1250, label='b', adj=c(0,0))
#text(x=7.9, y=2200, label=expression(paste(r^2, ' = 0.61')), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2100, label=expression(paste('RMSE = 326 kg ', ha^-1)), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2000, label=expression(paste('wet year mid-March standing forage (kg ', ha^-1, ') =' ~degree*'C * 360 - 1911')), adj = c(0, 0), cex = 1.1)
#text(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()
summary(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT)) #p=0.75 confirmed 4/17/19

#revised Figure 8c (was Figure 10a in Dissertation)
start_date <- 'Feb_15_2017'
end_date <- 'Mar_14_2017'
normalizeVars <- FALSE
forage_growth <- 'Mar2017growth'
dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_7cm_2017[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_22cm_2017[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig8c.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT[-13], df_0_30cm$growth_period[-13], col=forage_terrain_energy$energy_colors[-13], ylab="", xlab="", cex.axis=1, cex.lab=1, pch=19, cex=1.3, mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
mtext(text = expression('Feb 15 - Mar 14, 2017, 0-30 cm soil temperature ('*degree*'C)'), side = 1, line = 2.4, at=11.25)
mtext(text = expression('Feb 15 - Mar 14, 2017 growth (kg '~~ha^-1*')'), side = 2, line = 2.2, at=700)
#axis(side = 1, at=c(0.3, 0.4, 0.5, 0.6, 0.7), labels = c('30', '40', '50', '60', '70'), mgp=c(2.5, 1, 0))
#text(rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)), forage_terrain_energy$Apr2017growth, labels=round(forage_terrain_energy$elevation, 0), pos=1, offset = 0.5)
#abline(v=0.5, lty=2, col='grey')
abline(lm(df_0_30cm$growth_period[-13] ~ df_0_30cm$avg_soilT[-13]), lty=2)
#legend('topleft', legend=(c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), 'linear fit')), lty=c(NA, NA, NA, 2), pch = c(19,19,19,1), pt.cex = c(1.3, 1.3, 1.3, NA), col=c('blue', 'orange2', 'red3', 'black'), inset=0.01, cex = 1, y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
#text(x=9.5, y=1450, label='linear model results', cex=1, adj=c(0,0))
text(x=9.5, y=1400, label=expression(R^2~'= 0.49, p value = 0.004'), cex = 1, adj = c(0, 0)) #, RMSE = 234 kg'~ha^-1)
#text(x=8, y=1500, label=expression(paste('RMSE = 232 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=9.5, y=1250, label=expression('slope = 7.2 \u00B1 2.1  kg forage'~ha^-1~degree*C^-1~day^-1), adj = c(0, 0), cex = 1)
text(x=12.9, y=300, label='c', adj=c(0,0))
dev.off()

summary(lm(df_0_30cm$growth_period[-13] ~ df_0_30cm$avg_soilT[-13])) #R2=0.49; p value =0.004
#194.9 kg forage/deg C 27 days = 7.2 plus/minus 2.1
mean(forage_terrain_energy$Mar2017growth) / 27 #37 kg is in Fig 14 barplot, so 27 days is correct

#revised Figure 8d (was Figure 10b in Dissertation)
start_date <- 'Mar_15_2017'
end_date <- 'Apr_10_2017'
normalizeVars <- FALSE
forage_growth <- 'Apr2017growth'
dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_7cm_2017[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_22cm_2017[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig8d.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_depletion[-13], df_0_30cm$growth_period[-13], col=forage_terrain_energy$energy_colors[-13], ylab="", xaxt='n', yaxt='n', xlab="", cex.axis=1, cex.lab=1, pch=19, mgp=c(2.5, 1, 0), cex=1.3) #  cex=forage_terrain_energy$clp031417/750
axis(side = 1, at=c(0.3, 0.4, 0.5, 0.6), labels = c('30', '40', '50', '60'), mgp=c(2.5, 1, 0))
axis(side = 2, at=c(0, 500, 1000, 1500, 2000), labels = c('0', '', '1000', '', '2000'), mgp=c(2.5, 1, 0))
mtext(text = 'Mar 15 - Apr 10, 2017, 0-30 cm plant available water (%)', side = 1, line = 2.3, at=0.45)
mtext(text = expression('Mar 15 - Apr 10, 2017 growth (kg '~~ha^-1*')'), side = 2, line = 2.2, at=800)
#text(rowMeans(cbind(mean_depletion_Apr_growth, mean_depletion22_Apr_growth)), forage_terrain_energy$Apr2017growth, labels=round(forage_terrain_energy$elevation, 0), pos=1, offset = 0.5)
#abline(v=0.5, lty=2, col='grey')
abline(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_depletion), lty=2)
#legend('topleft', legend=(c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), 'linear fit')), lty=c(NA, NA, NA, 2), pch = c(19,19,19,1), pt.cex = c(1.5, 1.5, 1.5, NA), col=c('blue', 'orange2', 'red3', 'black'), inset=0.01, cex = 1, y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
#text(x=0.45, y=700, label='linear model results', cex=1, adj=c(0,0))
text(x=0.24, y=2000, label=expression(paste(R^2, ' = 0.44, p value = 0.007')), cex = 1, adj = c(0, 0))
#text(x=0.25, y=300, label=expression(paste('RMSE = 594 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=0.65, y=0, label='d', adj=c(0,0))
dev.off()

summary(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_depletion))
#verified lm summary matches plot on 4/17/19


#revised Figure 9a (was Figure 11a in Dissertation)
start_date <- 'Dec_01_2017'
end_date <- 'Mar_22_2018'
normalizeVars <- FALSE
forage_growth <- 'clp032218'
dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_7cm_2018[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_22cm_2018[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2018[ ,dates], 1, mean), apply(soilT_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig9a.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 2.75, units = 'in', res=res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, col=forage_terrain_energy$energy_colors, xlab = "", ylab = "", pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
mtext(text = expression('Dec 1, 2017 - Mar 22, 2018, 0-30 cm soil temperature ('*degree*'C)'), side = 1, line = 2.4, at = 11.75)
mtext(text = expression('Mar 22, 2018 standing forage (kg '~~ha^-1*')'), side = 2, line = 2.2, at=400)
abline(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT), lty=2)
curve(nlm_2018(x, 'growth_period', df_0_30cm), from=min(df_0_30cm$avg_soilT), to=max(df_0_30cm$avg_soilT), lty=2, add=TRUE)
#legend(x=9.5, y=500, legend=(c("< 1200", '1200-1410', '>1410')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, pt.cex = 1.3, cex = 1)
text(x=9.5, y=500, label='linear model results', cex = 1, adj = c(0, 0))
text(x=9.5, y=400, label=expression(paste(R^2, ' = 0.26, p value = 0.05')), cex = 1, adj = c(0, 0))
#text(x=8.1, y=75, label=expression(paste('RMSE = 226 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=9.5, y=250, label='non-linear model results', cex = 1, adj = c(0, 0))
text(x=9.5, y=150, label=expression(paste(R^2, ' = 0.50, p value = 0.01')), cex = 1, adj = c(0, 0))
#text(x=11.25, y=75, label=expression(paste('RMSE = 199 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=14.8, y=100, label='a', adj=c(0,0))
#text(x=7.9, y=130, label=expression(paste('dry year mid-March standing forage (kg ', ha^-1, ') = deg C * -57 + 1181')), adj = c(0, 0), cex = 1.2)
#text(avgsoilT_2018[], forage_terrain_energy$clp032218[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()
summary(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT)) #r^2=0.26, p value = 0.05
summary(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT + I(df_0_30cm$avg_soilT^2))) #R^2=0.50, p value = 0.01

#revised Figure 9b (was Figure 11b in dissertation)
start_date <- 'Dec_01_2017'
end_date <- 'Apr_15_2018'
normalizeVars <- FALSE
forage_growth <- 'clp041518'
dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_7cm_2018[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_22cm_2018[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2018[ ,dates], 1, mean), apply(soilT_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig9b.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 2.75, units = 'in', res=res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, col=forage_terrain_energy$energy_colors[], xlab = "", ylab = "", yaxt='n', pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
mtext(text = expression('Dec 1, 2017 - Apr 15, 2018, 0-30 cm soil temperature ('*degree*'C)'), side = 1, line=2.4, at=12.75)
mtext(text = expression('Apr 15, 2018 standing forage (kg '~~ha^-1*')'), side = 2, line = 2.2, at = 900)
axis(side = 2, at=c(600, 800, 1000, 1200, 1400), labels = c('600', '', '1000', '',  '1400'), mgp=c(2.5, 1, 0))
curve(nlm_2018(x, 'growth_period', df_0_30cm), from=min(df_0_30cm$avg_soilT), to=max(df_0_30cm$avg_soilT), lty=2, add = TRUE)
legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1)
text(x=11.8, y=1000, label='24 days later', cex=1, adj = c(0, 0))
text(x=11.8, y=850, label='non-linear model results', cex = 1, adj = c(0, 0))
text(x=11.8, y=700, label=expression(paste(R^2, ' = 0.62, p value = 0.002')), cex = 1, adj = c(0, 0))
#text(x=9.3, y=1275, label=expression(paste('RMSE = 242 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=15.6, y=500, label='b', adj=c(0,0))
#text(x=7.9, y=130, label=expression(paste('dry year mid-March standing forage (kg ', ha^-1, ') = deg C * -57 + 1181')), adj = c(0, 0), cex = 1.2)
#text(avgsoilT_2018[], forage_terrain_energy$clp041518[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()
summary(lm(df_0_30cm$growth_period[] ~ df_0_30cm$avg_soilT[] + I(df_0_30cm$avg_soilT[]^2))) #verified matches plot text on 4/17/19; different results than dissertation because this is 0-30 cm soil temperature

#revised Figure 9c for 0-30 cm (was Figure 11a for 0-15 cm in dissertation)
start_date <- 'Feb_16_2018'
end_date <- 'Mar_22_2018'
normalizeVars <- FALSE
forage_growth <- 'Mar2018growth'
dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_7cm_2018[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_22cm_2018[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2018[ ,dates], 1, mean), apply(soilT_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)
cex.adjuster <- 5

tiff(file = file.path(results, 'Fig9c.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, ylab="", xlab="", cex.axis=1, xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_7cm$avg_depletion < 0.1, 0.1*cex.adjuster, df_7cm$avg_depletion * cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
mtext(text = expression('Feb 16-Mar 22, 2018, 0-30 cm soil temperature ('*degree*'C)'), side = 1, line = 2.4, at=11.5)
mtext(text = expression('Feb 16-Mar 22, 2018 growth (kg '~~ha^-1*')'), side = 2, line = 2.2, at = 100)
abline(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT), lty=2)
legend('bottomleft', legend=c('<10% PAW, period mean', '25% PAW, period mean', '50% PAW, period mean'), pch = c(1,1,1), pt.cex = c(0.1*cex.adjuster, 0.25*cex.adjuster, 0.5*cex.adjuster), col=c('black', 'black', 'black'), inset=0.01, cex = 1)#, bty = 'n') #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
#text(x=12.5, y=580, label='linear model results', cex=1, adj=c(0,0))
text(x=12, y=450, label=expression(paste(R^2, ' = 0.29, p value = 0.03')), cex = 1, adj = c(0, 0))
#text(x=12.5, y=430, label=expression(paste('RMSE = 222 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=14.8, y=-250, label='c', adj=c(0,0))
#rect(xleft = 7.9, ytop = .05, xright = 12.15, ybottom = -0.22)
#abline(v=13, lty=2)
#text(x=9, y=0.41, label=expression('< 13'~degree*'C soil temperature'), cex=1, adj=c(0,0))
#text(x=9, y=0.38, label='soil temperature', cex=1, adj=c(0,0))
#text(x=9, y=0.37, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
#text(x=13.1, y=0.41, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
#text(x=13.1, y=0.375, label='soil temperature', cex=1, adj=c(0,0))
#text(x=13.1, y=0.33, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()
summary(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT)) #verified matches plot on 4/17/19: R2=0.29, p-value=0.03; RMSE=222 kg ha

#revised Figure 9d for 0-30 cm data (was Figure 11b and 0-15 cm in Dissertation)
start_date <- 'Mar_23_2018'
end_date <- 'Apr_15_2018'
normalizeVars <- FALSE
forage_growth <- 'Apr2018growth'
dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_7cm_2018[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_22cm_2018[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2018[ ,dates], 1, mean), apply(soilT_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)
cex.adjuster <- 5
tiff(file = file.path(results, 'Fig9d.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = res_plots)
par(mar=c(3.5, 3.5, 0.8, 0.8))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, ylab="", xlab="", cex.axis=1, cex.lab=1, pch=19, cex=df_0_30cm$avg_depletion*cex.adjuster, col=forage_terrain_energy$energy_colors)
mtext(text = expression('Mar 23-Apr 15, 2018, 0-30 cm soil temperature ('*degree*'C)'), side = 1, line = 2.4, at=16.8)
mtext(text = expression('Mar 23-Apr 15, 2018 growth (kg '~~ha^-1*')'), side = 2, line = 2.2, at=300)
abline(lm(df_0_30cm$growth_period ~ df_0_30cm$avg_soilT), lty = 2)
#axis(side = 2, at=c(0, 200, 400, 600, 800), labels = c('0', '200', '400', '600', '800'), mgp=c(2.5, 1, 0))
#legend('bottomright', legend=c('40% PAW', '55% PAW', '70% PAW'), pch = c(1,1,1), pt.cex = c(0.4*cex.adjuster, 0.55*cex.adjuster, 0.7*cex.adjuster), col='black', inset=0.01, cex = 1, title = '0-30 cm mean\nplant available water\nMar 23-Apr 15, 2018', bty='n') #used legend from March 2018 plot
#rect(xleft = 17.4, ybottom = -162, xright = 19.71, ytop = 275)
#text(x=14.1, y=825, label='linear model results', cex=1, adj=c(0,0))
text(x=14.1, y=750, label=expression(paste(R^2, ' = 0.38, p value = 0.01')), cex = 1, adj = c(0, 0))
#text(x=14.1, y=675, label=expression(paste('RMSE = 221 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=19.4, y=-100, label='d', adj=c(0,0))
dev.off()
summary(lm(growth_period ~ avg_soilT, data=df_0_30cm)) #R2=0.38, p-value=0.01, RMSE=221
summary(lm(growth_period ~ avg_soilT, data=df_7cm)) #because avg_soilT is a global variable
summary(lm(df_7cm$growth_period ~ df_7cm$avg_soilT))

#Fig 10 (was Fig 14 in Dissertation); analysis script in SM_T_interaction.vs.forage_growth.R
summary_0_30 <- data.frame(model_type=c('soil moisture (direct association)', 'soil temperature (direct assocition)', 'interactive associations'), Mar2017=c(0, 0.49, 0.08), Apr2017=c(0.44, 0, 0.08), Mar2018=c(0, 0.29, 0.26), Apr2018=c(0, 0.38, 0.1))
#make a bar plot that synthesizes results
tiff(file = file.path(results, 'Fig10.tif'), pointsize = 11, family = 'Times New Roman', width = 4, height = 4, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 3.5, 1, 1))
barplot(as.matrix(summary_0_30[,2:ncol(summary_0_30)]), beside = FALSE, col=c('lightblue', 'orange', 'lightgrey'), ylim=c(0,1), ylab = '', legend.text = c('soil moisture availability (direct association)', 'soil temperature (direct association)', 'interactive associations'), cex.axis = 1, cex.names = 1, cex.lab = 1, names.arg = c('Mar 2017\ngrowth', 'Apr 2017\ngrowth', 'Mar 2018\ngrowth', 'Apr 2018\ngrowth'), args.legend = list(x="topleft", inset=0.005, cex=1))
mtext(expression('Forage growth variance explained '~R^2), side=2, line=2)
text(0.7, 0.2, "+", cex=1.5)
text(1.9, 0.2, "+", cex=1.5)
text(3.1, 0.2, '-', cex=1.5)
text(4.3, 0.2, '+', cex=1.5)
#text(0.7, 0.65, expression(atop("37 \u00B1 20", kg~ha^-1~d^-1)))
text(0.05, 0.59, expression("37 \u00B1 20"~kg~ha^-1~day^-1), adj = c(0,0))
text(1.9, 0.55, "33 \u00B1 29")
text(3.1, 0.58, '8 \u00B1 7')
text(4.3, 0.51, '18 \u00B1 11')
dev.off()



#Figure S1
barcenters <- barplot(as.matrix(precip_summary_GS), beside = TRUE, col=c('blue', 'red3'), ylab = 'Precipitation (mm)', legend.text = c('2016-17 (wet)', '2017-18 (dry)'), cex.axis = 1, cex.names = 1, cex.lab = 1, args.legend = list(x="topleft", inset=0.1, cex=1))
tiff(file = file.path(results, 'FigS1.tif'), family = 'Times New Roman', pointsize = 11, width = 6.5, height = 4.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(3, 5, 1, 1))
barplot(as.matrix(precip_summary_GS), beside = TRUE, col=c('lightblue3', 'red3'), ylab = 'Precipitation (mm)', legend.text = c('2016-17 (wet)', '2017-18 (dry)'), ylim = c(0,320), cex.axis = 1, cex.names = 1, cex.lab = 1, args.legend = list(x="topleft", inset=0.1, cex=1))
segments(x0=barcenters[1,], y0=as.matrix(precip_summary_GS)[1,] + qnorm(0.975) * as.matrix(precip_error_summary)[1,] / sqrt(3), x1=barcenters[1,], y1=as.matrix(precip_summary_GS)[1,] + qnorm(0.025) * as.matrix(precip_error_summary)[1,] / sqrt(3), lwd = 1.2)
segments(x0=barcenters[2,], y0=as.matrix(precip_summary_GS)[2,] + qnorm(0.975) * as.matrix(precip_error_summary)[2,] / sqrt(3), x1=barcenters[2,], y1=as.matrix(precip_summary_GS)[2,] + qnorm(0.025) * as.matrix(precip_error_summary)[2,] / sqrt(3), lwd = 1.2)
arrows(x0=barcenters[1,], y0=as.matrix(precip_summary_GS)[1,] + qnorm(0.975) * as.matrix(precip_error_summary)[1,] / sqrt(3), x1=barcenters[1,], y1=as.matrix(precip_summary_GS)[1,] + qnorm(0.025) * as.matrix(precip_error_summary)[1,] / sqrt(3), lwd = 1.2, angle = 90, code = 3, length = 0.05)
arrows(x0=barcenters[2,], y0=as.matrix(precip_summary_GS)[2,] + qnorm(0.975) * as.matrix(precip_error_summary)[2,] / sqrt(3), x1=barcenters[2,], y1=as.matrix(precip_summary_GS)[2,] + qnorm(0.025) * as.matrix(precip_error_summary)[2,] / sqrt(3), lwd = 1.2, angle = 90, code = 3, length = 0.05)
dev.off()

#make a 1980-2018 plot
tiff(file = file.path(results, 'FigS2.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(WY_summary_GS$year[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)], WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)], ylab = '', xlab = '', type = 'b')
mtext(text = 'Growing season precipitation (Oct - May, mm)', side = 2, line = 2.4, at=310)
mtext(text = 'Water year', side = 1, line = 2.4, at=1995)
abline(h=quantile(WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)], probs=0.1), lty=2, col='red3')
abline(h=quantile(WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)], probs=0.9), lty=2, col='lightblue3')
abline(h=median(WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)]), lty=1, col='black')
text(1988, quantile(WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)], probs=0.9), '90th percentile', pos=3, offset = 0.3)
text(1986, median(WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)]), 'median', pos=3, offset = 0.3)
text(1995, quantile(WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==1975):nrow(WY_summary_GS)], probs=0.1), '10th percentile', pos=3, offset=0.3)
arrows(x0=2016, y0=WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==2017)] + 50, x1 = 2017, y1 = WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==2017)], length=0.05)
text(2016, WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==2017)] + 50, 'wet study yr', pos=3, offset=0.3)
arrows(x0=2016.75, y0=WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==2018)] - 50, x1 = 2018, y1 = WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==2018)], length=0.05)
text(2016.75, WY_summary_GS$precip_GS_mm[which(WY_summary_GS$year==2018)] - 50, 'dry study yr', pos=1, offset=0.3)
dev.off()

#Figure S3 as of 7/10/19
library(ggplot2)
species_comp <- read.csv('C:/Users/smdevine/Desktop/rangeland project/clip_plots/Camatta_species_comp_2017.csv', stringsAsFactors = FALSE)
species_comp$Composition_perc <- species_comp$Composition_perc * 100
species_comp$Genus[species_comp$Genus=='Vulpia'] <- 'Festuca' #per Royce Larsen's recommendaton
tapply(species_comp$Composition_perc, list(species_comp$Date, species_comp$Site_class), sum)
tapply(species_comp$Common_Name, list(species_comp$Date, species_comp$Site_class), length)

species_by_date <- as.data.frame(tapply(species_comp$Composition_perc, list(species_comp$Date, species_comp$Common_Name), mean))
species_by_date[1, order(species_by_date[1,])]
species_by_date[2, order(species_by_date[2,])]
species_by_date[3, order(species_by_date[3,])]
tapply(species_comp$Composition_perc, species_comp$Common_Name, mean)
unique(species_comp$Genus)
unique(species_comp$Species)
legend_labels <- species_comp[1:9,]
legend_labels <- legend_labels[order(legend_labels$Dummy_var),]
legend_labels <- paste(legend_labels$Genus, legend_labels$Species)

tiff(file = file.path(results,'FigS3.tif'), family = 'Times New Roman', width = 7.5, height = 6.5, units = 'in', pointsize = 11, res=res_plots, compression = 'lzw')
ggplot(data=species_comp) +
  geom_col(aes(y = Composition_perc, x = Site_class, fill = as.character(Dummy_var)), position='stack') +
  labs(x = 'Landscape position', y = 'Species composition (%)') + #title= 'Species composition in 2017 by date and landscape position'
  scale_fill_manual(values=c('forestgreen', 'springgreen', 'lightgreen', 'thistle', 'gold1', 'goldenrod2', 'lemonchiffon', 'rosybrown', 'brown'), name ='Species', labels=legend_labels) +
  theme_classic(base_size = 11) +
  theme(legend.position = 'bottom')  +
  #theme(legend.margin = unit(c(0.25, 0.5, 0.25, 0.25), 'in')) +
  facet_grid( ~ Date) +
  theme(plot.margin = unit(c(0.1, 0.75, 0.1, 0.3), 'in'))
dev.off()

#Fig S4 now as of 7/10/19
#set up data for plot
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

tiff(file = file.path(results, 'FigS4.tif'), family = 'Times New Roman', width = 6.5, height = 4.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(3.5, 4.5, 1, 1))
boxplot(days.below.AD2017, days.below.AD2017_22, days.below.AD2018, days.below.AD2018_22, ylim=c(0, length(seq.Date(as.Date(date1_2018, '%b_%d_%Y'), as.Date(date2_2018, '%b_%d_%Y'), by='day'))), xaxt='n', ylab = 'Days below 50% plant available water (Dec 1-Apr 15)', xlab = '', pars = list(boxwex=0.6, staplewex=0.4, outwex=0.4))
abline(v=2.5, lty=2)
axis(side = 1, at = c(1, 2, 3, 4), labels = FALSE, tick = TRUE)
mtext(text = c('2016-17 growing season', '2017-18 growing season'), side = 1, at=c(1.5, 3.5), line=2.5, cex=1)
mtext(text = c('0-15 cm', '15-30 cm', '0-15 cm', '15-30 cm'), side = 1, at = c(1, 2, 3, 4), line=1, cex = 1)
#legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), pch = 1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), inset=0.1, pt.cex = 1.2)
dev.off()

#Fig S5 now as of 7/10/19
#makes a plot of peak2017 vs. peak2018
forage_terrain_energy$curvature_cex <- ifelse(forage_terrain_energy$curvature_mean < -0.2, 1, ifelse(forage_terrain_energy$curvature_mean > -0.2 & forage_terrain_energy$curvature_mean < 0.2, 1.5, 2.5))
tiff(file = file.path(results, 'FigS5.tif', sep = ''), family = 'Times New Roman', width = 6.5, height = 5.5, pointsize = 11, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 1, 1))
plot(forage_terrain_energy$peak2017, forage_terrain_energy$peak2018, xlab=expression(paste('2017 peak forage (kg', ' ', ha^-1, ')')), ylab=expression(paste('2018 peak forage (kg', ' ', ha^-1, ')')), pch=19, cex=forage_terrain_energy$curvature_cex, ylim = c(300, 1600), xlim=c(1400, 4700), col=forage_terrain_energy$energy_colors, cex.axis=1, cex.lab=1)
abline(h=median(forage_terrain_energy$peak2018), lty=2)
abline(v=median(forage_terrain_energy$peak2017), lty=2)
#text(x=forage_terrain_energy$peak2017, y=forage_terrain_energy$peak2018, labels = forage_terrain_energy$landform_code, pos = 1, offset = 0.3)
#legend(x=2000, y=1625, legend=(c("< 1254", '1254-1341', "1341-1458", '>1458')), pch=19, col=c('blue', 'lightblue2', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2))) #4 color energy legend
legend(x=2800, y=1625, legend=(c("< 1200", '1200-1410', '>1410')), pch=19, pt.cex = c(1.5, 1.5, 1.5), col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^-2)), cex = 1) #3 color energy legend
legend(x=3700, y=1625, legend=(c("concave", 'linear', "convex")), pt.cex=c(1, 1.5, 2.5), pch=c(1, 1, 1), col=c('black', 'black', 'black'), title = 'Mean curvature', cex=1)
text(x=4000, y=650, label=paste('% slope 26', '\u00B1', '9'), cex=1)
text(x=4000, y=600, label=paste('elev 485', '\u00B1', '8'), cex=1)
text(x=4000, y=1300, label=paste('% slope 17', '\u00B1', '6'), cex=1)
text(x=4000, y=1250, label=paste('elev 490', '\u00B1', '10'), cex=1)
text(x=2000, y=1375, label=paste('% slope 20', '\u00B1', '7'), cex= 1)
text(x=2000, y=1325, label=paste('elev 496', '\u00B1', '6'), cex=1)
text(x=2000, y=650, label=paste('% slope 26', '\u00B1', '10'), cex=1)
text(x=2000, y=600, label=paste('elev 488', '\u00B1', '6'), cex=1)
#legend(x=3600, y=1625, legend=(c("1 = footslope", '2 = backslope', "3 = shoulder", '4 = summit')), title = 'hillslope position')
dev.off()