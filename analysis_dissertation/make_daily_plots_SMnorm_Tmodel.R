library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SMnorm_T_model_results'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'

depth <- 7
yr <- 2018
forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018.by.sensor.csv'), stringsAsFactors=FALSE)
list.files(file.path(results, 'terrain_characteristics'))
terrain_chars <- read.csv(file.path(results, 'terrain_characteristics', "sensor_terrain5mNov2016.csv"), stringsAsFactors = FALSE) #this was made with a 1.5 m buffer with the raster 'extract' function in terrain_analysis.R
colnames(terrain_chars)[1] <- 'location'
forage_terrain <- merge(forage_data, terrain_chars, by='location')
if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was jacked
}
vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
vwc_data_normalized <- vwc_data
vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE) #normalize vwc data
soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)

#function to plot daily effects
sensor.date <- 'Feb_10_2018'
clip.date <- 'clp041518'
mag.factor <- 1000
lm_results <- lm(forage_data[[clip.date]] ~ vwc_data_normalized[[sensor.date]] + soilT_data[[sensor.date]])
SMnorm_effect <- function(x) {
  y <- lm_results$coefficients[[2]] * x - mean(lm_results$coefficients[[2]] * x, na.rm=TRUE)}
SMnorm_effect(vwc_data_normalized[[sensor.date]])
T_effect <- function(x) {lm_results$coefficients[[3]] * x - mean(lm_results$coefficients[[3]] * x, na.rm=TRUE)} #T data missing from point 9 from 1/31-2/13/18
T_effect(soilT_data[[sensor.date]])
plot(SMnorm_effect(vwc_data_normalized[[sensor.date]]), T_effect(soilT_data[[sensor.date]]), xlab='soil moisture effect (kg /ha)', ylab='soil temperature effect (kg /ha)', main = paste(sensor.date, 'vs.', clip.date))#, cex = forage_data[[clip.date]] / mag.factor)
plot(vwc_data_normalized[[sensor.date]], soilT_data[[sensor.date]], ylab='soil temperature (deg C)', xlab='normalized soil moisture (std dev)', main = paste(sensor.date, 'vs.', clip.date))#, cex=forage_data[[clip.date]] / mag.factor)
lm_results$SMnorm_residuals <- lm_results$residuals + SMnorm_effect(vwc_data_normalized[[sensor.date]])
lm_results$T_residuals <- lm_results$residuals + T_effect(soilT_data[[sensor.date]])
plot(lm_results$fitted.values, forage_data[[clip.date]], type='p', xlab="fitted value (kg / ha)", ylab="actual (kg / ha)", main = paste(sensor.date, 'vs.', clip.date))
abline(0, 1, col='green', lty=2, lwd=2)
plot(vwc_data_normalized[[sensor.date]], lm_results$SMnorm_residuals, type='p', xlab="normalized soil moisture", ylab="biomass association with 7 cm normalized soil moisture (kg / ha)", main = paste(sensor.date, 'vs.', clip.date))
curve(SMnorm_effect, from = min(vwc_data_normalized[[sensor.date]]), to = max(vwc_data_normalized[[sensor.date]]), add=TRUE, type='l', col='red', lty=2, lwd=2)
text(vwc_data_normalized[[sensor.date]], lm_results$SMnorm_residuals, labels = round(forage_terrain$aspect, 0), pos = 1, offset = 0.5, cex = 0.7)
plot(soilT_data[[sensor.date]], lm_results$T_residuals, type='p', xlab="soil temperature at 7 cm", ylab="biomass association with 7 cm soil temperature (kg / ha)", main = paste(sensor.date, 'vs.', clip.date))
curve(T_effect, from = min(soilT_data[[sensor.date]]), to = max(soilT_data[[sensor.date]]), add=TRUE, type='l', col='red', lty=2, lwd=2)
text(soilT_data[[sensor.date]], lm_results$T_residuals, labels = round(forage_terrain$aspect, 0), pos = 1, offset = 0.5, cex = 0.7)

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

#check some relationships between 2017 and 2018 wetting events
plot(vwc_data_normalized_2017$)

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

