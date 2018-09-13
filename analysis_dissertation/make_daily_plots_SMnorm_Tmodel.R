library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SMnorm_T_model_results'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'

depth <- 7
yr <- 2017
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
lm.SMnorm_T_vs_biomass <- summary(lm(forage_data$clp031417 ~ vwc_data_normalized$Jan_08_2017 + soilT_data$Jan_08_2017 + forage_terrain$slope))

lm.SMnorm_T_vs_biomass.v2 <- summary(lm(forage_data$clp031417[!forage_data$location==13] ~ vwc_data_normalized$Jan_11_2017[!vwc_data_normalized$location==13] + soilT_data$Jan_11_2017[!soilT_data$location==13]))
plot(soilT_data$Jan_11_2017, forage_terrain$clp031417)
lm_biomass_vs_T <- summary(lm(forage_data$clp031417 ~ soilT_data$Jan_11_2017))
plot(vwc_data_normalized$Jan_11_2017, lm_biomass_vs_T$residuals)
persp(vwc_data_normalized$Jan_11_2017, soilT_data$Jan_11_2017, forage_terrain$clp031417)

#function to plot daily effects
sensor.date <- 'Dec_16_2016'
clip.date <- 'clp031417'
mag.factor <- 1000
lm_results <- lm(forage_data[[clip.date]] ~ vwc_data_normalized[[sensor.date]] + soilT_data[[sensor.date]])
SMnorm_effect <- function(x) {lm_results$coefficients[[2]] * x - mean(lm_results$coefficients[[2]] * x)}
SMnorm_effect(vwc_data_normalized[[sensor.date]])
T_effect <- function(x) {lm_results$coefficients[[3]] * x - mean(lm_results$coefficients[[3]] * x)}
T_effect(soilT_data[[sensor.date]])
plot(SMnorm_effect(vwc_data_normalized[[sensor.date]]), T_effect(soilT_data[[sensor.date]]), xlab='soil moisture effect (kg /ha)', ylab='soil temperature effect (kg /ha)', main = paste(sensor.date, 'vs.', clip.date), cex = forage_data[[clip.date]] / mag.factor)
plot(vwc_data_normalized[[sensor.date]], soilT_data[[sensor.date]], ylab='soil temperature (deg C)', xlab='normalized soil moisture (std dev)', main = paste(sensor.date, 'vs.', clip.date), cex=forage_data[[clip.date]] / mag.factor)
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

