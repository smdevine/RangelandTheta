forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
terrainDir <- file.path(results, 'terrain_characteristics')
library(raster)
library(car)
#terrain characteristic analysis
#read-in terrain characteristics from filtered 3m or 9m DEM from S Hogan, Mar 2017, 3m filtered from Grace Liu Nov 2016 or 10 m DEM from USGS
list.files(file.path(results, 'terrain_characteristics'))
terrain_chars <- read.csv(file.path(results, 'terrain_characteristics', "terrain_3m_filtered_Mar2017.csv"), stringsAsFactors = FALSE)
forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018.by.sensor.csv'), stringsAsFactors=FALSE)
#combine with sensor characteristics
forage_terrain <- merge(forage_data, terrain_chars, by='location')
colnames(forage_terrain)
#function to construct table of r2 and p-values for simple linear regression
# x <- forage_terrain_energy$clp050117
# y <- 'annual_kwh.m2'
get_stats <- function(x, y) {
  model.result <- summary(lm(x ~ forage_terrain_energy[ ,y]))
  df <- data.frame(col1 = round(model.result$coefficients[2, 4], 3), col2 = round(model.result$coefficients[2, 1], 3), col3 = round(model.result$r.squared, 2), col4 = sqrt(sum(model.result$residuals^2)/model.result$df[2]))
  colnames(df) <- c(paste0(y, '.p.val'), paste0(y, '.effect'), paste0(y, '.r2'), 'RMSE')
  df
}

#multiple regression
# x <- forage_terrain_energy$clp050117
# df_input <- forage_terrain_energy
# a <- 'annual_kwh.m2'
# b <- 'slope'
# c <- 'curvature_mean'
# d <- 'elevation'
get_stats_3multiple <- function(x, df_input, a, b, c) {
  #lm.result <- lm(x ~ df_input[ ,a] + df_input[ ,b] + df_input[ ,c])
  model.result <- summary(lm(x ~ df_input[ ,a] + df_input[ ,b] + df_input[ ,c]))
  df <- data.frame(col1 = pf(model.result$fstatistic[1], model.result$fstatistic[2], model.result$fstatistic[3], lower.tail = FALSE), col2 = round(model.result$r.squared, 2), col3 = round(model.result$coefficients[2, 4], 3), col4 = round(model.result$coefficients[2, 1], 3), col5 = round(model.result$coefficients[3, 4], 3), col6 = round(model.result$coefficients[3, 1], 3), col7 = round(model.result$coefficients[4, 4], 3), col8 = round(model.result$coefficients[4, 1], 3), col9=sqrt(sum(model.result$residuals^2)/model.result$df[2]))
  colnames(df) <- c('model.p.val', 'model.r2', paste0(a, '.p.val'), paste0(a, '.effect'), paste0(b, '.p.val'), paste0(b, '.effect'), paste0(c, '.p.val'), paste0(c, '.effect'), 'RMSE') #R calls the RMSE the RSE
  df
} 
#vif results
#df_input[, a] df_input[, b] df_input[, c] 
#2.897854      2.900142      1.004274 


get_stats_4multiple <- function(x, df_input, a, b, c, d) {
  lm.result <- lm(x ~ df_input[ ,a] + df_input[ ,b] + df_input[ ,c] + df_input[ ,d])
  model.result <- summary(lm(x ~ df_input[ ,a] + df_input[ ,b] + df_input[ ,c] + df_input[ ,d]))
  df <- data.frame(col1 = pf(model.result$fstatistic[1], model.result$fstatistic[2], model.result$fstatistic[3], lower.tail = FALSE), col2 = round(model.result$r.squared, 2), col3 = round(model.result$coefficients[2, 4], 3), col4 = round(model.result$coefficients[2, 1], 3), col5 = round(model.result$coefficients[3, 4], 3), col6 = round(model.result$coefficients[3, 1], 3), col7 = round(model.result$coefficients[4, 4], 3), col8 = round(model.result$coefficients[4, 1], 3), col9 = round(model.result$coefficients[5, 4], 3), col10 = round(model.result$coefficients[5, 1], 3), col11=sqrt(sum(model.result$residuals^2)/model.result$df[2]))
  colnames(df) <- c('model.p.val', 'model.r2', paste0(a, '.p.val'), paste0(a, '.effect'), paste0(b, '.p.val'), paste0(b, '.effect'), paste0(c, '.p.val'), paste0(c, '.effect'), paste0(d, '.p.val'), paste0(d, '.effect'), 'RMSE')
  df
}
#vif results
#df_input[, a] df_input[, b] df_input[, c] df_input[, d] 
#2.897872      2.911697      1.151569      1.162040

#multiple regression on only sensor pts with aspect as opposed to annual energy input
# mult.lm.results <- do.call(rbind, lapply(forage_terrain[,2:9], get_stats_multiple, df_input = forage_terrain, a='aspect', b='slope', c='curvature_mean'))
# mult.lm.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18')
# mult.lm.results
# write.csv(mult.lm.results, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_multlm_results.9mfiltered_aspect.csv'), row.names = FALSE)

#multiple regression on all pts
# mult.lm.results <- do.call(rbind, lapply(forage_terrain_allpts[,2:5], get_stats_multiple, df_input = forage_terrain_allpts, a='aspect', b='slope', c='curvature_mean'))
# mult.lm.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17')
# mult.lm.results
#write.csv(mult.lm.results, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_multlm_results.9.11.18.csv'), row.names = FALSE)

#read-in solar radiation values
list.files(file.path(terrainDir, 'solrad_analysis'))
solrad_9m <- shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_9m_v2.shp')) #all v2 files were sky size 500 x 500 and 64 calc directions
solrad_3m <- shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_3m_v2.shp'))
solrad_3m_df <- as.data.frame(solrad_3m)
colnames(solrad_3m_df)
solrad_3m_df$location <- 1:16
solrad_3m_df$annual_kwh.m2 <- apply(solrad_3m_df[ ,1:52], 1, sum) / 1000
length(seq.Date(as.Date('Oct_01_2017', '%b_%d_%Y'), as.Date('Apr_15_2018', '%b_%d_%Y'), by='day')) #197 days
format(as.Date('Oct_01_2017', '%b_%d_%Y'), '%V') #so, T38-T51
format(as.Date('Apr_15_2018', '%b_%d_%Y'), '%V') #and T0-T15
solrad_3m_df$seasonal_kwh.m2 <- apply(solrad_3m_df[ ,c(1:16, 39:52)], 1, sum) / 1000
plot(solrad_3m_df$annual_kwh.m2, solrad_3m_df$seasonal_kwh.m2)
solrad_9m_df <- as.data.frame(solrad_9m)
colnames(solrad_9m_df)
solrad_9m_df$location <- 1:16
solrad_9m_df$annual_kwh.m2 <- apply(solrad_9m_df[ ,1:52], 1, sum) / 1000
solrad_9m_df$seasonal_kwh.m2 <- apply(solrad_9m_df[ ,c(1:16, 39:52)], 1, sum) / 1000
plot(solrad_9m_df$annual_kwh.m2, solrad_9m_df$seasonal_kwh.m2)
plot(apply(solrad_9m_df[,1:52], 1, sum), apply(solrad_3m_df[ ,1:52], 1, sum))
abline(0,1)
summary(lm(apply(solrad_9m_df[,1:52], 1, sum) ~ apply(solrad_3m_df[ ,1:52], 1, sum)))
summary((apply(solrad_9m_df[,1:52], 1, sum) / 1000) / 365)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.016   3.487   3.709   3.663   3.979   4.037
summary((apply(solrad_3m_df[,1:52], 1, sum) / 1000) / 365)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v2 from Hogan)
#3.019   3.435   3.674   3.645   3.994   4.032 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v2 from Grace)
#3.083   3.452   3.682   3.659   3.983   4.023
solrad_10m <- shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_10m_5000.shp')) #v2 was with sky size of 500 and 64 calc directions
solrad_10m_df <- as.data.frame(solrad_10m)
colnames(solrad_10m_df)
solrad_10m_df$location <- 1:16
solrad_10m_df$annual_kwh.m2 <- apply(solrad_10m_df[ ,1:52], 1, sum) / 1000
solrad_10m_df$seasonal_kwh.m2 <- apply(solrad_10m_df[ ,c(1:16, 39:52)], 1, sum) / 1000
summary((apply(solrad_10m_df[ ,1:52], 1, sum) / 1000) / 365)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v2 summary)
#2.925   3.513   3.709   3.661   4.003   4.065
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v5000 summary) 
# 2.918   3.486   3.677   3.629   3.958   4.016
plot(apply(solrad_9m_df[,1:52], 1, sum), apply(solrad_10m_df[ ,1:52], 1, sum))
text(apply(solrad_9m_df[,1:52], 1, sum), apply(solrad_10m_df[ ,1:52], 1, sum), labels=solrad_9m_df$location, pos=1)
abline(0,1)

#merge forage, terrain, and energy data
forage_terrain_energy <- merge(forage_terrain, solrad_3m_df, by='location')
colnames(forage_terrain_energy)

#now do simple lm analysis and cbind results
energy.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='annual_kwh.m2'))
elevation.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='elevation'))
slope.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='slope'))
mean_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='curvature_mean'))
prof_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='curvature_profile'))
plan_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='curvature_plan'))
TCI.results <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats, y='TCI'))
overall.results <- cbind(elevation.results, energy.results, slope.results, mean_curv.results, prof_curv.results, plan_curv.results, TCI.results)
overall.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18')
write.csv(overall.results, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_lm_results_3m.filtered.Hogan.csv'), row.names = FALSE)

#now do 3 var multiple regression
mult.lm.results.3 <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats_3multiple, df_input = forage_terrain_energy, a='annual_kwh.m2', b='slope', c='curvature_mean'))
mult.lm.results.3$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18')
mult.lm.results.3
write.csv(mult.lm.results.3, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_3multlm_3mDEM.filt_Hogan_v2.csv'), row.names = FALSE)

#and 4 var mult lm
mult.lm.results.4 <- do.call(rbind, lapply(forage_terrain_energy[,2:9], get_stats_4multiple, df_input = forage_terrain_energy, a='annual_kwh.m2', b='slope', c='curvature_mean', d='elevation'))
mult.lm.results.4$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18')
mult.lm.results.4
write.csv(mult.lm.results.4, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_4multlm_3mDEM.filt_Hogan_v2.csv'), row.names = FALSE)

#export forage_terrain_energy merges as csv's
write.csv(forage_terrain_energy, file.path(results, 'tables', 'forage_terrain_energy_3m_Grace.csv'), row.names=FALSE)

#check some 3 m vs. 9 m  vs. 10 m solrad results to verify previous work
list.files(file.path(terrainDir, 'solrad_analysis'))
solrad_10m <- as.data.frame(shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_10m_v2.shp')))
solrad_10m_v1 <- as.data.frame(shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_10m.shp')))
solrad_9m <- as.data.frame(shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_9m_v2.shp')))
solrad_9m_v1 <- as.data.frame(shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_9m_Hogan.shp')))
solrad_3m <- as.data.frame(shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_3m_v2.shp')))
plot(apply(solrad_9m[,1:52], 1, sum), apply(solrad_10m[ ,1:52], 1, sum))
abline(0, 1)
plot(apply(solrad_10m_v1[,1:52], 1, sum), apply(solrad_10m[ ,1:52], 1, sum))
plot(apply(solrad_3m[,1:52], 1, sum), apply(solrad_9m[ ,1:52], 1, sum))
plot(apply(solrad_9m[,1:52], 1, sum), apply(solrad_9m_v1[ ,1:52], 1, sum))
abline(0, 1)

#model with interaction
#still in development
# x <- forage_terrain$clp021517
# a <- ''
# b <- 'curvature_mean'
# get_stats_multiple <- function(x, df_input, a, b) {
#   model.result <- summary(lm(x ~ df_input[ ,a] * df_input[ ,b]))
#   df <- data.frame(col1 = pf(model.result$fstatistic[1], model.result$fstatistic[2], model.result$fstatistic[3], lower.tail = FALSE), col2 = round(model.result$r.squared, 2), col3 = round(model.result$coefficients[2, 4], 3), col4 = round(model.result$coefficients[2, 1], 1), col5 = round(model.result$coefficients[3, 4], 3), col6 = round(model.result$coefficients[3, 1], 3), col7 = round(model.result$coefficients[4, 4], 3), col8 = round(model.result$coefficients[4, 1], 1))
#   colnames(df) <- c('model.p.val', 'model.r2', paste0(a, '.p.val'), paste0(a, '.effect'), paste0(b, '.p.val'), paste0(b, '.effect'), paste0(c, '.p.val'), paste0(c, '.effect'))
#   df
# }

#aspect is somewhat correlated with slope
plot(forage_terrain_energy$annual_kwh.m2, forage_terrain_energy$slope)
plot(forage_terrain$curvature_mean, apply(forage_terrain[,13:14], 1, mean))
plot(forage_terrain$elevation, forage_terrain$slope)

#check USGS dem results
list.files(file.path(results, 'terrain_characteristics'))
terrain_energy_USGS <- read.csv(file.path(results, 'terrain_characteristics', 'sensor_terrain_energy_USGS_10m.csv'), stringsAsFactors = FALSE)
forage_terrain_energy_USGS <- merge(forage_data, terrain_energy_USGS, by='location')
colnames(forage_terrain_energy_USGS)
forage_terrain_energy_USGS$annual_kwh.m2 <- apply(forage_terrain_energy_USGS[ ,14:65], 1, sum) / 1000
plot(forage_terrain_energy_USGS$annual_kwh.m2, forage_terrain_energy$annual_kwh.m2) #forage_terrain_energy can be 3m or 9m resolution, depending on fname read-in
abline(0, 1)
plot(forage_terrain_energy_USGS$curvature_mean, forage_terrain_energy$curvature_mean)
abline(0, 1)
plot(forage_terrain_energy_USGS$elevation, forage_terrain_energy$elevation)
abline(0, 1)
plot(forage_terrain_energy_USGS$slope, forage_terrain_energy$slope)
abline(0, 1)
plot(forage_terrain_energy_USGS$aspect, forage_terrain_energy$aspect)
abline(0, 1)
cor(forage_terrain_energy_USGS[,c(10:13,68)])
#4 var lm
mult.lm.results.4 <- do.call(rbind, lapply(forage_terrain_energy_USGS[,2:9], get_stats_4multiple, df_input = forage_terrain_energy_USGS, a='annual_kwh.m2', b='slope', c='curvature_mean', d='elevation'))
mult.lm.results.4$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18')
mult.lm.results.4
write.csv(mult.lm.results.4, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_4multlm_results.10mUSGS.csv'), row.names = FALSE)
#3 var lm
mult.lm.results.3 <- do.call(rbind, lapply(forage_terrain_energy_USGS[,2:9], get_stats_3multiple, df_input = forage_terrain_energy_USGS, a='annual_kwh.m2', b='slope', c='curvature_mean'))
mult.lm.results.3$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18')
mult.lm.results.3
write.csv(mult.lm.results.3, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_3multlm_results.10mUSGS.csv'), row.names = FALSE)
