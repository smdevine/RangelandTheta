forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
terrainDir <- file.path(results, 'terrain_characteristics')
spatialDir <- 'C:/Users/smdevine/Desktop/rangeland project/soilmoisture/sensor_coordinates'
library(raster)
library(car)
library(ppcor)
library(relaimpo)
library(extrafont)
library(extrafontdb)
loadfonts()
#terrain characteristic analysis
#read-in terrain characteristics from filtered 3m or 9m DEM from S Hogan, Mar 2017, 3m filtered from Grace Liu Nov 2016 or 10 m DEM from USGS
list.files(file.path(results, 'terrain_characteristics'))
terrain_chars <- read.csv(file.path(results, 'terrain_characteristics', "terrain_3m_filtered_Mar2017.csv"), stringsAsFactors = FALSE)
list.files(file.path(forageDir, 'summaries'))
forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
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

#temporary arguments for debugging
x <- forage_terrain_energy$clp050117
df_input <- forage_terrain_energy
solrad <- 'annual_kwh.m2_norm'
b <- 'slope_norm'
c <- 'curvature_mean_norm'
d <- 'elevation_norm'

get_stats_multiple_Solrad_poly <- function(x, df_input, solrad, b, c, d) {
  lm.result <- lm(x ~ df_input[ ,solrad] + I(df_input[ ,solrad]^2) + df_input[ ,b] + df_input[ ,c] + df_input[ ,d])
  model.result <- summary(lm.result)
  df <- data.frame(col1 = pf(model.result$fstatistic[1], model.result$fstatistic[2], model.result$fstatistic[3], lower.tail = FALSE), col2 = round(model.result$r.squared, 2), col3 = round(model.result$coefficients[2, 4], 3), col4 = round(model.result$coefficients[2, 1], 3), col5 = round(model.result$coefficients[3, 4], 3), col6 = round(model.result$coefficients[3, 1], 3), col7 = round(model.result$coefficients[4, 4], 3), col8 = round(model.result$coefficients[4, 1], 3), col9 = round(model.result$coefficients[5, 4], 3), col10 = round(model.result$coefficients[5, 1], 3), col11 = round(model.result$coefficients[6, 4], 3), col12 = round(model.result$coefficients[6, 1], 3),  col13=sqrt(sum(model.result$residuals^2)/model.result$df[2]))
  colnames(df) <- c('model.p.val', 'model.r2', paste0(solrad, '.p.val'), paste0(solrad, '.effect'), paste0(solrad, '.2.p.val'), paste0(solrad, '.2.effect'), paste0(b, '.p.val'), paste0(b, '.effect'), paste0(c, '.p.val'), paste0(c, '.effect'), paste0(d, '.p.val'), paste0(d, '.effect'), 'RMSE')
  df
}

get_stats_multiple_Solrad_poly_v2 <- function(x, df_input, solrad, b, c) {
  lm.result <- lm(x ~ df_input[ ,solrad] + I(df_input[ ,solrad]^2) + df_input[ ,b] + df_input[ ,c])
  model.result <- summary(lm.result)
  df <- data.frame(col1 = pf(model.result$fstatistic[1], model.result$fstatistic[2], model.result$fstatistic[3], lower.tail = FALSE), col2 = round(model.result$r.squared, 2), col3 = round(model.result$coefficients[2, 4], 3), col4 = round(model.result$coefficients[2, 1], 3), col5 = round(model.result$coefficients[3, 4], 3), col6 = round(model.result$coefficients[3, 1], 3), col7 = round(model.result$coefficients[4, 4], 3), col8 = round(model.result$coefficients[4, 1], 3), col9 = round(model.result$coefficients[5, 4], 3), col10 = round(model.result$coefficients[5, 1], 3),  col11=sqrt(sum(model.result$residuals^2)/model.result$df[2]))
  colnames(df) <- c('model.p.val', 'model.r2', paste0(solrad, '.p.val'), paste0(solrad, '.effect'), paste0(solrad, '.2.p.val'), paste0(solrad, '.2.effect'), paste0(b, '.p.val'), paste0(b, '.effect'), paste0(c, '.p.val'), paste0(c, '.effect'), 'RMSE')
  df
}


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
#solrad_9m <- shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_9m_v2.shp')) #all v2 files were sky size 500 x 500 and 64 calc directions
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
# solrad_9m_df <- as.data.frame(solrad_9m)
# colnames(solrad_9m_df)
# solrad_9m_df$location <- 1:16
# solrad_9m_df$annual_kwh.m2 <- apply(solrad_9m_df[ ,1:52], 1, sum) / 1000
# solrad_9m_df$seasonal_kwh.m2 <- apply(solrad_9m_df[ ,c(1:16, 39:52)], 1, sum) / 1000
# plot(solrad_9m_df$annual_kwh.m2, solrad_9m_df$seasonal_kwh.m2)
# plot(apply(solrad_9m_df[,1:52], 1, sum), apply(solrad_3m_df[ ,1:52], 1, sum))
# abline(0,1)
# summary(lm(apply(solrad_9m_df[,1:52], 1, sum) ~ apply(solrad_3m_df[ ,1:52], 1, sum)))
# summary((apply(solrad_9m_df[,1:52], 1, sum) / 1000) / 365)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.016   3.487   3.709   3.663   3.979   4.037
summary((apply(solrad_3m_df[,1:52], 1, sum) / 1000) / 365)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v2 from Hogan)
#3.019   3.435   3.674   3.645   3.994   4.032 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v2 from Grace)
#3.083   3.452   3.682   3.659   3.983   4.023
# solrad_10m <- shapefile(file.path(terrainDir, 'solrad_analysis', 'solrad_10m_5000.shp')) #v2 was with sky size of 500 and 64 calc directions
# solrad_10m_df <- as.data.frame(solrad_10m)
# colnames(solrad_10m_df)
# solrad_10m_df$location <- 1:16
# solrad_10m_df$annual_kwh.m2 <- apply(solrad_10m_df[ ,1:52], 1, sum) / 1000
# solrad_10m_df$seasonal_kwh.m2 <- apply(solrad_10m_df[ ,c(1:16, 39:52)], 1, sum) / 1000
# summary((apply(solrad_10m_df[ ,1:52], 1, sum) / 1000) / 365)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v2 summary)
#2.925   3.513   3.709   3.661   4.003   4.065
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. (v5000 summary) 
# 2.918   3.486   3.677   3.629   3.958   4.016
# plot(apply(solrad_9m_df[,1:52], 1, sum), apply(solrad_10m_df[ ,1:52], 1, sum))
# text(apply(solrad_9m_df[,1:52], 1, sum), apply(solrad_10m_df[ ,1:52], 1, sum), labels=solrad_9m_df$location, pos=1)
# abline(0,1)

#merge forage, terrain, and energy data
forage_terrain_energy <- merge(forage_terrain, solrad_3m_df, by='location')
colnames(forage_terrain_energy)

#now do simple lm analysis and cbind results
energy.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='annual_kwh.m2'))
elevation.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='elevation'))
slope.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='slope'))
mean_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='curvature_mean'))
prof_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='curvature_profile'))
plan_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='curvature_plan'))
TCI.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='TCI'))
overall.results <- cbind(elevation.results, energy.results, slope.results, mean_curv.results, prof_curv.results, plan_curv.results, TCI.results)
overall.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak_2017', 'peak_2018')
write.csv(overall.results, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_lm_results_3m.filtered.Hogan_final.csv'), row.names = FALSE)

#now do 3 var multiple regression
mult.lm.results.3 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats_3multiple, df_input = forage_terrain_energy, a='annual_kwh.m2', b='slope', c='curvature_mean'))
mult.lm.results.3$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak2017', 'peak2018')
mult.lm.results.3
write.csv(mult.lm.results.3, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_3multlm_3mDEM.filt_Hogan_final.csv'), row.names = FALSE)

#and 4 var mult lm
mult.lm.results.4 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats_4multiple, df_input = forage_terrain_energy, a='annual_kwh.m2', b='slope', c='curvature_mean', d='elevation'))
mult.lm.results.4$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak2017', 'peak2018')
mult.lm.results.4
write.csv(mult.lm.results.4, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_4multlm_3mDEM.filt_Hogan_final.csv'), row.names = FALSE)

#export forage_terrain_energy merges as csv's
write.csv(forage_terrain_energy, file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), row.names=FALSE)

#read-in this file
forage_terrain_energy <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
list.files(terrainDir)

#rel_elev <- read.csv(file.path(terrainDir, 'sensor_rel_elevs_3mfiltered_Hogan.csv'), stringsAsFactors = FALSE)
#forage_terrain_energy <- merge(forage_terrain_energy, rel_elev, by='location')
#double-check presence of influential data in April 2017 model
boxplot(forage_terrain_energy$peak2017)
boxplot(forage_terrain_energy$clp031417) #outlier on this date
forage_terrain_energy[order(forage_terrain_energy$peak2017) , c('location', 'peak2017')]
summary(lm(peak2017 ~ curvature_mean + slope + annual_kwh.m2, data=forage_terrain_energy[-c(13,14),]))


#get pairwise semi-partial correclation for each pair of variables given others
# spcor(forage_terrain_energy[,c('clp021517', 'annual_kwh.m2', 'slope', 'curvature_mean')], method = 'pearson')
# spcor(forage_terrain_energy[,c('clp031417', 'annual_kwh.m2', 'slope', 'curvature_mean')], method = 'pearson')
# spcor(forage_terrain_energy[,c('clp041017', 'annual_kwh.m2', 'slope', 'curvature_mean')], method = 'pearson')
# spcor(forage_terrain_energy[,c('clp050117', 'annual_kwh.m2', 'slope', 'curvature_mean')], method = 'pearson')
# pcor(forage_terrain_energy[,c('clp050117', 'annual_kwh.m2', 'slope', 'curvature_mean')], method = 'pearson')

#run lm models with normalized input data to get effects in standard deviation units
normalize_var <- function(x) {
  (x - mean(x)) / sd(x)
}

hist(normalize_var(forage_terrain_energy$curvature_mean))
hist(normalize_var(forage_terrain_energy$annual_kwh.m2))
hist(normalize_var(forage_terrain_energy$slope))
hist(normalize_var(forage_terrain_energy$elevation))
forage_terrain_energy$curvature_mean_norm <- normalize_var(forage_terrain_energy$curvature_mean)
forage_terrain_energy$slope_norm <- normalize_var(forage_terrain_energy$slope)
forage_terrain_energy$annual_kwh.m2_norm <- normalize_var(forage_terrain_energy$annual_kwh.m2)
forage_terrain_energy$elevation_norm <- normalize_var(forage_terrain_energy$elevation)

energy.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='annual_kwh.m2_norm'))
slope.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='slope_norm'))
mean_curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='curvature_mean_norm'))
elevation.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats, y='elevation_norm'))
overall.results <- cbind(energy.results, slope.results, mean_curv.results, elevation.results)
overall.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak_2017', 'peak_2018')
write.csv(overall.results, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_lm_norm_results_3m.filtered.Hogan_final.csv'), row.names = FALSE)


mult.lm.results.4 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats_4multiple, df_input = forage_terrain_energy, a='annual_kwh.m2_norm', b='slope_norm', c='curvature_mean_norm', d='elevation_norm'))
mult.lm.results.4$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak2017', 'peak2018')
mult.lm.results.4
write.csv(mult.lm.results.4, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_4mult_norm_lm_3mDEM.filt_Hogan_final.csv'), row.names = FALSE)

mult.lm.results.3 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats_3multiple, df_input = forage_terrain_energy, a='annual_kwh.m2_norm', b='slope_norm', c='curvature_mean_norm'))
mult.lm.results.3$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak2017', 'peak2018')
mult.lm.results.3
write.csv(mult.lm.results.3, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_3mult_norm_lm_3mDEM.filt_Hogan_final.csv'), row.names = FALSE)

#re-run analyis with solrad modeled as polynomial
mult.lm.results.4 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats_multiple_Solrad_poly, df_input = forage_terrain_energy, solrad='annual_kwh.m2_norm', b='slope_norm', c='curvature_mean_norm', d='elevation_norm'))
mult.lm.results.4$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak2017', 'peak2018')
mult.lm.results.4
write.csv(mult.lm.results.4, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_mult_norm_solrad_poly_3mDEM.filt_Hogan_final.csv'), row.names = FALSE)

#re-run analyis with solrad modeled as polynomial and no elevation
mult.lm.results.3 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], get_stats_multiple_Solrad_poly_v2, df_input = forage_terrain_energy, solrad='annual_kwh.m2_norm', b='slope_norm', c='curvature_mean_norm'))
mult.lm.results.3$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak2017', 'peak2018')
mult.lm.results.3
write.csv(mult.lm.results.3, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_mult_norm_solrad_poly_no_elev_3mDEM.filt_Hogan_final.csv'), row.names = FALSE)

#check 2 var models
mult2var_peak2017 <- lm(peak2017 ~ elevation + curvature_mean + slope, data=forage_terrain_energy)
summary(mult2var_peak2017)
plot(mult2var_peak2017)

mult2var_peak2018 <- lm(peak2018 ~ annual_kwh.m2 + slope, data=forage_terrain_energy)
summary(mult2var_peak2018) #r2=0.6
vif(mult2var_peak2018)
plot(mult2var_peak2018)

#check residuals for peak 2017 and 2018
mult3var_peak2017 <- lm(peak2017 ~ annual_kwh.m2 + slope + curvature_mean, data=forage_terrain_energy)
plot(mult3var_peak2017)
mult3var_peak2018 <- lm(peak2018 ~ annual_kwh.m2 + slope + curvature_mean, data=forage_terrain_energy)
mult4var_peak2017 <- lm(peak2017 ~ annual_kwh.m2 + slope + curvature_mean + elevation, data=forage_terrain_energy)
summary(mult4var_peak2017)
plot(mult4var_peak2017)

#explore combos with SM and T
exploratory <- lm(peak2017 ~ soilT_data$Jan_11_2017 + vwc_data_normalized$Jan_11_2017 + elevation, data=forage_terrain_energy)
summary(exploratory)
plot(forage_terrain_energy$peak2017, exploratory$residuals)
abline(h=0, lty=2)
text(forage_terrain_energy$peak2017, exploratory$residuals, labels=forage_terrain_energy$location, pos=1, offset=0.1)
exploratory$residuals[order(exploratory$residuals)]
plot(forage_terrain_energy$annual_kwh.m2, exploratory$residuals)
abline(h=0, lty=2)
summary(lm(exploratory$residuals ~ forage_terrain_energy$TCI))
plot(vwc_data_normalized$Jan_11_2017, soilT_data$Jan_11_2017)
summary(lm(vwc_data_normalized$Jan_11_2017 ~ soilT_data$Jan_11_2017))
plot(vwc_data_normalized$Jan_11_2017, forage_terrain_energy$peak2017)
summary(lm(vwc_data_normalized$Jan_11_2017 ~ forage_terrain_energy$peak2017))
plot(soilT_data$Jan_11_2017, forage_terrain_energy$peak2017)
summary(lm(soilT_data$Jan_11_2017 ~ forage_terrain_energy$peak2017))
plot(vwc_data_normalized$Jan_11_2017, forage_terrain_energy$slope)
summary(lm(vwc_data_normalized$Jan_11_2017 ~ forage_terrain_energy$slope))

#check visually for non-linear relationships in terrain vs. peak forage
plot_lms <- function(x, y) {
  lm_result <- summary(lm(forage_terrain_energy[,y] ~ forage_terrain_energy[,x]))
  plot(forage_terrain_energy[,x], forage_terrain_energy[,y], ylab=paste(y, ' forage (kg/ha)'), xlab=x, main=paste('r^2 = ', round(lm_result$r.squared, 2)))
  abline(lm_result$coefficients[1], lm_result$coefficients[2], lty=2)
}
mapply(plot_lms, c('curvature_mean', 'slope', 'annual_kwh.m2'), 'peak2017')
mapply(plot_lms, c('curvature_mean', 'slope', 'annual_kwh.m2'), 'peak2018')

#check some models were relationship with energy is non-linear
summary(lm(peak2017 ~ poly(annual_kwh.m2, 2), data = forage_terrain_energy))
summary(lm(peak2017 ~ poly(annual_kwh.m2, 2) + slope, data = forage_terrain_energy))
summary(lm(peak2017 ~ poly(annual_kwh.m2, 2) + elevation, data = forage_terrain_energy))
summary(lm(peak2017 ~ poly(annual_kwh.m2, 2) + curvature_mean, data = forage_terrain_energy))
summary(lm(peak2017 ~ poly(annual_kwh.m2, 2) + elevation + curvature_mean, data = forage_terrain_energy))
summary(lm(peak2018 ~ poly(annual_kwh.m2, 2), data = forage_terrain_energy))
plot(lm(peak2018 ~ poly(annual_kwh.m2, 2), data = forage_terrain_energy))#13 and 8 are worst perfoming on either end of spectrum, but no clear pattern in residuals and no high leverage datapoints based on Cook's distance
summary(lm(peak2018 ~ poly(annual_kwh.m2, 2) + slope, data = forage_terrain_energy))
plot(lm(peak2018 ~ poly(annual_kwh.m2, 2) + slope, data = forage_terrain_energy))
summary(lm(peak2018 ~ poly(annual_kwh.m2, 2) + curvature_mean + slope, data = forage_terrain_energy))
summary(lm(peak2018 ~ poly(annual_kwh.m2, 2) + elevation + slope, data = forage_terrain_energy))
summary(lm(peak2018 ~ poly(annual_kwh.m2, 2) + curvature_mean + elevation, data = forage_terrain_energy))
summary(lm(peak2018 ~ poly(annual_kwh.m2, 2) + curvature_mean + slope + elevation, data = forage_terrain_energy))

nl_2var2018 <- lm(peak2018 ~ poly(annual_kwh.m2, 2), data = forage_terrain_energy)
summary(nl_2var2018)
plot(nl_2var2018)
nl_3var2018 <- lm(peak2018 ~ poly(annual_kwh.m2, 2) + slope, data = forage_terrain_energy)
summary(nl_3var2018)
vif(nl_3var2018)
plot(nl_3var2018)

#spearman rank correlation
#location 12 and 16 are tied, crazy
#location 12 was higher 3 out of 4 dates, so data changed from 2026.8 to 2026.81 for purpose of test
#forage_terrain_energy$peak2017[forage_terrain_energy$location==12] <- 2026.81
# df <- forage_terrain_energy
# x <- df$peak2017
# y <- 'curvature_mean'
# mtd = 'spearman'
rank_test <- function(x, df, y, mtd) {
  test <- cor.test(x, df[[y]], method = mtd)
  result <- data.frame(col.1=test$p.value, col.2=test$estimate)
  colnames(result) <- c(paste0(y, '.p.val.', mtd), paste0(y, if(mtd=='pearson') {'.tau.'} else {'.rho.'}, mtd))
  result
}
energy.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='annual_kwh.m2', mtd='pearson'))
elevation.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='elevation', mtd='pearson'))
curv.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='curvature_mean', mtd='pearson'))
slope.results <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='slope', mtd='pearson'))
overall.results <- cbind(elevation.results, energy.results, slope.results, curv.results)
overall.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak_2017', 'peak_2018')
write.csv(overall.results, file.path(results, 'forage_vs_terrain', 'forage_vs_terrain_pearson_results.csv'), row.names = FALSE)

forage.results.feb17 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp021517', mtd='pearson'))
forage.results.mar17<- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp031417', mtd='pearson'))
forage.results.apr17 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp041017', mtd='pearson'))
forage.results.may17 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp050117', mtd='pearson'))
forage.results.feb18 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp021518', mtd='pearson'))
forage.results.mar18<- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp032218', mtd='pearson'))
forage.results.apr18 <- do.call(rbind, lapply(forage_terrain_energy[,2:11], rank_test, df=forage_terrain_energy, y='clp041518', mtd='pearson'))
overall.forage.results <- cbind(forage.results.feb17, forage.results.mar17, forage.results.apr17, forage.results.may17, forage.results.feb18, forage.results.mar18, forage.results.apr18)
overall.forage.results$clip.date <- c('2/15/17', '3/14/17', '4/10/17', '5/1/17', '1/16/18', '2/15/18', '3/22/18', '4/15/18', 'peak_2017', 'peak_2018')
write.csv(overall.forage.results, file.path(results, 'forage_vs_terrain', 'forage_vs_forage_pearson_results.csv'), row.names = FALSE)


#do some plotting of 2017 vs. 2018 and visualization of terrain effects
#forage_terrain_energy$landform_class <- c('backslope', 'backslope', 'shoulder', 'footslope', 'backslope', 'backslope', 'footslope', 'summit', 'summit', 'backslope', 'backslope', 'backslope', 'footslope', 'backslope', 'footslope', 'backslope')
#forage_terrain_energy$landform_code <- ifelse(forage_terrain_energy$landform_class=='footslope', 1, ifelse(forage_terrain_energy$landform_class=='backslope', 2, ifelse(forage_terrain_energy$landform_class=='shoulder', 3, 4)))
#forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > summary(forage_terrain_energy$annual_kwh.m2)[2] & forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[3], 'lightblue2', ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[5], 'orange2', 'red3'))) four color scheme
#forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > summary(forage_terrain_energy$annual_kwh.m2)[2] & forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[5], 'orange2', 'red3')) #tres colores
#definition based on aspect classes
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, 'orange2', 'red3')) #tres colores
forage_terrain_energy$productivity_group <- c(1, 2, 4, 4, 3, 3, 1, 4, 3, 2, 2, 4, 1, 1, 2, 3) #determined from plotting
forage_terrain_energy$slope_percent <- tan(forage_terrain_energy$slope * pi / 180) * 100
tapply(forage_terrain_energy$slope_percent, forage_terrain_energy$productivity_group, mean)
tapply(forage_terrain_energy$slope_percent, forage_terrain_energy$productivity_group, sd)
tapply(forage_terrain_energy$elevation, forage_terrain_energy$productivity_group, mean)
tapply(forage_terrain_energy$elevation, forage_terrain_energy$productivity_group, sd)
sum(forage_terrain_energy$curvature_mean < -0.2)
sum(forage_terrain_energy$curvature_mean > -0.2 & forage_terrain_energy$curvature_mean < 0.2)
sum(forage_terrain_energy$curvature_mean > 0.2)
forage_terrain_energy$curvature_cex <- ifelse(forage_terrain_energy$curvature_mean < -0.2, 1, ifelse(forage_terrain_energy$curvature_mean > -0.2 & forage_terrain_energy$curvature_mean < 0.2, 1.5, 2.5))
forage_terrain_energy$curvature_cex_v2 <- seq(from=0.5, to=2.75, length.out = 16)[rank(forage_terrain_energy$curvature_mean)]
forage_terrain_energy$slope_cex_v2 <- seq(from=0.5, to=2.75, length.out = 16)[rank(forage_terrain_energy$slope)]
forage_terrain_energy$elevation_cex_v2 <- seq(from=0.5, to=2.75, length.out = 16)[rank(forage_terrain_energy$elevation)]
forage_terrain_energy$aspect_cex_v2 <- seq(from=0.5, to=2.75, length.out = 16)[rank(forage_terrain_energy$annual_kwh.m2)]
#forage_terrain_energy$slope_cex <- ifelse(forage_terrain_energy$slope < summary(forage_terrain_energy$slope)[2], 2.5, ifelse(forage_terrain_energy$slope > summary(forage_terrain_energy$slope)[2] & forage_terrain_energy$slope < summary(forage_terrain_energy$slope)[3], 1.75, ifelse(forage_terrain_energy$slope > summary(forage_terrain_energy$slope)[3] & forage_terrain_energy$slope < summary(forage_terrain_energy$slope)[5], 1.25, 0.75))) #4 classes

#make a temporal plot of forage growth
png(file = file.path(results, 'figures', 'finals', 'forage_vs_terrain', 'forage_vs_terrain_detail_v2.png'), family = 'Book Antiqua', width = 800, height = 700, units = 'px', res=100)
par(mar=c(3.5, 4.5, 2, 2))
plot(x=rep(0.25,16), forage_terrain_energy$clp021517, type = 'p', col=forage_terrain_energy$energy_colors, pch=1, ylim=c(0, 4600), xlim=c(0,5), xaxt='n', xlab='', ylab=expression(paste('standing forage (kg', ' ', ha^-1, ')')), cex.axis=1.2, cex.lab=1.2, cex=1.2) #cex=forage_terrain_energy$curvature_cex_v2
points(x=rep(1,16), forage_terrain_energy$clp031417, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) #cex=forage_terrain_energy$slope_cex_v2
points(x=rep(1.75,16), forage_terrain_energy$clp041017, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) #cex=forage_terrain_energy$elevation_cex_v2) this is non-significant but notable
points(x=rep(2.5,16), forage_terrain_energy$clp050117, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) # cex=forage_terrain_energy$elevation_cex_v2) non-significant but notable
abline(v=2.875, lty=2)
points(x=rep(3.25,16), forage_terrain_energy$clp021518, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2)# cex=forage_terrain_energy$curvature_cex_v2)
points(x=rep(4,16), forage_terrain_energy$clp032218, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2) #cex=forage_terrain_energy$aspect_cex_v2) non-significant
points(x=rep(4.75,16), forage_terrain_energy$clp041518, col=forage_terrain_energy$energy_colors, pch=1, cex=1.2)# cex=forage_terrain_energy$slope_cex_v2)
#text(x=0.25, y= 2500, label = 'mean curvature', srt = 90, cex=1.1)
#text(x=1, y= 3300, label = 'aspect & slope', srt = 90, cex = 1.1)
#text(x=1.75, y= 500, label = 'NS', srt = 90, cex = 1.1)
#text(x=2.5, y= 500, label = 'NS', srt = 90, cex = 1.1)
#text(x=3.25, y = 1300, label = 'mean curvature', srt=90, cex = 1.1)
#text(x=4, y = 1600, label = 'non-linear aspect', srt=90, cex=1.1)
#text(x=4.75, y=2600, label='slope & non-linear aspect', srt=90, cex = 1.1)
axis(side = 1, at = c(0.25, 1, 1.75, 2.5, 3.25, 4, 4.75), labels = c('Feb 15', 'Mar 22', 'Apr 10', 'May 1', 'Feb 15', 'Mar 22', 'Apr 15'), cex=1.2)
legend('topright', legend=(c("< 1200", '1200-1410', '>1410')), pch=1, pt.cex = c(1.5, 1.5, 1.5), col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.2)
text(x=0.375, y=4400, label='wet year', cex=1.3)
text(x=3.375, y=4400, label='dry year', cex=1.3)
dev.off()

#2017 vs. 2018 relationship
plot(forage_terrain_energy$peak2017, forage_terrain_energy$peak2018, xlab=expression(paste('2017 peak forage (kg', ' ', ha^-1, ')')), ylab=expression(paste('2018 peak forage (kg', ' ', ha^-1, ')')), pch=19, cex=forage_terrain_energy$curvature_cex, ylim = c(300, 1600), xlim=c(1400, 4700), col=forage_terrain_energy$energy_colors)
text(x=forage_terrain_energy$peak2017, y=forage_terrain_energy$peak2018, labels = forage_terrain_energy$location, pos = 1, offset = 0.3)
abline(h=median(forage_terrain_energy$peak2018), lty=2)
abline(v=median(forage_terrain_energy$peak2017), lty=2)


#make a plot of peak2017 vs. peak2018
png(file = file.path(results, 'figures', 'finals', 'forage_vs_terrain', 'peak2017.vs.2018.forage.v4.png', sep = ''), family = 'Book Antiqua', width = 800, height = 700, units = 'px', res=100)
par(mar=c(4.5, 4.5, 2, 2))
plot(forage_terrain_energy$peak2017, forage_terrain_energy$peak2018, xlab=expression(paste('2017 peak forage (kg', ' ', ha^-1, ')')), ylab=expression(paste('2018 peak forage (kg', ' ', ha^-1, ')')), pch=19, cex=forage_terrain_energy$curvature_cex, ylim = c(300, 1600), xlim=c(1400, 4700), col=forage_terrain_energy$energy_colors, cex.axis=1.2, cex.lab=1.2)
abline(h=median(forage_terrain_energy$peak2018), lty=2)
abline(v=median(forage_terrain_energy$peak2017), lty=2)
#text(x=forage_terrain_energy$peak2017, y=forage_terrain_energy$peak2018, labels = forage_terrain_energy$landform_code, pos = 1, offset = 0.3)
#legend(x=2000, y=1625, legend=(c("< 1254", '1254-1341', "1341-1458", '>1458')), pch=19, col=c('blue', 'lightblue2', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2))) #4 color energy legend
legend(x=2800, y=1625, legend=(c("< 1200", '1200-1410', '>1410')), pch=19, pt.cex = c(1.5, 1.5, 1.5), col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), cex = 1.2) #3 color energy legend
legend(x=3700, y=1625, legend=(c("concave", 'linear', "convex")), pt.cex=c(1, 1.5, 2.5), pch=c(1, 1, 1), col=c('black', 'black', 'black'), title = 'Mean curvature', cex=1.2)
text(x=4000, y=650, label=paste('% slope 26', '\u00B1', '9'), cex=1.2)
text(x=4000, y=600, label=paste('elev 485', '\u00B1', '8'), cex=1.2)
text(x=4000, y=1300, label=paste('% slope 17', '\u00B1', '6'), cex=1.2)
text(x=4000, y=1250, label=paste('elev 490', '\u00B1', '10'), cex=1.2)
text(x=2000, y=1375, label=paste('% slope 20', '\u00B1', '7'), cex= 1.2)
text(x=2000, y=1325, label=paste('elev 496', '\u00B1', '6'), cex=1.2)
text(x=2000, y=650, label=paste('% slope 26', '\u00B1', '10'), cex=1.2)
text(x=2000, y=600, label=paste('elev 488', '\u00B1', '6'), cex=1.2)
#legend(x=3600, y=1625, legend=(c("1 = footslope", '2 = backslope', "3 = shoulder", '4 = summit')), title = 'hillslope position')
dev.off()


hist(forage_terrain_energy$peak2017)
hist(forage_terrain_energy$peak2018)
hist(forage_terrain_energy$annual_kwh.m2)
hist(forage_terrain_energy$slope)
hist(forage_terrain_energy$curvature_mean)

#March 2017 vs April 2017 forage growth
forage_terrain_energy <- forage
plot(forage_terrain_energy$clp031417[forage_terrain_energy$annual_kwh.m2 < mean(forage_terrain_energy$annual_kwh.m2)], forage_terrain_energy$clp041017[forage_terrain_energy$annual_kwh.m2 < mean(forage_terrain_energy$annual_kwh.m2)])

autocorr_test_terrain <- function(nsim) {
  set.seed(19801976)
  terrain_data <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
  sensor_pts <- shapefile(file.path(spatialDir, '5TM_sensor_locations_Camatta.shp'))
  names(sensor_pts)[1] <- 'location'
  coords <- sensor_pts[which(sensor_pts$location %in% terrain_data$location), c('Est_10N', 'Nrt_10N')]
  terrain_data_sp <- SpatialPointsDataFrame(coords=coords, proj4string = crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=terrain_data)
  #then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(terrain_data_sp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(forage_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  dates <- colnames(forage_data)[2:ncol(forage_data)]
  for (i in 1:length(dates)) {
    result <- moran.mc(forage_data[[dates[i]]], idw_list, nsim = nsim)
    if (i==1) {
      results <- matrix(c(dates[i], result$statistic, result$p.value), nrow=1, ncol=3, byrow=TRUE)
      next
    }
    results <- rbind(results, c(dates[i], result$statistic, result$p.value))
  }
  results <- as.data.frame(results)
  colnames(results) <- c('date', 'Moran I statistic', 'p_value')
  results$n_pts <- nrow(forage_data)
  if (!dir.exists(file.path(forageDir, 'summaries', 'autocorrelation_test_abs'))) {
    dir.create(file.path(forageDir, 'summaries', 'autocorrelation_test_abs'))
  }
  write.csv(results, file.path(forageDir, 'summaries', 'autocorrelation_test_abs', 'forage_autocorrtest.csv'), row.names = FALSE)
}
autocorr_test_forage(nsim=999)

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

#aspect is relatively correlated with slope
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
