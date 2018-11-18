library(boot)
library(car)
library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/growth_models'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
climateDir <- 'C:/Users/smdevine/Desktop/rangeland project/climate_data'
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
forage_terrain_energy <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, 'orange2', 'red3'))
forage_terrain_energy$Mar2017growth <- forage_terrain_energy$clp031417 - forage_terrain_energy$clp021517
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
forage_terrain_energy$May2017growth <- forage_terrain_energy$clp050117 - forage_terrain_energy$clp041017
forage_terrain_energy$Mar2018growth <- forage_terrain_energy$clp032218 - forage_terrain_energy$clp021518
forage_terrain_energy$Apr2018growth <- forage_terrain_energy$clp041518 - forage_terrain_energy$clp032218
soilT_data_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_data_7cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_data_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_data_22cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
depletion_vwc_2017 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2018.csv'), stringsAsFactors=FALSE)
depletion_vwc_2017_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2018.csv'), stringsAsFactors=FALSE)
normalize_var <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
#get r2 values again for depletion vs. T
list.files('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/')
depletion_T7_r2_2017 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2017biomass_7cm.csv', stringsAsFactors = FALSE)
depletion_T22_r2_2017 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2017biomass_22cm.csv', stringsAsFactors = FALSE)
depletion_T7_r2_2018 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2018biomass_7cm.csv', stringsAsFactors = FALSE)
depletion_T22_r2_2018 <- read.csv('C:/Users/smdevine/Desktop/rangeland project/results/depletionVWC_T_model_results/depletionVWC_T_vs_Apr2018biomass_22cm.csv', stringsAsFactors = FALSE)
depletion_T_r2_2017_2018 <- data.frame(dates=c(depletion_T7_r2_2017$dates, '2017-7-30', depletion_T7_r2_2018$dates), r2.depletion.vs.T7=c(depletion_T7_r2_2017$r2.SM.vs.T, NA, depletion_T7_r2_2018$r2.SM.vs.T), p.val.depletion.vs.T7=c(depletion_T7_r2_2017$p.value.SM.vs.T, NA, depletion_T7_r2_2018$p.value.SM.vs.T), slope.depletion.vs.T7=c(depletion_T7_r2_2017$slope.SM.vs.T, NA, depletion_T7_r2_2018$slope.SM.vs.T), r2.depletion.vs.T22=c(depletion_T22_r2_2017$r2.SM.vs.T, NA, depletion_T22_r2_2018$r2.SM.vs.T), p.val.depletion.vs.T22=c(depletion_T22_r2_2017$p.value.SM.vs.T, NA, depletion_T22_r2_2018$p.value.SM.vs.T), slope.depletion.vs.T22=c(depletion_T22_r2_2017$slope.SM.vs.T, NA, depletion_T22_r2_2018$slope.SM.vs.T))
#7 cm plot
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'depletion_vs_T', paste0('WY2017_2018.r2.depletion_vs_T7cm.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(1, 4.5, 0.5, 4.5))
plot(as.Date(depletion_T_r2_2017_2018$dates[depletion_T_r2_2017_2018$p.val.depletion.vs.T7 < 0.05], format='%Y-%m-%d'), depletion_T_r2_2017_2018$r2.depletion.vs.T7[depletion_T_r2_2017_2018$p.val.depletion.vs.T7 < 0.05], type = 'p', col=ifelse(depletion_T_r2_2017_2018$slope.depletion.vs.T7[depletion_T_r2_2017_2018$p.val.depletion.vs.T7 < 0.05] < 0, 'red', 'blue'), xaxt='n', xlab = "", ylim=c(0,1), ylab = '')
#axis(side=1, at=as.Date(c('2016-12-01', '2017-02-01', '2017-04-01', '2017-06-01', '2017-12-01', '2018-02-01', '2018-04-01', '2018-06-01')), labels=c('Dec 2016', 'Feb 2017', 'Apr 2017', 'Jun 2017', 'Dec 2017', 'Feb 2018', 'Apr 2018', 'June 2018'))
axis(side = 4, at = c(0, 0.0625, 0.125, 0.1875, 0.25), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=0.3)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.00625, type='s', col='lightblue', cex=0.5)
legend('topright', legend = c('warmer = drier (p < 0.05)', 'colder = drier (p < 0.05)'), pch = 1, col=c('red', 'blue'), inset = 0.01)
mtext(text='daily 7 cm VWC depletion variance', side=2, line=3.5)
mtext(text=expression('explained by soil temperature ('~r^2*')'), side=2, line=2.25)
dev.off()

#22 cm plot
tiff(file = file.path(resultsFigures, 'finals', 'SM_T_effects_visualizations', 'depletion_vs_T', paste0('WY2017_2018.r2.depletion_vs_T22cm.tif')), family = 'Times New Roman', pointsize = 11, width = 9, height = 3, units = 'in', res=150)
par(mar=c(2.25, 4.5, 0.5, 4.5))
plot(as.Date(depletion_T_r2_2017_2018$dates[depletion_T_r2_2017_2018$p.val.depletion.vs.T22 < 0.05], format='%Y-%m-%d'), depletion_T_r2_2017_2018$r2.depletion.vs.T22[depletion_T_r2_2017_2018$p.val.depletion.vs.T22 < 0.05], type = 'p', col=ifelse(depletion_T_r2_2017_2018$slope.depletion.vs.T22[depletion_T_r2_2017_2018$p.val.depletion.vs.T22 < 0.05] < 0, 'red', 'blue'), xaxt='n', xlab = "", ylim=c(0,1), ylab = '')
axis(side=1, at=as.Date(c('2016-12-01', '2017-02-01', '2017-04-01', '2017-06-01', '2017-12-01', '2018-02-01', '2018-04-01', '2018-06-01')), labels=c('Dec 2016', 'Feb 2017', 'Apr 2017', 'Jun 2017', 'Dec 2017', 'Feb 2018', 'Apr 2018', 'June 2018'))
axis(side = 4, at = c(0, 0.0625, 0.125, 0.1875, 0.25), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=0.2)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 0.00625, type='s', col='lightblue', cex=0.5)
legend('topright', legend = c('warmer = drier (p < 0.05)', 'colder = drier (p < 0.05)'), pch = 1, col=c('red', 'blue'), inset = 0.01)
mtext(text='daily 22 cm VWC depletion variance', side=2, line=3.5)
mtext(text=expression('explained by soil temperature ('~r^2*')'), side=2, line=2.25)
dev.off()


# year <- 2017
# start_date <- 'Feb_16_2017'
# end_date <- 'Mar_14_2017'
# forage_growth <- 'Mar2017growth'
# normalizeVars <- TRUE
SM_T_interaction_model <- function(year, start_date, end_date, forage_growth, normalizeVars, growth_name) {
  if (year==2017) {
    dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
    avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
    #print(avg_depletion_7cm)
    avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
    #print(avg_depletion_22cm)
    avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))} #if normalization, average raw means first
    #print(avg_depletion)
    avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_data_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_data_7cm_2017[ ,dates], 1, mean)}
    #print(avg_soilT_7cm)
    avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_data_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_data_22cm_2017[ ,dates], 1, mean)}
    #print(avg_soilT_22cm)
    avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_data_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
    #print(avg_soilT)
  }
  if (year==2018) {
    dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
    avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
    avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
    avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
    avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_data_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_7cm_2018[ ,dates], 1, mean)}
    avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_data_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_22cm_2018[ ,dates], 1, mean)}
    avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_data_7cm_2018[ ,dates], 1, mean), apply(soilT_data_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
  }
  df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
  df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
  df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)
  SM_T_vs_growth_analysis <- data.frame(growth.period=forage_growth, growth.name=growth_name, start_date = start_date, end_date = end_date, model.name=rep(c('soilT', 'depletion', 'interaction', 'additive'), 3), model.code=rep(c(2, 1, 4, 3), 3), depth=c(rep('0_15', 4), rep('15_30', 4), rep('0_30', 4)), aic=NA, p.value.model= NA, r2.model=NA, RMSE=NA, CV.RMSE=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.SM.T=NA, p.value.SM.T=NA, model.deg.freedom=NA, p.value.SM.vs.T=NA, r2.SM.vs.T=NA, vif.depletion=NA, vif.soilT=NA, vif.interaction=NA, depletion.mean=NA, depletion.sd=NA, soilT.mean=NA, soilT.sd=NA)
  lm.7.soilT <- summary(lm(growth_period ~ avg_soilT_7cm, data = df_7cm))
  lm.7.soilT$p.value.model <- lm.7.soilT$coefficients[2, 4]
  glm.7.soilT <- glm(growth_period ~ avg_soilT_7cm, data = df_7cm)
  glm.7.soilT_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!is.na(df_7cm[,2]), ], glmfit = glm.7.soilT)$delta[1])
  lm.7.depletion <- summary(lm(growth_period ~ avg_depletion_7cm, data = df_7cm))
  lm.7.depletion$p.value.model <- lm.7.depletion$coefficients[2, 4]
  glm.7.depletion <- glm(growth_period ~ avg_depletion_7cm, data = df_7cm)
  glm.7.depletion_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!is.na(df_7cm[,3]),], glmfit = glm.7.depletion)$delta[1])
  lm.7.interaction <- summary(lm(growth_period ~ avg_depletion_7cm * avg_soilT_7cm, data = df_7cm))
  lm.7.interaction$p.value.model <- pf(lm.7.interaction$fstatistic[1], lm.7.interaction$fstatistic[2], lm.7.interaction$fstatistic[3], lower.tail = FALSE)
  glm.7.interaction <- glm(growth_period ~ avg_depletion_7cm * avg_soilT_7cm, data = df_7cm)
  glm.7.interaction_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!(is.na(df_7cm[,2]) | is.na(df_7cm[,3])), ], glmfit = glm.7.interaction)$delta[1])
  
  lm.7.additive <- summary(lm(growth_period ~ avg_depletion_7cm + avg_soilT_7cm, data = df_7cm))
  lm.7.additive$p.value.model <- pf(lm.7.additive$fstatistic[1], lm.7.additive$fstatistic[2], lm.7.additive$fstatistic[3], lower.tail = FALSE)
  glm.7.additive <- glm(growth_period ~ avg_depletion_7cm + avg_soilT_7cm, data = df_7cm)
  glm.7.additive_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!(is.na(df_7cm[,2]) | is.na(df_7cm[,3])), ], glmfit = glm.7.additive)$delta[1])
  
  lm.22.soilT <- summary(lm(growth_period ~ avg_soilT_22cm, data = df_22cm))
  lm.22.soilT$p.value.model <- lm.22.soilT$coefficients[2, 4]
  glm.22.soilT <- glm(growth_period ~ avg_soilT_22cm, data = df_22cm)
  glm.22.soilT_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!is.na(df_22cm[,2]), ], glmfit = glm.22.soilT)$delta[1])
  lm.22.depletion <- summary(lm(growth_period ~ avg_depletion_22cm, data = df_22cm))
  lm.22.depletion$p.value.model <- lm.22.depletion$coefficients[2, 4]
  glm.22.depletion <- glm(growth_period ~ avg_depletion_22cm, data = df_22cm)
  glm.22.depletion_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!is.na(df_22cm[,3]),], glmfit = glm.22.depletion)$delta[1])
  lm.22.interaction <- summary(lm(growth_period ~ avg_depletion_22cm * avg_soilT_22cm, data = df_22cm))
  lm.22.interaction$p.value.model <- pf(lm.22.interaction$fstatistic[1], lm.22.interaction$fstatistic[2], lm.22.interaction$fstatistic[3], lower.tail = FALSE)
  glm.22.interaction <- glm(growth_period ~ avg_depletion_22cm * avg_soilT_22cm, data = df_22cm)
  glm.22.interaction_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!(is.na(df_22cm[,2]) | is.na(df_22cm[,3])), ], glmfit = glm.22.interaction)$delta[1])
  
  lm.22.additive <- summary(lm(growth_period ~ avg_depletion_22cm + avg_soilT_22cm, data = df_22cm))
  lm.22.additive$p.value.model <- pf(lm.22.additive$fstatistic[1], lm.22.additive$fstatistic[2], lm.22.additive$fstatistic[3], lower.tail = FALSE)
  glm.22.additive <- glm(growth_period ~ avg_depletion_22cm + avg_soilT_22cm, data = df_22cm)
  glm.22.additive_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!(is.na(df_22cm[,2]) | is.na(df_22cm[,3])), ], glmfit = glm.22.additive)$delta[1])
  
  lm.0_30.soilT <- summary(lm(growth_period ~ avg_soilT, data = df_0_30cm))
  lm.0_30.soilT$p.value.model <- lm.0_30.soilT$coefficients[2, 4]
  glm.0_30.soilT <- glm(growth_period ~ avg_soilT, data = df_0_30cm)
  glm.0_30.soilT_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!is.na(df_0_30cm[,2]), ], glmfit = glm.0_30.soilT)$delta[1])
  lm.0_30.depletion <- summary(lm(growth_period ~ avg_depletion, data = df_0_30cm))
  lm.0_30.depletion$p.value.model <- lm.0_30.depletion$coefficients[2, 4]
  glm.0_30.depletion <- glm(growth_period ~ avg_depletion, data = df_0_30cm)
  glm.0_30.depletion_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!is.na(df_0_30cm[,3]),], glmfit = glm.0_30.depletion)$delta[1])
  lm.0_30.interaction <- summary(lm(growth_period ~ avg_depletion * avg_soilT, data = df_0_30cm))
  lm.0_30.interaction$p.value.model <- pf(lm.0_30.interaction$fstatistic[1], lm.0_30.interaction$fstatistic[2], lm.0_30.interaction$fstatistic[3], lower.tail = FALSE)
  glm.0_30.interaction <- glm(growth_period ~ avg_depletion * avg_soilT, data = df_0_30cm)
  glm.0_30.interaction_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!(is.na(df_0_30cm[,2]) | is.na(df_0_30cm[,3])), ], glmfit = glm.0_30.interaction)$delta[1])
  lm.0_30.additive <- summary(lm(growth_period ~ avg_depletion + avg_soilT, data = df_0_30cm))
  lm.0_30.additive$p.value.model <- pf(lm.0_30.additive$fstatistic[1], lm.0_30.additive$fstatistic[2], lm.0_30.additive$fstatistic[3], lower.tail = FALSE)
  glm.0_30.additive <- glm(growth_period ~ avg_depletion + avg_soilT, data = df_0_30cm)
  glm.0_30.additive_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!(is.na(df_0_30cm[,2]) | is.na(df_0_30cm[,3])), ], glmfit = glm.0_30.additive)$delta[1])
  lm.7.SM.vs.T <- summary(lm(avg_depletion_7cm ~ avg_soilT_7cm))
  lm.22.SM.vs.T <- summary(lm(avg_depletion_22cm ~ avg_soilT_22cm))
  lm.0_30.SM.vs.T <- summary(lm(avg_depletion ~ avg_soilT))
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'r2.model'] <- c(lm.7.soilT$r.squared, lm.7.depletion$r.squared, lm.7.interaction$r.squared, lm.7.additive$r.squared)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'r2.model'] <- c(lm.22.soilT$r.squared, lm.22.depletion$r.squared, lm.22.interaction$r.squared, lm.22.additive$r.squared)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'r2.model'] <- c(lm.0_30.soilT$r.squared, lm.0_30.depletion$r.squared, lm.0_30.interaction$r.squared, lm.0_30.additive$r.squared)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'p.value.model'] <- c(lm.7.soilT$p.value.model, lm.7.depletion$p.value.model, lm.7.interaction$p.value.model, lm.7.additive$p.value.model)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'p.value.model'] <- c(lm.22.soilT$p.value.model, lm.22.depletion$p.value.model, lm.22.interaction$p.value.model, lm.22.additive$p.value.model)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.model'] <- c(lm.0_30.soilT$p.value.model, lm.0_30.depletion$p.value.model, lm.0_30.interaction$p.value.model, lm.0_30.additive$p.value.model)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'slope.SM'] <- c(NA, lm.7.depletion$coefficients[2, 1], lm.7.interaction$coefficients[2, 1], lm.7.additive$coefficients[2, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'slope.SM'] <- c(NA, lm.22.depletion$coefficients[2, 1], lm.22.interaction$coefficients[2, 1], lm.22.additive$coefficients[2, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.SM'] <- c(NA, lm.0_30.depletion$coefficients[2, 1], lm.0_30.interaction$coefficients[2, 1], lm.0_30.additive$coefficients[2, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'p.value.SM'] <- c(NA, lm.7.depletion$coefficients[2, 4], lm.7.interaction$coefficients[2, 4], lm.7.additive$coefficients[2, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'p.value.SM'] <- c(NA, lm.22.depletion$coefficients[2, 4], lm.22.interaction$coefficients[2, 4], lm.22.additive$coefficients[2, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.SM'] <- c(NA, lm.0_30.depletion$coefficients[2, 4], lm.0_30.interaction$coefficients[2, 4], lm.0_30.additive$coefficients[2, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'slope.T'] <- c(lm.7.soilT$coefficients[2, 1], NA, lm.7.interaction$coefficients[3, 1], lm.7.additive$coefficients[3, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'slope.T'] <- c(lm.22.soilT$coefficients[2, 1], NA, lm.22.interaction$coefficients[3, 1], lm.22.additive$coefficients[3, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.T'] <- c(lm.0_30.soilT$coefficients[2, 1], NA, lm.0_30.interaction$coefficients[3, 1], lm.0_30.interaction$coefficients[3, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'p.value.T'] <- c(lm.7.soilT$coefficients[2, 4], NA, lm.7.interaction$coefficients[3, 4], lm.7.additive$coefficients[3, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'p.value.T'] <- c(lm.22.soilT$coefficients[2, 4], NA, lm.22.interaction$coefficients[3, 4], lm.22.additive$coefficients[3, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.T'] <- c(lm.0_30.soilT$coefficients[2, 4], NA, lm.0_30.interaction$coefficients[3, 4], lm.0_30.additive$coefficients[3, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'slope.SM.T'] <- c(NA, NA, lm.7.interaction$coefficients[4, 1], NA)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'slope.SM.T'] <- c(NA, NA, lm.22.interaction$coefficients[4, 1], NA)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.SM.T'] <- c(NA, NA, lm.0_30.interaction$coefficients[4, 1], NA)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_15', 'p.value.SM.T'] <- c(NA, NA, lm.7.interaction$coefficients[4, 4], NA)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='15_30', 'p.value.SM.T'] <- c(NA, NA, lm.22.interaction$coefficients[4, 4], NA)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.SM.T'] <- c(NA, NA, lm.0_30.interaction$coefficients[4, 4], NA)
  SM_T_vs_growth_analysis$RMSE <- c(lm.7.soilT$sigma, lm.7.depletion$sigma, lm.7.interaction$sigma, lm.7.additive$sigma, lm.22.soilT$sigma, lm.22.depletion$sigma, lm.22.interaction$sigma, lm.22.additive$sigma, lm.0_30.soilT$sigma, lm.0_30.depletion$sigma, lm.0_30.interaction$sigma, lm.0_30.additive$sigma)
  SM_T_vs_growth_analysis$model.deg.freedom <- c(lm.7.soilT$df[2], lm.7.depletion$df[2], lm.7.interaction$df[2], lm.7.additive$df[2], lm.22.soilT$df[2], lm.22.depletion$df[2], lm.22.interaction$df[2], lm.22.additive$df[2], lm.0_30.soilT$df[2], lm.0_30.depletion$df[2], lm.0_30.interaction$df[2], lm.0_30.additive$df[2])
  SM_T_vs_growth_analysis$aic <- c(glm.7.soilT$aic, glm.7.depletion$aic, glm.7.interaction$aic, glm.7.additive$aic, glm.22.soilT$aic, glm.22.depletion$aic, glm.22.interaction$aic, glm.22.additive$aic, glm.0_30.soilT$aic, glm.0_30.depletion$aic, glm.0_30.interaction$aic, glm.0_30.additive$aic)
  SM_T_vs_growth_analysis$CV.RMSE <- c(glm.7.soilT_CV_RMSE, glm.7.depletion_CV_RMSE, glm.7.interaction_CV_RMSE, glm.7.additive_CV_RMSE, glm.22.soilT_CV_RMSE, glm.22.depletion_CV_RMSE, glm.22.interaction_CV_RMSE, glm.22.additive_CV_RMSE, glm.0_30.soilT_CV_RMSE, glm.0_30.depletion_CV_RMSE, glm.0_30.interaction_CV_RMSE, glm.0_30.additive_CV_RMSE)
  SM_T_vs_growth_analysis$r2.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_15'] <- lm.7.SM.vs.T$r.squared
  SM_T_vs_growth_analysis$r2.SM.vs.T[SM_T_vs_growth_analysis$depth=='15_30'] <- lm.22.SM.vs.T$r.squared
  SM_T_vs_growth_analysis$r2.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_30'] <- lm.0_30.SM.vs.T$r.squared
  SM_T_vs_growth_analysis$p.value.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_15'] <- lm.7.SM.vs.T$coefficients[2, 4]
  SM_T_vs_growth_analysis$p.value.SM.vs.T[SM_T_vs_growth_analysis$depth=='15_30'] <- lm.22.SM.vs.T$coefficients[2, 4]
  SM_T_vs_growth_analysis$p.value.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_30'] <- lm.0_30.SM.vs.T$coefficients[2, 4]
  SM_T_vs_growth_analysis$depletion.mean[SM_T_vs_growth_analysis$depth=='0_15'] <- mean(avg_depletion_7cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$depletion.mean[SM_T_vs_growth_analysis$depth=='15_30'] <- mean(avg_depletion_22cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$depletion.mean[SM_T_vs_growth_analysis$depth=='0_30'] <- mean(avg_depletion, na.rm = TRUE)
  SM_T_vs_growth_analysis$depletion.sd[SM_T_vs_growth_analysis$depth=='0_15'] <- sd(avg_depletion_7cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$depletion.sd[SM_T_vs_growth_analysis$depth=='15_30'] <- sd(avg_depletion_22cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$depletion.sd[SM_T_vs_growth_analysis$depth=='0_30'] <- sd(avg_depletion, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.mean[SM_T_vs_growth_analysis$depth=='0_15'] <- mean(avg_soilT_7cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.mean[SM_T_vs_growth_analysis$depth=='15_30'] <- mean(avg_soilT_22cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.mean[SM_T_vs_growth_analysis$depth=='0_30'] <- mean(avg_soilT, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.sd[SM_T_vs_growth_analysis$depth=='0_15'] <- sd(avg_soilT_7cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.sd[SM_T_vs_growth_analysis$depth=='15_30'] <- sd(avg_soilT_22cm, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.sd[SM_T_vs_growth_analysis$depth=='0_30'] <- sd(avg_soilT, na.rm = TRUE)
  SM_T_vs_growth_analysis$vif.depletion <- c(NA, NA, vif(glm.7.interaction)[1], vif(glm.7.additive)[1], NA, NA, vif(glm.22.interaction)[1], vif(glm.22.additive)[1], NA, NA, vif(glm.0_30.interaction)[1], vif(glm.0_30.additive)[1])
  SM_T_vs_growth_analysis$vif.soilT <- c(NA, NA, vif(glm.7.interaction)[2], vif(glm.7.additive)[2], NA, NA, vif(glm.22.interaction)[2], vif(glm.22.additive)[2], NA, NA, vif(glm.0_30.interaction)[2], vif(glm.0_30.additive)[2])
  SM_T_vs_growth_analysis$vif.interaction <- c(NA, NA, vif(glm.7.interaction)[3], NA, NA, NA, vif(glm.22.interaction)[3], NA, NA, NA, vif(glm.0_30.interaction)[3], NA)
  write.csv(SM_T_vs_growth_analysis, file.path(model_resultsDir, paste0(forage_growth, if(normalizeVars) {'.normalized.'}, 'vs_all_depths_model_results.csv')), row.names = FALSE)
  #SM_T_vs_growth_analysis
}
SM_T_interaction_model(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', TRUE, 'Feb 2017 standing forage')
SM_T_interaction_model(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', FALSE, 'Feb 2017 standing forage')
SM_T_interaction_model(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', TRUE, 'Mar 2017 growth')
SM_T_interaction_model(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', FALSE, 'Mar 2017 growth')
SM_T_interaction_model(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', TRUE, 'Apr 2017 growth')
SM_T_interaction_model(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', FALSE, 'Apr 2017 growth')
SM_T_interaction_model(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', TRUE, 'Apr 2017 standing forage')
SM_T_interaction_model(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', FALSE, 'Apr 2017 standing forage')
SM_T_interaction_model(2017, 'Dec_01_2016', 'Mar_14_2017', 'clp031417', TRUE, 'Mar 2017 standing forage')
SM_T_interaction_model(2017, 'Dec_01_2016', 'Mar_14_2017', 'clp031417', FALSE, 'Mar 2017 standing forage')
SM_T_interaction_model(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', TRUE, 'Feb 2018 standing forage')
SM_T_interaction_model(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', FALSE, 'Feb 2018 standing forage')
SM_T_interaction_model(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', TRUE, 'Mar 2018 growth')
SM_T_interaction_model(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', FALSE, 'Mar 2018 growth')
SM_T_interaction_model(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', TRUE, 'Apr 2018 growth')
SM_T_interaction_model(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', FALSE, 'Apr 2018 growth')
SM_T_interaction_model(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', TRUE, 'Apr 2018 standing forage')
SM_T_interaction_model(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', FALSE, 'Apr 2018 standing forage')
SM_T_interaction_model(2018, 'Jan_10_2018', 'Mar_22_2018', 'clp032218', TRUE, 'Mar 2018 standing forage')
SM_T_interaction_model(2018, 'Jan_10_2018', 'Mar_22_2018', 'clp032218', FALSE, 'Mar 2018 standing forage')

#got this data manually out of csv's produced above
summary_0_30 <- data.frame(model_type=c('soil moisture (direct association)', 'soil temperature (direct assocition)', 'interactive associations'), Mar2017=c(0, 0.49, 0.08), Apr2017=c(0.44, 0, 0.08), Mar2018=c(0, 0.29, 0.26), Apr2018=c(0, 0.38, 0.1))
#make a bar plot that synthesizes results
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations',  'SM_T_0_30_interaction.tif'), pointsize = 11, family = 'Times New Roman', width = 4, height = 4, units = 'in', res=150)
par(mar=c(2.5, 3.5, 1, 1))
barplot(as.matrix(summary_0_30[,2:ncol(summary_0_30)]), beside = FALSE, col=c('lightblue', 'orange', 'lightgrey'), ylim=c(0,1), ylab = '', legend.text = c('soil moisture availability (direct association)', 'soil temperature (direct association)', 'interactive associations'), cex.axis = 1, cex.names = 1, cex.lab = 1, names.arg = c('Mar 2017\ngrowth', 'Apr 2017\ngrowth', 'Mar 2018\ngrowth', 'Apr 2018\ngrowth'), args.legend = list(x="topleft", inset=0.005, cex=1))
mtext(expression('Forage growth variance explained '~r^2), side=2, line=2)
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

#to get coords of bars
testbarplot <- barplot(as.matrix(summary_0_30[,2:ncol(summary_0_30)]), beside = FALSE, col=c('lightblue', 'orange', 'lightgrey'), ylim=c(0,1), ylab = expression('Variance explained '~r^2), legend.text = c('soil moisture avail. (direct association)', 'soil temperature (direct assocition)', 'interactive associations'), cex.axis = 1, cex.names = 1, cex.lab = 1, names.arg = c('Mar 2017\ngrowth', 'Apr 2017\ngrowth', 'Mar 2018\ngrowth', 'Apr 2018\ngrowth'), args.legend = list(x="topleft", cex=1))
text(0.7, 0.2, "+", cex=1.5)
text(1.9, 0.2, "+", cex=1.5)
text(3.1, 0.2, '-', cex=1.5)
text(4.3, 0.2, '+', cex=1.5)
text(0.7, 0.7, "+")
text(1.9, 0.2, "+")
text(3.1, 0.2, '-')
text(4.3, 0.2, '+')

#simplify to reporting of 0-30 cm results
# SM_T_interaction_model_0_30 <- function(year, start_date, end_date, forage_growth, normalizeVars) {
#   if (year==2017) {
#     dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
#     avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
#     #print(avg_depletion_7cm)
#     avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
#     #print(avg_depletion_22cm)
#     avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))} #if normalization, average raw means first
#     #print(avg_depletion)
#     avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_data_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_data_7cm_2017[ ,dates], 1, mean)}
#     #print(avg_soilT_7cm)
#     avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_data_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_data_22cm_2017[ ,dates], 1, mean)}
#     #print(avg_soilT_22cm)
#     avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_data_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
#     #print(avg_soilT)
#   }
#   if (year==2018) {
#     dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
#     avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
#     avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
#     avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
#     avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_data_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_7cm_2018[ ,dates], 1, mean)}
#     avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_data_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_22cm_2018[ ,dates], 1, mean)}
#     avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_data_7cm_2018[ ,dates], 1, mean), apply(soilT_data_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
#   }
#   df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
#   df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
#   df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)
#   SM_T_vs_growth_analysis <- data.frame(growth.period=forage_growth, start_date = start_date, end_date = end_date, model.name=c('soilT', 'depletion', 'interaction', 'additive'), model.code=c(2, 1, 4, 3), depth='0_30', aic=NA, p.value.model= NA, r2.model=NA, RMSE=NA, CV.RMSE=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.SM.T=NA, p.value.SM.T=NA, model.deg.freedom=NA, p.value.SM.vs.T=NA, r2.SM.vs.T=NA, vif.depletion=NA, vif.soilT=NA, vif.interaction=NA, depletion.mean=NA, depletion.sd=NA, soilT.mean=NA, soilT.sd=NA)
#   lm.7.soilT <- summary(lm(growth_period ~ avg_soilT_7cm, data = df_7cm))
#   lm.7.soilT$p.value.model <- lm.7.soilT$coefficients[2, 4]
#   glm.7.soilT <- glm(growth_period ~ avg_soilT_7cm, data = df_7cm)
#   glm.7.soilT_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!is.na(df_7cm[,2]), ], glmfit = glm.7.soilT)$delta[1])
#   lm.7.depletion <- summary(lm(growth_period ~ avg_depletion_7cm, data = df_7cm))
#   lm.7.depletion$p.value.model <- lm.7.depletion$coefficients[2, 4]
#   glm.7.depletion <- glm(growth_period ~ avg_depletion_7cm, data = df_7cm)
#   glm.7.depletion_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!is.na(df_7cm[,3]),], glmfit = glm.7.depletion)$delta[1])
#   lm.7.interaction <- summary(lm(growth_period ~ avg_depletion_7cm * avg_soilT_7cm, data = df_7cm))
#   lm.7.interaction$p.value.model <- pf(lm.7.interaction$fstatistic[1], lm.7.interaction$fstatistic[2], lm.7.interaction$fstatistic[3], lower.tail = FALSE)
#   glm.7.interaction <- glm(growth_period ~ avg_depletion_7cm * avg_soilT_7cm, data = df_7cm)
#   glm.7.interaction_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!(is.na(df_7cm[,2]) | is.na(df_7cm[,3])), ], glmfit = glm.7.interaction)$delta[1])
#   lm.7.additive <- summary(lm(growth_period ~ avg_depletion_7cm + avg_soilT_7cm, data = df_7cm))
#   lm.7.additive$p.value.model <- pf(lm.7.additive$fstatistic[1], lm.7.additive$fstatistic[2], lm.7.additive$fstatistic[3], lower.tail = FALSE)
#   glm.7.additive <- glm(growth_period ~ avg_depletion_7cm + avg_soilT_7cm, data = df_7cm)
#   glm.7.additive_CV_RMSE <- sqrt(cv.glm(data = df_7cm[!(is.na(df_7cm[,2]) | is.na(df_7cm[,3])), ], glmfit = glm.7.additive)$delta[1])
#   lm.22.soilT <- summary(lm(growth_period ~ avg_soilT_22cm, data = df_22cm))
#   lm.22.soilT$p.value.model <- lm.22.soilT$coefficients[2, 4]
#   glm.22.soilT <- glm(growth_period ~ avg_soilT_22cm, data = df_22cm)
#   glm.22.soilT_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!is.na(df_22cm[,2]), ], glmfit = glm.22.soilT)$delta[1])
#   lm.22.depletion <- summary(lm(growth_period ~ avg_depletion_22cm, data = df_22cm))
#   lm.22.depletion$p.value.model <- lm.22.depletion$coefficients[2, 4]
#   glm.22.depletion <- glm(growth_period ~ avg_depletion_22cm, data = df_22cm)
#   glm.22.depletion_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!is.na(df_22cm[,3]),], glmfit = glm.22.depletion)$delta[1])
#   lm.22.interaction <- summary(lm(growth_period ~ avg_depletion_22cm * avg_soilT_22cm, data = df_22cm))
#   lm.22.interaction$p.value.model <- pf(lm.22.interaction$fstatistic[1], lm.22.interaction$fstatistic[2], lm.22.interaction$fstatistic[3], lower.tail = FALSE)
#   glm.22.interaction <- glm(growth_period ~ avg_depletion_22cm * avg_soilT_22cm, data = df_22cm)
#   glm.22.interaction_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!(is.na(df_22cm[,2]) | is.na(df_22cm[,3])), ], glmfit = glm.22.interaction)$delta[1])
#   
#   lm.22.additive <- summary(lm(growth_period ~ avg_depletion_22cm + avg_soilT_22cm, data = df_22cm))
#   lm.22.additive$p.value.model <- pf(lm.22.additive$fstatistic[1], lm.22.additive$fstatistic[2], lm.22.additive$fstatistic[3], lower.tail = FALSE)
#   glm.22.additive <- glm(growth_period ~ avg_depletion_22cm + avg_soilT_22cm, data = df_22cm)
#   glm.22.additive_CV_RMSE <- sqrt(cv.glm(data = df_22cm[!(is.na(df_22cm[,2]) | is.na(df_22cm[,3])), ], glmfit = glm.22.additive)$delta[1])
#   
#   lm.0_30.soilT <- summary(lm(growth_period ~ avg_soilT, data = df_0_30cm))
#   lm.0_30.soilT$p.value.model <- lm.0_30.soilT$coefficients[2, 4]
#   glm.0_30.soilT <- glm(growth_period ~ avg_soilT, data = df_0_30cm)
#   glm.0_30.soilT_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!is.na(df_0_30cm[,2]), ], glmfit = glm.0_30.soilT)$delta[1])
#   lm.0_30.depletion <- summary(lm(growth_period ~ avg_depletion, data = df_0_30cm))
#   lm.0_30.depletion$p.value.model <- lm.0_30.depletion$coefficients[2, 4]
#   glm.0_30.depletion <- glm(growth_period ~ avg_depletion, data = df_0_30cm)
#   glm.0_30.depletion_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!is.na(df_0_30cm[,3]),], glmfit = glm.0_30.depletion)$delta[1])
#   lm.0_30.interaction <- summary(lm(growth_period ~ avg_depletion * avg_soilT, data = df_0_30cm))
#   lm.0_30.interaction$p.value.model <- pf(lm.0_30.interaction$fstatistic[1], lm.0_30.interaction$fstatistic[2], lm.0_30.interaction$fstatistic[3], lower.tail = FALSE)
#   glm.0_30.interaction <- glm(growth_period ~ avg_depletion * avg_soilT, data = df_0_30cm)
#   glm.0_30.interaction_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!(is.na(df_0_30cm[,2]) | is.na(df_0_30cm[,3])), ], glmfit = glm.0_30.interaction)$delta[1])
#   lm.0_30.additive <- summary(lm(growth_period ~ avg_depletion + avg_soilT, data = df_0_30cm))
#   lm.0_30.additive$p.value.model <- pf(lm.0_30.additive$fstatistic[1], lm.0_30.additive$fstatistic[2], lm.0_30.additive$fstatistic[3], lower.tail = FALSE)
#   glm.0_30.additive <- glm(growth_period ~ avg_depletion + avg_soilT, data = df_0_30cm)
#   glm.0_30.additive_CV_RMSE <- sqrt(cv.glm(data = df_0_30cm[!(is.na(df_0_30cm[,2]) | is.na(df_0_30cm[,3])), ], glmfit = glm.0_30.additive)$delta[1])
#   lm.7.SM.vs.T <- summary(lm(avg_depletion_7cm ~ avg_soilT_7cm))
#   lm.22.SM.vs.T <- summary(lm(avg_depletion_22cm ~ avg_soilT_22cm))
#   lm.0_30.SM.vs.T <- summary(lm(avg_depletion ~ avg_soilT))
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'r2.model'] <- c(lm.0_30.soilT$r.squared, lm.0_30.depletion$r.squared, lm.0_30.interaction$r.squared, lm.0_30.additive$r.squared)
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.model'] <- c(lm.0_30.soilT$p.value.model, lm.0_30.depletion$p.value.model, lm.0_30.interaction$p.value.model, lm.0_30.additive$p.value.model)
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.SM'] <- c(NA, lm.0_30.depletion$coefficients[2, 1], lm.0_30.interaction$coefficients[2, 1], lm.0_30.additive$coefficients[2, 1])
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.SM'] <- c(NA, lm.0_30.depletion$coefficients[2, 4], lm.0_30.interaction$coefficients[2, 4], lm.0_30.additive$coefficients[2, 4])
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.T'] <- c(lm.0_30.soilT$coefficients[2, 1], NA, lm.0_30.interaction$coefficients[3, 1], lm.0_30.interaction$coefficients[3, 1])
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.T'] <- c(lm.0_30.soilT$coefficients[2, 4], NA, lm.0_30.interaction$coefficients[3, 4], lm.0_30.additive$coefficients[3, 4])
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.SM.T'] <- c(NA, NA, lm.0_30.interaction$coefficients[4, 1], NA)
#   SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.SM.T'] <- c(NA, NA, lm.0_30.interaction$coefficients[4, 4], NA)
#   SM_T_vs_growth_analysis$RMSE <- c(lm.0_30.soilT$sigma, lm.0_30.depletion$sigma, lm.0_30.interaction$sigma, lm.0_30.additive$sigma)
#   SM_T_vs_growth_analysis$model.deg.freedom <- c(lm.0_30.soilT$df[2], lm.0_30.depletion$df[2], lm.0_30.interaction$df[2], lm.0_30.additive$df[2])
#   SM_T_vs_growth_analysis$aic <- c(glm.0_30.soilT$aic, glm.0_30.depletion$aic, glm.0_30.interaction$aic, glm.0_30.additive$aic)
#   SM_T_vs_growth_analysis$CV.RMSE <- c(glm.0_30.soilT_CV_RMSE, glm.0_30.depletion_CV_RMSE, glm.0_30.interaction_CV_RMSE, glm.0_30.additive_CV_RMSE)
#   SM_T_vs_growth_analysis$r2.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_30'] <- lm.0_30.SM.vs.T$r.squared
#   SM_T_vs_growth_analysis$p.value.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_30'] <- lm.0_30.SM.vs.T$coefficients[2, 4]
#   SM_T_vs_growth_analysis$depletion.mean[SM_T_vs_growth_analysis$depth=='0_30'] <- mean(avg_depletion, na.rm = TRUE)
#   SM_T_vs_growth_analysis$depletion.sd[SM_T_vs_growth_analysis$depth=='0_30'] <- sd(avg_depletion, na.rm = TRUE)
#   SM_T_vs_growth_analysis$soilT.mean[SM_T_vs_growth_analysis$depth=='0_30'] <- mean(avg_soilT, na.rm = TRUE)
#   SM_T_vs_growth_analysis$soilT.sd[SM_T_vs_growth_analysis$depth=='0_30'] <- sd(avg_soilT, na.rm = TRUE)
#   SM_T_vs_growth_analysis$vif.depletion <- c(NA, NA, vif(glm.0_30.interaction)[1], vif(glm.0_30.additive)[1])
#   SM_T_vs_growth_analysis$vif.soilT <- c(NA, NA, vif(glm.0_30.interaction)[2], vif(glm.0_30.additive)[2])
#   SM_T_vs_growth_analysis$vif.interaction <- c(NA, NA, vif(glm.0_30.interaction)[3], NA)
#   write.csv(SM_T_vs_growth_analysis, file.path(model_resultsDir, '0_30cm_only', paste0(forage_growth, if(normalizeVars) {'.normalized.'}, 'vs_0_30cm_model_results.csv')), row.names = FALSE)
#   #SM_T_vs_growth_analysis
# }
# SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', TRUE)
# SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', FALSE)
# SM_T_interaction_model_0_30(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', TRUE)
# SM_T_interaction_model_0_30(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', FALSE)
# SM_T_interaction_model_0_30(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', TRUE)
# SM_T_interaction_model_0_30(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', FALSE)
# SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', TRUE)
# SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', FALSE)
# SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', TRUE)
# SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', FALSE)
# SM_T_interaction_model_0_30(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', TRUE)
# SM_T_interaction_model_0_30(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', FALSE)
# SM_T_interaction_model_0_30(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', TRUE)
# SM_T_interaction_model_0_30(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', FALSE)
# SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', TRUE)
# SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', FALSE)

#get list of these that are normalized
normalized_results_fnames <- list.files(path = file.path(model_resultsDir), pattern = glob2rx(paste0('*normalized.vs_all_depths_model_results.csv')), full.names = TRUE)#file.path(model_resultsDir, paste0(forage_growth, if(normalizeVars) {'.normalized.'}, 'vs_all_depths_model_results.csv'
all_0_30cm_results <- do.call(rbind, lapply(normalized_results_fnames, read.csv, stringsAsFactors=FALSE))
all_0_30cm_results <- all_0_30cm_results[all_0_30cm_results$depth=='0_30',]
all_0_30cm_results <- all_0_30cm_results[order(as.Date(all_0_30cm_results$start_date, format = '%b_%d_%Y'), as.Date(all_0_30cm_results$end_date, format='%b_%d_%Y'), all_0_30cm_results$model.code), ]
write.csv(all_0_30cm_results, file.path(model_resultsDir, '0_30cm_only', 'forage_growth_vs.normalized.0_30cm_all_model_results.csv'), row.names=FALSE)

all_0_15cm_results <- do.call(rbind, lapply(normalized_results_fnames, read.csv, stringsAsFactors=FALSE))
all_0_15cm_results <- all_0_15cm_results[all_0_15cm_results$depth=='0_15',]
all_0_15cm_results <- all_0_15cm_results[order(as.Date(all_0_15cm_results$start_date, format = '%b_%d_%Y'), as.Date(all_0_15cm_results$end_date, format='%b_%d_%Y'), all_0_15cm_results$model.code), ]
write.csv(all_0_15cm_results, file.path(model_resultsDir, '0_15cm_only', 'forage_growth_vs.normalized.0_15cm_all_model_results.csv'), row.names=FALSE)

all_15_30cm_results <- do.call(rbind, lapply(normalized_results_fnames, read.csv, stringsAsFactors=FALSE))
all_15_30cm_results <- all_15_30cm_results[all_15_30cm_results$depth=='15_30',]
all_15_30cm_results <- all_15_30cm_results[order(as.Date(all_15_30cm_results$start_date, format = '%b_%d_%Y'), as.Date(all_15_30cm_results$end_date, format='%b_%d_%Y'), all_15_30cm_results$model.code), ]
write.csv(all_15_30cm_results, file.path(model_resultsDir, '15_30cm_only', 'forage_growth_vs.normalized.15_30cm_all_model_results.csv'), row.names=FALSE)

results_fnames <- list.files(path = file.path(model_resultsDir), pattern = glob2rx(paste0('*vs_all_depths_model_results.csv')), full.names = TRUE)#file.path(model_resultsDir, paste0(forage_growth, if(normalizeVars) {'.normalized.'}, 'vs_all_depths_model_results.csv'
results_fnames <- results_fnames[!grepl('normalized', results_fnames)]
all_0_30cm_results <- do.call(rbind, lapply(results_fnames, read.csv, stringsAsFactors=FALSE))
all_0_30cm_results <- all_0_30cm_results[all_0_30cm_results$depth=='0_30',]
all_0_30cm_results <- all_0_30cm_results[order(as.Date(all_0_30cm_results$start_date, format = '%b_%d_%Y'), as.Date(all_0_30cm_results$end_date, format='%b_%d_%Y'), all_0_30cm_results$model.code), ]
write.csv(all_0_30cm_results, file.path(model_resultsDir, '0_30cm_only', 'forage_growth_vs.0_30cm_all_model_results.csv'), row.names=FALSE)

all_0_15cm_results <- do.call(rbind, lapply(results_fnames, read.csv, stringsAsFactors=FALSE))
all_0_15cm_results <- all_0_15cm_results[all_0_15cm_results$depth=='0_15',]
all_0_15cm_results <- all_0_15cm_results[order(as.Date(all_0_15cm_results$start_date, format = '%b_%d_%Y'), as.Date(all_0_15cm_results$end_date, format='%b_%d_%Y'), all_0_15cm_results$model.code), ]
write.csv(all_0_15cm_results, file.path(model_resultsDir, '0_15cm_only', 'forage_growth_vs.0_15cm_all_model_results.csv'), row.names=FALSE)
#to-do: paste 2017 forage growth vs. SM * T figures from 'make_daily_plots_SMnorm_Tmodel.R' here

#2018 figures
#Mar 2018 growth fig
#SM_T_interaction_model(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', TRUE, 'Mar 2018 growth')
start_date <- 'Feb_16_2018'
end_date <- 'Mar_22_2018'
normalizeVars <- FALSE
forage_growth <- 'Mar2018growth'
dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_data_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_7cm_2018[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_data_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_22cm_2018[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_data_7cm_2018[ ,dates], 1, mean), apply(soilT_data_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

#0-30 cm plot forage growth vs. soil T plot
cex.adjuster <- 5
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Mar2018.growth.vs.soilT30cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4.25, 1, 1))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, ylab=expression('forage growth, Feb 16-Mar 22 (kg'~~ha^-1*')'), xlab=expression('0-30 cm soil temperature, Feb 16-Mar 22, 2018 mean ('*degree*'C)'), cex.axis=1, xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_0_30cm$avg_depletion < 0.1, 0.1*cex.adjuster, df_0_30cm$avg_depletion * cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
#axis(side = 2, at=c(-0.2, 0, 0.2, 0.4), labels = c('-20', '0', '20', '40'), mgp=c(2.5, 1, 0))
legend('bottomleft', legend=c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), '<10% PAW', '25% PAW', '40% PAW'), pch = c(19,19,19,1,1,1), pt.cex = c(0.25*cex.adjuster, 0.25*cex.adjuster, 0.25*cex.adjuster, 0.1*cex.adjuster, 0.25*cex.adjuster, 0.4*cex.adjuster), col=c('blue', 'orange2', 'red3', 'black', 'black', 'black'), inset=0.01, cex = 1)#, bty = 'n') #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
#rect(xleft = 7.9, ytop = .05, xright = 12.15, ybottom = -0.22)
#abline(v=13, lty=2)
#text(x=9, y=0.41, label=expression('< 13'~degree*'C soil temperature'), cex=1, adj=c(0,0))
#text(x=9, y=0.38, label='soil temperature', cex=1, adj=c(0,0))
#text(x=9, y=0.37, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
#text(x=13.1, y=0.41, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
#text(x=13.1, y=0.375, label='soil temperature', cex=1, adj=c(0,0))
#text(x=13.1, y=0.33, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()

#0-7 cm forage vs. temperature plot with aws symbolization
cex.adjuster <- 4
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Mar2018.growth.vs.soilT7cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4.25, 1, 1))
plot(df_7cm$avg_soilT, df_7cm$growth_period, ylab=expression('Forage growth, Feb 16-Mar 22, 2018 (kg'~~ha^-1*')'), xlab=expression('7 cm soil temperature, Feb 16-Mar 22, 2018 mean ('*degree*'C)'), cex.axis=1, xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_7cm$avg_depletion < 0.1, 0.1*cex.adjuster, df_7cm$avg_depletion * cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
#axis(side = 2, at=c(-0.2, 0, 0.2, 0.4), labels = c('-20', '0', '20', '40'), mgp=c(2.5, 1, 0))
abline(lm(growth_period ~ avg_soilT, data=df_7cm), lty=2)
legend('bottomleft', legend=c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), '<10% PAW, period mean', '25% PAW, period mean', '50% PAW, period mean'), pch = c(19,19,19,1,1,1), pt.cex = c(0.25*cex.adjuster, 0.25*cex.adjuster, 0.25*cex.adjuster, 0.1*cex.adjuster, 0.25*cex.adjuster, 0.5*cex.adjuster), col=c('blue', 'orange2', 'red3', 'black', 'black', 'black'), inset=0.01, cex = 1)#, bty = 'n') #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
text(x=12.5, y=580, label='linear model results', cex=1, adj=c(0,0))
text(x=12.5, y=505, label=expression(paste(r^2, ' = 0.29, p.val = 0.03')), cex = 1, adj = c(0, 0))
text(x=12.5, y=430, label=expression(paste('RMSE = 222 kg ', ha^-1)), cex = 1, adj = c(0, 0))
#rect(xleft = 7.9, ytop = .05, xright = 12.15, ybottom = -0.22)
#abline(v=13, lty=2)
#text(x=9, y=0.41, label=expression('< 13'~degree*'C soil temperature'), cex=1, adj=c(0,0))
#text(x=9, y=0.38, label='soil temperature', cex=1, adj=c(0,0))
#text(x=9, y=0.37, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
#text(x=13.1, y=0.41, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
#text(x=13.1, y=0.375, label='soil temperature', cex=1, adj=c(0,0))
#text(x=13.1, y=0.33, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()

#0-7 cm plot
cex.adjuster <- 175
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Mar2018.growth.vs.SM_T_int7cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4.5, units = 'in', res = 150)
par(mar=c(4, 4, 1, 1))
plot(df_7cm$avg_soilT, df_7cm$avg_depletion, ylab='0-15 cm plant available water, Feb 16-Mar 22, 2018 mean (%)', xlab=expression('0-15 cm soil temperature, Feb 16-Mar 22, 2018 mean ('*degree*'C)'), cex.axis=1, yaxt='n', xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_7cm$growth_period < 100, 100/cex.adjuster, df_7cm$growth_period/cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
axis(side = 2, at=c(-0.2, 0, 0.2, 0.4, 0.6), labels = c('-20', '0', '20', '40', '60'), mgp=c(2.5, 1, 0))
legend('bottomleft', legend=c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), expression('<100 kg'~ha^-1~'growth'), expression('300 kg'~ha^-1~'growth'), expression('600 kg'~ha^-1~'growth')), pch = c(19,19,19,1,1,1), pt.cex = c(300/cex.adjuster, 300/cex.adjuster, 300/cex.adjuster, 100/cex.adjuster, 300/cex.adjuster, 600/cex.adjuster), col=c('blue', 'orange2', 'red3', 'black', 'black', 'black'), inset=0.01, cex = 1) #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
abline(v=13, lty=2)
#text(x=10.5, y=0.6, label=expression('< 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=9, y=0.54, label=expression('< 13'~degree*'C soil temperature'), cex=1, adj=c(0,0))
text(x=9, y=0.48, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
text(x=13.1, y=0.54, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=13.1, y=0.48, label='soil temperature', cex=1, adj=c(0,0))
text(x=13.1, y=0.4, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()

#0-30 cm plot
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Mar2018.growth.vs.SM_T_int30cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4.5, units = 'in', res = 150)
par(mar=c(4, 4, 1, 1))
plot(df_0_30cm$avg_soilT, df_0_30cm$avg_depletion, ylab='0-30 cm plant available water, Feb 16-Mar 22, 2018 mean (%)', xlab=expression('0-30 cm soil temperature, Feb 16-Mar 22, 2018 mean ('*degree*'C)'), cex.axis=1, yaxt='n', xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_0_30cm$growth_period < 100, 100/cex.adjuster, df_0_30cm$growth_period/cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
axis(side = 2, at=c(-0.2, 0, 0.2, 0.4), labels = c('-20', '0', '20', '40'), mgp=c(2.5, 1, 0))
legend('bottomleft', legend=c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), expression('<100 kg'~ha^-1~'growth'), expression('300 kg'~ha^-1~'growth'), expression('600 kg'~ha^-1~'growth')), pch = c(19,19,19,1,1,1), pt.cex = c(300/cex.adjuster, 300/cex.adjuster, 300/cex.adjuster, 100/cex.adjuster, 300/cex.adjuster, 600/cex.adjuster), col=c('blue', 'orange2', 'red3', 'black', 'black', 'black'), inset=0.01, cex = 1) #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
abline(v=13, lty=2)
text(x=9, y=0.42, label=expression('< 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=9, y=0.38, label='soil temperature', cex=1, adj=c(0,0))
text(x=9, y=0.34, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
text(x=13.1, y=0.42, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=13.1, y=0.38, label='soil temperature', cex=1, adj=c(0,0))
text(x=13.1, y=0.34, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()

#0-30 cm plot v2
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Mar2018.growth.vs.SM_T_int30cm.v2.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4.25, 1, 1))
plot(df_0_30cm$avg_soilT, df_0_30cm$avg_depletion, ylab='0-30 cm mean plant available water (%)', xlab=expression('0-30 cm soil temperature, Feb 16-Mar 22, 2018 mean ('*degree*'C)'), cex.axis=1, yaxt='n', xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_0_30cm$growth_period < 100, 100/cex.adjuster, df_0_30cm$growth_period/cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
axis(side = 2, at=c(-0.2, 0, 0.2, 0.4), labels = c('-20', '0', '20', '40'), mgp=c(2.5, 1, 0))
legend('bottomleft', legend=c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), expression('<100 kg'~ha^-1~'growth'), expression('300 kg'~ha^-1~'growth'), expression('600 kg'~ha^-1~'growth')), pch = c(19,19,19,1,1,1), pt.cex = c(300/cex.adjuster, 300/cex.adjuster, 300/cex.adjuster, 100/cex.adjuster, 300/cex.adjuster, 600/cex.adjuster), col=c('blue', 'orange2', 'red3', 'black', 'black', 'black'), inset=0.01, cex = 1, bty = 'n') #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
rect(xleft = 7.9, ytop = .05, xright = 12.15, ybottom = -0.22)
abline(v=13, lty=2)
text(x=9, y=0.41, label=expression('< 13'~degree*'C soil temperature'), cex=1, adj=c(0,0))
#text(x=9, y=0.38, label='soil temperature', cex=1, adj=c(0,0))
text(x=9, y=0.37, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
text(x=13.1, y=0.41, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=13.1, y=0.375, label='soil temperature', cex=1, adj=c(0,0))
text(x=13.1, y=0.33, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()

#15-30 cm plot for Mar 2018 growth
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Mar2018.growth.vs.SM_T_int22cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4.5, units = 'in', res = 150)
par(mar=c(4, 4, 1, 1))
plot(df_22cm$avg_soilT, df_22cm$avg_depletion, ylab='15-30 cm plant available water, Feb 16-Mar 22, 2018 mean (%)', xlab=expression('15-30 cm soil temperature, Feb 16-Mar 22, 2018 mean ('*degree*'C)'), cex.axis=1, yaxt='n', xlim=c(8.1, 15.1), cex.lab=1, pch=19, cex=ifelse(df_22cm$growth_period < 100, 100/cex.adjuster, df_22cm$growth_period/cex.adjuster), col=forage_terrain_energy$energy_colors) #mgp=c(2.5, 1, 0)) #  cex=forage_terrain_energy$clp031417/750
axis(side = 2, at=c(-0.2, 0, 0.2, 0.4), labels = c('-20', '0', '20', '40'), mgp=c(2.5, 1, 0))
legend('bottomleft', legend=c(expression('< 1200 annual kWh'~m^-2), expression('1200-1410 annual kWh'~m^-2), expression('>1410 annual kWh'~m^-2), expression('<100 kg'~ha^-1~'growth'), expression('300 kg'~ha^-1~'growth'), expression('600 kg'~ha^-1~'growth')), pch = c(19,19,19,1,1,1), pt.cex = c(300/cex.adjuster, 300/cex.adjuster, 300/cex.adjuster, 100/cex.adjuster, 300/cex.adjuster, 600/cex.adjuster), col=c('blue', 'orange2', 'red3', 'black', 'black', 'black'), inset=0.01, cex = 1) #y.intersp =1) # expression('1000 kg'~ha^-1~'Mar 14, 2017'), expression('2500 kg'~ha^-1~'Mar 14, 2017')
abline(v=13, lty=2)
text(x=9.5, y=0.28, label=expression('< 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=9.5, y=0.25, label='soil temperature', cex=1, adj=c(0,0))
text(x=9.5, y=0.21, label=expression('414 \u00B1 160 kg'~ha^-1~'growth'), cex=1, adj=c(0,0))
text(x=13.1, y=0.28, label=expression('> 13'~degree*'C'), cex=1, adj=c(0,0))
text(x=13.1, y=0.25, label='soil temperature', cex=1, adj=c(0,0))
text(x=13.1, y=0.21, label=expression('84 \u00B1 240 kg'~ha^-1), cex=1, adj=c(0,0))
dev.off()

#Apr 2018 growth plots
# SM_T_interaction_model(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', TRUE, 'Apr 2018 growth')
start_date <- 'Mar_23_2018'
end_date <- 'Apr_15_2018'
normalizeVars <- FALSE
forage_growth <- 'Apr2018growth'
dates <- which(colnames(depletion_vwc_2018)==start_date):which(colnames(depletion_vwc_2018)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018[ ,dates], 1, mean))} else {apply(depletion_vwc_2018[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2018_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2018_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2018[ ,dates], 1, mean), apply(depletion_vwc_2018_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_data_7cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_7cm_2018[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_data_22cm_2018[ ,dates], 1, mean))} else {apply(soilT_data_22cm_2018[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_data_7cm_2018[ ,dates], 1, mean), apply(soilT_data_22cm_2018[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

#0-30 cm forage vs soil T plot
cex.adjuster <- 4
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Apr2018.growth.vs.SM_T_int30cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4.25, 1, 1))
plot(df_0_30cm$avg_soilT, df_0_30cm$growth_period, ylab=expression(' Forage growth, Mar 23-Apr 15, 2018 (kg '~~ha^-1*')'), xlab=expression('0-30 cm soil temperature, Mar 23-Apr 15, 2018 mean ('*degree*'C)'), cex.axis=1, cex.lab=1, pch=19, cex=df_0_30cm$avg_depletion*cex.adjuster, col=forage_terrain_energy$energy_colors)
#axis(side = 2, at=c(0, 200, 400, 600, 800), labels = c('0', '200', '400', '600', '800'), mgp=c(2.5, 1, 0))
legend('bottomright', legend=c('40% PAW', '55% PAW', '70% PAW'), pch = c(1,1,1), pt.cex = c(0.4*cex.adjuster, 0.55*cex.adjuster, 0.7*cex.adjuster), col='black', inset=0.01, cex = 1, title = '0-30 cm mean\nplant available water\nMar 23-Apr 15, 2018', bty='n')
rect(xleft = 17.4, ybottom = -162, xright = 19.71, ytop = 275)
dev.off()

#7 cm forage vs soil T plot
cex.adjuster <- 4
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2018 growth', 'Apr2018.growth.vs.temp7cm.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 4, units = 'in', res = 150)
par(mar=c(4, 4.25, 1, 1))
plot(df_7cm$avg_soilT, df_7cm$growth_period, ylab=expression('Forage growth, Mar 23-Apr 15, 2018 (kg '~~ha^-1*')'), xlab=expression('7 cm soil temperature, Mar 23-Apr 15, 2018 mean ('*degree*'C)'), cex.axis=1, cex.lab=1, pch=19, cex=df_7cm$avg_depletion*cex.adjuster, col=forage_terrain_energy$energy_colors)
abline(lm(growth_period ~ avg_soilT, data = df_7cm), lty = 2)
#axis(side = 2, at=c(0, 200, 400, 600, 800), labels = c('0', '200', '400', '600', '800'), mgp=c(2.5, 1, 0))
#legend('bottomright', legend=c('40% PAW', '55% PAW', '70% PAW'), pch = c(1,1,1), pt.cex = c(0.4*cex.adjuster, 0.55*cex.adjuster, 0.7*cex.adjuster), col='black', inset=0.01, cex = 1, title = '0-30 cm mean\nplant available water\nMar 23-Apr 15, 2018', bty='n') #used legend from March 2018 plot
#rect(xleft = 17.4, ybottom = -162, xright = 19.71, ytop = 275)
text(x=14.1, y=825, label='linear model results', cex=1, adj=c(0,0))
text(x=14.1, y=750, label=expression(paste(r^2, ' = 0.38, p.val = 0.01')), cex = 1, adj = c(0, 0))
text(x=14.1, y=675, label=expression(paste('RMSE = 221 kg ', ha^-1)), cex = 1, adj = c(0, 0))
dev.off()
