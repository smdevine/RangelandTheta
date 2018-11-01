library(boot)
library(car)
library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/growth_models'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
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
# year <- 2017
# start_date <- 'Feb_16_2017'
# end_date <- 'Mar_14_2017'
# forage_growth <- 'Mar2017growth'
# normalizeVars <- TRUE
SM_T_interaction_model <- function(year, start_date, end_date, forage_growth, normalizeVars) {
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
  SM_T_vs_growth_analysis <- data.frame(growth.period=forage_growth, start_date = start_date, end_date = end_date, model.name=rep(c('soilT', 'depletion', 'interaction', 'additive'), 3), depth=c(rep('0_15', 4), rep('15_30', 4), rep('0_30', 4)), aic=NA, p.value.model= NA, r2.model=NA, RMSE=NA, CV.RMSE=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.SM.T=NA, p.value.SM.T=NA, model.deg.freedom=NA, p.value.SM.vs.T=NA, r2.SM.vs.T=NA, vif.depletion=NA, vif.soilT=NA, vif.interaction=NA, depletion.mean=NA, depletion.sd=NA, soilT.mean=NA, soilT.sd=NA)
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
  SM_T_vs_growth_analysis
}
#SM_T_interaction_model(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', FALSE)
Feb2017growth <- SM_T_interaction_model(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', TRUE)
Feb2017growth <- SM_T_interaction_model(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', FALSE)
Mar2017growth <- SM_T_interaction_model(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', TRUE)
Mar2017growth <- SM_T_interaction_model(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', FALSE)
Apr2017growth <- SM_T_interaction_model(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', TRUE)
Apr2017growth <- SM_T_interaction_model(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', FALSE)
Apr.forage.2017 <- SM_T_interaction_model(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', TRUE)
Apr.forage.2017 <- SM_T_interaction_model(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', FALSE)
#May2017growth <- SM_T_interaction_model(2017, 'Apr_11_2017', 'May_01_2017', 'May2017growth', TRUE)
#peak2017 <- SM_T_interaction_model(2017, 'Dec_01_2016', 'May_01_2017', 'peak2017', TRUE)
Feb2018growth <- SM_T_interaction_model(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', TRUE)
Feb2018growth <- SM_T_interaction_model(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', FALSE)
Mar2018growth <- SM_T_interaction_model(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', TRUE)
Mar2018growth <- SM_T_interaction_model(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', FALSE)
Apr2018growth <- SM_T_interaction_model(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', TRUE)
Apr2018growth <- SM_T_interaction_model(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', FALSE)
Apr.forage.2018 <- SM_T_interaction_model(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', TRUE)
Apr.forage.2018 <- SM_T_interaction_model(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', FALSE)
#peak2018 <- SM_T_interaction_model(2018, 'Jan_10_2018', 'Apr_15_2018', 'peak2018', TRUE)

summary_0_30 <- data.frame(model_type=c('soil moisture (direct association)', 'soil temperature (direct assocition)', 'interactive associations'), Mar2017=c(0, 0.49, 0.08), Apr2017=c(0.44, 0, 0.08), Mar2018=c(0, 0.29, 0.26), Apr2018=c(0, 0.38, 0.1))
#make a bar plot that synthesizes results
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations',  'SM_T_0_30_interaction.tif'), pointsize = 11, family = 'Times New Roman', width = 4, height = 4, units = 'in', res=150)
par(mar=c(2.5, 3.5, 1, 1))
barplot(as.matrix(summary_0_30[,2:ncol(summary_0_30)]), beside = FALSE, col=c('lightblue', 'orange', 'lightgrey'), ylim=c(0,1), ylab = '', legend.text = c('soil moisture avail. (direct association)', 'soil temperature (direct association)', 'interactive associations'), cex.axis = 1, cex.names = 1, cex.lab = 1, names.arg = c('Mar 2017\ngrowth', 'Apr 2017\ngrowth', 'Mar 2018\ngrowth', 'Apr 2018\ngrowth'), args.legend = list(x="topleft", inset=0.005, cex=1))
mtext(expression('Forage growth variance explained '~r^2), side=2, line=2)
text(0.7, 0.2, "+", cex=1.5)
text(1.9, 0.2, "+", cex=1.5)
text(3.1, 0.2, '-', cex=1.5)
text(4.3, 0.2, '+', cex=1.5)
#text(0.7, 0.65, expression(atop("37 \u00B1 20", kg~ha^-1~d^-1)))
text(0.05, 0.58, expression("37 \u00B1 20"~kg~ha^-1~day^-1), adj = c(0,0))
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
SM_T_interaction_model_0_30 <- function(year, start_date, end_date, forage_growth, normalizeVars) {
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
  SM_T_vs_growth_analysis <- data.frame(growth.period=forage_growth, start_date = start_date, end_date = end_date, model.name=c('soilT', 'depletion', 'interaction', 'additive'), depth='0_30', aic=NA, p.value.model= NA, r2.model=NA, RMSE=NA, CV.RMSE=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.SM.T=NA, p.value.SM.T=NA, model.deg.freedom=NA, p.value.SM.vs.T=NA, r2.SM.vs.T=NA, vif.depletion=NA, vif.soilT=NA, vif.interaction=NA, depletion.mean=NA, depletion.sd=NA, soilT.mean=NA, soilT.sd=NA)
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
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'r2.model'] <- c(lm.0_30.soilT$r.squared, lm.0_30.depletion$r.squared, lm.0_30.interaction$r.squared, lm.0_30.additive$r.squared)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.model'] <- c(lm.0_30.soilT$p.value.model, lm.0_30.depletion$p.value.model, lm.0_30.interaction$p.value.model, lm.0_30.additive$p.value.model)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.SM'] <- c(NA, lm.0_30.depletion$coefficients[2, 1], lm.0_30.interaction$coefficients[2, 1], lm.0_30.additive$coefficients[2, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.SM'] <- c(NA, lm.0_30.depletion$coefficients[2, 4], lm.0_30.interaction$coefficients[2, 4], lm.0_30.additive$coefficients[2, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.T'] <- c(lm.0_30.soilT$coefficients[2, 1], NA, lm.0_30.interaction$coefficients[3, 1], lm.0_30.interaction$coefficients[3, 1])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.T'] <- c(lm.0_30.soilT$coefficients[2, 4], NA, lm.0_30.interaction$coefficients[3, 4], lm.0_30.additive$coefficients[3, 4])
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'slope.SM.T'] <- c(NA, NA, lm.0_30.interaction$coefficients[4, 1], NA)
  SM_T_vs_growth_analysis[SM_T_vs_growth_analysis$depth=='0_30', 'p.value.SM.T'] <- c(NA, NA, lm.0_30.interaction$coefficients[4, 4], NA)
  SM_T_vs_growth_analysis$RMSE <- c(lm.0_30.soilT$sigma, lm.0_30.depletion$sigma, lm.0_30.interaction$sigma, lm.0_30.additive$sigma)
  SM_T_vs_growth_analysis$model.deg.freedom <- c(lm.0_30.soilT$df[2], lm.0_30.depletion$df[2], lm.0_30.interaction$df[2], lm.0_30.additive$df[2])
  SM_T_vs_growth_analysis$aic <- c(glm.0_30.soilT$aic, glm.0_30.depletion$aic, glm.0_30.interaction$aic, glm.0_30.additive$aic)
  SM_T_vs_growth_analysis$CV.RMSE <- c(glm.0_30.soilT_CV_RMSE, glm.0_30.depletion_CV_RMSE, glm.0_30.interaction_CV_RMSE, glm.0_30.additive_CV_RMSE)
  SM_T_vs_growth_analysis$r2.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_30'] <- lm.0_30.SM.vs.T$r.squared
  SM_T_vs_growth_analysis$p.value.SM.vs.T[SM_T_vs_growth_analysis$depth=='0_30'] <- lm.0_30.SM.vs.T$coefficients[2, 4]
  SM_T_vs_growth_analysis$depletion.mean[SM_T_vs_growth_analysis$depth=='0_30'] <- mean(avg_depletion, na.rm = TRUE)
  SM_T_vs_growth_analysis$depletion.sd[SM_T_vs_growth_analysis$depth=='0_30'] <- sd(avg_depletion, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.mean[SM_T_vs_growth_analysis$depth=='0_30'] <- mean(avg_soilT, na.rm = TRUE)
  SM_T_vs_growth_analysis$soilT.sd[SM_T_vs_growth_analysis$depth=='0_30'] <- sd(avg_soilT, na.rm = TRUE)
  SM_T_vs_growth_analysis$vif.depletion <- c(NA, NA, vif(glm.0_30.interaction)[1], vif(glm.0_30.additive)[1])
  SM_T_vs_growth_analysis$vif.soilT <- c(NA, NA, vif(glm.0_30.interaction)[2], vif(glm.0_30.additive)[2])
  SM_T_vs_growth_analysis$vif.interaction <- c(NA, NA, vif(glm.0_30.interaction)[3], NA)
  write.csv(SM_T_vs_growth_analysis, file.path(model_resultsDir, '0_30cm_only', paste0(forage_growth, if(normalizeVars) {'.normalized.'}, 'vs_0_30cm_model_results.csv')), row.names = FALSE)
  #SM_T_vs_growth_analysis
}
SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', TRUE)
SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Feb_15_2017', 'clp021517', FALSE)
SM_T_interaction_model_0_30(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', TRUE)
SM_T_interaction_model_0_30(2017, 'Feb_16_2017', 'Mar_14_2017', 'Mar2017growth', FALSE)
SM_T_interaction_model_0_30(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', TRUE)
SM_T_interaction_model_0_30(2017, 'Mar_15_2017', 'Apr_10_2017', 'Apr2017growth', FALSE)
SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', TRUE)
SM_T_interaction_model_0_30(2017, 'Dec_01_2016', 'Apr_10_2017', 'clp041017', FALSE)
SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', TRUE)
SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Feb_15_2018', 'clp021518', FALSE)
SM_T_interaction_model_0_30(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', TRUE)
SM_T_interaction_model_0_30(2018, 'Feb_16_2018', 'Mar_22_2018', 'Mar2018growth', FALSE)
SM_T_interaction_model_0_30(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', TRUE)
SM_T_interaction_model_0_30(2018, 'Mar_23_2018', 'Apr_15_2018', 'Apr2018growth', FALSE)
SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', TRUE)
SM_T_interaction_model_0_30(2018, 'Jan_10_2018', 'Apr_15_2018', 'clp041518', FALSE)
