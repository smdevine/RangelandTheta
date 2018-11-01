library(extrafont)
library(extrafontdb)
loadfonts()
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
vwc_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'VWC', paste0('MeanVWC_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
vwc_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'VWC', paste0('MeanVWC_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_7cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_7cm_2018_max <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MaxT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
depletion_vwc_2017 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_7cm_2018.csv'), stringsAsFactors=FALSE)
depletion_vwc_2017_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2017.csv'), stringsAsFactors=FALSE)
depletion_vwc_2018_22 <- read.csv(file.path(results, 'processed_soil_moisture/Jul2018/depletion_vwc/depletion_vwc_22cm_2018.csv'), stringsAsFactors=FALSE)
forage_terrain_energy <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
#add energy colors
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, 'orange2', 'red3')) #tres colores #tres colores
# forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > summary(forage_terrain_energy$annual_kwh.m2)[2] & forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[5], 'orange2', 'red3'))
forage_terrain_energy$May2017growth <- forage_terrain_energy$clp050117 - forage_terrain_energy$clp041017
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
forage_terrain_energy$Mar2017growth <- forage_terrain_energy$clp031417 - forage_terrain_energy$clp021517
rank_test <- function(x, df, y, mtd) {
  test <- cor.test(x, df[[y]], method = mtd)
  result <- data.frame(col.1=test$p.value, col.2=test$estimate)
  colnames(result) <- c(paste0(y, '.p.val.', mtd), paste0(y, if(mtd=='pearson') {'.tau.'} else {'.rho.'}, mtd))
  result
}
#gap-fill avg soil T 3-9/17 - 4/15/17 for location 13, which was not previously gap-filled because VWC cannot practically be gap-filled during this timframe
# df <- soilT_7cm_2017
# location_fix <- 13
# location_use <- 4
# location_use2 <- 14
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

#get some soil T stats
apply(soilT_7cm_2018[ ,2:ncol(soilT_7cm_2018)], 2, function(x) max(x) - min(x))
apply(soilT_7cm_2017[ ,2:ncol(soilT_7cm_2018)], 2, function(x) max(x) - min(x))
dates_2017 <- which(colnames(soilT_7cm_2017)=='Jan_10_2017'):which(colnames(soilT_7cm_2017)=='Apr_15_2017')
dates_2018 <- which(colnames(soilT_7cm_2017)=='Jan_10_2017'):which(colnames(soilT_7cm_2017)=='Apr_15_2017')
soilT_2017_mean <- apply(soilT_7cm_2017[ ,dates_2017], 1, mean)
soilT_2018_mean <- apply(soilT_7cm_2018[ ,dates_2018], 1, mean)
summary(soilT_2017_mean)
summary(soilT_2018_mean)
summary(soilT_2018_mean - soilT_2017_mean)

#non-linear function to plot 2018 curves
nlm_2018 <- function(x, date) {
  nlm_obj <- lm(forage_terrain_energy[,date][] ~ avgsoilT_2018[] + I(avgsoilT_2018[]^2))
  nlm_obj$coefficients[1] + nlm_obj$coefficients[2] * x + nlm_obj$coefficients[3] * x^2
}

#overall relationship between Mar forage 2017 and soil T
avgsoilT_dates_2017 <- which(colnames(soilT_7cm_2017)=='Dec_01_2016'):which(colnames(soilT_7cm_2017)=='Mar_15_2017') #because location 13 lost on Mar 10th
avgsoilT_2017 <- apply(soilT_7cm_2017[ ,avgsoilT_dates_2017], 1, mean)
summary(avgsoilT_2017)
sd(avgsoilT_2017, na.rm = TRUE)
hist(avgsoilT_2017)
avgVWCdepletion_2017 <- apply(depletion_vwc_2017[ ,avgsoilT_dates_2017], 1, mean)
summary(avgVWCdepletion_2017)
plot(avgsoilT_2017, avgVWCdepletion_2017)
plot(avgsoilT_2017[-13], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13]), lty=2)
summary(lm(forage_terrain_energy$clp031417 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13])) #61% variance explained; 370 kg/ha per deg C warming; p.val < 0.001
plot(avgsoilT_2017, forage_terrain_energy$clp031417, col=forage_terrain_energy$energy_colors)
exp_model <- lm(log(forage_terrain_energy$clp031417) ~ log(avgsoilT_2017))
plot(avgsoilT_2017, log(forage_terrain_energy$clp031417))
summary(exp_model)
plot(exp_model)
exp_model_func <- function(x) {exp_model$coefficients[1] + log(x)^exp_model$coefficients[2]}
exp_model_func(avgsoilT_2017)
curve(exp_model_func, from=min(avgsoilT_2017), to=max(avgsoilT_2017), add = TRUE)
plot(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13]))
plot(lm(forage_terrain_energy$clp031417 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp031417 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp031417[-13] ~ forage_terrain_energy$slope[-13])) #23% variance explained; p-val=0.07
summary(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13] + forage_terrain_energy$slope[-13])) #64% variance explained but slope parameter NS and positive (p=0.37)
#finalize as figure
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '7 cm temperature vs. forage', 'Mar.forage.2017_vs_soilT.v2.tif'), family = 'Times New Roman', width = 4.5, pointsize=11, height = 4, units = 'in', res=150)
par(mar=c(4, 4, 0.5, 0.5))
plot(avgsoilT_2017[-13], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13], xlab = expression(paste('Dec 1, 2016 - Mar 15, 2017, mean 7 cm soil temperature ('*degree*'C)')), ylab = expression('Mar 14, 2017 standing forage (kg '~~ha^-1*')'), pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
abline(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13]), lty=2)
legend('topleft', legend=(c("< 1200", '1200-1410', '>1410')), pch = 19, col=c('blue', 'orange2', 'red3'), pt.cex = 1.3, title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1)
text(x=8.9, y=2600, label='linear model results', cex=1, adj=c(0,0))
text(x=8.9, y=2450, label=expression(r^2~'= 0.61, p.val < 0.001'), cex = 1, adj = c(0, 0))
text(x=8.9, y=2300, label=expression('slope = 360 \u00B1 81 kg forage'~ha^-1~degree*C^-1), adj = c(0, 0), cex = 1)
text(x=8.9, y=2150, label=expression('RMSE = 326 kg forage'~ha^-1), cex = 1, adj = c(0, 0))
#text(avgsoilT_2017[-13], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[-13])
dev.off()
#check energy classification
forage_terrain_energy[order(forage_terrain_energy$annual_kwh.m2), c('location', 'annual_kwh.m2')]

#overall relationship between Apr forage 2017 and soil T
avgsoilT_dates_2017 <- which(colnames(soilT_7cm_2017)=='Dec_01_2016'):which(colnames(soilT_7cm_2017)=='Apr_10_2017')
avgsoilT_2017 <- apply(soilT_7cm_2017[ ,avgsoilT_dates_2017], 1, mean, na.rm=TRUE)
summary(avgsoilT_2017)
sd(avgsoilT_2017, na.rm = TRUE)
hist(avgsoilT_2017)
plot(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp041017[] ~ avgsoilT_2017[]), lty=2)
text(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
summary(lm(forage_terrain_energy$clp041017[] ~ avgsoilT_2017[])) #NS: p.val=0.70
plot(lm(forage_terrain_energy$clp041017[] ~ avgsoilT_2017[]))
plot(lm(forage_terrain_energy$clp041017 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp041017 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp041017[] ~ forage_terrain_energy$slope[])) #23% variance explained; p-val=0.07
summary(lm(forage_terrain_energy$clp041017[] ~ avgsoilT_2017[] + forage_terrain_energy$slope[])) #64% variance explained but slope parameter NS and positive (p=0.37)
#finalize as figure
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '7 cm temperature vs. forage', 'Apr.forage.2017_vs_soilT.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 4, units = 'in', res=150)
par(mar=c(4, 4, 0.5, 0.5))
plot(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[], xlab = expression('Dec 1, 2016 - Apr 10, 2017, mean 7 cm soil temperature ('*degree*'C)'), ylab = expression('Apr 10, 2017 standing forage (kg '~~ha^-1*')'), pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
abline(lm(forage_terrain_energy$clp041017[] ~ avgsoilT_2017[]), lty=2)
#legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=9.1, y=4400, label='19 days later', cex=1, adj=c(0,0))
text(x=9.1, y=4200, label='linear relationship non-significant (p = 0.70)', cex = 1, adj=c(0,0))
#text(x=7.9, y=2200, label=expression(paste(r^2, ' = 0.61')), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2100, label=expression(paste('RMSE = 326 kg ', ha^-1)), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2000, label=expression(paste('wet year mid-March standing forage (kg ', ha^-1, ') =' ~degree*'C * 360 - 1911')), adj = c(0, 0), cex = 1.1)
#text(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()

#overall relationship between May forage 2017 and soil T
avgsoilT_dates_2017 <- which(colnames(soilT_7cm_2017)=='Dec_01_2016'):which(colnames(soilT_7cm_2017)=='May_01_2017')
avgsoilT_2017 <- apply(soilT_7cm_2017[ ,avgsoilT_dates_2017], 1, mean, na.rm=TRUE)
summary(avgsoilT_2017)
sd(avgsoilT_2017, na.rm = TRUE)
hist(avgsoilT_2017)
plot(avgsoilT_2017[], forage_terrain_energy$clp050117[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp050117[] ~ avgsoilT_2017[]), lty=2)
text(avgsoilT_2017[], forage_terrain_energy$clp050117[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
summary(lm(forage_terrain_energy$clp050117[] ~ avgsoilT_2017[])) #NS: p.val=0.83
plot(lm(forage_terrain_energy$clp050117[] ~ avgsoilT_2017[]))
summary(lm(forage_terrain_energy$clp050117[-13] ~ forage_terrain_energy$slope[-13])) #23% variance explained; p-val=0.07
summary(lm(forage_terrain_energy$clp050117[] ~ avgsoilT_2017[] + forage_terrain_energy$slope[])) #64% variance explained but slope parameter NS and positive (p=0.37)
#finalize as figure
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', 'Apr.forage.2017_vs_soilT.png'), family = 'Book Antiqua', width = 800, height = 650, units = 'px', res=100)
par(mar=c(4.8, 4.8, 1, 1))
plot(avgsoilT_2017[], forage_terrain_energy$clp050117[], col=forage_terrain_energy$energy_colors[], xlab = expression(paste('Dec 1, 2016 - Apr 10, 2017, mean soil temperature, 7 cm depth ('*degree*'C)')), ylab = expression(paste('Apr 10, 2017 standing forage (kg', ' ', ha^-1, ')')), pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.3)
abline(lm(forage_terrain_energy$clp050117[] ~ avgsoilT_2017[]), lty=2)
#legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=9.3, y=4400, label='19 days later', cex=1.3, adj=c(0,0))
text(x=9.3, y=4200, label='linear relationship non-significant (p = 0.70)', cex = 1.3, adj=c(0,0))
#text(x=7.9, y=2200, label=expression(paste(r^2, ' = 0.61')), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2100, label=expression(paste('RMSE = 326 kg ', ha^-1)), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2000, label=expression(paste('wet year mid-March standing forage (kg ', ha^-1, ') =' ~degree*'C * 360 - 1911')), adj = c(0, 0), cex = 1.1)
#text(avgsoilT_2017[], forage_terrain_energy$clp050117[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()


#overall relationship between Mar forage 2018 and soil T
avgsoilT_dates_2018 <- which(colnames(soilT_7cm_2018)=='Dec_01_2017'):which(colnames(soilT_7cm_2018)=='Mar_22_2018') #because location 13 lost on Mar 10th
avgsoilT_2018 <- apply(soilT_7cm_2018[ ,avgsoilT_dates_2018], 1, mean)
summary(avgsoilT_2018)
sd(avgsoilT_2018, na.rm = TRUE)
hist(avgsoilT_2018)
plot(avgsoilT_2018[], forage_terrain_energy$clp032218[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[]), lty=2)
curve(nlm_2018(x, 'clp032218'), from=min(avgsoilT_2018), to=max(avgsoilT_2018), lty=2, add=TRUE)
summary(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[] + I(avgsoilT_2018[]^2))) #46% variance explained; p.val=0.018
summary(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[])) #25% variance explained (p=0.048); -57 kg/ha per deg C warming + 1181
plot(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[]))
summary(lm(forage_terrain_energy$clp032218[] ~ forage_terrain_energy$slope[])) #<1% variance explained
summary(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[] + forage_terrain_energy$slope[])) #<1% variance explained

#finalize as figure
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '7 cm temperature vs. forage', 'Mar.forage.2018_vs_soilT.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 4, units = 'in', res=150)
par(mar=c(4, 4, 0.5, 0.5))
plot(avgsoilT_2018[], forage_terrain_energy$clp032218[], col=forage_terrain_energy$energy_colors[], xlab = expression('Dec 1, 2017 - Mar 22, 2018, mean 7 cm soil temperature ('*degree*'C)'), ylab = expression('Mar 22, 2018 standing forage (kg '~~ha^-1*')'), pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
abline(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[]), lty=2)
curve(nlm_2018(x, 'clp032218'), from=min(avgsoilT_2018), to=max(avgsoilT_2018), lty=2, add=TRUE)
legend(x=9.5, y=500, legend=(c("< 1200", '1200-1410', '>1410')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, pt.cex = 1.3, cex = 1)
text(x=8.1, y=250, label='linear model results', cex = 1, adj = c(0, 0))
text(x=8.1, y=175, label=expression(paste(r^2, ' = 0.25, p.val = 0.048')), cex = 1, adj = c(0, 0))
text(x=8.1, y=100, label=expression(paste('RMSE = 226 kg ', ha^-1)), cex = 1, adj = c(0, 0))
text(x=11, y=250, label='non-linear model results', cex = 1, adj = c(0, 0))
text(x=11, y=175, label=expression(paste(r^2, ' = 0.46, p.val = 0.018')), cex = 1, adj = c(0, 0))
text(x=11, y=100, label=expression(paste('RMSE = 198 kg ', ha^-1)), cex = 1, adj = c(0, 0))
#text(x=7.9, y=130, label=expression(paste('dry year mid-March standing forage (kg ', ha^-1, ') = deg C * -57 + 1181')), adj = c(0, 0), cex = 1.2)
#text(avgsoilT_2018[], forage_terrain_energy$clp032218[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()

#overall relationship between Apr forage 2018 and soil T
avgsoilT_dates_2018 <- which(colnames(soilT_7cm_2018)=='Dec_01_2017'):which(colnames(soilT_7cm_2018)=='Apr_15_2018') #because location 13 lost on Mar 10th
avgsoilT_2018 <- apply(soilT_7cm_2018[ ,avgsoilT_dates_2018], 1, mean)
summary(avgsoilT_2018)
sd(avgsoilT_2018, na.rm = TRUE)
hist(avgsoilT_2018)
plot(avgsoilT_2018[], forage_terrain_energy$peak2018[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp041518[] ~ avgsoilT_2018[]), lty=2)

summary(lm(forage_terrain_energy$peak2018[] ~ avgsoilT_2018[])) #
summary(lm(forage_terrain_energy$peak2018[-14] ~ poly(avgsoilT_2018[-14], 2))) #14 is an outlier based on Cook's distance
plot(lm(forage_terrain_energy$peak2018[-14] ~ poly(avgsoilT_2018[-14], 2)))
summary(lm(forage_terrain_energy$clp041518[] ~ avgsoilT_2018[] + I(avgsoilT_2018[]^2)))
nlm_2018(avgsoilT_2018, 'clp041518')
#finalize as figure
tiff(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '7 cm temperature vs. forage', 'Apr.forage.2018_vs_soilT.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 4, units = 'in', res=150)
par(mar=c(4, 4, 0.5, 0.5))
plot(avgsoilT_2018[], forage_terrain_energy$clp041518[], col=forage_terrain_energy$energy_colors[], xlab = expression('Dec 1, 2017 - Apr 15, 2018, mean 7 cm soil temperature ('*degree*'C)'), ylab = expression('Apr 15, 2018 standing forage (kg '~~ha^-1*')'), pch=19, cex=1.3, cex.axis=1, cex.lab=1, mgp=c(2.5, 1, 0))
#abline(lm(forage_terrain_energy$clp041518[] ~ avgsoilT_2018[]), lty=2)
curve(nlm_2018(x, 'clp041518'), from=min(avgsoilT_2018), to=max(avgsoilT_2018), lty=2, add = TRUE)
#legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=9.3, y=1500, label='24 days later', cex=1, adj = c(0, 0))
text(x=9.3, y=1425, label='non-linear model results', cex = 1, adj = c(0, 0))
text(x=9.3, y=1350, label=expression(paste(r^2, ' = 0.57, p.val = 0.004')), cex = 1, adj = c(0, 0))
text(x=9.3, y=1275, label=expression(paste('RMSE = 245 kg ', ha^-1)), cex = 1, adj = c(0, 0))
#text(x=7.9, y=130, label=expression(paste('dry year mid-March standing forage (kg ', ha^-1, ') = deg C * -57 + 1181')), adj = c(0, 0), cex = 1.2)
#text(avgsoilT_2018[], forage_terrain_energy$clp041518[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()

#plot Mar week 0 drydown
Mar_week0 <- which(colnames(vwc_7cm_2017)=='Feb_21_2017'):which(colnames(vwc_7cm_2017)=='Feb_27_2017')
for (i in 1:nrow(vwc_7cm_2017)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week0], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week0], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week0], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week0], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4))}
}

Mar2017_week0_drydown <- as.data.frame(t(apply(vwc_7cm_2017[,Mar_week0], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week0_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week0_drydown$drydown.slopes <- -Mar2017_week0_drydown$drydown.slopes
Mar2017_week0_drydown
summary(Mar2017_week0_drydown$drydown.slopes)
sd(Mar2017_week0_drydown$drydown.slopes)
hist(Mar2017_week0_drydown$drydown.slopes) #appears lognormal
Mar2017_avgsoilT_week0 <- apply(soilT_7cm_2017[ ,Mar_week0], 1, mean)
summary(Mar2017_avgsoilT_week0)
sd(Mar2017_avgsoilT_week0)
hist(Mar2017_avgsoilT_week0) #right skewed

#plot Mar week 1 drydown
Mar_week1 <- which(colnames(vwc_7cm_2017)=='Feb_28_2017'):which(colnames(vwc_7cm_2017)=='Mar_06_2017')
for (i in 1:nrow(vwc_7cm_2017)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week1], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week1], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week1], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week1], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4))}
}

Mar2017_week1_drydown <- as.data.frame(t(apply(vwc_7cm_2017[,Mar_week1], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week1_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week1_drydown$drydown.slopes <- -Mar2017_week1_drydown$drydown.slopes
Mar2017_week1_drydown
summary(Mar2017_week1_drydown$drydown.slopes)
sd(Mar2017_week1_drydown$drydown.slopes)
hist(Mar2017_week1_drydown$drydown.slopes) #appears lognormal
Mar2017_avgsoilT_week1 <- apply(soilT_7cm_2017[ ,Mar_week1], 1, mean)
summary(Mar2017_avgsoilT_week1)
sd(Mar2017_avgsoilT_week1)
hist(Mar2017_avgsoilT_week1) #right skewed

#check soil T vs. solrad
plot(forage_terrain_energy$annual_kwh.m2[], Mar2017_avgsoilT_week1, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_avgsoilT_week1 ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_avgsoilT_week1 ~ forage_terrain_energy$annual_kwh.m2[]))


#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2, Mar2017_week1_drydown$drydown.slopes)
abline(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))

#drydown slopes vs. soil temp Mar week 1
plot(Mar2017_avgsoilT_week1, Mar2017_week1_drydown$drydown.slopes, col=forage_terrain_energy$energy_colors, cex=forage_terrain_energy$curvature_mean*2)
abline(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1))
text(Mar2017_avgsoilT_week1, Mar2017_week1_drydown$drydown.slopes, labels = forage_terrain_energy$location, pos=1, offset = 0.5)
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1)) #38% of variance in drydown slopes explained by soil temperature (p=0.01)
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1 + forage_terrain_energy$clp021517)) #48% of variance explained (p=0.015) by soil temperature (p < 0.01) + Feb standing biomass (p=0.14), whereby higher standing biomass is associated with weaker drawdown
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1 + forage_terrain_energy$clp031417)) #45% of variance explained (p=0.02) by soil temperature (p < 0.01) + Mar standing biomass (p=0.22), whereby higher standing biomass is associated with weaker drawdown
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$clp021517)) #<1% explained by Feb standing biomass alone
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$clp031417)) #3% by Mar standing biomass alone

#Mar forage vs soil temp week 1
plot(Mar2017_avgsoilT_week1, forage_terrain_energy$clp031417[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp031417[] ~ Mar2017_avgsoilT_week1))
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week1[-13])) #38% of variance in Mar forage explained by Mar avg soil T (p=0.012); 59% of variance explained with location 13 removed

#Mar forage vs. drydown slopes Mar week 1 + soil T week 1
plot(Mar2017_week1_drydown$drydown.slopes[c(-8,-13)], forage_terrain_energy$clp031417[c(-8,-13)], col=forage_terrain_energy$energy_colors[c(-8,-13)])
abline(lm(forage_terrain_energy$clp031417[c(-8,-13)] ~ Mar2017_week1_drydown$drydown.slopes[c(-8,-13)]))
plot(lm(forage_terrain_energy$clp031417[c(-8,-13)] ~ Mar2017_week1_drydown$drydown.slopes[c(-8,-13)]))
summary(lm(forage_terrain_energy$clp031417[c(-8,-13)] ~ Mar2017_week1_drydown$drydown.slopes[c(-8,-13)])) #slope positive but p=0.54 with all data; remove sites 8 and 13 and slope is positive and 37% of variability explained (p=0.02)
summary(lm(forage_terrain_energy$clp031417[c(-8,-13)] ~ Mar2017_avgsoilT_week1[c(-8,-13)] + Mar2017_week1_drydown$drydown.slopes[c(-8,-13)])) #45% of variance explained, mostly by soil temperature (p<0.01) with negative slope on drawdown (p=0.22) (all data); 70% of variability explained with outliers removed and drawdown is 

#Mar forage gain vs. soil temp Mar week 1
plot(Mar2017_avgsoilT_week1[-13], forage_terrain_energy$Mar2017growth[-13])
abline(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week1[-13]))
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week1[-13])) #30% of Mar forage gain explained by warmer soil temperatures (p=0.03); 47% explained with 13 removed p.val=0.005

#Mar forage gain vs. drydown slopes Mar week 1 + soil T
plot(Mar2017_week1_drydown$drydown.slopes[-13], forage_terrain_energy$Mar2017growth[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_week1_drydown$drydown.slopes[-13]))
text(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$Mar2017growth[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_week1_drydown$drydown.slopes[-13])) #5% variance in March growth explained with all points and slope positive (p=0.41); 15% variance explained with 13 removed (p=0.16)
plot(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_week1_drydown$drydown.slopes[-13])) #8 is also an outlier when looking at data this way; low growth, high drawdown location
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week1[-13] + Mar2017_week1_drydown$drydown.slopes[-13])) #32% variance explained (p=0.08); drydown slope negative (p=0.54); soil T positive (p=0.04) with all points;
plot(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week1[-13] + Mar2017_week1_drydown$drydown.slopes[-13])) #no outliers based on leverage and cook's distance

#Apr forage vs. soil T Mar week 1
plot(Mar2017_avgsoilT_week1, forage_terrain_energy$clp041017[])
abline(lm(forage_terrain_energy$clp041017[] ~ Mar2017_avgsoilT_week1))
summary(lm(forage_terrain_energy$clp041017[] ~ Mar2017_avgsoilT_week1)) #2% variance explained, slope is positive still

#Apr forage vs. drydown slopes Mar week 1
plot(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp041017[] ~ Mar2017_week1_drydown$drydown.slopes)) 
summary(lm(forage_terrain_energy$clp041017[] ~ Mar2017_week1_drydown$drydown.slopes))#8% variance explained; p=0.29
summary(lm(forage_terrain_energy$clp041017[] ~ Mar2017_avgsoilT_week1 + Mar2017_week1_drydown$drydown.slopes))#24% variance explained; p=0.17; soilT positive (p=0.13); drydown negative (p=0.08)
summary(lm(forage_terrain_energy$clp041017[] ~ forage_terrain_energy$annual_kwh.m2[] + Mar2017_week1_drydown$drydown.slopes)) #22% variance explained; same relationships

#Apr forage gain vs. soil T Mar week 1
plot(Mar2017_avgsoilT_week1, forage_terrain_energy$Apr2017growth[])
abline(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1)) #20% variance explained with negative soil T slope; p=0.08
plot(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1)) #14 is an outlier for soil T model

#Apr forage gain vs. soil T Mar week 1 + drydown slopes Mar week 1
plot(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[])
abline(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week1_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week1_drydown$drydown.slopes + forage_terrain_energy$clp031417)) #27% variance explained with negative slope (p=0.04) and not improved by terrain chars
plot(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week1_drydown$drydown.slopes)) # no obvious outliers
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1 + Mar2017_week1_drydown$drydown.slopes)) #30% variance explained; no coeff is sig; p-val = 0.1
plot(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1 + Mar2017_week1_drydown$drydown.slopes)) # 14 is is very close to being an outlier
summary(lm(forage_terrain_energy$Apr2017growth[-14] ~ Mar2017_avgsoilT_week1[-14] + Mar2017_week1_drydown$drydown.slopes[-14])) #48% variance explained
summary(lm(forage_terrain_energy$Apr2017growth[-14] ~ Mar2017_avgsoilT_week1[-14]))
summary(lm(forage_terrain_energy$Apr2017growth[-14] ~ Mar2017_week1_drydown$drydown.slopes[-14]))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1 * Mar2017_week1_drydown$drydown.slopes)) #40% variance explained (p=0.097)

#peak forage vs. soil T Mar week 1
plot(Mar2017_avgsoilT_week1, forage_terrain_energy$peak2017[])
abline(lm(forage_terrain_energy$peak2017[] ~ Mar2017_avgsoilT_week1))
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_avgsoilT_week1)) #5% variance explained (p=0.38)

#peak forage vs. soil T Mar week 1 + drawdown Mar week 1
plot(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$peak2017[])
abline(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week1_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week1_drydown$drydown.slopes)) #6% variance explained (p=0.35)
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week1_drydown$drydown.slopes + Mar2017_avgsoilT_week1)) #30% variance explained (p=0.096)
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week1_drydown$drydown.slopes + Mar2017_avgsoilT_week1 + forage_terrain_energy$elevation)) #42% variance explained (p=0.08) with elevation; all params p < 0.15 and consistent with previous results

#rank correlation tests to confirm above
#vs. Mar forage
rank_test(Mar2017_avgsoilT_week1, Mar2017_week1_drydown, 'drydown.slopes', mtd = 'spearman') #p.val=0.003; r.rank=0.7
rank_test(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy, 'Mar2017growth', mtd = 'spearman') #pval=0.28, r.rank=0.29
rank_test(Mar2017_avgsoilT_week1, forage_terrain_energy, 'Mar2017growth', mtd = 'spearman') #pval=0.009; r.rank=0.64
rank_test(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy, 'clp031417', mtd = 'spearman') #p.val=0.19; r.rank=0.35
rank_test(Mar2017_avgsoilT_week1, forage_terrain_energy, 'clp031417', mtd = 'spearman') # p.val < 0.001; r.rank=0.76

#vs. Apr forage growth
rank_test(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy, 'Apr2017growth', mtd = 'spearman') #p.val=0.02 r.rank=-0.56
rank_test(Mar2017_avgsoilT_week1, forage_terrain_energy, 'Apr2017growth', mtd = 'spearman') #pval=0.05; r.rank=-0.50
rank_test(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy, 'clp041017', mtd = 'spearman') #p.val=0.40 r.rank=-0.23
rank_test(Mar2017_avgsoilT_week1, forage_terrain_energy, 'clp041017', mtd = 'spearman') #pval=0.72; r.rank=-0.1

#vs. peak forage overall
rank_test(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy, 'peak2017', mtd = 'spearman')
rank_test(Mar2017_avgsoilT_week1, forage_terrain_energy, 'peak2017', mtd = 'spearman')

#Apr vs. Mar forage
plot(forage_terrain_energy$clp031417[-13], forage_terrain_energy$clp041017[-13])
summary(lm(forage_terrain_energy$clp041017[] ~ forage_terrain_energy$clp031417[])) #40% variance explained with all points; 13 is outlier based on cook's distance
plot(lm(forage_terrain_energy$clp041017[] ~ forage_terrain_energy$clp031417[]))
summary(lm(forage_terrain_energy$clp041017[-13] ~ forage_terrain_energy$clp031417[-13])) #32% variance explained with 13 removed
plot(lm(forage_terrain_energy$clp041017[-13] ~ forage_terrain_energy$clp031417[-13])) #now, location 14 is outlier
plot(lm(forage_terrain_energy$clp041017[c(-13, -14)] ~ forage_terrain_energy$clp031417[c(-13, -14)])) #now ok
summary(lm(forage_terrain_energy$clp041017[c(-13, -14)] ~ forage_terrain_energy$clp031417[c(-13, -14)]))#only 13% variance explained (p.val=0.21)


#TO-DO--copy approach from above; main question is to what extent soil moisture and temperature differences are related to the growth from mid-March to mid-April
#can also show that increased growth from mid-February was temperature not soil moisture related
#Mar week 2 drydown
Mar_week2 <- which(colnames(vwc_7cm_2017)=='Mar_07_2017'):which(colnames(vwc_7cm_2017)=='Mar_13_2017')
for (i in 1:nrow(vwc_7cm_2017)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week2], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week2], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week2], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week2], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4))}
}

Mar2017_week2_drydown <- as.data.frame(t(apply(vwc_7cm_2017[,Mar_week2], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week2_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week2_drydown$drydown.slopes <- -Mar2017_week2_drydown$drydown.slopes
Mar2017_week2_drydown
Mar2017_avgsoilT_week2 <- apply(soilT_7cm_2017[,Mar_week2], 1, mean)
mean(Mar2017_week2_drydown$drydown.slopes)
summary(Mar2017_week2_drydown$drydown.slopes)
sd(Mar2017_week2_drydown$drydown.slopes)
summary(Mar2017_avgsoilT_week2)
sd(Mar2017_avgsoilT_week2)

#check soil T vs. solrad
plot(forage_terrain_energy$annual_kwh.m2[], Mar2017_avgsoilT_week2, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_avgsoilT_week2 ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_avgsoilT_week2 ~ forage_terrain_energy$annual_kwh.m2[])) #r^2=0.86


#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2, Mar2017_week2_drydown$drydown.slopes)
abline(lm(Mar2017_week2_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[])) #r^2=0.24, p=0.052

#drydown slopes vs. soil temp Mar week 2
plot(Mar2017_avgsoilT_week2, Mar2017_week2_drydown$drydown.slopes, col=forage_terrain_energy$energy_colors, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_week2_drydown$drydown.slopes ~ Mar2017_avgsoilT_week2))
text(Mar2017_avgsoilT_week2, Mar2017_week2_drydown$drydown.slopes, labels = forage_terrain_energy$location, pos=1, offset = 0.5)
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ Mar2017_avgsoilT_week2)) #18% of variance in drydown slopes explained by soil temperature (p=0.10)
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ Mar2017_avgsoilT_week2 + forage_terrain_energy$clp021517)) #31% of variance explained (p=0.086) by soil temperature (p =0.0369) + Feb standing biomass (p=0.13), whereby higher standing biomass is associated with weaker drawdown
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ Mar2017_avgsoilT_week2 + forage_terrain_energy$clp031417)) #22% of variance explained (p=0.20) by soil temperature (p = .08) + Mar standing biomass (p=0.45)
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ forage_terrain_energy$clp021517)) #3% explained by Feb standing biomass alone
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ forage_terrain_energy$clp031417)) #<1% by Mar standing biomass alone

#Mar forage vs soil temp week 2
plot(Mar2017_avgsoilT_week2, forage_terrain_energy$clp031417[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp031417[] ~ Mar2017_avgsoilT_week2))
summary(lm(forage_terrain_energy$clp031417[] ~ Mar2017_avgsoilT_week2[])) #24% of variance in Mar forage explained by Mar avg soil T (p=0.05); 50% of variance explained with location 13 removed

#Mar forage vs. drydown slopes Mar week 2 + soil T week 2
plot(Mar2017_week2_drydown$drydown.slopes[], forage_terrain_energy$clp031417[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp031417[] ~ Mar2017_week2_drydown$drydown.slopes[]))
text(Mar2017_week2_drydown$drydown.slopes[], forage_terrain_energy$clp031417[], pos = 1, offset=0.5, labels = forage_terrain_energy$location)
plot(lm(forage_terrain_energy$clp031417[-c(3,7,8,9,13)] ~ Mar2017_week2_drydown$drydown.slopes[-c(3,7,8,9,13)]))
summary(lm(forage_terrain_energy$clp031417[] ~ Mar2017_week2_drydown$drydown.slopes[])) #slope positive but p=0.88 with all data; 
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_week2_drydown$drydown.slopes[-13])) #slope positive but p=0.92 with location 13 removed
summary(lm(forage_terrain_energy$clp031417[-c(3,7,8,9,13)] ~ Mar2017_week2_drydown$drydown.slopes[-c(3,7,8,9,13)])) #remove sites 3,7,8,9, and 13 and slope is positive and 88% of variability explained (p < 0.001)
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week2[-13] + Mar2017_week2_drydown$drydown.slopes[-13])) #27% of variance explained, mostly by soil temperature (p=0.13) with negative slope on drawdown (p=0.45) (all data); with #13 removed, 59% of variance explained, still mostly tempearture (p = 0.001) and negative slope on drawdown (p=0.13)
plot(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week2[-13] + Mar2017_week2_drydown$drydown.slopes[-13])) #location 8 is almost an outlier based on standardized residulas and leverage

#Mar forage gain vs. soil temp Mar week 2
plot(Mar2017_avgsoilT_week2[-13], forage_terrain_energy$Mar2017growth[-13])
abline(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2[-13]))
text(Mar2017_avgsoilT_week2[-13], forage_terrain_energy$Mar2017growth[-13], labels = forage_terrain_energy$location[-13], pos=1, offset = 0.5)
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2[-13])) #20% of Mar forage gain explained by warmer soil temperatures (p=0.09); 44% of Mar forage gain explained with location 13 removed
plot(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2[-13])) #but still several outliers

#Mar forage gain vs. drydown slopes Mar week 2 + soil T
plot(Mar2017_week2_drydown$drydown.slopes[], forage_terrain_energy$Mar2017growth[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_week2_drydown$drydown.slopes[]))
text(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$Mar2017growth[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
summary(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_week2_drydown$drydown.slopes[])) #4% variance in March growth explained with all points and slope positive (p=0.41); 8% variance explained with location 13 removed; 14% variance explained with 8 and 13 removed
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2[-13] + Mar2017_week2_drydown$drydown.slopes[-13])) #20% variance explained (p=0.24); drydown slope positive (p=0.99); soil T positive (p=0.13); 44% with location 13 removed (p=0.03)
plot(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2[-13] + Mar2017_week2_drydown$drydown.slopes[-13]))
vif(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2[-13] + Mar2017_week2_drydown$drydown.slopes[-13])) #vif is 1.22 not a concern

#Apr forage vs. soil T Mar week 2
plot(Mar2017_avgsoilT_week2, forage_terrain_energy$clp041017[])
abline(lm(forage_terrain_energy$clp041017[] ~ Mar2017_avgsoilT_week2))
summary(lm(forage_terrain_energy$clp041017[] ~ Mar2017_avgsoilT_week2)) # < 1% variance explained, slope is positive still

#Apr forage vs. drydown slopes Mar week 2
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp041017[] ~ Mar2017_week2_drydown$drydown.slopes)) 
summary(lm(forage_terrain_energy$clp041017[] ~ Mar2017_week2_drydown$drydown.slopes))#20% variance explained; p=0.08
summary(lm(forage_terrain_energy$clp041017[] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes))#26% variance explained; p=0.14; soilT positive (p=0.30); drydown negative (p=0.05)
summary(lm(forage_terrain_energy$clp041017[] ~ forage_terrain_energy$annual_kwh.m2[] + Mar2017_week2_drydown$drydown.slopes)) #30% variance explained; same relationships

#Apr forage gain vs. soil T Mar week 2
plot(Mar2017_avgsoilT_week2, forage_terrain_energy$Apr2017growth[])
abline(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2))
summary(lm(forage_terrain_energy$Apr2017growth[-14] ~ Mar2017_avgsoilT_week2[-14]))
#20% variance explained with negative soil T slope; p=0.08

#Apr forage gain vs. soil T Mar week 2 + drydown slopes Mar week 2
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[])
abline(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week2_drydown$drydown.slopes)) #36% variance explained with negative slope (p=0.014); no outliers residuals are 1,000+ lbs
plot(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes)) #40% variance explained (p-val=0.035); drydown slope is negative (p=0.054); soil T relationship is negative (p=0.35)
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2 * Mar2017_week2_drydown$drydown.slopes)) #46% variance explained (p=0.051)
#terrain characteristics do not help
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes + forage_terrain_energy$elevation)) #40% variance explained; elevation is p.val=0.99
plot(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes + forage_terrain_energy$elevation))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes + forage_terrain_energy$curvature_mean)) #also 40% with curvature at p.val=0.92
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes + forage_terrain_energy$slope))

#peak forage vs. soil T Mar week 2
plot(Mar2017_avgsoilT_week2, forage_terrain_energy$peak2017[])
abline(lm(forage_terrain_energy$peak2017[] ~ Mar2017_avgsoilT_week2))
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_avgsoilT_week2)) #1.5% variance explained (p=0.65)

#peak forage vs. soil T Mar week 2 + drawdown Mar week 2
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$peak2017[])
abline(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week2_drydown$drydown.slopes)) #9% variance explained (p=0.25)
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week2_drydown$drydown.slopes + Mar2017_avgsoilT_week2)) #17% variance explained (p=0.3)
summary(lm(forage_terrain_energy$peak2017[] ~ Mar2017_week2_drydown$drydown.slopes + Mar2017_avgsoilT_week2 + forage_terrain_energy$elevation)) #29% variance explained (p=0.23) with elevation; all params p < 0.31 and consistent with previous results

#rank correlation tests to confirm above
#vs. Mar forage
rank_test(Mar2017_avgsoilT_week2, Mar2017_week2_drydown, 'drydown.slopes', mtd = 'spearman') #p.val=0.067; r.rank=0.47
rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy, 'Mar2017growth', mtd = 'spearman') #pval=0.37, r.rank=0.24
rank_test(Mar2017_avgsoilT_week2, forage_terrain_energy, 'Mar2017growth', mtd = 'spearman') #pval=0.025; r.rank=0.56
rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy, 'clp031417', mtd = 'spearman') #p.val=0.48; r.rank=0.19
rank_test(Mar2017_avgsoilT_week2, forage_terrain_energy, 'clp031417', mtd = 'spearman') # p.val = 0.01; r.rank=0.63

#vs. Apr forage growth
rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy, 'Apr2017growth', mtd = 'spearman') #p.val=0.01 r.rank=-0.61
rank_test(Mar2017_avgsoilT_week2, forage_terrain_energy, 'Apr2017growth', mtd = 'spearman') #pval=0.067; r.rank=-0.47
rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy, 'clp041017', mtd = 'spearman') #p.val=0.20 r.rank=-0.34
rank_test(Mar2017_avgsoilT_week2, forage_terrain_energy, 'clp041017', mtd = 'spearman') #pval=0.95; r.rank=-0.017

#vs. peak forage overall
rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy, 'peak2017', mtd = 'spearman')
rank_test(Mar2017_avgsoilT_week2, forage_terrain_energy, 'peak2017', mtd = 'spearman')


#plot Mar week 3 drydown
Mar_week3 <- which(colnames(vwc_7cm_2017)=='Mar_14_2017'):which(colnames(vwc_7cm_2017)=='Mar_20_2017')
for (i in 1:nrow(vwc_7cm_2017)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week3], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week3], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.32), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week3], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week3], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.32))}
}

Mar2017_week3_drydown <- as.data.frame(t(apply(vwc_7cm_2017[-13,Mar_week3], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week3_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week3_drydown$drydown.slopes <- -Mar2017_week3_drydown$drydown.slopes
Mar2017_week3_drydown
Mar2017_avgsoilT_week3 <- apply(soilT_7cm_2017[-13,Mar_week3], 1, mean)
summary(Mar2017_week3_drydown$drydown.slopes)
sd(Mar2017_week3_drydown$drydown.slopes)
summary(Mar2017_avgsoilT_week3)
sd(Mar2017_avgsoilT_week3)

#check soil T vs. solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_avgsoilT_week3, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_avgsoilT_week3 ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_avgsoilT_week3 ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2=0.84


#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_week3_drydown$drydown.slopes)
abline(lm(Mar2017_week3_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2 < 0.01, p=0.77

#drydown slopes vs. soil temp Mar week 3
plot(Mar2017_avgsoilT_week3, Mar2017_week3_drydown$drydown.slopes, col=forage_terrain_energy$energy_colors[-13], cex=forage_terrain_energy$clp031417[-13]/1000)
abline(lm(Mar2017_week3_drydown$drydown.slopes ~ Mar2017_avgsoilT_week3))
text(Mar2017_avgsoilT_week3, Mar2017_week3_drydown$drydown.slopes, labels = forage_terrain_energy$location[-13], pos=1, offset = 0.5)
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ Mar2017_avgsoilT_week3)) #<1% of variance in drydown slopes explained by soil temperature (p=0.83)
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ Mar2017_avgsoilT_week3 + forage_terrain_energy$clp021517)) #31% of variance explained (p=0.086) by soil temperature (p =0.0369) + Feb standing biomass (p=0.13), whereby higher standing biomass is associated with weaker drawdown
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ Mar2017_avgsoilT_week3 + forage_terrain_energy$clp031417[-13])) #<2% of variance explained (p=0.89) by soil temperature (p = .91) + Mar standing biomass (p=0.68)
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ forage_terrain_energy$clp021517[-13])) #15% explained by Feb standing biomass alone
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ forage_terrain_energy$clp031417[-13])) #<2% by Mar standing biomass alone

#Mar forage vs soil temp week 3
plot(Mar2017_avgsoilT_week3, forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week3))
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week3[])) # 43% of March forage variance explained with location 13 removed

#Mar forage vs. drydown slopes Mar week 3 + soil T week 3
plot(Mar2017_week3_drydown$drydown.slopes[], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_week3_drydown$drydown.slopes[]))
text(Mar2017_week3_drydown$drydown.slopes[], forage_terrain_energy$clp031417[-13], pos = 1, offset=0.5, labels = forage_terrain_energy$location[-13])
plot(lm(forage_terrain_energy$clp031417[-c(3,7,8,9, 13)] ~ Mar2017_week3_drydown$drydown.slopes[-c(3,7,8,9)]))
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_week3_drydown$drydown.slopes[])) #slope positive but p=0.64 with location 13 removed 
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week3[] + Mar2017_week3_drydown$drydown.slopes[])) #44% explained with location 13 removed
plot(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week3[] + Mar2017_week3_drydown$drydown.slopes[])) #location 8 is almost an outlier based on standardized residulas and leverage

#Mar forage gain vs. soil temp Mar week 3
plot(Mar2017_avgsoilT_week3[], forage_terrain_energy$Mar2017growth[-13])
abline(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week3[]))
text(Mar2017_avgsoilT_week3[], forage_terrain_energy$Mar2017growth[-13], labels = forage_terrain_energy$location[-13], pos=1, offset = 0.5)
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week3[])) #44% of Mar forage gain explained with location 13 removed
plot(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week3[-13])) #but still several outliers

#Mar forage gain vs. drydown slopes Mar week 3 + soil T
plot(Mar2017_week3_drydown$drydown.slopes[], forage_terrain_energy$Mar2017growth[-13], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_week3_drydown$drydown.slopes[]))
text(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy$Mar2017growth[-13], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[-13])
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_week3_drydown$drydown.slopes[])) #5% variance explained with location 13 removed slope positive
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week3[] + Mar2017_week3_drydown$drydown.slopes[])) #51% variance explained; soil T p-val=.006; SM drawdown positive p-val=0.21
plot(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week3[] + Mar2017_week3_drydown$drydown.slopes[]))
vif(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week3[] + Mar2017_week3_drydown$drydown.slopes[])) #vif is 1.22 not a concern

#Apr forage vs. soil T Mar week 3
plot(Mar2017_avgsoilT_week3, forage_terrain_energy$clp041017[-13])
abline(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week3))
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week3)) # < 1% variance explained, slope totally flat

#Apr forage vs. drydown slopes Mar week 3
plot(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy$clp041017[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week3_drydown$drydown.slopes)) 
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week3_drydown$drydown.slopes))#16% variance explained; p=0.14, slope negative
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week3 + Mar2017_week3_drydown$drydown.slopes))#16% variance explained; p=0.34; soilT negative (p=0.30); drydown negative (p=0.05)
summary(lm(forage_terrain_energy$clp041017[-13] ~ forage_terrain_energy$annual_kwh.m2[-13] + Mar2017_week3_drydown$drydown.slopes)) #17% variance explained; same relationships

#Apr forage gain vs. soil T Mar week 3
plot(Mar2017_avgsoilT_week3, forage_terrain_energy$Apr2017growth[-13])
abline(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week3))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week3)) #19% variance explained with negative soil T slope; p=0.11

#Apr forage gain vs. soil T Mar week 3 + drydown slopes Mar week 3
plot(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
abline(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week3_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week3_drydown$drydown.slopes)) #16% variance explained with negative slope (p=0.14); no outliers residuals are 1,000+ lbs
plot(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week3_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week3 + Mar2017_week3_drydown$drydown.slopes)) #38% variance explained (p-val=0.06); drydown slope is negative (p=0.084); soil T relationship is negative (p=0.067)
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week3 * Mar2017_week3_drydown$drydown.slopes)) #46% variance explained (p=0.051)

#peak forage vs. soil T Mar week 3
plot(Mar2017_avgsoilT_week3, forage_terrain_energy$peak2017[-13])
abline(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week3))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week3)) #<1% variance explained (p=0.65)

#peak forage vs. soil T Mar week 3 + drawdown Mar week 3
plot(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy$peak2017[-13])
abline(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week3_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week3_drydown$drydown.slopes)) #7% variance explained (p=0.34)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week3_drydown$drydown.slopes + Mar2017_avgsoilT_week3)) #7% variance explained (p=0.3)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week3_drydown$drydown.slopes + Mar2017_avgsoilT_week3 + forage_terrain_energy$elevation[-13])) #19% variance explained (p=0.50)

#rank correlation tests to confirm above
#vs. Mar forage
rank_test(Mar2017_avgsoilT_week3, Mar2017_week3_drydown, 'drydown.slopes', mtd = 'spearman') #p.val=0.50; r.rank=-0.11
rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy[-13,], 'Mar2017growth', mtd = 'spearman') #pval=0.22, r.rank=0.34
rank_test(Mar2017_avgsoilT_week3, forage_terrain_energy[-13,], 'Mar2017growth', mtd = 'spearman') #pval=0.034; r.rank=0.56
rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') #p.val=0.51; r.rank=-0.19
rank_test(Mar2017_avgsoilT_week3, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') # p.val = 0.03; r.rank=0.58

#vs. Apr forage growth
rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Apr2017growth', mtd = 'spearman') #p.val=0.16 r.rank=-0.39
rank_test(Mar2017_avgsoilT_week3, forage_terrain_energy[-13,], 'Apr2017growth', mtd = 'spearman') #pval=0.12; r.rank=-0.43
rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') #p.val=0.20 r.rank=-0.35
rank_test(Mar2017_avgsoilT_week3, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') #pval=0.83; r.rank=-0.06

#vs. peak forage overall
rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy, 'peak2017', mtd = 'spearman')
rank_test(Mar2017_avgsoilT_week3, forage_terrain_energy, 'peak2017', mtd = 'spearman')

#plot Mar week 4 drydown
Mar_week4 <- which(colnames(vwc_7cm_2017)=='Mar_28_2017'):which(colnames(vwc_7cm_2017)=='Apr_03_2017')
for (i in 1:nrow(vwc_7cm_2017)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week4], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week4], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.32), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week4], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week4], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.32))}
}

Mar2017_week4_drydown <- as.data.frame(t(apply(vwc_7cm_2017[-13,Mar_week4], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week4_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week4_drydown$drydown.slopes <- -Mar2017_week4_drydown$drydown.slopes
Mar2017_week4_drydown
Mar2017_avgsoilT_week4 <- apply(soilT_7cm_2017[-13,Mar_week4], 1, mean)
summary(Mar2017_week4_drydown$drydown.slopes)
sd(Mar2017_week4_drydown$drydown.slopes)
summary(Mar2017_avgsoilT_week4)
sd(Mar2017_avgsoilT_week4)

#check soil T vs. solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_avgsoilT_week4, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_avgsoilT_week4 ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_avgsoilT_week4 ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2=0.78

#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_week4_drydown$drydown.slopes)
abline(lm(Mar2017_week4_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_week4_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2 < 0.52, p=0.003

#drydown slopes vs. soil temp Mar week4
plot(Mar2017_avgsoilT_week4, Mar2017_week4_drydown$drydown.slopes, col=forage_terrain_energy$energy_colors[-13], cex=forage_terrain_energy$clp031417[-13]/1000)
abline(lm(Mar2017_week4_drydown$drydown.slopes ~ Mar2017_avgsoilT_week4))
text(Mar2017_avgsoilT_week4, Mar2017_week4_drydown$drydown.slopes, labels = forage_terrain_energy$location[-13], pos=1, offset = 0.5)
summary(lm(Mar2017_week4_drydown$drydown.slopes ~ Mar2017_avgsoilT_week4)) #33% of variance in drydown slopes explained by soil temperature with relationship now negative (p=0.03)
summary(lm(Mar2017_week4_drydown$drydown.slopes ~ Mar2017_avgsoilT_week4 + forage_terrain_energy$clp031417[-13])) #33% of variance explained (p=0.89) by soil temperature (p = 0.048) + Mar standing biomass (p=0.72)
summary(lm(Mar2017_week4_drydown$drydown.slopes ~ forage_terrain_energy$clp031417[-13])) #<2% by Mar standing biomass alone

#Apr forage vs. soil T Mar week 4
plot(Mar2017_avgsoilT_week4, forage_terrain_energy$clp041017[-13])
abline(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week4))
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week4)) # < 1% variance explained, slope slightly negative

#Apr forage vs. drydown slopes Mar week 4
plot(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy$clp041017[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week4_drydown$drydown.slopes)) 
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week4_drydown$drydown.slopes))#3% variance explained; p=0.54, slope negative
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week4 + Mar2017_week4_drydown$drydown.slopes))#3% variance explained
summary(lm(forage_terrain_energy$clp041017[-13] ~ forage_terrain_energy$annual_kwh.m2[-13] + Mar2017_week4_drydown$drydown.slopes)) #5% variance explained; same relationships

#Apr forage gain vs. soil T Mar week 4
plot(Mar2017_avgsoilT_week4, forage_terrain_energy$Apr2017growth[-13])
abline(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week4))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week4)) #23% variance explained with negative soil T slope; p=0.07

#Apr forage gain vs. soil T Mar week 4 + drydown slopes Mar week 4
plot(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
abline(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week4_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week4_drydown$drydown.slopes)) #14% variance explained with negative slope (p=0.17); no outliers residuals are 1,000+ lbs
plot(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week4_drydown$drydown.slopes)) #8 is almost an outlier in terms of leverage
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week4 + Mar2017_week4_drydown$drydown.slopes)) #24% variance explained (p-val=0.19); drydown slope is negative (p=0.084); soil T relationship is negative (p=0.067)
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week4 * Mar2017_week4_drydown$drydown.slopes)) #27% variance explained (p=0.31)

#peak forage vs. soil T Mar week 4
plot(Mar2017_avgsoilT_week4, forage_terrain_energy$peak2017[-13])
abline(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week4))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week4)) #<1% variance explained (p=0.65)

#peak forage vs. soil T Mar week 4 + drawdown Mar week 4
plot(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy$peak2017[-13])
abline(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week4_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week4_drydown$drydown.slopes)) #7% variance explained (p=0.34)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week4_drydown$drydown.slopes + Mar2017_avgsoilT_week4)) #7% variance explained (p=0.3)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week4_drydown$drydown.slopes + Mar2017_avgsoilT_week4 + forage_terrain_energy$elevation[-13])) #19% variance explained (p=0.50)

#rank correlation tests to confirm above
rank_test(Mar2017_avgsoilT_week4, Mar2017_week4_drydown, 'drydown.slopes', mtd = 'spearman') #p.val=0.03; r.rank=-0.56
rank_test(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') #p.val=0.30; r.rank=-0.29
rank_test(Mar2017_avgsoilT_week4, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') # p.val = 0.09; r.rank=0.46

#vs. Apr forage growth
rank_test(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Apr2017growth', mtd = 'spearman') #p.val=0.42 r.rank=0.23
rank_test(Mar2017_avgsoilT_week4, forage_terrain_energy[-13,], 'Apr2017growth', mtd = 'spearman') #pval=0.05; r.rank=-0.51
rank_test(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') #p.val=0.77 r.rank=0.08
rank_test(Mar2017_avgsoilT_week4, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') #pval=0.52; r.rank=-0.17

#vs. peak forage overall
rank_test(Mar2017_week4_drydown$drydown.slopes, forage_terrain_energy[-13,], 'peak2017', mtd = 'spearman')
rank_test(Mar2017_avgsoilT_week4, forage_terrain_energy[-13,], 'peak2017', mtd = 'spearman')

#plot Mar week 5 drydown
Mar_week5 <- which(colnames(vwc_7cm_2017)=='Apr_04_2017'):which(colnames(vwc_7cm_2017)=='Apr_10_2017')
for (i in c(1:12,14:16)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week5], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week5], col=forage_terrain_energy$energy_colors[i], ylim=c(0.14, 0.32), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week5], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week5], col=forage_terrain_energy$energy_colors[i], ylim=c(0.14, 0.32))}
}

Mar2017_week5_drydown <- as.data.frame(t(apply(vwc_7cm_2017[-13,Mar_week5], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week5_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week5_drydown$drydown.slopes <- -Mar2017_week5_drydown$drydown.slopes
Mar2017_week5_drydown
Mar2017_avgsoilT_week5 <- apply(soilT_7cm_2017[-13,Mar_week5], 1, mean)
summary(Mar2017_week5_drydown$drydown.slopes)
sd(Mar2017_week5_drydown$drydown.slopes)
summary(Mar2017_avgsoilT_week5)
sd(Mar2017_avgsoilT_week5)

#check soil T vs. solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_avgsoilT_week5, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_avgsoilT_week5 ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_avgsoilT_week5 ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2=0.87

#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_week5_drydown$drydown.slopes)
abline(lm(Mar2017_week5_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_week5_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2 = 0.13, p=0.18

#drydown slopes vs. soil temp Mar week5
plot(Mar2017_avgsoilT_week5, Mar2017_week5_drydown$drydown.slopes, col=forage_terrain_energy$energy_colors[-13], cex=forage_terrain_energy$clp031417[-13]/1000)
abline(lm(Mar2017_week5_drydown$drydown.slopes ~ Mar2017_avgsoilT_week5))
text(Mar2017_avgsoilT_week5, Mar2017_week5_drydown$drydown.slopes, labels = forage_terrain_energy$location[-13], pos=1, offset = 0.5)
summary(lm(Mar2017_week5_drydown$drydown.slopes ~ Mar2017_avgsoilT_week5)) #17% of variance in drydown slopes explained by soil temperature with relationship now negative (p=0.12)
summary(lm(Mar2017_week5_drydown$drydown.slopes ~ Mar2017_avgsoilT_week5 + forage_terrain_energy$clp031417[-13])) #20% of variance explained (p=0.25) by soil temperature (p = 0.37) + Mar standing biomass (p=0.52)
summary(lm(Mar2017_week5_drydown$drydown.slopes ~ forage_terrain_energy$clp031417[-13])) #<15% by Mar standing biomass alone

#Apr forage vs. soil T Mar week 5
plot(Mar2017_avgsoilT_week5, forage_terrain_energy$clp050117[-13])
abline(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_avgsoilT_week5))
summary(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_avgsoilT_week5)) # < 1% variance explained, slope slightly negative

#Apr forage vs. drydown slopes Mar week 5
plot(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy$clp041017[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week5_drydown$drydown.slopes)) 
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week5_drydown$drydown.slopes))#1% variance explained; p=0.70, slope negative
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week5 + Mar2017_week5_drydown$drydown.slopes))#3% variance explained
summary(lm(forage_terrain_energy$clp041017[-13] ~ forage_terrain_energy$annual_kwh.m2[-13] + Mar2017_week5_drydown$drydown.slopes)) #2% variance explained; same relationships

#Apr forage gain vs. soil T Mar week 5
plot(Mar2017_avgsoilT_week5, forage_terrain_energy$Apr2017growth[-13])
abline(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week5))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week5)) #26% variance explained with negative soil T slope; p=0.054

#Apr forage gain vs. soil T Mar week 5 + drydown slopes Mar week 5
plot(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
abline(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week5_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week5_drydown$drydown.slopes)) #1% variance explained with positive slope (p=0.68); no outliers residuals are 1,000+ lbs
plot(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week5_drydown$drydown.slopes)) #8 is almost an outlier in terms of leverage
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week5 + Mar2017_week5_drydown$drydown.slopes)) #27% variance explained (p-val=0.16); drydown slope is negative (p=0.68); soil T relationship is negative (p=0.064)
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_avgsoilT_week5 * Mar2017_week5_drydown$drydown.slopes)) #29% variance explained (p=0.27)

#peak forage vs. soil T Mar week 5
plot(Mar2017_avgsoilT_week5, forage_terrain_energy$peak2017[-13])
abline(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week5))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week5)) #<1% variance explained (p=0.65)

#peak forage vs. soil T Mar week 5 + drawdown Mar week 5
plot(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy$peak2017[-13])
abline(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week5_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week5_drydown$drydown.slopes)) #7% variance explained (p=0.34)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week5_drydown$drydown.slopes + Mar2017_avgsoilT_week5)) #7% variance explained (p=0.3)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week5_drydown$drydown.slopes + Mar2017_avgsoilT_week5 + forage_terrain_energy$elevation[-13])) #19% variance explained (p=0.50)

#rank correlation tests to confirm above
rank_test(Mar2017_avgsoilT_week5, Mar2017_week5_drydown, 'drydown.slopes', mtd = 'spearman') #p.val=0.33; r.rank=-0.27
rank_test(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') #p.val=0.26; r.rank=-0.31
rank_test(Mar2017_avgsoilT_week5, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') # p.val = 0.034; r.rank=0.56

#vs. Apr forage growth
rank_test(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Apr2017growth', mtd = 'spearman') #p.val=0.81 r.rank=0.07
rank_test(Mar2017_avgsoilT_week5, forage_terrain_energy[-13,], 'Apr2017growth', mtd = 'spearman') #pval=0.04; r.rank=-0.53
rank_test(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') #p.val=0.85 r.rank=-0.05
rank_test(Mar2017_avgsoilT_week5, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') #pval=0.58; r.rank=-0.15

#vs. peak forage overall
rank_test(Mar2017_week5_drydown$drydown.slopes, forage_terrain_energy[-13,], 'peak2017', mtd = 'spearman')
rank_test(Mar2017_avgsoilT_week5, forage_terrain_energy[-13,], 'peak2017', mtd = 'spearman')

#plot Mar week 6 drydown (6 day period on account of rain)
Mar_week6 <- which(colnames(vwc_7cm_2017)=='Apr_11_2017'):which(colnames(vwc_7cm_2017)=='Apr_16_2017')
for (i in c(1:12,14:16)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week6], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week6], col=forage_terrain_energy$energy_colors[i], ylim=c(0.14, 0.32), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week6], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week6], col=forage_terrain_energy$energy_colors[i], ylim=c(0.14, 0.32))}
}

Mar2017_week6_drydown <- as.data.frame(t(apply(vwc_7cm_2017[-13,Mar_week6], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week6_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week6_drydown$drydown.slopes <- -Mar2017_week6_drydown$drydown.slopes
Mar2017_week6_drydown
Mar2017_avgsoilT_week6 <- apply(soilT_7cm_2017[-13,Mar_week6], 1, mean)
summary(Mar2017_week6_drydown$drydown.slopes)
sd(Mar2017_week6_drydown$drydown.slopes)
summary(Mar2017_avgsoilT_week6)
sd(Mar2017_avgsoilT_week6)

#check soil T vs. solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_avgsoilT_week6, cex=forage_terrain_energy$clp031417/1000)
abline(lm(Mar2017_avgsoilT_week6 ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_avgsoilT_week6 ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2=0.82

#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_week6_drydown$drydown.slopes)
abline(lm(Mar2017_week6_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(Mar2017_week6_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13])) #r^2 = 0.51, p=0.003

#drydown slopes vs. soil temp Mar week6
plot(Mar2017_avgsoilT_week6, Mar2017_week6_drydown$drydown.slopes, col=forage_terrain_energy$energy_colors[-13], cex=forage_terrain_energy$clp031417[-13]/1000)
abline(lm(Mar2017_week6_drydown$drydown.slopes ~ Mar2017_avgsoilT_week6))
text(Mar2017_avgsoilT_week6, Mar2017_week6_drydown$drydown.slopes, labels = forage_terrain_energy$location[-13], pos=1, offset = 0.5)
summary(lm(Mar2017_week6_drydown$drydown.slopes ~ Mar2017_avgsoilT_week6)) #53% of variance in drydown slopes explained by soil temperature with relationship now negative (p=0.002)
summary(lm(Mar2017_week6_drydown$drydown.slopes ~ Mar2017_avgsoilT_week6 + forage_terrain_energy$clp041017[-13])) #57% of variance explained (p=0.006) by soil temperature (p = 0.003) + Apr standing biomass (p=0.303)
summary(lm(Mar2017_week6_drydown$drydown.slopes ~ forage_terrain_energy$clp041017[-13])) #8% by Apr standing biomass alone

#May forage vs. soil T Mar week 6
plot(Mar2017_avgsoilT_week6, forage_terrain_energy$clp050117[-13])
abline(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_avgsoilT_week6))
summary(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_avgsoilT_week6)) # < 16% variance explained, slope slightly negative

#May forage vs. drydown slopes Mar week 6
plot(Mar2017_week6_drydown$drydown.slopes, forage_terrain_energy$clp050117[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_week6_drydown$drydown.slopes)) 
summary(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_week6_drydown$drydown.slopes))#6% variance explained; p=0.70, slope negative
summary(lm(forage_terrain_energy$clp050117[-13] ~ Mar2017_avgsoilT_week6 + Mar2017_week6_drydown$drydown.slopes))#16% variance explained
summary(lm(forage_terrain_energy$clp050117[-13] ~ forage_terrain_energy$annual_kwh.m2[-13] + Mar2017_week6_drydown$drydown.slopes)) #7% variance explained; same relationships

#May forage gain vs. soil T Mar week 6
plot(Mar2017_avgsoilT_week6, forage_terrain_energy$May2017growth[-13])
abline(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_avgsoilT_week6))
summary(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_avgsoilT_week6)) #3% variance explained with negative soil T slope; p=0.054

#Apr forage gain vs. soil T Mar week 6 + drydown slopes Mar week 6
plot(Mar2017_week6_drydown$drydown.slopes, forage_terrain_energy$May2017growth[-13])
abline(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_week6_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_week6_drydown$drydown.slopes)) #1% variance explained with negative slope (p=0.68); no outliers residuals are 1,000+ lbs
plot(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_week6_drydown$drydown.slopes)) #8 is almost an outlier in terms of leverage
summary(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_avgsoilT_week6 + Mar2017_week6_drydown$drydown.slopes)) #15% variance explained (p-val=0.36); drydown slope is negative (p=0.20); soil T relationship is negative (p=0.18)
summary(lm(forage_terrain_energy$May2017growth[-13] ~ Mar2017_avgsoilT_week6 * Mar2017_week6_drydown$drydown.slopes)) #29% variance explained (p=0.27)


#rank correlation tests to confirm above
rank_test(Mar2017_avgsoilT_week6, Mar2017_week6_drydown, 'drydown.slopes', mtd = 'spearman') #p.val=0.0123; r.rank=-0.63
rank_test(Mar2017_week6_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp031417', mtd = 'spearman') #p.val=0.41; r.rank=-0.22
rank_test(Mar2017_avgsoilT_week6, forage_terrain_energy[-13,], 'clp041017', mtd = 'spearman') # p.val = 0.54; r.rank=-0.17

#vs. May forage growth
rank_test(Mar2017_week6_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'May2017growth', mtd = 'spearman') #p.val=0.84 r.rank=-0.057
rank_test(Mar2017_avgsoilT_week6, forage_terrain_energy[-13,], 'May2017growth', mtd = 'spearman') #pval=0.50; r.rank=-0.19
rank_test(Mar2017_week6_drydown$drydown.slopes, forage_terrain_energy[-13,], 'clp050117', mtd = 'spearman') #p.val=0.137 r.rank=0.40
rank_test(Mar2017_avgsoilT_week6, forage_terrain_energy[-13,], 'clp050117', mtd = 'spearman') #pval=0.12; r.rank=-0.42


####
#7 cm Apr drawdown slopes
Apr2017_avgsoilT <- apply(soilT_7cm_2017[,which(colnames(soilT_7cm_2017)=='Apr_01_2017'):which(colnames(soilT_7cm_2017)=='Apr_10_2017')], 1, mean)[-13]
Apr2017_drydown <- as.data.frame(t(apply(vwc_7cm_2017[-13,which(colnames(vwc_7cm_2017)=='Apr_01_2017'):which(colnames(vwc_7cm_2017)=='Apr_10_2017')], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Apr2017_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Apr2017_drydown$drydown.slopes <- -Apr2017_drydown$drydown.slopes
Apr2017_drydown
mean(Apr2017_drydown$drydown.slopes)
plot(Apr2017_drydown$drydown.slopes, forage_terrain_energy$clp050117[-13])
plot(Apr2017_avgsoilT, Apr2017_drydown$drydown.slopes)
plot(forage_terrain_energy$annual_kwh.m2[-13], Apr2017_avgsoilT)
summary(lm(Apr2017_drydown$drydown.slopes ~ Apr2017_avgsoilT))
summary(lm(Apr2017_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
mult.lm.result <- lm(forage_terrain_energy$clp041017[-13] ~ Apr2017_avgsoilT + Apr2017_drydown$drydown.slopes)
summary(mult.lm.result)
#plot(mult.lm.result)
lm.result <- lm(forage_terrain_energy$clp041017[-13] ~ Apr2017_drydown$drydown.slopes)
summary(lm.result)
forage_terrain_energy$May2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
summary(lm(forage_terrain_energy$May2017growth[-13] ~ Apr2017_drydown$drydown.slopes))
plot(Apr2017_drydown$drydown.slopes, forage_terrain_energy$May2017growth[-13])
#plot(lm.result)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Apr2017_avgsoilT))

#precip data for Mar1-Apr16
precip_data_dry_2017 <- precip_data[which(precip_data$Date=="3/1/2017"):which(precip_data$Date=='4/20/2017'), ]

#relationship between March 1 - Apr 15 soil T and growth
avgsoilT_dates_2017 <- which(colnames(soilT_7cm_2017)=='Mar_01_2017'):which(colnames(soilT_7cm_2017)=='Apr_10_2017')
avgsoilT_2017 <- apply(soilT_7cm_2017[ ,avgsoilT_dates_2017], 1, mean, na.rm=TRUE)
plot(avgsoilT_2017, forage_terrain_energy$Apr2017growth)
summary(lm(forage_terrain_energy$Apr2017growth ~ avgsoilT_2017))

#function to get drawdown slopes all together for plot
drawdown_slopes <- function(df, start_date, end_date, df2) {
  date_indices <- which(colnames(df)==start_date):which(colnames(df)==end_date)
  drydown_df <- as.data.frame(t(apply(df[ ,date_indices], 1, function(x) {
    if (all(is.na(x))) {
      rep(NA, 3)
    } else {
        lm.summary <- summary(lm(x ~ seq_along(x)))
        c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
    }})))
  colnames(drydown_df) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
  drydown_df$drydown.slopes <- -drydown_df$drydown.slopes
  drydown_df$drydown.slopes <- ifelse(drydown_df$drydown.slopes < 0, 0, drydown_df$drydown.slopes)
  drydown_df$avgsoilT <- apply(df2[ ,date_indices], 1, mean)
  drydown_df$soilT_color <- ifelse(drydown_df$avgsoilT < summary(drydown_df$avgsoilT)[2], 'blue', ifelse(drydown_df$avgsoilT > summary(drydown_df$avgsoilT)[2] & drydown_df$avgsoilT < summary(drydown_df$avgsoilT)[5], 'orange2', 'red3'))
  drydown_df
}

#7 cm drawdowns
#week0 <- drawdown_slopes(vwc_7cm_2017, 'Feb_25_2017', 'Feb_28_2017')
multiweek <- drawdown_slopes(vwc_7cm_2017, 'Feb_28_2017', 'Mar_14_2017', soilT_7cm_2017)
week1 <- drawdown_slopes(vwc_7cm_2017, 'Feb_28_2017', 'Mar_06_2017', soilT_7cm_2017)
week2 <- drawdown_slopes(vwc_7cm_2017, 'Mar_07_2017', 'Mar_13_2017', soilT_7cm_2017)
week3 <- drawdown_slopes(vwc_7cm_2017, 'Mar_14_2017', 'Mar_20_2017', soilT_7cm_2017)
wetup1 <- drawdown_slopes(vwc_7cm_2017, 'Mar_21_2017', 'Mar_26_2017', soilT_7cm_2017)
week4 <- drawdown_slopes(vwc_7cm_2017, 'Mar_27_2017', 'Apr_02_2017', soilT_7cm_2017)
week5 <- drawdown_slopes(vwc_7cm_2017, 'Apr_03_2017', 'Apr_09_2017', soilT_7cm_2017)
week6 <- drawdown_slopes(vwc_7cm_2017, 'Apr_10_2017', 'Apr_16_2017', soilT_7cm_2017) #because wet-up began on Apr 17
wetup2 <- drawdown_slopes(vwc_7cm_2017, 'Apr_17_2017', 'Apr_20_2017', soilT_7cm_2017)
week7 <- drawdown_slopes(vwc_7cm_2017, 'Apr_21_2017', 'Apr_27_2017', soilT_7cm_2017)
week8 <- drawdown_slopes(vwc_7cm_2017, 'Apr_28_2017', 'May_04_2017', soilT_7cm_2017)
drawdown_collection <- list(week1, week2, week3, wetup1, week4, week5, week6, wetup2, week7, week8, multiweek)
names(drawdown_collection) <- c('week1', 'week2', 'week3', 'wetup1', 'week4', 'week5', 'week6', 'wetup2', 'week7', 'week8', 'multiweek')
#summary stats
lapply(drawdown_collection, function(x) summary(150*x$drydown.slopes))
#look at relationship between temperature and soil moisture drawdown
lapply(drawdown_collection, function(x) summary(lm(x$drydown.slopes ~ x$avgsoilT)))
#look at some relationships with Mar and Apr growth
lapply(drawdown_collection, function(x) summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ x$drydown.slopes[-13])))
lapply(drawdown_collection, function(x) summary(lm(forage_terrain_energy$clp031417 ~ x$drydown.slopes)))
lapply(drawdown_collection, function(x) summary(lm(forage_terrain_energy$clp031417 ~ x$avgsoilT)))
lapply(drawdown_collection, function(x) summary(lm(forage_terrain_energy$Apr2017growth ~ x$drydown.slopes + mean_depletion_spring_growth)))
lapply(drawdown_collection, function(x) summary(lm(forage_terrain_energy$clp041017 ~ x$drydown.slopes + x$avgsoilT)))
lapply(drawdown_collection, function(x) summary(lm(forage_terrain_energy$Apr2017growth[-14] ~ x$drydown.slopes[-14] + x$avgsoilT[-14])))
for (i in seq_along(drawdown_collection)) {
  plot(drawdown_collection[[i]]$drydown.slopes, forage_terrain_energy$Mar2017growth, main = names(drawdown_collection)[i])
}
for (i in seq_along(drawdown_collection)) {
  plot(drawdown_collection[[i]]$drydown.slopes, forage_terrain_energy$Apr2017growth, main = names(drawdown_collection)[i])
}
brk <- 0.6
start_pt <- 0.2
pt.cex <- 1.2
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2017 drawdown', 'SM_drawdown_7cm_MarApr2017.png'), family = 'Book Antiqua', width = 1000, height = 700, units = 'px', res=100)
par(mar=c(3.5, 4.5, 1, 4.5))
plot(x=rep(start_pt,16), week1$drydown.slopes*150, type = 'p', col=week1$soilT_color, pch=1, ylim=c(0, 1.2), xlim=c(0,4), xaxt='n', xlab='', ylab=expression('soil moisture drawdown upper 15 cm soil (mm H'[2]*'O'~day^-1*')'), cex.axis=1.1, cex.lab=1.1, cex=pt.cex)
points(x=rep(start_pt+brk*1,16), week2$drydown.slopes*150, col=week2$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*2,16), week3$drydown.slopes*150, col=week3$soilT_color, pch=1, cex=pt.cex)
text(x=start_pt+brk*3, y=0.6, label='11 mm\nprecipitation\nMar 21 -\nMar 25', cex=1)
points(x=rep(start_pt+brk*4,16), week4$drydown.slopes*150, col=week4$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*5,16), week5$drydown.slopes*150, col=week5$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*6,16), week6$drydown.slopes*150, col=week6$soilT_color, pch=1, cex=pt.cex)
axis(side = 1, at = seq(start_pt, by=brk, length.out = 7), labels = c('Feb 28 -\nMar 6', 'Mar 7 -\nMar 13', 'Mar 14 -\nMar 20', 'wet-up', 'Mar 27 -\nApr 2', 'Apr 3 -\nApr 9', 'Apr 10 -\nApr 16'), cex=1.1, mgp=c(4, 2, 0))
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {mean(x$avgsoilT)}) / 20, lty=1, col='grey')
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {max(x$avgsoilT)}) / 20, lty=2, col='grey')
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {min(x$avgsoilT)}) / 20, lty=2, col='grey')
axis(side = 4, at = c(0.25, 0.5, 0.75, 1, 1.25), labels = c('5', '10', '15', '20', '25'), cex=1.1, col = 'grey', col.axis='grey', mgp=c(3, 1, 0.5))
mtext(expression(paste('all site soil temperature, 7 cm depth ('*degree*'C)')), side=4, line=3, at=0.75, cex=1.1, col='grey')
legend(x=-0.1, y = 1.2, legend=(c('all site mean', 'all site max/min', "lower quartile (cool)", 'inter-quartile', 'upper quartile (warm)')), lty=c(1, 2, NA, NA, NA), pch=c(NA, NA, 1, 1, 1), pt.cex = pt.cex, col=c('grey', 'grey', 'blue', 'orange2', 'red3'), text.col = c('grey', 'grey', 'black', 'black', 'black'), title = expression('7 cm soil temperature ('*degree*'C)'), title.col = 'black', inset=0.005)
dev.off()

#22 cm drawdowns
#week0 <- drawdown_slopes(vwc_7cm_2017, 'Feb_25_2017', 'Feb_28_2017')
week1_22 <- drawdown_slopes(vwc_22cm_2017, 'Feb_28_2017', 'Mar_06_2017', soilT_22cm_2017)
week2_22 <- drawdown_slopes(vwc_22cm_2017, 'Mar_07_2017', 'Mar_13_2017', soilT_22cm_2017)
week3_22 <- drawdown_slopes(vwc_22cm_2017, 'Mar_14_2017', 'Mar_20_2017', soilT_22cm_2017)
wetup1_22 <- drawdown_slopes(vwc_22cm_2017, 'Mar_21_2017', 'Mar_26_2017', soilT_22cm_2017)
week4_22 <- drawdown_slopes(vwc_22cm_2017, 'Mar_27_2017', 'Apr_02_2017', soilT_22cm_2017)
week5_22 <- drawdown_slopes(vwc_22cm_2017, 'Apr_03_2017', 'Apr_09_2017', soilT_22cm_2017)
week6_22 <- drawdown_slopes(vwc_22cm_2017, 'Apr_10_2017', 'Apr_16_2017', soilT_22cm_2017) #because wet-up began on Apr 17
wetup2_22 <- drawdown_slopes(vwc_22cm_2017, 'Apr_17_2017', 'Apr_20_2017', soilT_22cm_2017)
week7_22 <- drawdown_slopes(vwc_22cm_2017, 'Apr_21_2017', 'Apr_27_2017', soilT_22cm_2017)
week8_22 <- drawdown_slopes(vwc_22cm_2017, 'Apr_28_2017', 'May_04_2017', soilT_22cm_2017)
drawdown_collection_22 <- list(week1_22, week2_22, week3_22, wetup1_22, week4_22, week5_22, week6_22, wetup2_22, week7_22, week8_22)
names(drawdown_collection_22) <- c('week1', 'week2', 'week3', 'wetup1', 'week4', 'week5', 'week6', 'wetup2', 'week7', 'week8')
lapply(drawdown_collection_22, function(x) summary(150*x$drydown.slopes))
lapply(drawdown_collection_22, function(x) summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ x$drydown.slopes[-13])))
lapply(drawdown_collection_22, function(x) summary(lm(forage_terrain_energy$Apr2017growth ~ x$drydown.slopes)))
for (i in seq_along(drawdown_collection_22)) {
  plot(drawdown_collection_22[[i]]$drydown.slopes, forage_terrain_energy$Mar2017growth, main = names(drawdown_collection_22)[i])
}
for (i in seq_along(drawdown_collection_22)) {
  plot(drawdown_collection_22[[i]]$drydown.slopes, forage_terrain_energy$Apr2017growth, main = names(drawdown_collection_22)[i])
}
drawdown_collection_22
brk <- 0.6
start_pt <- 0.2
pt.cex <- 1.2
soilTfactor <- 31.25
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', '2017 drawdown', 'SM_drawdown_22cm_2017.png'), family = 'Book Antiqua', width = 1000, height = 700, units = 'px', res=100)
par(mar=c(3.5, 4.5, 1, 4.5))
plot(x=rep(start_pt,16), week1_22$drydown.slopes*150, type = 'p', col=week1_22$soilT_color, pch=1, ylim=c(-0.1, 0.9), xlim=c(0,5.8), xaxt='n', xlab='', ylab=expression('soil moisture drawdown 15-30 cm soil depth (mm H'[2]*'O'~day^-1*')'), cex.axis=1.1, cex.lab=1.1, cex=pt.cex)
points(x=rep(start_pt+brk*1,16), week2_22$drydown.slopes*150, col=week2_22$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*2,16), week3_22$drydown.slopes*150, col=week3_22$soilT_color, pch=1, cex=pt.cex)
text(x=start_pt+brk*3, y=0.4, label='11 mm\nprecipitation', cex=1.1)
points(x=rep(start_pt+brk*3,16), wetup1_22$drydown.slopes*150, col=wetup1_22$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*4,16), week4_22$drydown.slopes*150, col=week4_22$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*5,16), week5_22$drydown.slopes*150, col=week5_22$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*6,16), week6_22$drydown.slopes*150, col=week6_22$soilT_color, pch=1, cex=pt.cex)
text(x=start_pt+brk*7, y=0.4, label='14 mm\nprecipitation', cex=1.1)
points(x=rep(start_pt+brk*8,16), week7_22$drydown.slopes*150, col=week7_22$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*9,16), week8_22$drydown.slopes*150, col=week8_22$soilT_color, pch=1, cex=pt.cex)
axis(side = 1, at = seq(start_pt, by=brk, length.out = 10), labels = c('Feb 28 -\nMar 6', 'Mar 7 -\nMar 13', 'Mar 14 -\nMar 20', 'wet-up', 'Mar 27 -\nApr 2', 'Apr 3 -\nApr 9', 'Apr 10 -\nApr 16', 'wet-up', 'Apr 21 -\nApr 27', 'Apr 28 -\nMay 4'), cex=1.1, mgp=c(4, 2, 0))
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {mean(x$avgsoilT)}) / soilTfactor, lty=1)
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {max(x$avgsoilT)}) / soilTfactor, lty=2, col='grey')
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {min(x$avgsoilT)}) / soilTfactor, lty=2, col='grey')
axis(side = 4, at = c(5, 10, 15, 20, 25)/soilTfactor, labels = c('5', '10', '15', '20', '25'), cex=1.1)
mtext(expression(paste('all site soil temperature, 22 cm depth ('*degree*'C)')), side=4, line=2.5, at=0.4, cex=1.1)
legend('top', legend=(c("lower quartile (cool)", 'inter-quartile', 'upper quartile (warm)')), pch=1, pt.cex = rep(pt.cex, 3), col=c('blue', 'orange2', 'red3'), title = expression('soil temperature ('*degree*'C)'), cex = 1, inset=0.005) #x=seq(start_pt, by=brk, length.out = 10)[2] + 0.1, y=0.3
legend(x=-0.2, y=0.93, legend=c('all site mean', 'all site max/min'), lty=c(1, 2), col=c('black', 'grey'), title = expression('soil temperature ('*degree*'C)'))
dev.off()


#merge 7 and 22 cm weekly summaries
drawdown_collection_7_22 <- mapply(function(x, y) {data.frame(drydown.combined = x$drydown.slopes + y$drydown.slopes, avgsoilT = rowMeans(cbind(x$avgsoilT, y$avgsoilT)), drydown.7=x$drydown.slopes, drydown.22=y$drydown.slopes, soilT7=x$avgsoilT, soilT22=y$avgsoilT)}, x = drawdown_collection, y = drawdown_collection_22, SIMPLIFY = FALSE)
class(drawdown_collection_7_22)
names(drawdown_collection_7_22) <- c('week1', 'week2', 'week3', 'wetup1', 'week4', 'week5', 'week6', 'wetup2', 'week7', 'week8')
drawdown_collection_7_22 <- lapply(drawdown_collection_7_22, function(x) {
  x$water.uptake.shallow <- ifelse(x$drydown.combined  == 0, NA, round(100 * (x$drydown.7 / x$drydown.combined), 1))
  x}
  )
lapply(drawdown_collection_7_22, function(x) summary(x$water.uptake.shallow))
lapply(drawdown_collection_7_22, function(x) summary(lm(forage_terrain_energy$clp041017 ~ x$water.uptake.shallow)))
lapply(drawdown_collection_7_22, function(x) summary(lm(forage_terrain_energy$Mar2017growth ~ x$water.uptake.shallow)))
lapply(drawdown_collection_7_22, function(x) summary(lm(forage_terrain_energy$Apr2017growth ~ x$water.uptake.shallow)))
for (i in seq_along(drawdown_collection_7_22)) {
  plot(drawdown_collection_7_22[[i]]$water.uptake.shallow, forage_terrain_energy$Mar2017growth, main = names(drawdown_collection_7_22)[i])
}
for (i in seq_along(drawdown_collection_7_22)) {
  plot(drawdown_collection_7_22[[i]]$water.uptake.shallow, forage_terrain_energy$Apr2017growth, main = names(drawdown_collection_7_22)[i])
}
lapply(drawdown_collection_7_22, function(x) {
  plot(x$avgsoilT, x$drydown.combined, main=names())
  lm.result <- lm(x$drydown.combined ~ x$avgsoilT)
  abline(lm.result)
  text(x=(min(x$avgsoilT) + 2), y=max(x$drydown.combined, na.rm = TRUE), label = round(summary(lm.result)$r.squared, 3), col='red')})

###full drawdown plot for 2017 including two wetup periods
par(mar=c(3.5, 4.5, 1, 4.5))
plot(x=rep(start_pt,16), week1$drydown.slopes*150, type = 'p', col=week1$soilT_color, pch=1, ylim=c(0, 1.3), xlim=c(0,5.8), xaxt='n', xlab='', ylab=expression('soil moisture drawdown upper 15 cm soil (mm H'[2]*'O'~day^-1*')'), cex.axis=1.1, cex.lab=1.1, cex=pt.cex)
points(x=rep(start_pt+brk*1,16), week2$drydown.slopes*150, col=week2$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*2,16), week3$drydown.slopes*150, col=week3$soilT_color, pch=1, cex=pt.cex)
text(x=start_pt+brk*3, y=0.6, label='11 mm\nprecipitation', cex=1)
points(x=rep(start_pt+brk*4,16), week4$drydown.slopes*150, col=week4$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*5,16), week5$drydown.slopes*150, col=week5$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*6,16), week6$drydown.slopes*150, col=week6$soilT_color, pch=1, cex=pt.cex)
text(x=start_pt+brk*7, y=0.6, label='14 mm\nprecipitation', cex=1)
points(x=rep(start_pt+brk*8,16), week7$drydown.slopes*150, col=week7$soilT_color, pch=1, cex=pt.cex)
points(x=rep(start_pt+brk*9,16), week8$drydown.slopes*150, col=week8$soilT_color, pch=1, cex=pt.cex)
axis(side = 1, at = seq(start_pt, by=brk, length.out = 10), labels = c('Feb 28 -\nMar 6', 'Mar 7 -\nMar 13', 'Mar 14 -\nMar 20', 'wet-up', 'Mar 27 -\nApr 2', 'Apr 3 -\nApr 9', 'Apr 10 -\nApr 16', 'wet-up', 'Apr 21 -\nApr 27', 'Apr 28 -\nMay 4'), cex=1.1, mgp=c(4, 2, 0))
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {mean(x$avgsoilT)}) / 20, lty=1, col='grey')
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {max(x$avgsoilT)}) / 20, lty=2, col='grey')
lines(x=seq(start_pt, by=brk, length.out = 10), sapply(drawdown_collection, function(x) {min(x$avgsoilT)}) / 20, lty=2, col='grey')
axis(side = 4, at = c(0.25, 0.5, 0.75, 1, 1.25), labels = c('5', '10', '15', '20', '25'), cex=1.1, col = 'grey', col.axis='grey', mgp=c(3, 1, 0.5))
mtext(expression(paste('all site soil temperature, 7 cm depth ('*degree*'C)')), side=4, line=3, at=0.75, cex=1.1, col='grey')
legend(x=-0.15, y = 1.33, legend=(c("lower quartile (cool)", 'inter-quartile', 'upper quartile (warm)')), pch=1, pt.cex = rep(pt.cex, 3), col=c('blue', 'orange2', 'red3'), title = expression('soil temperature ('*degree*'C)'), cex = 1, inset=0.005) #x=seq(start_pt, by=brk, length.out = 10)[2] + 0.1, y=0.3
legend(x=2.7, y=1.33, legend=c('all site mean', 'all site max/min'), lty=c(1, 2), col=c('grey', 'grey'), title = expression('soil temperature ('*degree*'C)'))




#####
#needs to be revised
#22 cm Mar 2017
Mar2017_drydown_22 <- as.data.frame(t(apply(vwc_22cm_2017[,which(colnames(vwc_22cm_2017)=='Mar_04_2017'):which(colnames(vwc_22cm_2017)=='Mar_20_2017')], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_drydown_22) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_drydown_22$drydown.slopes <- -Mar2017_drydown_22$drydown.slopes
Mar2017_drydown_22
mean(Mar2017_drydown_22$drydown.slopes)
100 * mean(Mar2017_drydown$drydown.slopes) / mean(Mar2017_drydown_22$drydown.slopes)
plot(Mar2017_drydown_22$drydown.slopes, forage_terrain_energy$clp041017)
plot(Mar2017_drydown_22$drydown.slopes, forage_terrain_energy$Apr2017growth)
summary(lm(forage_terrain_energy$clp041017 ~ Mar2017_drydown_22$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth ~ Mar2017_drydown_22$drydown.slopes))
summary(lm(Mar2017_drydown_22$drydown.slopes ~ forage_terrain_energy$elevation+ forage_terrain_energy$curvature_mean))
summary(lm(Mar2017_drydown_22$drydown.slopes ~ forage_terrain_energy$elevation))

#22 cm early Apr drawdown
Apr2017_drydown_22 <- as.data.frame(t(apply(vwc_22cm_2017[-13,which(colnames(vwc_22cm_2017)=='Apr_01_2017'):which(colnames(vwc_22cm_2017)=='Apr_10_2017')], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Apr2017_drydown_22) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Apr2017_drydown_22$drydown.slopes <- -Apr2017_drydown_22$drydown.slopes
Apr2017_drydown_22
mean(Apr2017_drydown_22$drydown.slopes)
100 * mean(Apr2017_drydown$drydown.slopes) / mean(Apr2017_drydown_22$drydown.slopes)
plot(Apr2017_drydown_22$drydown.slopes, forage_terrain_energy$clp041017[-13])
plot(Apr2017_drydown_22$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
summary(lm(forage_terrain_energy$clp041017[-13] ~ Apr2017_drydown_22$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Apr2017_drydown_22$drydown.slopes))
cor(Apr2017_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13], method='spearman')
summary(lm(Apr2017_drydown_22$drydown.slopes ~ forage_terrain_energy$elevation[-13] + forage_terrain_energy$curvature_mean[-13]))
summary(lm(Apr2017_drydown_22$drydown.slopes ~ forage_terrain_energy$elevation[-13]))
