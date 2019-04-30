#remake of Figure 9a-b, 10a-b, 12a-b, and 13a-b from dissertation
#condensing 4 into 2: Figure 7a-d (was Figure 9a-b and 10a-b) and Figure 9a-b (was 12a-b and 13a-b)
library(extrafont)
library(extrafontdb)
loadfonts()
dataDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/'
results <- 'C:/Users/smdevine/Desktop/rangeland project/dissertation Chp2/for publication/figures'
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
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

#add energy colors
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 <= 1200, 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > 1200 & forage_terrain_energy$annual_kwh.m2 < 1410, 'orange2', 'red3')) #tres colores #tres colores
forage_terrain_energy$May2017growth <- forage_terrain_energy$clp050117 - forage_terrain_energy$clp041017
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
forage_terrain_energy$Mar2017growth <- forage_terrain_energy$clp031417 - forage_terrain_energy$clp021517
forage_terrain_energy$Mar2018growth <- forage_terrain_energy$clp032218 - forage_terrain_energy$clp021518
forage_terrain_energy$Apr2018growth <- forage_terrain_energy$clp041518 - forage_terrain_energy$clp032218

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

nlm_2018 <- function(x, date, df) {
  nlm_obj <- lm(df[,date][] ~ df$avg_soilT[] + I(df$avg_soilT[]^2))
  nlm_obj$coefficients[1] + nlm_obj$coefficients[2] * x + nlm_obj$coefficients[3] * x^2
}

#revised Figure 7a (was Figure 9a in dissertation)
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
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results,'Fig7a.tif'), family = 'Times New Roman', width = 4.5, pointsize=11, height = 2.75, units = 'in', res=150)
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

#revised Figure 7b (was Figure 9b in dissertation)
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
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig7b.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 2.75, units = 'in', res=150)
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

#revised Figure 7c (was Figure 10a in Dissertation)
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
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig7c.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = 150)
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

#revised Figure 7d (was Figure 10b in Dissertation)
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
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

tiff(file = file.path(results, 'Fig7d.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = 150)
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

tiff(file = file.path(results, 'Fig9a_test.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 2.75, units = 'in', res=600)
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

tiff(file = file.path(results, 'Fig9b.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 2.75, units = 'in', res=150)
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

tiff(file = file.path(results, 'Fig9c.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = 150)
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
tiff(file = file.path(results, 'Fig9d.tif'), pointsize = 11, family = 'Times New Roman', width = 4.5, height = 2.75, units = 'in', res = 150)
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

#check forage growth vs. terrain
rank_test <- function(x, df, y, mtd) {
  test <- cor.test(x, df[[y]], method = mtd)
  result <- data.frame(col.1=test$p.value, col.2=test$estimate)
  colnames(result) <- c(paste0(y, '.p.val.', mtd), paste0(y, if(mtd=='pearson') {'.cor.'} else {'.rho.'}, mtd))
  result
}
method_corr <- 'spearman'
elev_corrs <- do.call(rbind, lapply(forage_terrain_energy[,c('Mar2017growth', 'Apr2017growth', 'May2017growth', 'Mar2018growth', 'Apr2018growth')], rank_test, df=forage_terrain_energy, y='elevation', mtd=method_corr))
slope_corrs <- do.call(rbind, lapply(forage_terrain_energy[,c('Mar2017growth', 'Apr2017growth', 'May2017growth', 'Mar2018growth', 'Apr2018growth')], rank_test, df=forage_terrain_energy, y='slope', mtd=method_corr))
solrad_corrs <- do.call(rbind, lapply(forage_terrain_energy[,c('Mar2017growth', 'Apr2017growth', 'May2017growth', 'Mar2018growth', 'Apr2018growth')], rank_test, df=forage_terrain_energy, y='annual_kwh.m2', mtd=method_corr))
curv_corrs <- do.call(rbind, lapply(forage_terrain_energy[,c('Mar2017growth', 'Apr2017growth', 'May2017growth', 'Mar2018growth', 'Apr2018growth')], rank_test, df=forage_terrain_energy, y='curvature_mean', mtd=method_corr))
overall.results <- cbind(elev_corrs, solrad_corrs, slope_corrs, curv_corrs)
#overall.results$clip.date <- c('Mar2017growth', 'Apr2017growth', 'May2017growth', 'Mar2018growth', 'Apr2018growth')
write.csv(overall.results, file.path(dataDir, 'forage_vs_terrain', paste0('forage.growth_vs_terrain_', method_corr,'_results.csv')), row.names = TRUE)

#corrs with soil moisture
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
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)

method_corr <- 'spearman'
terrain_corrs <- do.call(rbind, lapply(forage_terrain_energy[-13,c('elevation', 'annual_kwh.m2', 'slope', 'curvature_mean')], rank_test, df=df_7cm[-13,], y='avg_depletion_7cm', mtd=method_corr))
terrain_corrs
write.csv(terrain_corrs, file.path(dataDir, 'forage_vs_terrain', paste0('depletionApr2017_7cm_vs_terrain_', method_corr,'_results.csv')), row.names = TRUE)

#explore 0-30 cm depletion relationship with terrain
summary(lm(df_0_30cm$avg_depletion ~ df_0_30cm$avg_soilT)) #also NS: p value=0.147; r2=0.155
summary(lm(df_0_30cm$avg_depletion ~ elevation, data = forage_terrain_energy)) #p value=0.14; RMSE=0.124
summary(lm(df_0_30cm$avg_depletion ~ annual_kwh.m2, data = forage_terrain_energy)) #p value = 0.145; RMSE=0.123
summary(lm(df_0_30cm$avg_depletion ~ elevation + annual_kwh.m2, data = forage_terrain_energy)) #best model: p val=0.167; r2=0.258; RMSE=0.120
summary(lm(df_0_30cm$avg_depletion ~ elevation + annual_kwh.m2 + curvature_mean, data = forage_terrain_energy))
summary(lm(df_0_30cm$avg_depletion ~ elevation + annual_kwh.m2 + slope, data = forage_terrain_energy))
summary(lm(df_0_30cm$avg_depletion ~ elevation * slope, data = forage_terrain_energy))

#now with 7 cm data
summary(lm(df_7cm$avg_depletion_7cm ~ df_7cm$avg_soilT_7cm)) #also NS: p value=0.090; r2=0.206
summary(lm(df_7cm$avg_depletion_7cm ~ elevation, data = forage_terrain_energy)) #p value=0.43
summary(lm(df_7cm$avg_depletion_7cm ~ curvature_mean, data = forage_terrain_energy))
summary(lm(df_7cm$avg_depletion_7cm ~ slope, data = forage_terrain_energy))
summary(lm(df_7cm$avg_depletion_7cm ~ annual_kwh.m2, data = forage_terrain_energy)) #p-value=0.089; r2=0.206
summary(lm(df_7cm$avg_depletion_7cm ~ elevation + annual_kwh.m2, data = forage_terrain_energy))
summary(lm(df_7cm$avg_depletion_7cm ~ elevation + annual_kwh.m2 + curvature_mean, data = forage_terrain_energy))
summary(lm(df_7cm$avg_depletion_7cm ~ elevation + annual_kwh.m2 + slope, data = forage_terrain_energy))
summary(lm(df_7cm$avg_depletion_7cm ~ elevation * slope, data = forage_terrain_energy))

#and lastly with 22 cm data
summary(lm(df_22cm$avg_depletion_22cm ~ df_22cm$avg_soilT_22cm))
summary(lm(df_22cm$avg_depletion_22cm ~ elevation, data = forage_terrain_energy)) #p-value = 0.098; r2=0.2
summary(lm(df_22cm$avg_depletion_22cm ~ curvature_mean, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ annual_kwh.m2, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ slope, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ elevation + annual_kwh.m2, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ elevation + annual_kwh.m2 + curvature_mean, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ elevation + annual_kwh.m2 + slope, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ elevation * slope, data = forage_terrain_energy))
summary(lm(df_22cm$avg_depletion_22cm ~ elevation * curvature_mean, data = forage_terrain_energy))

#some verifications of Apr2017 growth relationships
plot(forage_terrain_energy$annual_kwh.m2, forage_terrain_energy$Apr2017growth)
plot(forage_terrain_energy$annual_kwh.m2, df_0_30cm$avg_soilT)
plot(df_0_30cm$avg_soilT, forage_terrain_energy$Apr2017growth)
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ df_0_30cm$avg_soilT[-13]))
summary(lm(forage_terrain_energy$Apr2017growth ~ df_0_30cm$avg_depletion))

#need correlation matrix of rapid growth period aggregated soil moisture (monthly) again st terrain 
#eg.
#Feb 2017, Mar 2017, Apr 2017, May 2017
#elevation....correlations
#mean curv...correlations
#insolation...correlations
#slope...correlations

#draft 4/27/19
start_date <- 'Mar_15_2017'
end_date <- 'Apr_10_2017'
normalizeVars <- FALSE
date_name <- 'Apr2017'
forage_growth <- paste0(date_name, 'growth')
dates <- which(colnames(depletion_vwc_2017)==start_date):which(colnames(depletion_vwc_2017)==end_date)
avg_depletion_7cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017[ ,dates], 1, mean))} else {apply(depletion_vwc_2017[ ,dates], 1, mean)}
avg_depletion_22cm <- if(normalizeVars) {normalize_var(apply(depletion_vwc_2017_22[ ,dates], 1, mean))} else {apply(depletion_vwc_2017_22[ ,dates], 1, mean)}
avg_depletion <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(depletion_vwc_2017[ ,dates], 1, mean), apply(depletion_vwc_2017_22[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_depletion_7cm, avg_depletion_22cm))}
avg_soilT_7cm <- if(normalizeVars) {normalize_var(apply(soilT_7cm_2017[ ,dates], 1, mean))} else {apply(soilT_7cm_2017[ ,dates], 1, mean)}
avg_soilT_22cm <- if(normalizeVars) {normalize_var(apply(soilT_22cm_2017[ ,dates], 1, mean))} else {apply(soilT_22cm_2017[ ,dates], 1, mean)}
avg_soilT <- if(normalizeVars) {normalize_var(rowMeans(cbind(apply(soilT_7cm_2017[ ,dates], 1, mean), apply(soilT_data_22cm_2017[ ,dates], 1, mean))))} else {rowMeans(cbind(avg_soilT_7cm, avg_soilT_22cm))}
#use assign here to assign dynamic variable names to each of these data.frames created
df_7cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_7cm, avg_depletion_7cm)
df_22cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT_22cm, avg_depletion_22cm)
df_0_30cm <- data.frame(growth_period = forage_terrain_energy[[forage_growth]], avg_soilT, avg_depletion)