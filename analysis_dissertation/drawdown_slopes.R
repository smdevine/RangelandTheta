library(extrafont)
library(extrafontdb)
loadfonts()
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
vwc_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'VWC', paste0('MeanVWC_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
vwc_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'VWC', paste0('MeanVWC_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_7cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_7cm_2018 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MeanT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_7cm_2018_max <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2018', 'Temperature', paste0('MaxT_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
soilT_22cm_2017 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', '2017', 'Temperature', paste0('MeanT_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
forage_terrain_energy <- read.csv(file.path(results, 'tables', 'forage_terrain_energy_3m_final.csv'), stringsAsFactors = FALSE)
#add energy colors
forage_terrain_energy$energy_colors <- ifelse(forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[2], 'blue', ifelse(forage_terrain_energy$annual_kwh.m2 > summary(forage_terrain_energy$annual_kwh.m2)[2] & forage_terrain_energy$annual_kwh.m2 < summary(forage_terrain_energy$annual_kwh.m2)[5], 'orange2', 'red3'))
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
  fix_indices <- which(is.na(df[location_fix, 2:ncol(df)]))
  df[location_fix, fix_indices] <- lm.result$coefficients[1] + lm.result$coefficients[2] * df[location_use, fix_indices] + lm.result$coefficients[3] * df[location_use2, fix_indices]
  df
}
soilT_7cm_2017 <- gap_fill_soilTv3(soilT_7cm_2017, 13, 4, 14, 0.99)

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
plot(avgsoilT_2017[-13], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13])
abline(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13]), lty=2)
summary(lm(forage_terrain_energy$clp031417 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13])) #61% variance explained; 370 kg/ha per deg C warming; p.val < 0.001
plot(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13]))
plot(lm(forage_terrain_energy$clp031417 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp031417 ~ avgsoilT_2017))
summary(lm(forage_terrain_energy$clp031417[-13] ~ forage_terrain_energy$slope[-13])) #23% variance explained; p-val=0.07
summary(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13] + forage_terrain_energy$slope[-13])) #64% variance explained but slope parameter NS and positive (p=0.37)
#finalize as figure
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', 'Mar.forage.2017_vs_soilT.png'), family = 'Book Antiqua', width = 800, height = 650, units = 'px', res=100)
par(mar=c(4.8, 4.8, 1, 1))
plot(avgsoilT_2017[-13], forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13], xlab = expression(paste('Dec 1, 2016 - Mar 15, 2017, mean soil temperature, 7 cm depth ('*degree*'C)')), ylab = expression(paste('Mar 14, 2017 standing forage (kg', ' ', ha^-1, ')')), pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.3)
abline(lm(forage_terrain_energy$clp031417[-13] ~ avgsoilT_2017[-13]), lty=2)
legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.3)
text(x=7.9, y=2100, label='linear model results', cex=1.3, adj=c(0,0))
text(x=7.9, y=1950, label=expression(paste(r^2, ' = 0.61, p.val < 0.001')), cex = 1.3, adj = c(0, 0))
text(x=7.9, y=1800, label=expression(paste('RMSE = 326 kg ', ha^-1)), cex = 1.3, adj = c(0, 0))
#text(x=7.9, y=2000, label=expression(paste('wet year mid-March standing forage (kg ', ha^-1, ') =' ~degree*'C * 360 - 1911')), adj = c(0, 0), cex = 1.2)
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
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', 'Apr.forage.2017_vs_soilT.png'), family = 'Book Antiqua', width = 800, height = 650, units = 'px', res=100)
par(mar=c(4.8, 4.8, 1, 1))
plot(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[], xlab = expression(paste('Dec 1, 2016 - Apr 10, 2017, mean soil temperature, 7 cm depth ('*degree*'C)')), ylab = expression(paste('Apr 10, 2017 standing forage (kg', ' ', ha^-1, ')')), pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.3)
abline(lm(forage_terrain_energy$clp041017[] ~ avgsoilT_2017[]), lty=2)
#legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=9.3, y=4400, label='19 days later', cex=1.3, adj=c(0,0))
text(x=9.3, y=4200, label='linear relationship non-significant (p = 0.70)', cex = 1.3, adj=c(0,0))
#text(x=7.9, y=2200, label=expression(paste(r^2, ' = 0.61')), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2100, label=expression(paste('RMSE = 326 kg ', ha^-1)), cex = 1.1, adj = c(0, 0))
#text(x=7.9, y=2000, label=expression(paste('wet year mid-March standing forage (kg ', ha^-1, ') =' ~degree*'C * 360 - 1911')), adj = c(0, 0), cex = 1.1)
#text(avgsoilT_2017[], forage_terrain_energy$clp041017[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
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
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', 'Mar.forage.2018_vs_soilT.png'), family = 'Book Antiqua', width = 800, height = 650, units = 'px', res=100)
par(mar=c(4.8, 4.8, 1, 1))
plot(avgsoilT_2018[], forage_terrain_energy$clp032218[], col=forage_terrain_energy$energy_colors[], xlab = expression(paste('Dec 1, 2017 - Mar 22, 2018, mean soil temperature, 7 cm depth ('*degree*'C)')), ylab = expression(paste('Mar 22, 2018 standing forage (kg', ' ', ha^-1, ')')), pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.3)
abline(lm(forage_terrain_energy$clp032218[] ~ avgsoilT_2018[]), lty=2)
curve(nlm_2018(x, 'clp032218'), from=min(avgsoilT_2018), to=max(avgsoilT_2018), lty=2, add=TRUE)
#legend(x=7.9, y=340, legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=8.2, y=250, label='linear model results', cex = 1.3, adj = c(0, 0))
text(x=8.2, y=175, label=expression(paste(r^2, ' = 0.25, p.val = 0.048')), cex = 1.3, adj = c(0, 0))
text(x=8.2, y=100, label=expression(paste('RMSE = 226 kg ', ha^-1)), cex = 1.3, adj = c(0, 0))
text(x=11, y=250, label='non-linear model results', cex = 1.3, adj = c(0, 0))
text(x=11, y=175, label=expression(paste(r^2, ' = 0.46, p.val = 0.018')), cex = 1.3, adj = c(0, 0))
text(x=11, y=100, label=expression(paste('RMSE = 198 kg ', ha^-1)), cex = 1.3, adj = c(0, 0))
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
png(file = file.path(results, 'figures', 'finals', 'SM_T_effects_visualizations', 'Apr.forage.2018_vs_soilT.png'), family = 'Book Antiqua', width = 800, height = 650, units = 'px', res=100)
par(mar=c(4.8, 4.8, 1, 1))
plot(avgsoilT_2018[], forage_terrain_energy$clp041518[], col=forage_terrain_energy$energy_colors[], xlab = expression(paste('Dec 1, 2017 - Apr 15, 2018, mean soil temperature, 7 cm depth ('*degree*'C)')), ylab = expression(paste('Apr 15, 2018 standing forage (kg', ' ', ha^-1, ')')), pch=19, cex=1.2, cex.axis=1.3, cex.lab=1.3)
#abline(lm(forage_terrain_energy$clp041518[] ~ avgsoilT_2018[]), lty=2)
curve(nlm_2018(x, 'clp041518'), from=min(avgsoilT_2018), to=max(avgsoilT_2018), lty=2, add = TRUE)
#legend('topleft', legend=(c("< 1254", '1254-1458', '>1458')), pch = 19, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01, cex = 1.1)
text(x=9.3, y=1500, label='24 days later', cex=1.3, adj = c(0, 0))
text(x=9.3, y=1425, label='non-linear model results', cex = 1.3, adj = c(0, 0))
text(x=9.3, y=1350, label=expression(paste(r^2, ' = 0.57, p.val = 0.004')), cex = 1.3, adj = c(0, 0))
text(x=9.3, y=1275, label=expression(paste('RMSE = 245 kg ', ha^-1)), cex = 1.3, adj = c(0, 0))
#text(x=7.9, y=130, label=expression(paste('dry year mid-March standing forage (kg ', ha^-1, ') = deg C * -57 + 1181')), adj = c(0, 0), cex = 1.2)
#text(avgsoilT_2018[], forage_terrain_energy$clp041518[], col=forage_terrain_energy$energy_colors[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
dev.off()

#max soil T
lapply(soilT_7cm_2018_max[,]


rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Mar2017growth', mtd = 'spearman')
rank_test(Mar2017_avgsoilT_week2, forage_terrain_energy[-13, ], 'Mar2017growth', mtd = 'spearman')
rank_test(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Apr2017growth', mtd = 'spearman')

rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Apr2017growth', mtd = 'spearman')
rank_test(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy[-13, ], 'Mar2017growth', mtd = 'spearman')

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
plot(forage_terrain_energy$annual_kwh.m2[], Mar2017_avgsoilT_week1)
abline(lm(Mar2017_avgsoilT_week1 ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_avgsoilT_week1 ~ forage_terrain_energy$annual_kwh.m2[]))

#drydown slopes vs solrad
plot(forage_terrain_energy$annual_kwh.m2, Mar2017_week1_drydown$drydown.slopes)
abline(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[]))

#drydown slopes vs. soil temp Mar week 1
plot(Mar2017_avgsoilT_week1, Mar2017_week1_drydown$drydown.slopes)
abline(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1))
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1)) #38% of variance in drydown slopes explained by soil temperature (p=0.01)
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1 + forage_terrain_energy$clp021517)) #48% of variance explained (p=0.015) by soil temperature (p < 0.01) + Feb standing biomass (p=0.14), whereby higher standing biomass is associated with weaker drawdown
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ Mar2017_avgsoilT_week1 + forage_terrain_energy$clp031417)) #45% of variance explained (p=0.02) by soil temperature (p < 0.01) + Mar standing biomass (p=0.22), whereby higher standing biomass is associated with weaker drawdown
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$clp021517)) #<1% explained by Feb standing biomass alone
summary(lm(Mar2017_week1_drydown$drydown.slopes ~ forage_terrain_energy$clp031417)) #3% by Mar standing biomass alone

#Mar forage vs soil temp week 1
plot(Mar2017_avgsoilT_week1, forage_terrain_energy$clp031417[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp031417[] ~ Mar2017_avgsoilT_week1))
summary(lm(forage_terrain_energy$clp031417[] ~ Mar2017_avgsoilT_week1)) #38% of variance in Mar forage explained by Mar avg soil T (p=0.012)

#Mar forage vs. drydown slopes Mar week 1 + soil T week 1
plot(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$clp031417[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$clp031417[] ~ Mar2017_week1_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$clp031417[] ~ Mar2017_week1_drydown$drydown.slopes)) #slope positive but p=0.54
summary(lm(forage_terrain_energy$clp031417[] ~ Mar2017_avgsoilT_week1 + Mar2017_week1_drydown$drydown.slopes)) #45% of variance explained, mostly by soil temperature (p<0.01) with negative slope on drawdown (p=0.22)

#Mar forage gain vs. soil temp Mar week 1
plot(Mar2017_avgsoilT_week1, forage_terrain_energy$Mar2017growth[])
abline(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_avgsoilT_week1))
summary(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_avgsoilT_week1)) #30% of Mar forage gain explained by warmer soil temperatures (p=0.03)

#Mar forage gain vs. drydown slopes Mar week 1 + soil T
plot(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$Mar2017growth[], col=forage_terrain_energy$energy_colors[])
abline(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_week1_drydown$drydown.slopes))
text(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$Mar2017growth[], pos = 1, offset = 0.5, labels = forage_terrain_energy$location[])
test <- lm(forage_terrain_energy$Mar2017growth[c(-8,-13)] ~ Mar2017_week1_drydown$drydown.slopes[c(-8, -13)])
summary(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_week1_drydown$drydown.slopes)) #5% variance in March growth explained with all points and slope positive (p=0.41); 15% variance explained with 13 removed (p=0.16)
summary(lm(forage_terrain_energy$Mar2017growth[] ~ Mar2017_avgsoilT_week1 + Mar2017_week1_drydown$drydown.slopes)) #32% variance explained (p=0.08); drydown slope negative (p=0.54); soil T positive (p=0.04)

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

#Apr forage gain vs. soil T Mar week 1 + drydown slopes Mar week 1
plot(Mar2017_week1_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[])
abline(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week1_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_week1_drydown$drydown.slopes)) #27% variance explained with negative slope (p=0.04) and not improved by terrain chars
summary(lm(forage_terrain_energy$Apr2017growth[] ~ Mar2017_avgsoilT_week1 + Mar2017_week1_drydown$drydown.slopes)) #30% variance explained; no coeff is sig
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

#Mar week 2 drydown
Mar_week2 <- which(colnames(vwc_7cm_2017)=='Mar_07_2017'):which(colnames(vwc_7cm_2017)=='Mar_13_2017')
for (i in 1:nrow(vwc_7cm_2017)) {
  if (i==1) {
    plot(as.Date(colnames(vwc_7cm_2017)[Mar_week2], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week2], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4), type='l')
  } else {lines(as.Date(colnames(vwc_7cm_2017)[Mar_week2], '%b_%d_%Y'), vwc_7cm_2017[i, Mar_week2], col=forage_terrain_energy$energy_colors[i], ylim=c(0.18, 0.4))}
}

Mar2017_week2_drydown <- as.data.frame(t(apply(vwc_7cm_2017[-13,Mar_week2], 1, function(x) {
  lm.summary <- summary(lm(x ~ seq_along(x)))
  c(lm.summary$r.squared, lm.summary$coefficients[2, 1], lm.summary$coefficients[2, 4])
})))
colnames(Mar2017_week2_drydown) <- c('r.squared', 'drydown.slopes', 'slopes.p.val')
Mar2017_week2_drydown$drydown.slopes <- -Mar2017_week2_drydown$drydown.slopes
Mar2017_week2_drydown
Mar2017_avgsoilT_week2 <- apply(soilT_7cm_2017[-13 ,Mar_week2], 1, mean)
mean(Mar2017_week2_drydown$drydown.slopes)
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$clp041017[-13])
plot(Mar2017_avgsoilT_week2, Mar2017_week2_drydown$drydown.slopes)
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_avgsoilT_week2)
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_week2_drydown$drydown.slopes)
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ Mar2017_avgsoilT_week2))
summary(lm(Mar2017_week2_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_avgsoilT_week2 + Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_week2_drydown$drydown.slopes)) #NS
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week2_drydown$drydown.slopes))
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$clp031417[-13], col=forage_terrain_energy$energy_colors[-13])
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
plot(Mar2017_week2_drydown$drydown.slopes, forage_terrain_energy$Mar2017growth[-13])
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_week2_drydown$drydown.slopes))
summary(lm(forage_terrain_energy$Mar2017growth[-13] ~ Mar2017_week2_drydown$drydown.slopes))
#plot(lm.result)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week2))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week2_drydown$drydown.slopes))


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
mean(Mar2017_week3_drydown$drydown.slopes)
plot(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy$clp041017[-13])
plot(Mar2017_avgsoilT_week3, Mar2017_week3_drydown$drydown.slopes)
plot(forage_terrain_energy$annual_kwh.m2[-13], Mar2017_avgsoilT_week3)
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ Mar2017_avgsoilT_week3))
summary(lm(Mar2017_week3_drydown$drydown.slopes ~ forage_terrain_energy$seasonal_kwh.m2[-13]))
mult.lm.result <- lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_avgsoilT_week3 + Mar2017_week3_drydown$drydown.slopes)
summary(mult.lm.result)
#plot(mult.lm.result)
lm.result <- lm(forage_terrain_energy$clp041017[-13] ~ Mar2017_week3_drydown$drydown.slopes)
summary(lm.result)
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
summary(lm(forage_terrain_energy$clp031417[-13] ~ Mar2017_week3_drydown$drydown.slopes)) #NS
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Mar2017_week3_drydown$drydown.slopes))
plot(Mar2017_week3_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
#plot(lm.result)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_avgsoilT_week3))
summary(lm(forage_terrain_energy$peak2017[-13] ~ Mar2017_week3_drydown$drydown.slopes))

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
plot(Apr2017_drydown$drydown.slopes, forage_terrain_energy$clp041017[-13])
plot(Apr2017_avgsoilT, Apr2017_drydown$drydown.slopes)
plot(forage_terrain_energy$annual_kwh.m2[-13], Apr2017_avgsoilT)
summary(lm(Apr2017_drydown$drydown.slopes ~ Apr2017_avgsoilT))
summary(lm(Apr2017_drydown$drydown.slopes ~ forage_terrain_energy$annual_kwh.m2[-13]))
mult.lm.result <- lm(forage_terrain_energy$clp041017[-13] ~ Apr2017_avgsoilT + Apr2017_drydown$drydown.slopes)
summary(mult.lm.result)
#plot(mult.lm.result)
lm.result <- lm(forage_terrain_energy$clp041017[-13] ~ Apr2017_drydown$drydown.slopes)
summary(lm.result)
forage_terrain_energy$Apr2017growth <- forage_terrain_energy$clp041017 - forage_terrain_energy$clp031417
summary(lm(forage_terrain_energy$Apr2017growth[-13] ~ Apr2017_drydown$drydown.slopes))
plot(Apr2017_drydown$drydown.slopes, forage_terrain_energy$Apr2017growth[-13])
#plot(lm.result)
summary(lm(forage_terrain_energy$peak2017[-13] ~ Apr2017_avgsoilT))


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
