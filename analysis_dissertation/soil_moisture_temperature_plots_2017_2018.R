#plotting 2017-2018 daily data by location x depth location from 2017 and 2018 summaries after patching soil temperatures when soil moisture had been patched (see soil_moisture_processing_2018.R)
#uses same object name (vwc_data_all) to plot either soil temperature or moisture



data_name <- 'Temperature'
varname <- 'T'
depth <- '22'
yr <- '2017_2018'
vwc_data_all <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, data_name, paste0('Median', varname, '_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
NA_start <- which(colnames(vwc_data_all)=='Jul_30_2017')
NA_end <- which(colnames(vwc_data_all) == 'Nov_20_2017')
endcol <- ncol(vwc_data_all)
dates_all <- seq.Date(as.Date(colnames(vwc_data_all)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_all)[ncol(vwc_data_all)], '%b_%d_%Y'), by='day')
dates_all <- dates_all[-(which(dates_all=='2017-07-30'):which(dates_all=='2017-11-20'))]
weeks_all <- seq.Date(as.Date(colnames(vwc_data_all)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_all)[ncol(vwc_data_all)], '%b_%d_%Y'), by='week')
weeks_all <- weeks_all[-(which(weeks_all=='2017-07-15'):which(weeks_all=='2017-11-25'))]
vwc_data_all_temp <- vwc_data_all[ ,-(NA_start:NA_end)]
for (i in 1:nrow(vwc_data_all_temp)) {
  plot(as.factor(dates_all), vwc_data_all_temp[i, 2:ncol(vwc_data_all_temp)], type='b', xlab='Date', ylab='Daily Median VWC at 7 cm (mean of 2 sensors)', xaxt='n', main=paste('Location', vwc_data_all_temp$location[i])) #, ',', vwc_data_all_temp$aspect_cardinal[i], 'aspect'))
  axis(side = 1, at=as.factor(dates_all))
  #text(x=dates2018[75], y=0.18, labels=paste(round(vwc_data_all_temp$mean_curv[i], 2), 'mean curvature', ',', round(vwc_data_all_temp$cti[i], 2), 'Compound Topographic Index'))
}

#plot combined soil moisture
a <- match(as.Date(precip_data$Date, '%m/%d/%Y'), dates_all)
precip_data_merge <- precip_data[-which(is.na(a)), ]
precip_days_dummy <- match(as.Date(precip_data_merge$Date, '%m/%d/%Y'), dates_all)

#plotting for soil moisture
png(file = file.path(results, 'figures', paste0('WY2017_2018soilmoisture.', depth, 'cm.v2.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4))
for (i in 1:nrow(vwc_data_all_temp)) {
  if (i == 1) {
    plot(x=as.factor(dates_all), y=vwc_data_all_temp[i, 2:ncol(vwc_data_all_temp)], type='l', xaxt='n', col=i+1, ylim=c(0.05, 0.4), xlim=c(15, length(dates_all) - 15), xlab = "", ylab='5TM volumetric soil moisture', main = paste('WY2017-2018 soil moisture,', depth, 'cm depth, Camatta catchment'))
    #text(x=dates_all[length(dates_all)-50], y=vwc_data_all_temp[i, ncol(vwc_data_all_temp)-50], labels=vwc_data_all_temp[i, 1], cex=0.7)
  } else {
    lines.default(as.factor(dates_all), vwc_data_all_temp[i, 2:ncol(vwc_data_all_temp)], xaxt='n', col=i+1)
    #text(x=dates_all[length(dates_all)- 50], y=vwc_data_all_temp[i, (ncol(vwc_data_all_temp)- 50)], labels=vwc_data_all_temp[i, 1], cex=0.7)
  }
}
date_indices <- c(which(dates_all=='2016-12-01'), which(dates_all=='2017-01-01'), which(dates_all=='2017-02-01'), which(dates_all=='2017-03-01'), which(dates_all=='2017-04-01'), which(dates_all=='2017-05-01'), which(dates_all=='2017-06-01'), which(dates_all=='2017-07-01'), which(dates_all=='2017-08-01'), which(dates_all=='2017-09-01'), which(dates_all=='2017-10-01'), which(dates_all=='2017-11-01'), which(dates_all=='2017-12-01'), which(dates_all=='2018-01-01'), which(dates_all=='2018-02-01'), which(dates_all=='2018-03-01'), which(dates_all=='2018-04-01'), which(dates_all=='2018-05-01'), which(dates_all=='2018-06-01'), which(dates_all=='2018-07-01'))
axis(side = 1, at=date_indices, labels = format(dates_all[date_indices], '%m/%d/%y'))
#add precip
axis(side = 4, at = c(0.1, 0.2, 0.3, 0.4), labels = c('10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(precip_days_dummy, precip_data_merge$Rainfall..mm. / 100, type='s', col='blue')
dev.off()

#plot for soil temperature
png(file = file.path(results, 'figures', paste0('WY2017_2018soiltemperature.', depth, 'cm.v2.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4))
for (i in 1:nrow(vwc_data_all_temp)) {
  if (i == 1) {
    plot(x=as.factor(dates_all), y=vwc_data_all_temp[i, 2:ncol(vwc_data_all_temp)], type='l', xaxt='n', col=i+1, ylim=c(0, 40), xlim=c(15, length(dates_all) - 15), xlab = "", ylab='deg C', main = paste('WY2017-2018 soil temperature,', depth, 'cm depth, Camatta catchment'))
    #text(x=dates_all[length(dates_all)-50], y=vwc_data_all_temp[i, ncol(vwc_data_all_temp)-50], labels=vwc_data_all_temp[i, 1], cex=0.7)
  } else {
    lines.default(as.factor(dates_all), vwc_data_all_temp[i, 2:ncol(vwc_data_all_temp)], xaxt='n', col=i+1)
    #text(x=dates_all[length(dates_all)- 50], y=vwc_data_all_temp[i, (ncol(vwc_data_all_temp)- 50)], labels=vwc_data_all_temp[i, 1], cex=0.7)
  }
}
date_indices <- c(which(dates_all=='2016-12-01'), which(dates_all=='2017-01-01'), which(dates_all=='2017-02-01'), which(dates_all=='2017-03-01'), which(dates_all=='2017-04-01'), which(dates_all=='2017-05-01'), which(dates_all=='2017-06-01'), which(dates_all=='2017-07-01'), which(dates_all=='2017-08-01'), which(dates_all=='2017-09-01'), which(dates_all=='2017-10-01'), which(dates_all=='2017-11-01'), which(dates_all=='2017-12-01'), which(dates_all=='2018-01-01'), which(dates_all=='2018-02-01'), which(dates_all=='2018-03-01'), which(dates_all=='2018-04-01'), which(dates_all=='2018-05-01'), which(dates_all=='2018-06-01'), which(dates_all=='2018-07-01'))
axis(side = 1, at=date_indices, labels = format(dates_all[date_indices], '%m/%d/%y'))
dev.off()