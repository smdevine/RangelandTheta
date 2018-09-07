library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SM_T_model_results'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
list.files(model_results)

#get dry periods for 2017 and 2018
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
sum(precip_data$Rainfall..mm.[which(precip_data$Date=='1/10/2018'):which(precip_data$Date=='2/25/2018')]) #only 10.2 mm precip between these dates over
sum(precip_data$Rainfall..mm.[which(precip_data$Date=='1/10/2018'):which(precip_data$Date=='2/25/2018')] > 0) #over 4 days



#make combined plots of significant 2017 associations
depth <- 22
model_results_2017 <- lapply(list.files(model_resultsDir, pattern = glob2rx(paste0('*2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
names(model_results_2017) <- list.files(model_resultsDir, pattern = glob2rx(paste0('*2017*', depth, 'cm*')))
names(model_results_2017)
names(model_results_2017) <- c('Apr', 'Feb', 'Mar', 'May')
model_results_2017 <- lapply(model_results_2017, function(x) x <- x[-which(x$p.value.model > 0.05), ])
lapply(model_results_2017, nrow)
lapply(model_results_2017, print)
clipdates2017 <- as.Date(c('2017-04-10', '2017-02-15', '2017-03-14', '2017-05-01'))
for (i in seq_along(model_results_2017)) {
  model_results_2017[[i]] <- model_results_2017[[i]][as.Date(model_results_2017[[i]]$dates) <= clipdates2017[i], ]
}
lapply(model_results_2017, function(x) range(x$slope.SM))

#2017 SM plot
png(file = file.path(resultsFigures, 'combined.dates', paste0('WY2017.SM.', depth, 'cm.forage_SM+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2017$Feb$dates[model_results_2017$Feb$p.value.SM < 0.05]), model_results_2017$Feb$slope.SM[model_results_2017$Feb$p.value.SM < 0.05], xaxt='n', xlab='', ylab = paste('kg/ha association of +1 std dev soil moisture'), ylim=c(-7000, 1000), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm depth soil moisture and forage growth relationship, 2017'))
points(as.Date(model_results_2017$Mar$dates[model_results_2017$Mar$p.value.SM < 0.05]), model_results_2017$Mar$slope.SM[model_results_2017$Mar$p.value.SM < 0.05], pch = 19, col ='grey')
points(as.Date(model_results_2017$Apr$dates[model_results_2017$Apr$p.value.SM < 0.05]), model_results_2017$Apr$slope.SM[model_results_2017$Apr$p.value.SM < 0.05], pch = 19)
points(as.Date(model_results_2017$May$dates[model_results_2017$May$p.value.SM < 0.05]), model_results_2017$May$slope.SM[model_results_2017$May$p.value.SM < 0.05], pch = 8, xlab='')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("2/15/17", "3/14/17", "4/10/17", '5/1/17')), pch=c(1, 19, 19, 8), col=c('black', 'grey', 'black', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2017-02-21', '2017-03-20')))
dev.off()

#2017 Temperature plot
lapply(model_results_2017, function(x) range(x$slope.T))
png(file = file.path(resultsFigures, 'combined.dates', paste0('WY2017.T.', depth, 'cm.forage_SM+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2017$Feb$dates[model_results_2017$Feb$p.value.T < 0.05]), model_results_2017$Feb$slope.T[model_results_2017$Feb$p.value.T < 0.05], xaxt='n', xlab='', ylab = 'kg/ha association of +1 deg C soil temperature', ylim=c(-50, 1300), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm depth soil temperature and forage growth relationship, 2017'))
points(as.Date(model_results_2017$Mar$dates[model_results_2017$Mar$p.value.T < 0.05]), model_results_2017$Mar$slope.T[model_results_2017$Mar$p.value.T < 0.05], pch = 19, col='grey')
points(as.Date(model_results_2017$Apr$dates[model_results_2017$Apr$p.value.T < 0.05]), model_results_2017$Apr$slope.T[model_results_2017$Apr$p.value.T < 0.05], pch = 19)
points(as.Date(model_results_2017$May$dates[model_results_2017$May$p.value.T < 0.05]), model_results_2017$May$slope.T[model_results_2017$May$p.value.T < 0.05], pch = 8, xlab='')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
#legend("topright", legend=(c("2/15/17", "3/14/17", "4/10/17", '5/1/17')), pch=c(1, 19, 19, 8), col=c('black', 'grey', 'black', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2017-02-21', '2017-03-20')))
dev.off()

#make plots of 2018 significant associations
depth <- 7
model_results_2018 <- lapply(list.files(model_resultsDir, pattern = glob2rx(paste0('*2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
names(model_results_2018) <- list.files(model_resultsDir, pattern = glob2rx(paste0('*2018*', depth, 'cm*')))
names(model_results_2018)
names(model_results_2018) <- c('Apr', 'Feb', 'Mar')
model_results_2018 <- lapply(model_results_2018, function(x) x <- x[-which(x$p.value.model > 0.05), ])
lapply(model_results_2018, nrow)
lapply(model_results_2018, print)
clipdates2018 <- as.Date(c('2018-04-15', '2018-02-15', '2018-03-22'))
for (i in seq_along(model_results_2018)) {
  model_results_2018[[i]] <- model_results_2018[[i]][as.Date(model_results_2018[[i]]$dates) <= clipdates2018[i], ]
}
lapply(model_results_2018, nrow)
lapply(model_results_2018, function(x) range(x$slope.SM))

#2018 7 cm SM plot
png(file = file.path(resultsFigures, 'combined.dates', paste0('WY2018.SM.', depth, 'cm.forage_SM+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.SM < 0.05]), model_results_2018$Feb$slope.SM[model_results_2018$Feb$p.value.SM < 0.05], xaxt='n', xlab='', ylab = paste('kg/ha association of +1 std dev soil moisture'), ylim=c(-3300, 2300), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth soil moisture and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.SM < 0.05]), model_results_2018$Mar$slope.SM[model_results_2018$Mar$p.value.SM < 0.05], pch = 19, col ='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.SM < 0.05]), model_results_2018$Apr$slope.SM[model_results_2018$Apr$p.value.SM < 0.05], pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
#legend("bottomright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/10', '2018/02/25')))
dev.off()

#2018 Temp plot
lapply(model_results_2018, function(x) range(x$slope.T))
png(file = file.path(resultsFigures, 'combined.dates', paste0('WY2018.T.', depth, 'cm.forage_SM+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.T < 0.05]), model_results_2018$Feb$slope.T[model_results_2018$Feb$p.value.T < 0.05], xaxt='n', xlab='', ylab = 'kg/ha association of +1 deg C soil temperature', ylim=c(-150, 150), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth soil temperature and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.T < 0.05]), model_results_2018$Mar$slope.T[model_results_2018$Mar$p.value.T < 0.05], pch = 19, col='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.T < 0.05]), model_results_2018$Apr$slope.T[model_results_2018$Apr$p.value.T < 0.05], pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("bottomright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/10', '2018/02/25')))
dev.off()

