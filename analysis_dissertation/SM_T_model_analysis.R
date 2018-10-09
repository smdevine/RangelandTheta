#go to line 159
library(car)
library(extrafont)
library(extrafontdb)
loadfonts()
model_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SMnorm_T_model_results'
model.abs_resultsDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/SMabs_T_model_results'
model_resultsDir_old <- 'C:/Users/smdevine/Desktop/rangeland project/results/SM_T_model_results'
resultsFigures <- 'C:/Users/smdevine/Desktop/rangeland project/results/figures'
forageDir <- 'C:/Users/smdevine/Desktop/rangeland project/clip_plots'
terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/terrain_analysis_r'
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
min_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {min(x, na.rm = TRUE)}
}
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}

depth <- 7
yr <- 2017
# clpname <- 'clp031417'
#daily mean normalized soil moisture + temperature vs. biomass model
SMnorm_T_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  vwc_data_normalized <- vwc_data
  vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE) #normalize vwc data
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  #model biomass as function of temperature and soil moisture
  SM_T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, p.value.SM.vs.T=NA, r2.SM.vs.T=NA, vif=NA)
  for (i in 2:ncol(vwc_data_normalized)) {
    lm.result <- lm(forage_data[[clpname]] ~ vwc_data_normalized[,i] + soilT_data[,i])
    lm.summary <- summary(lm.result)
    lm.SM.vs.T <- lm(vwc_data_normalized[,i] ~ soilT_data[,i])
    SM_T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_T_vs_biomass_analysis[i-1, 'slope.SM'] <- lm.summary$coefficients[2, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[3, 1]
    SM_T_vs_biomass_analysis[i-1, 'p.value.SM'] <- lm.summary$coefficients[2, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T'] <- lm.summary$coefficients[3, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.SM.vs.T'] <- summary(lm.SM.vs.T)$coefficients[2, 4]
    SM_T_vs_biomass_analysis[i-1, 'r2.SM.vs.T'] <- summary(lm.SM.vs.T)$r.squared
    SM_T_vs_biomass_analysis[i-1, 'vif'] <- vif(lm.result)[1]
  }
  write.csv(SM_T_vs_biomass_analysis, file.path(results, 'SMnorm_T_model_results', paste0('SMnorm_T_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
# colnames(forage_data) # "clp021517" "clp031417" "clp041017" "clp050117" "clp011618" "clp021518" "clp032218" "clp041518"
SMnorm_T_model(7, 2017, 'clp021517', 'Feb')
SMnorm_T_model(7, 2017, 'clp031417', 'Mar')
SMnorm_T_model(7, 2017, 'clp041017', 'Apr')
SMnorm_T_model(7, 2017, 'clp050117', 'May')
SMnorm_T_model(7, 2017, 'peak2017', 'peak2017')
SMnorm_T_model(22, 2017, 'clp021517', 'Feb')
SMnorm_T_model(22, 2017, 'clp031417', 'Mar')
SMnorm_T_model(22, 2017, 'clp041017', 'Apr')
SMnorm_T_model(22, 2017, 'clp050117', 'May')
SMnorm_T_model(22, 2017, 'peak2017', 'peak2017')
SMnorm_T_model(7, 2018, 'clp021518', 'Feb')
SMnorm_T_model(7, 2018, 'clp032218', 'Mar')
SMnorm_T_model(7, 2018, 'clp041518', 'Apr')
SMnorm_T_model(7, 2018, 'peak2018', 'peak2018')
SMnorm_T_model(22, 2018, 'clp021518', 'Feb')
SMnorm_T_model(22, 2018, 'clp032218', 'Mar')
SMnorm_T_model(22, 2018, 'clp041518', 'Apr')
SMnorm_T_model(22, 2018, 'peak2018', 'peak2018')
# depth <- 7
# yr <- 2017
# clpname <- 'clp031417'
# month <- 'Mar'
T_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(soilT_data)[2], '%b_%d_%Y'), as.Date(colnames(soilT_data)[ncol(soilT_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(soilT_data)[2], '%b_%d_%Y'), as.Date(colnames(soilT_data)[ncol(soilT_data)], '%b_%d_%Y'), by='week')
  #model biomass as function of temperature and soil moisture
  T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.T=NA, intercept=NA)
  for (i in 2:ncol(soilT_data)) {
    lm.result <- lm(forage_data[[clpname]] ~ soilT_data[,i])
    lm.summary <- summary(lm.result)
    T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[2, 1]
    T_vs_biomass_analysis[i-1, 'intercept'] <- lm.summary$coefficients[1, 1]
  }
  write.csv(T_vs_biomass_analysis, file.path(results, 'T_model_results', paste0('T_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
T_model(7, 2017, 'clp021517', 'Feb')
T_model(7, 2017, 'clp031417', 'Mar')
T_model(7, 2017, 'clp041017', 'Apr')
T_model(7, 2017, 'clp050117', 'May')
T_model(7, 2017, 'peak2017', 'peak2017')
T_model(22, 2017, 'clp021517', 'Feb')
T_model(22, 2017, 'clp031417', 'Mar')
T_model(22, 2017, 'clp041017', 'Apr')
T_model(22, 2017, 'clp050117', 'May')
T_model(22, 2017, 'peak2017', 'peak2017')
T_model(7, 2018, 'clp021518', 'Feb')
T_model(7, 2018, 'clp032218', 'Mar')
T_model(7, 2018, 'clp041518', 'Apr')
T_model(7, 2018, 'peak2018', 'peak2018')
T_model(22, 2018, 'clp021518', 'Feb')
T_model(22, 2018, 'clp032218', 'Mar')
T_model(22, 2018, 'clp041518', 'Apr')
T_model(22, 2018, 'peak2018', 'peak2018')
#stack r2 from SM + T and T vs biomass models for different clipping dates, cutting off model at clipping date

#get dry periods for 2017 and 2018
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
sum(precip_data$Rainfall..mm.[which(precip_data$Date=='1/10/2018'):which(precip_data$Date=='2/25/2018')]) #only 10.2 mm precip between these dates over
sum(precip_data$Rainfall..mm.[which(precip_data$Date=='1/10/2018'):which(precip_data$Date=='2/25/2018')] > 0) #over 4 days
sum(precip_data$Rainfall..mm.[which(precip_data$Date=='2/21/2017'):which(precip_data$Date=='3/20/2017')]) #only 2.8 mm precip over 30 days
sum(precip_data$Rainfall..mm.[which(precip_data$Date=='3/21/2017'):which(precip_data$Date=='4/17/2017')]) #only 13.5 mm precip

#make combined plots of r2 from SMnorm+T model and Tmodel for different dates in 2017
depth <- 22
yr <- 2017
SMnorm_Tmodel_results_2017 <- lapply(list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2017 <- as.Date(c('2017-04-10', '2017-02-15', '2017-03-14', '2017-05-01'))
SMnorm_Tmodel_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_Tmodel_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMnorm_Tmodel_results_2017) <- list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')))
names(SMnorm_Tmodel_results_2017)
names(SMnorm_Tmodel_results_2017) <- c('Apr', 'Feb', 'Mar', 'May')
Tmodel_results_2017 <- lapply(list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
Tmodel_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], Tmodel_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(Tmodel_results_2017) <- list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')))
names(Tmodel_results_2017)
names(Tmodel_results_2017) <- c('Apr', 'Feb', 'Mar', 'May')

#2017 plot
png(file = file.path(resultsFigures, 'finals', paste0('WY2017.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 1))
plot(as.Date(SMnorm_Tmodel_results_2017$Mar$dates), SMnorm_Tmodel_results_2017$Mar$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm model results in', yr, '(wet year)'), col='grey')
lines(as.Date(Tmodel_results_2017$Mar$dates), Tmodel_results_2017$Mar$r2.model, col ='grey', lty=2)
lines(as.Date(SMnorm_Tmodel_results_2017$Apr$dates), SMnorm_Tmodel_results_2017$Apr$r2.model, col ='black')
lines(as.Date(Tmodel_results_2017$Apr$dates), Tmodel_results_2017$Apr$r2.model, col ='black', lty=2)
lines(as.Date(SMnorm_Tmodel_results_2017$May$dates), SMnorm_Tmodel_results_2017$May$r2.model, col='red')
lines(as.Date(Tmodel_results_2017$May$dates), Tmodel_results_2017$May$r2.model, col='red', lty=2)
#lines(as.Date(SMnorm_Tmodel_results_2017$May$dates), SMnorm_Tmodel_results_2017$May$r2.SM.vs.T, col='lightblue')
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("SMnorm + T vs. March biomass model", 'T vs. March biomass model', "SMnorm + T vs. April biomass model", 'T vs. April biomass model', 'SMnorm + T vs. May biomass model', 'T vs. May biomass model')), lty=c(1, 2, 1, 2, 1, 2), col=c('grey', 'grey', 'black', 'black', 'red', 'red'), inset = 0.005, cex=0.9)
dev.off()

#make combined plots of r2 from SMnorm+T model and Tmodel for different dates in 2018
depth <- 22
yr <- 2018
SMnorm_Tmodel_results_2018 <- lapply(list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- as.Date(c('2018-04-15', '2018-02-15', '2018-03-22'))
SMnorm_Tmodel_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_Tmodel_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMnorm_Tmodel_results_2018) <- list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')))
names(SMnorm_Tmodel_results_2018)
names(SMnorm_Tmodel_results_2018) <- c('Apr', 'Feb', 'Mar')
Tmodel_results_2018 <- lapply(list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
Tmodel_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], Tmodel_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(Tmodel_results_2018) <- list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')))
names(Tmodel_results_2018)
names(Tmodel_results_2018) <- c('Apr', 'Feb', 'Mar')
#and plot
png(file = file.path(resultsFigures, 'finals', paste0('WY2018.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 1))
plot(as.Date(SMnorm_Tmodel_results_2018$Mar$dates), SMnorm_Tmodel_results_2018$Mar$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm model results in', yr, '(dry year)'), col='grey')
lines(as.Date(Tmodel_results_2018$Mar$dates), Tmodel_results_2018$Mar$r2.model, col ='grey', lty=2)
lines(as.Date(SMnorm_Tmodel_results_2018$Apr$dates), SMnorm_Tmodel_results_2018$Apr$r2.model, col ='black')
lines(as.Date(Tmodel_results_2018$Apr$dates), Tmodel_results_2018$Apr$r2.model, col ='black', lty=2)
#lines(as.Date(SMnorm_Tmodel_results_2018$Apr$dates), SMnorm_Tmodel_results_2018$Apr$r2.SM.vs.T, col='lightblue')
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("SMnorm + T vs. March biomass model", 'T vs. March biomass model', "SMnorm + T vs. April biomass model", 'T vs. April biomass model')), lty=c(1, 2, 1, 2), col=c('grey', 'grey', 'black', 'black'), inset = 0.005, cex=0.9)
dev.off()

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
#min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.SM))), na.rm = TRUE)

#2017 SM plot
png(file = file.path(resultsFigures, 'finals', paste0('WY2017.SM.', depth, 'cm.forage_SMnorm+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4.5))
plot(as.Date(model_results_2017$Feb$dates[model_results_2017$Feb$p.value.SM < 0.05]), model_results_2017$Feb$slope.SM[model_results_2017$Feb$p.value.SM < 0.05], xaxt='n', xlab='', ylab = paste('kg/ha association of +1 std dev soil moisture'), ylim=c(min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.SM))), na.rm=TRUE), max(unlist(lapply(model_results_2017, function(x) max_modified(x$slope.SM))), na.rm=TRUE)), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm depth soil moisture and forage growth relationship, 2017'))
points(as.Date(model_results_2017$Mar$dates[model_results_2017$Mar$p.value.SM < 0.05]), model_results_2017$Mar$slope.SM[model_results_2017$Mar$p.value.SM < 0.05], pch = 19, col ='grey')
points(as.Date(model_results_2017$Apr$dates[model_results_2017$Apr$p.value.SM < 0.05]), model_results_2017$Apr$slope.SM[model_results_2017$Apr$p.value.SM < 0.05], pch = 19)
points(as.Date(model_results_2017$May$dates[model_results_2017$May$p.value.SM < 0.05]), model_results_2017$May$slope.SM[model_results_2017$May$p.value.SM < 0.05], pch = 8, xlab='')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("right", legend=(c("2/15/17", "3/14/17", "4/10/17", '5/1/17')), pch=c(1, 19, 19, 8), col=c('black', 'grey', 'black', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2017-02-21', '2017-03-20'))) #only 2.8 mm precip over 30 days
axis(side = 4, at = c(-8000, -6000, -4000, -2000, 0), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 200 - 8000, type='s', col='lightblue', cex=0.5)
dev.off()

#2017 Temperature plot
png(file = file.path(resultsFigures, 'finals', paste0('WY2017.T.', depth, 'cm.forage_SMnorm+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4.5))
plot(as.Date(model_results_2017$Feb$dates[model_results_2017$Feb$p.value.T < 0.05]), model_results_2017$Feb$slope.T[model_results_2017$Feb$p.value.T < 0.05], xaxt='n', xlab='', ylab = 'kg/ha association of +1 deg C soil temperature', ylim=c(min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.T))), na.rm = TRUE), max(unlist(lapply(model_results_2017, function(x) max_modified(x$slope.T))), na.rm = TRUE)), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm depth soil temperature and forage growth relationship, 2017'))
points(as.Date(model_results_2017$Mar$dates[model_results_2017$Mar$p.value.T < 0.05]), model_results_2017$Mar$slope.T[model_results_2017$Mar$p.value.T < 0.05], pch = 19, col='grey')
points(as.Date(model_results_2017$Apr$dates[model_results_2017$Apr$p.value.T < 0.05]), model_results_2017$Apr$slope.T[model_results_2017$Apr$p.value.T < 0.05], pch = 19)
points(as.Date(model_results_2017$May$dates[model_results_2017$May$p.value.T < 0.05]), model_results_2017$May$slope.T[model_results_2017$May$p.value.T < 0.05], pch = 8, xlab='')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
#legend("topright", legend=(c("2/15/17", "3/14/17", "4/10/17", '5/1/17')), pch=c(1, 19, 19, 8), col=c('black', 'grey', 'black', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2017-02-21', '2017-03-20')))
dev.off()

#make plots of 2018 significant associations
depth <- 22
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
png(file = file.path(resultsFigures, 'finals', paste0('WY2018.SM.', depth, 'cm.forage_SMnorm+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4.5))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.SM < 0.05]), model_results_2018$Feb$slope.SM[model_results_2018$Feb$p.value.SM < 0.05], xaxt='n', xlab='', ylab = paste('kg/ha association of +1 std dev soil moisture'), ylim=c(min(unlist(lapply(model_results_2018, function(x) min_modified(x$slope.SM))), na.rm=TRUE), max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.SM))), na.rm=TRUE)), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth soil moisture and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.SM < 0.05]), model_results_2018$Mar$slope.SM[model_results_2018$Mar$p.value.SM < 0.05], pch = 19, col ='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.SM < 0.05]), model_results_2018$Apr$slope.SM[model_results_2018$Apr$p.value.SM < 0.05], pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
#legend("bottomright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/12', '2018/02/25')))
axis(side = 4, at = c(-3000, -2000, -1000, 0, 1000), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 100 - 3000, type='s', col='lightblue', cex=0.5)
dev.off()

#2018 Temp plot
lapply(model_results_2018, function(x) range(x$slope.T))
png(file = file.path(resultsFigures, 'finals', paste0('WY2018.T.', depth, 'cm.forage_SMnorm+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4.5))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.T < 0.05]), model_results_2018$Feb$slope.T[model_results_2018$Feb$p.value.T < 0.05], xaxt='n', xlab='', ylab = 'kg/ha association of +1 deg C soil temperature', ylim=c(min(unlist(lapply(model_results_2018, function(x) min_modified(x$slope.T))), na.rm = TRUE), if(max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.T))), na.rm = TRUE) < 0) {0} else {max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.T))), na.rm = TRUE)}), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth soil temperature and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.T < 0.05]), model_results_2018$Mar$slope.T[model_results_2018$Mar$p.value.T < 0.05], pch = 19, col='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.T < 0.05]), model_results_2018$Apr$slope.T[model_results_2018$Apr$p.value.T < 0.05], pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/12', '2018/02/25')))
dev.off()

#daily mean absolute soil moisture + temperature vs. biomass model
SMabs_T_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was jacked
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  #model biomass as function of temperature and soil moisture
  SM_T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, r2.SM.vs.T=NA)
  for (i in 2:ncol(vwc_data)) {
    lm.summary <- summary(lm(forage_data[[clpname]] ~ vwc_data[,i] + soilT_data[,i]))
    SM_T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_T_vs_biomass_analysis[i-1, 'slope.SM'] <- lm.summary$coefficients[2, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[3, 1]
    SM_T_vs_biomass_analysis[i-1, 'p.value.SM'] <- lm.summary$coefficients[2, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T'] <- lm.summary$coefficients[3, 4]
    SM_T_vs_biomass_analysis[i-1, 'r2.SM.vs.T'] <- summary(lm(vwc_data[,i] ~ soilT_data[,i]))$r.squared
  }
  write.csv(SM_T_vs_biomass_analysis, file.path(results, 'SMabs_T_model_results', paste0('SMabs_T_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
#colnames(forage_data) # "clp021517" "clp031417" "clp041017" "clp050117" "clp011618" "clp021518" "clp032218" "clp041518"
SMabs_T_model(7, 2017, 'clp021517', 'Feb')
SMabs_T_model(7, 2017, 'clp031417', 'Mar')
SMabs_T_model(7, 2017, 'clp041017', 'Apr')
SMabs_T_model(7, 2017, 'clp050117', 'May')
SMabs_T_model(7, 2017, 'peak2017', 'peak')
SMabs_T_model(22, 2017, 'clp021517', 'Feb')
SMabs_T_model(22, 2017, 'clp031417', 'Mar')
SMabs_T_model(22, 2017, 'clp041017', 'Apr')
SMabs_T_model(22, 2017, 'clp050117', 'May')
SMabs_T_model(22, 2017, 'peak2017', 'peak')
SMabs_T_model(7, 2018, 'clp021518', 'Feb')
SMabs_T_model(7, 2018, 'clp032218', 'Mar')
SMabs_T_model(7, 2018, 'clp041518', 'Apr')
SMabs_T_model(7, 2018, 'peak2018', 'peak')
SMabs_T_model(22, 2018, 'clp021518', 'Feb')
SMabs_T_model(22, 2018, 'clp032218', 'Mar')
SMabs_T_model(22, 2018, 'clp041518', 'Apr')
SMabs_T_model(22, 2018, 'peak2018', 'peak')

#make r2 stacked plots for absolute SM + T models
depth <- 22
yr <- 2017
SMabs_Tmodel_results_2017 <- lapply(list.files(file.path(results, 'SMabs_T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2017 <- as.Date(c('2017-04-10', '2017-02-15', '2017-03-14', '2017-05-01'))
SMabs_Tmodel_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_Tmodel_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMabs_Tmodel_results_2017) <- list.files(file.path(results, 'SMabs_T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')))
names(SMabs_Tmodel_results_2017)
names(SMabs_Tmodel_results_2017) <- c('Apr', 'Feb', 'Mar', 'May')
Tmodel_results_2017 <- lapply(list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
Tmodel_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], Tmodel_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(Tmodel_results_2017) <- list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2017*', depth, 'cm*')))
names(Tmodel_results_2017)
names(Tmodel_results_2017) <- c('Apr', 'Feb', 'Mar', 'May')

#2017 plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2017.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 1))
plot(as.Date(SMabs_Tmodel_results_2017$Mar$dates), SMabs_Tmodel_results_2017$Mar$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm model results in', yr, '(wet year)'), col='grey')
lines(as.Date(Tmodel_results_2017$Mar$dates), Tmodel_results_2017$Mar$r2.model, col ='grey', lty=2)
lines(as.Date(SMabs_Tmodel_results_2017$Apr$dates), SMabs_Tmodel_results_2017$Apr$r2.model, col ='black')
lines(as.Date(Tmodel_results_2017$Apr$dates), Tmodel_results_2017$Apr$r2.model, col ='black', lty=2)
lines(as.Date(SMabs_Tmodel_results_2017$May$dates), SMabs_Tmodel_results_2017$May$r2.model, col='red')
lines(as.Date(Tmodel_results_2017$May$dates), Tmodel_results_2017$May$r2.model, col='red', lty=2)
#lines(as.Date(SMabs_Tmodel_results_2017$May$dates), SMabs_Tmodel_results_2017$May$r2.SM.vs.T, col='lightblue')
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("SMabs + T vs. March biomass model", 'T vs. March biomass model', "SMabs + T vs. April biomass model", 'T vs. April biomass model', 'SMabs + T vs. May biomass model', 'T vs. May biomass model')), lty=c(1, 2, 1, 2, 1, 2), col=c('grey', 'grey', 'black', 'black', 'red', 'red'), inset = 0.005, cex=0.9)
dev.off()

#make combined plots of r2 from SMabs+T model and Tmodel for different dates in 2018
depth <- 22
yr <- 2018
SMabs_Tmodel_results_2018 <- lapply(list.files(file.path(results, 'SMabs_T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- as.Date(c('2018-04-15', '2018-02-15', '2018-03-22'))
SMabs_Tmodel_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_Tmodel_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMabs_Tmodel_results_2018) <- list.files(file.path(results, 'SMabs_T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')))
names(SMabs_Tmodel_results_2018)
names(SMabs_Tmodel_results_2018) <- c('Apr', 'Feb', 'Mar')
Tmodel_results_2018 <- lapply(list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
Tmodel_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], Tmodel_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(Tmodel_results_2018) <- list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*2018*', depth, 'cm*')))
names(Tmodel_results_2018)
names(Tmodel_results_2018) <- c('Apr', 'Feb', 'Mar')
#and plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2018.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 1))
plot(as.Date(SMabs_Tmodel_results_2018$Mar$dates), SMabs_Tmodel_results_2018$Mar$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm model results in', yr, '(dry year)'), col='grey')
lines(as.Date(Tmodel_results_2018$Mar$dates), Tmodel_results_2018$Mar$r2.model, col ='grey', lty=2)
lines(as.Date(SMabs_Tmodel_results_2018$Apr$dates), SMabs_Tmodel_results_2018$Apr$r2.model, col ='black')
lines(as.Date(Tmodel_results_2018$Apr$dates), Tmodel_results_2018$Apr$r2.model, col ='black', lty=2)
#lines(as.Date(SMabs_Tmodel_results_2018$Apr$dates), SMabs_Tmodel_results_2018$Apr$r2.SM.vs.T, col='lightblue')
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("SMabs + T vs. March biomass model", 'T vs. March biomass model', "SMabs + T vs. April biomass model", 'T vs. April biomass model')), lty=c(1, 2, 1, 2), col=c('grey', 'grey', 'black', 'black'), inset = 0.005, cex=0.9)
dev.off()

#make combined plots of significant 2017 associations
depth <- 22
model_results_2017 <- lapply(list.files(model.abs_resultsDir, pattern = glob2rx(paste0('*2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
names(model_results_2017) <- list.files(model.abs_resultsDir, pattern = glob2rx(paste0('*2017*', depth, 'cm*')))
names(model_results_2017)
names(model_results_2017) <- c('Apr', 'Feb', 'Mar', 'May')
model_results_2017 <- lapply(model_results_2017, function(x) x <- x[-which(x$p.value.model > 0.05), ])
lapply(model_results_2017, nrow)
lapply(model_results_2017, print)
clipdates2017 <- as.Date(c('2017-04-10', '2017-02-15', '2017-03-14', '2017-05-01'))
for (i in seq_along(model_results_2017)) {
  model_results_2017[[i]] <- model_results_2017[[i]][as.Date(model_results_2017[[i]]$dates) <= clipdates2017[i], ]
}
min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.SM))), na.rm = TRUE)

#2017 SM plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2017.SM.', depth, 'cm.forage_SMabs+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2017$Feb$dates[model_results_2017$Feb$p.value.SM < 0.05]), model_results_2017$Feb$slope.SM[model_results_2017$Feb$p.value.SM < 0.05], xaxt='n', xlab='', ylab = paste('kg/ha association of 0.01 vol. soil moisture'), ylim=c(min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.SM))), na.rm=TRUE), max(unlist(lapply(model_results_2017, function(x) max_modified(x$slope.SM))), na.rm=TRUE)), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm depth soil moisture and forage growth relationship, 2017'))
points(as.Date(model_results_2017$Mar$dates[model_results_2017$Mar$p.value.SM < 0.05]), model_results_2017$Mar$slope.SM[model_results_2017$Mar$p.value.SM < 0.05], pch = 19, col ='grey')
points(as.Date(model_results_2017$Apr$dates[model_results_2017$Apr$p.value.SM < 0.05]), model_results_2017$Apr$slope.SM[model_results_2017$Apr$p.value.SM < 0.05], pch = 19)
points(as.Date(model_results_2017$May$dates[model_results_2017$May$p.value.SM < 0.05]), model_results_2017$May$slope.SM[model_results_2017$May$p.value.SM < 0.05], pch = 8, xlab='')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("bottomright", legend=(c("2/15/17", "3/14/17", "4/10/17", '5/1/17')), pch=c(1, 19, 19, 8), col=c('black', 'grey', 'black', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2017-02-21', '2017-03-20'))) #only 2.8 mm precip over 30 days
dev.off()

#2017 Temperature plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2017.T.', depth, 'cm.forage_SMabs+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2017$Feb$dates[model_results_2017$Feb$p.value.T < 0.05]), model_results_2017$Feb$slope.T[model_results_2017$Feb$p.value.T < 0.05], xaxt='n', xlab='', ylab = 'kg/ha association of +1 deg C soil temperature', ylim=c(min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.T))), na.rm = TRUE), max(unlist(lapply(model_results_2017, function(x) max_modified(x$slope.T))), na.rm = TRUE)), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, main=paste(depth, 'cm depth soil temperature and forage growth relationship, 2017'))
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
model_results_2018 <- lapply(list.files(model.abs_resultsDir, pattern = glob2rx(paste0('*2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
names(model_results_2018) <- list.files(model.abs_resultsDir, pattern = glob2rx(paste0('*2018*', depth, 'cm*')))
names(model_results_2018)
names(model_results_2018) <- c('Apr', 'Feb', 'Mar', 'peak2018')
model_results_2018 <- lapply(model_results_2018, function(x) x <- x[-which(x$p.value.model > 0.05), ])
lapply(model_results_2018, nrow)
lapply(model_results_2018, print)
clipdates2018 <- as.Date(c('2018-04-15', '2018-02-15', '2018-03-22', '2018-04-15'))
for (i in seq_along(model_results_2018)) {
  model_results_2018[[i]] <- model_results_2018[[i]][as.Date(model_results_2018[[i]]$dates) <= clipdates2018[i], ]
}
lapply(model_results_2018, nrow)

#2018 7 cm SM plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2018.SM.', depth, 'cm.forage_SMabs+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 4))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.SM < 0.05]), model_results_2018$Feb$slope.SM[model_results_2018$Feb$p.value.SM < 0.05] * 0.01, xaxt='n', xlab='', ylab = paste('kg/ha association of + 0.01 vol. soil moisture'), ylim=c(min(unlist(lapply(model_results_2018, function(x) min_modified(x$slope.SM))), na.rm=TRUE) * 0.01, max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.SM))), na.rm=TRUE) * 0.01), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth absolute soil moisture and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.SM < 0.05]), model_results_2018$Mar$slope.SM[model_results_2018$Mar$p.value.SM < 0.05] * 0.01, pch = 19, col ='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.SM < 0.05]), model_results_2018$Apr$slope.SM[model_results_2018$Apr$p.value.SM < 0.05] * 0.01, pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/12', '2018/02/25')))
axis(side = 4, at = c(-125, -100, -75, -50, -25), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 2.5 - 125, type='s', col='lightblue', cex=0.5)
dev.off()

#2018 22 cm SM plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2018.SM.', depth, 'cm.forage_SMabs+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.SM < 0.05]), model_results_2018$Feb$slope.SM[model_results_2018$Feb$p.value.SM < 0.05] * 0.01, xaxt='n', xlab='', ylab = paste('kg/ha association of + 0.01 vol. soil moisture'), ylim=c(min(unlist(lapply(model_results_2018, function(x) min_modified(x$slope.SM))), na.rm=TRUE) * 0.01, max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.SM))), na.rm=TRUE) * 0.01), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth absolute soil moisture and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.SM < 0.05]), model_results_2018$Mar$slope.SM[model_results_2018$Mar$p.value.SM < 0.05] * 0.01, pch = 19, col ='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.SM < 0.05]), model_results_2018$Apr$slope.SM[model_results_2018$Apr$p.value.SM < 0.05] * 0.01, pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/12', '2018/02/25')))
axis(side = 4, at = c(-75, -60, -45, -30, -15), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 1.5 - 75, type='s', col='lightblue', cex=0.5)
dev.off()

#2018 Temp plot
png(file = file.path(resultsFigures, 'finals', 'abs.model', paste0('WY2018.T.', depth, 'cm.forage_SMabs+Tmodel.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4, 2, 2))
plot(as.Date(model_results_2018$Feb$dates[model_results_2018$Feb$p.value.T < 0.05]), model_results_2018$Feb$slope.T[model_results_2018$Feb$p.value.T < 0.05], xaxt='n', xlab='', ylab = 'kg/ha association of +1 deg C soil temperature', ylim=c(min(unlist(lapply(model_results_2018, function(x) min_modified(x$slope.T))), na.rm = TRUE), if(max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.T))), na.rm = TRUE) < 0) {0} else {max(unlist(lapply(model_results_2018, function(x) max_modified(x$slope.T))), na.rm = TRUE)}), xlim = as.Date(c('2017-12-01', '2018-05-01')), pch = 1, main=paste(depth, 'cm depth soil temperature and forage growth relationship, 2018'))
points(as.Date(model_results_2018$Mar$dates[model_results_2018$Mar$p.value.T < 0.05]), model_results_2018$Mar$slope.T[model_results_2018$Mar$p.value.T < 0.05], pch = 19, col='grey')
points(as.Date(model_results_2018$Apr$dates[model_results_2018$Apr$p.value.T < 0.05]), model_results_2018$Apr$slope.T[model_results_2018$Apr$p.value.T < 0.05], pch = 19)
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("2/15/18", "3/22/18", "4/15/18")), pch=c(1, 19, 19), col=c('black', 'grey', 'black'), inset = 0.05, title='association with clipping dates')
abline(v=as.Date(c('2018/01/10', '2018/02/25')))
dev.off()

#SMnorm both depths model
SMnorm_7_22_model <- function(yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  vwc_data_7 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_7cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  if (yr == 2018) {
    vwc_data_7 <- vwc_data_7[!vwc_data_7$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  vwc_data_22 <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_22cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data_7)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_7)[ncol(vwc_data_7)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data_7)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data_7)[ncol(vwc_data_7)], '%b_%d_%Y'), by='week')
  vwc_data_normalized_7 <- vwc_data_7
  vwc_data_normalized_7[ ,2:ncol(vwc_data_normalized_7)] <- (vwc_data_normalized_7[ ,2:ncol(vwc_data_normalized_7)] - rowMeans(vwc_data_normalized_7[ ,2:ncol(vwc_data_normalized_7)], na.rm = TRUE)) / apply(vwc_data_normalized_7[ ,2:ncol(vwc_data_normalized_7)], 1, sd, na.rm=TRUE) #normalize vwc data at 7 cm
  vwc_data_normalized_22 <- vwc_data_22
  vwc_data_normalized_22[ ,2:ncol(vwc_data_normalized_22)] <- (vwc_data_normalized_22[ ,2:ncol(vwc_data_normalized_22)] - rowMeans(vwc_data_normalized_22[ ,2:ncol(vwc_data_normalized_22)], na.rm = TRUE)) / apply(vwc_data_normalized_22[ ,2:ncol(vwc_data_normalized_22)], 1, sd, na.rm=TRUE) #normalize vwc data at 7 cm
  #soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  #model biomass as function of soil moisture at both depths
  SM_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM.7=NA, p.value.SM.7=NA, slope.SM.22=NA, p.value.SM.22=NA, p.value.SM7.vs.SM22=NA, r2.SM7.vs.SM22=NA, vif=NA)
  for (i in 2:ncol(vwc_data_normalized_7)) {
    lm.result <- lm(forage_data[[clpname]] ~ vwc_data_normalized_7[,i] + vwc_data_normalized_22[,i])
    lm.summary <- summary(lm.result)
    lm.SM7.vs.SM22 <- lm(vwc_data_normalized_7[,i] ~ vwc_data_normalized_22[,i])
    SM_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_analysis[i-1, 'slope.SM.7'] <- lm.summary$coefficients[2, 1]
    SM_analysis[i-1, 'slope.SM.22'] <- lm.summary$coefficients[3, 1]
    SM_analysis[i-1, 'p.value.SM.7'] <- lm.summary$coefficients[2, 4]
    SM_analysis[i-1, 'p.value.SM.22'] <- lm.summary$coefficients[3, 4]
    SM_analysis[i-1, 'p.value.SM7.vs.SM22'] <- summary(lm.SM7.vs.SM22)$coefficients[2, 4]
    SM_analysis[i-1, 'r2.SM7.vs.SM22'] <- summary(lm.SM7.vs.SM22)$r.squared
    SM_analysis[i-1, 'vif'] <- vif(lm.result)[1]
  }
  write.csv(SM_analysis, file.path(results, 'SMnorm_both.depths_model_results', paste0('SMnorm_both.depths_vs_', month, yr, 'biomass.csv')), row.names = FALSE)
}
# colnames(forage_data) # "clp021517" "clp031417" "clp041017" "clp050117" "clp011618" "clp021518" "clp032218" "clp041518"
SMnorm_7_22_model(2017, 'clp021517', 'Feb')
SMnorm_7_22_model(2017, 'clp031417', 'Mar')
SMnorm_7_22_model(2017, 'clp041017', 'Apr')
SMnorm_7_22_model(2017, 'clp050117', 'May')
SMnorm_7_22_model(2017, 'peak2017', 'peak2017')
SMnorm_7_22_model(2017, 'clp021517', 'Feb')
SMnorm_7_22_model(2017, 'clp031417', 'Mar')
SMnorm_7_22_model(2017, 'clp041017', 'Apr')
SMnorm_7_22_model(2017, 'clp050117', 'May')
SMnorm_7_22_model(2017, 'peak2017', 'peak2017')
SMnorm_7_22_model(2018, 'clp021518', 'Feb')
SMnorm_7_22_model(2018, 'clp032218', 'Mar')
SMnorm_7_22_model(2018, 'clp041518', 'Apr')
SMnorm_7_22_model(2018, 'peak2018', 'peak2018')
SMnorm_7_22_model(2018, 'clp021518', 'Feb')
SMnorm_7_22_model(2018, 'clp032218', 'Mar')
SMnorm_7_22_model(2018, 'clp041518', 'Apr')
SMnorm_7_22_model(2018, 'peak2018', 'peak2018')

# depth <- 7
# yr <- 2018
# clpname <- 'clp041518'
SMnorm_T2_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  vwc_data_normalized <- vwc_data
  vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE) #normalize vwc data
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  #model biomass as function of temperature and soil moisture
  SM_T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.T2=NA, p.value.T2=NA, note.T.model=NA)
  for (i in 2:ncol(vwc_data_normalized)) {
    if (sum(is.na(soilT_data[,i])) > 0) {
      lm.result <- lm(forage_data[[clpname]] ~ vwc_data_normalized[,i] + soilT_data[,i] + I(soilT_data[,i]^2))
      SM_T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'T + I(T^2) approach'
    } else {lm.result <- lm(forage_data[[clpname]] ~ vwc_data_normalized[,i] + poly(soilT_data[,i], 2))
            SM_T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'poly(T, 2) approach'
      }
    lm.summary <- summary(lm.result)
    #lm.SM.vs.T <- lm(vwc_data_normalized[,i] ~ soilT_data[,i])
    SM_T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_T_vs_biomass_analysis[i-1, 'slope.SM'] <- lm.summary$coefficients[2, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[3, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T2'] <- lm.summary$coefficients[4, 1]
    SM_T_vs_biomass_analysis[i-1, 'p.value.SM'] <- lm.summary$coefficients[2, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T'] <- lm.summary$coefficients[3, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T2'] <- lm.summary$coefficients[4, 4]
    #SM_T_vs_biomass_analysis[i-1, 'vif'] <- vif(lm.result)[1]
  }
  write.csv(SM_T_vs_biomass_analysis, file.path(results, 'SMnorm_T2_model_results', paste0('SMnorm_T2_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
# colnames(forage_data) # "clp021517" "clp031417" "clp041017" "clp050117" "clp011618" "clp021518" "clp032218" "clp041518"

#
SMnorm_T2_model(7, 2017, 'clp021517', 'Feb')
SMnorm_T2_model(7, 2017, 'clp031417', 'Mar')
SMnorm_T2_model(7, 2017, 'clp041017', 'Apr')
SMnorm_T2_model(7, 2017, 'clp050117', 'May')
SMnorm_T2_model(7, 2017, 'peak2017', 'peak2017')
SMnorm_T2_model(22, 2017, 'clp021517', 'Feb')
SMnorm_T2_model(22, 2017, 'clp031417', 'Mar')
SMnorm_T2_model(22, 2017, 'clp041017', 'Apr')
SMnorm_T2_model(22, 2017, 'clp050117', 'May')
SMnorm_T2_model(22, 2017, 'peak2017', 'peak2017')
SMnorm_T2_model(7, 2018, 'clp021518', 'Feb')
SMnorm_T2_model(7, 2018, 'clp032218', 'Mar')
SMnorm_T2_model(7, 2018, 'clp041518', 'Apr')
SMnorm_T2_model(7, 2018, 'peak2018', 'peak2018')
SMnorm_T2_model(22, 2018, 'clp021518', 'Feb')
SMnorm_T2_model(22, 2018, 'clp032218', 'Mar')
SMnorm_T2_model(22, 2018, 'clp041518', 'Apr')
SMnorm_T2_model(22, 2018, 'peak2018', 'peak2018')

SMnorm_normT2_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  vwc_data_normalized <- vwc_data
  vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE) #normalize vwc data
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  soilT_data_norm <- soilT_data
  soilT_data_norm[ ,2:ncol(soilT_data_norm)] <- apply(soilT_data_norm[ ,2:ncol(soilT_data_norm)], 2, function(x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)})
  #model biomass as function of temperature and soil moisture
  SM_T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.T2=NA, p.value.T2=NA, note.T.model=NA)
  for (i in 2:ncol(vwc_data_normalized)) {
    if (sum(is.na(soilT_data_norm[,i])) > 0) {
      lm.result <- lm(forage_data[[clpname]] ~ vwc_data_normalized[,i] + soilT_data_norm[,i] + I(soilT_data_norm[,i]^2))
      SM_T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'T + I(T^2) approach'
    } else {lm.result <- lm(forage_data[[clpname]] ~ vwc_data_normalized[,i] + poly(soilT_data_norm[,i], 2))
    SM_T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'poly(T, 2) approach'
    }
    lm.summary <- summary(lm.result)
    #lm.SM.vs.T <- lm(vwc_data_normalized[,i] ~ soilT_data[,i])
    SM_T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_T_vs_biomass_analysis[i-1, 'slope.SM'] <- lm.summary$coefficients[2, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[3, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T2'] <- lm.summary$coefficients[4, 1]
    SM_T_vs_biomass_analysis[i-1, 'p.value.SM'] <- lm.summary$coefficients[2, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T'] <- lm.summary$coefficients[3, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T2'] <- lm.summary$coefficients[4, 4]
    #SM_T_vs_biomass_analysis[i-1, 'vif'] <- vif(lm.result)[1]
  }
  write.csv(SM_T_vs_biomass_analysis, file.path(results, 'SMnorm_normT2_model_results', paste0('SMnorm_normT2_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
# colnames(forage_data) # "clp021517" "clp031417" "clp041017" "clp050117" "clp011618" "clp021518" "clp032218" "clp041518"

#
SMnorm_normT2_model(7, 2017, 'clp021517', 'Feb')
SMnorm_normT2_model(7, 2017, 'clp031417', 'Mar')
SMnorm_normT2_model(7, 2017, 'clp041017', 'Apr')
SMnorm_normT2_model(7, 2017, 'clp050117', 'May')
SMnorm_normT2_model(7, 2017, 'peak2017', 'peak2017')
SMnorm_normT2_model(22, 2017, 'clp021517', 'Feb')
SMnorm_normT2_model(22, 2017, 'clp031417', 'Mar')
SMnorm_normT2_model(22, 2017, 'clp041017', 'Apr')
SMnorm_normT2_model(22, 2017, 'clp050117', 'May')
SMnorm_normT2_model(22, 2017, 'peak2017', 'peak2017')
SMnorm_normT2_model(7, 2018, 'clp021518', 'Feb')
SMnorm_normT2_model(7, 2018, 'clp032218', 'Mar')
SMnorm_normT2_model(7, 2018, 'clp041518', 'Apr')
SMnorm_normT2_model(7, 2018, 'peak2018', 'peak2018')
SMnorm_normT2_model(22, 2018, 'clp021518', 'Feb')
SMnorm_normT2_model(22, 2018, 'clp032218', 'Mar')
SMnorm_normT2_model(22, 2018, 'clp041518', 'Apr')
SMnorm_normT2_model(22, 2018, 'peak2018', 'peak2018')

SMabs_T2_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  #vwc_data_normalized <- vwc_data
  #vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE) #normalize vwc data
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  #model biomass as function of temperature and soil moisture
  SM_T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM=NA, p.value.SM=NA, slope.T=NA, p.value.T=NA, slope.T2=NA, p.value.T2=NA, note.T.model=NA)
  for (i in 2:ncol(vwc_data)) {
    if (sum(is.na(soilT_data[,i])) > 0) {
      lm.result <- lm(forage_data[[clpname]] ~ vwc_data[,i] + soilT_data[,i] + I(soilT_data[,i]^2))
      SM_T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'T + I(T^2) approach'
    } else {lm.result <- lm(forage_data[[clpname]] ~ vwc_data[,i] + poly(soilT_data[,i], 2))
    SM_T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'poly(T, 2) approach'
    }
    lm.summary <- summary(lm.result)
    #lm.SM.vs.T <- lm(vwc_data_normalized[,i] ~ soilT_data[,i])
    SM_T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_T_vs_biomass_analysis[i-1, 'slope.SM'] <- lm.summary$coefficients[2, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[3, 1]
    SM_T_vs_biomass_analysis[i-1, 'slope.T2'] <- lm.summary$coefficients[4, 1]
    SM_T_vs_biomass_analysis[i-1, 'p.value.SM'] <- lm.summary$coefficients[2, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T'] <- lm.summary$coefficients[3, 4]
    SM_T_vs_biomass_analysis[i-1, 'p.value.T2'] <- lm.summary$coefficients[4, 4]
    #SM_T_vs_biomass_analysis[i-1, 'vif'] <- vif(lm.result)[1]
  }
  write.csv(SM_T_vs_biomass_analysis, file.path(results, 'SMabs_T2_model_results', paste0('SMabs_T2_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}

#
SMabs_T2_model(7, 2017, 'clp021517', 'Feb')
SMabs_T2_model(7, 2017, 'clp031417', 'Mar')
SMabs_T2_model(7, 2017, 'clp041017', 'Apr')
SMabs_T2_model(7, 2017, 'clp050117', 'May')
SMabs_T2_model(7, 2017, 'peak2017', 'peak2017')
SMabs_T2_model(22, 2017, 'clp021517', 'Feb')
SMabs_T2_model(22, 2017, 'clp031417', 'Mar')
SMabs_T2_model(22, 2017, 'clp041017', 'Apr')
SMabs_T2_model(22, 2017, 'clp050117', 'May')
SMabs_T2_model(22, 2017, 'peak2017', 'peak2017')
SMabs_T2_model(7, 2018, 'clp021518', 'Feb')
SMabs_T2_model(7, 2018, 'clp032218', 'Mar')
SMabs_T2_model(7, 2018, 'clp041518', 'Apr')
SMabs_T2_model(7, 2018, 'peak2018', 'peak2018')
SMabs_T2_model(22, 2018, 'clp021518', 'Feb')
SMabs_T2_model(22, 2018, 'clp032218', 'Mar')
SMabs_T2_model(22, 2018, 'clp041518', 'Apr')
SMabs_T2_model(22, 2018, 'peak2018', 'peak2018')

yr <- 2017
depth <- 7
clpname <- 'peak2017'
T2_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was missing Dec 2017-Feb 2018
  }
  soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(soilT_data)[2], '%b_%d_%Y'), as.Date(colnames(soilT_data)[ncol(soilT_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(soilT_data)[2], '%b_%d_%Y'), as.Date(colnames(soilT_data)[ncol(soilT_data)], '%b_%d_%Y'), by='week')
  #model biomass as function of temperature and soil moisture
  T_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.T=NA, p.value.T=NA, slope.T2=NA, p.value.T2=NA, intercept=NA, p.value.int=NA, note.T.model=NA)
  for (i in 2:ncol(soilT_data)) {
    if (sum(is.na(soilT_data[,i])) > 0) {
      lm.result <- lm(forage_data[[clpname]] ~ soilT_data[,i] + I(soilT_data[,i]^2))
      T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'T + I(T^2) approach'
    } else {lm.result <- lm(forage_data[[clpname]] ~ poly(soilT_data[,i], 2))
            T_vs_biomass_analysis[i-1, 'note.T.model'] <- 'poly(T, 2) approach'
    }
    lm.summary <- summary(lm.result)
    T_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    T_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    T_vs_biomass_analysis[i-1, 'intercept'] <- lm.summary$coefficients[1, 1]
    T_vs_biomass_analysis[i-1, 'slope.T'] <- lm.summary$coefficients[2, 1]
    T_vs_biomass_analysis[i-1, 'slope.T2'] <- lm.summary$coefficients[3, 1]
    T_vs_biomass_analysis[i-1, 'p.value.int'] <- lm.summary$coefficients[1, 4]
    T_vs_biomass_analysis[i-1, 'p.value.T'] <- lm.summary$coefficients[2, 4]
    T_vs_biomass_analysis[i-1, 'p.value.T2'] <- lm.summary$coefficients[3, 4]
  }
  write.csv(T_vs_biomass_analysis, file.path(results, 'T2_model_results', paste0('T2_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
T2_model(7, 2017, 'clp021517', 'Feb')
T2_model(7, 2017, 'clp031417', 'Mar')
T2_model(7, 2017, 'clp041017', 'Apr')
T2_model(7, 2017, 'clp050117', 'May')
T2_model(7, 2017, 'peak2017', 'peak2017')
T2_model(22, 2017, 'clp021517', 'Feb')
T2_model(22, 2017, 'clp031417', 'Mar')
T2_model(22, 2017, 'clp041017', 'Apr')
T2_model(22, 2017, 'clp050117', 'May')
T2_model(22, 2017, 'peak2017', 'peak2017')
T2_model(7, 2018, 'clp021518', 'Feb')
T2_model(7, 2018, 'clp032218', 'Mar')
T2_model(7, 2018, 'clp041518', 'Apr')
T2_model(7, 2018, 'peak2018', 'peak2018')
T2_model(22, 2018, 'clp021518', 'Feb')
T2_model(22, 2018, 'clp032218', 'Mar')
T2_model(22, 2018, 'clp041518', 'Apr')
T2_model(22, 2018, 'peak2018', 'peak2018')

#daily mean absolute soil moisture vs. biomass model
SMabs_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was jacked
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  #model biomass as function of daily soil moisture
  SM_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SM=NA, p.value.SM=NA, intercept=NA, p.value.int=NA)
  for (i in 2:ncol(vwc_data)) {
    lm.summary <- summary(lm(forage_data[[clpname]] ~ vwc_data[,i]))
    SM_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_vs_biomass_analysis[i-1, 'intercept'] <- lm.summary$coefficients[1, 1]
    SM_vs_biomass_analysis[i-1, 'slope.SM'] <- lm.summary$coefficients[2, 1]
    SM_vs_biomass_analysis[i-1, 'p.value.int'] <- lm.summary$coefficients[1, 4]
    SM_vs_biomass_analysis[i-1, 'p.value.SM'] <- lm.summary$coefficients[2, 4]
  }
  write.csv(SM_vs_biomass_analysis, file.path(results, 'SMabs_model_results', paste0('SMabs_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
SMabs_model(7, 2017, 'clp021517', 'Feb')
SMabs_model(7, 2017, 'clp031417', 'Mar')
SMabs_model(7, 2017, 'clp041017', 'Apr')
SMabs_model(7, 2017, 'clp050117', 'May')
SMabs_model(7, 2017, 'peak2017', 'peak2017')
SMabs_model(22, 2017, 'clp021517', 'Feb')
SMabs_model(22, 2017, 'clp031417', 'Mar')
SMabs_model(22, 2017, 'clp041017', 'Apr')
SMabs_model(22, 2017, 'clp050117', 'May')
SMabs_model(22, 2017, 'peak2017', 'peak2017')
SMabs_model(7, 2018, 'clp021518', 'Feb')
SMabs_model(7, 2018, 'clp032218', 'Mar')
SMabs_model(7, 2018, 'clp041518', 'Apr')
SMabs_model(7, 2018, 'peak2018', 'peak2018')
SMabs_model(22, 2018, 'clp021518', 'Feb')
SMabs_model(22, 2018, 'clp032218', 'Mar')
SMabs_model(22, 2018, 'clp041518', 'Apr')
SMabs_model(22, 2018, 'peak2018', 'peak2018')

SMnorm_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was jacked
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  vwc_data_normalized <- vwc_data
  vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE)
  #model biomass as function of daily soil moisture
  SM_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SMnorm=NA, p.value.SMnorm=NA, intercept=NA, p.value.int=NA)
  for (i in 2:ncol(vwc_data)) {
    lm.summary <- summary(lm(forage_data[[clpname]] ~ vwc_data_normalized[,i]))
    SM_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_vs_biomass_analysis[i-1, 'intercept'] <- lm.summary$coefficients[1, 1]
    SM_vs_biomass_analysis[i-1, 'slope.SMnorm'] <- lm.summary$coefficients[2, 1]
    SM_vs_biomass_analysis[i-1, 'p.value.int'] <- lm.summary$coefficients[1, 4]
    SM_vs_biomass_analysis[i-1, 'p.value.SMnorm'] <- lm.summary$coefficients[2, 4]
  }
  write.csv(SM_vs_biomass_analysis, file.path(results, 'SMnorm_model_results', paste0('SMnorm_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
SMnorm_model(7, 2017, 'clp021517', 'Feb')
SMnorm_model(7, 2017, 'clp031417', 'Mar')
SMnorm_model(7, 2017, 'clp041017', 'Apr')
SMnorm_model(7, 2017, 'clp050117', 'May')
SMnorm_model(7, 2017, 'peak2017', 'peak2017')
SMnorm_model(22, 2017, 'clp021517', 'Feb')
SMnorm_model(22, 2017, 'clp031417', 'Mar')
SMnorm_model(22, 2017, 'clp041017', 'Apr')
SMnorm_model(22, 2017, 'clp050117', 'May')
SMnorm_model(22, 2017, 'peak2017', 'peak2017')
SMnorm_model(7, 2018, 'clp021518', 'Feb')
SMnorm_model(7, 2018, 'clp032218', 'Mar')
SMnorm_model(7, 2018, 'clp041518', 'Apr')
SMnorm_model(7, 2018, 'peak2018', 'peak2018')
SMnorm_model(22, 2018, 'clp021518', 'Feb')
SMnorm_model(22, 2018, 'clp032218', 'Mar')
SMnorm_model(22, 2018, 'clp041518', 'Apr')
SMnorm_model(22, 2018, 'peak2018', 'peak2018')

SMnorm2_model <- function(depth, yr, clpname, month) {
  forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)
  if (depth == 22 & yr == 2018) {
    forage_data <- forage_data[!forage_data$location==3,] #because 22 cm data at location 3 was jacked
  }
  vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
  dates <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='day')
  weeks <- seq.Date(as.Date(colnames(vwc_data)[2], '%b_%d_%Y'), as.Date(colnames(vwc_data)[ncol(vwc_data)], '%b_%d_%Y'), by='week')
  vwc_data_normalized <- vwc_data
  vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] <- (vwc_data_normalized[ ,2:ncol(vwc_data_normalized)] - rowMeans(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], na.rm = TRUE)) / apply(vwc_data_normalized[ ,2:ncol(vwc_data_normalized)], 1, sd, na.rm=TRUE)
  #model biomass as function of daily soil moisture
  SM_vs_biomass_analysis <- data.frame(dates=dates, p.value.model= NA, r2.model=NA, slope.SMnorm=NA, p.value.SMnorm=NA, slope.SMnorm2=NA, p.value.SMnorm2=NA, intercept=NA, p.value.int=NA, note.vwc.model=NA)
  for (i in 2:ncol(vwc_data_normalized)) {
    if (sum(is.na(vwc_data_normalized[,i])) > 0) {
      lm.summary <- summary(lm(forage_data[[clpname]] ~ vwc_data_normalized[,i] + I(vwc_data_normalized[,i]^2)))
      SM_vs_biomass_analysis[i-1, 'note.vwc.model'] <- 'vwc_norm + I(vwc_norm^2) approach'
    } else {lm.summary <- summary(lm(forage_data[[clpname]] ~ poly(vwc_data_normalized[,i], 2)))
    SM_vs_biomass_analysis[i-1, 'note.vwc.model'] <- 'poly(vwc_norm, 2) approach'
    }
    SM_vs_biomass_analysis[i-1, 'p.value.model'] <- pf(lm.summary$fstatistic[1], lm.summary$fstatistic[2], lm.summary$fstatistic[3], lower.tail = FALSE)
    SM_vs_biomass_analysis[i-1, 'r2.model'] <- lm.summary$r.squared
    SM_vs_biomass_analysis[i-1, 'intercept'] <- lm.summary$coefficients[1, 1]
    SM_vs_biomass_analysis[i-1, 'slope.SMnorm'] <- lm.summary$coefficients[2, 1]
    SM_vs_biomass_analysis[i-1, 'slope.SMnorm2'] <- lm.summary$coefficients[3, 1]
    SM_vs_biomass_analysis[i-1, 'p.value.int'] <- lm.summary$coefficients[1, 4]
    SM_vs_biomass_analysis[i-1, 'p.value.SMnorm'] <- lm.summary$coefficients[2, 4]
    SM_vs_biomass_analysis[i-1, 'p.value.SMnorm2'] <- lm.summary$coefficients[3, 4]
  }
  write.csv(SM_vs_biomass_analysis, file.path(results, 'SMnorm2_model_results', paste0('SMnorm2_vs_', month, yr, 'biomass_', depth, 'cm.csv')), row.names = FALSE)
}
SMnorm2_model(7, 2017, 'clp021517', 'Feb')
SMnorm2_model(7, 2017, 'clp031417', 'Mar')
SMnorm2_model(7, 2017, 'clp041017', 'Apr')
SMnorm2_model(7, 2017, 'clp050117', 'May')
SMnorm2_model(7, 2017, 'peak2017', 'peak2017')
SMnorm2_model(22, 2017, 'clp021517', 'Feb')
SMnorm2_model(22, 2017, 'clp031417', 'Mar')
SMnorm2_model(22, 2017, 'clp041017', 'Apr')
SMnorm2_model(22, 2017, 'clp050117', 'May')
SMnorm2_model(22, 2017, 'peak2017', 'peak2017')
SMnorm2_model(7, 2018, 'clp021518', 'Feb')
SMnorm2_model(7, 2018, 'clp032218', 'Mar')
SMnorm2_model(7, 2018, 'clp041518', 'Apr')
SMnorm2_model(7, 2018, 'peak2018', 'peak2018')
SMnorm2_model(22, 2018, 'clp021518', 'Feb')
SMnorm2_model(22, 2018, 'clp032218', 'Mar')
SMnorm2_model(22, 2018, 'clp041518', 'Apr')
SMnorm2_model(22, 2018, 'peak2018', 'peak2018')

#some residual checking
normalize_VWCdata <- function(df) {
  df[ ,2:ncol(df)] <- (df[ ,2:ncol(df)] - rowMeans(df[ ,2:ncol(df)], na.rm = TRUE)) / apply(df[ ,2:ncol(df)], 1, sd, na.rm=TRUE)
  df
}
depth <- 7
yr <- 2017
vwc_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'VWC', paste0('MeanVWC_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
vwc_data_normalized <- normalize_VWCdata(vwc_data)
soilT_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/results/processed_soil_moisture/Jul2018/daily_by_location', yr, 'Temperature', paste0('MeanT_', depth, 'cm_dailymeans_by_location.csv')), stringsAsFactors = FALSE)
forage_data <- read.csv(file.path(forageDir, 'summaries', 'forage2017_2018_summary.csv'), stringsAsFactors=FALSE)

#2018 example result for SMnorm_T2 model
SM_T2_7cm_1.20.2018 <- lm(forage_data$peak2018 ~ vwc_data_normalized$Jan_20_2018 + poly(soilT_data$Jan_20_2018, 2))
plot(SM_T2_7cm_1.20.2018)
summary(SM_T2_7cm_1.20.2018)
plot(SM_T2_7cm_1.11.2017$fitted.values, SM_T2_7cm_1.20.2018$residuals)
abline(h=0, lty=2)
text(forage_data$peak2018, SM_T2_7cm_1.20.2018$residuals, labels = forage_data$location)
hist(SM_T2_7cm_1.20.2018$residuals)

#plot for SMnorm + T result
SM_T_7cm_1.20.2018 <- lm(forage_data$peak2018 ~ vwc_data_normalized$Jan_20_2018 + soilT_data$Jan_20_2018)
summary(SM_T_7cm_1.20.2018)
plot(SM_T_7cm_1.20.2018)
hist(SM_T_7cm_1.20.2018$residuals)

#2017 example result SMnorm + T model
date <- 'Jan_09_2017'
SM_T_7cm_2017 <- lm(forage_data$peak2017 ~ vwc_data_normalized[[date]] + soilT_data[[date]])
summary(SM_T_7cm_2017)
par(mfrow =c(2,2))
plot(SM_T_7cm_2017)
par(mfrow =c(1,1))
hist(SM_T_7cm_2017$residuals)

#and SMnorm + T2 model
date <- 'Jan_10_2017'
SM_T2_7cm_2017 <- lm(forage_data$peak2017 ~ vwc_data_normalized[[date]] + poly(soilT_data[[date]], 2))
summary(lm(forage_data$peak2017 ~ vwc_data_normalized[[date]] + soilT_data[[date]] + I(soilT_data[[date]]^2)))
summary(SM_T2_7cm_2017)
par(mfrow =c(2,2))
plot(SM_T2_7cm_2017)
par(mfrow =c(1,1))
hist(SM_T2_7cm_2017$residuals)
#make combined plots of r2 from SMnorm+T2 model, T2model, and SMnorm model for different dates in 2018
depth <- 22
yr <- 2018
SMnorm_T2model_results_2018 <- lapply(list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- '2018-04-15'
SMnorm_T2model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_T2model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMnorm_T2model_results_2018) <- list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMnorm_T2model_results_2018)
names(SMnorm_T2model_results_2018) <- c('peak2018')#c('Apr', 'Feb', 'Mar', 'May')
T2model_results_2018 <- lapply(list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
T2model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], T2model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(T2model_results_2018) <- list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(T2model_results_2018)
names(T2model_results_2018) <- "peak2018" #c('Apr', 'Feb', 'Mar', 'May')
SMnorm_model_results_2018 <- lapply(list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
SMnorm_model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMnorm_model_results_2018) <- list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMnorm_model_results_2018)
names(SMnorm_model_results_2018) <- c('peak2018') #c('Apr', 'Feb', 'Mar', 'May')

#2018 plot
png(file = file.path(resultsFigures, 'finals', 'SMnorm_T2_model', paste0('WY2018.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMnorm_T2model_results_2018$peak2018$dates), SMnorm_T2model_results_2018$peak2018$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2017-12-01', '2018-04-15')), pch = 1, col='black')#, cex.axis=0.9) #main=paste(depth, 'cm model results in', yr, '(wet year)')
lines(as.Date(T2model_results_2018$peak2018$dates), T2model_results_2018$peak2018$r2.model, col ='red', lty=2)
lines(as.Date(SMnorm_model_results_2018$peak2018$dates), SMnorm_model_results_2018$peak$r2.model, col ='blue', lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("peak forage vs. SMnorm + poly(T, 2) model", 'peak forage vs. poly(T, 2) model', "peak forage vs. SMnorm model")), lty=c(1, 2, 2), col=c('black', 'red', 'blue'), inset = 0.01, cex=1.1)
#axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'), cex.axis=0.9)
#mtext("mm precipitation per day", side=4, line=2.5)
#lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. / 100, type='s', col='lightblue', cex=0.5)
dev.off()

#make combined plots of r2 from SMnorm+T2 model, T2model, and SMnorm model for different dates in 2017
depth <- 22
yr <- 2017
SMnorm_T2model_results_2017 <- lapply(list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2017 <- '2017-05-01'
SMnorm_T2model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_T2model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMnorm_T2model_results_2017) <- list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMnorm_T2model_results_2017)
names(SMnorm_T2model_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')
T2model_results_2017 <- lapply(list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
T2model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], T2model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(T2model_results_2017) <- list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(T2model_results_2017)
names(T2model_results_2017) <- "peak2017" #c('Apr', 'Feb', 'Mar', 'May')
SMnorm_model_results_2017 <- lapply(list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
SMnorm_model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMnorm_model_results_2017) <- list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMnorm_model_results_2017)
names(SMnorm_model_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')

#2017 plot
png(file = file.path(resultsFigures, 'finals', 'SMnorm_T2_model', paste0('WY2017.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMnorm_T2model_results_2017$peak2017$dates), SMnorm_T2model_results_2017$peak2017$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1)# cex.axis=0.9, main=paste(depth, 'cm model results in', yr, '(wet year)'), col='black')
lines(as.Date(T2model_results_2017$peak2017$dates), T2model_results_2017$peak2017$r2.model, col ='red', lty=2)
lines(as.Date(SMnorm_model_results_2017$peak2017$dates), SMnorm_model_results_2017$peak$r2.model, col ='blue', lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("peak forage vs. SMnorm + poly(T, 2) model", 'peak forage vs. poly(T, 2) model', "peak forage vs. SMnorm model")), lty=c(1, 2, 2), col=c('black', 'red', 'blue'), inset = 0.01, cex=1.1)
#axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'), cex.axis=0.9)
#mtext("mm precipitation per day", side=4, line=2.5)
#lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. / 100, type='s', col='lightblue', cex=0.5)
dev.off()

#make combined plots of r2 from SMabs+T2 model, T2model, and SMabs model for different dates in 2018
depth <- 7
yr <- 2018
SMabs_T2model_results_2018 <- lapply(list.files(file.path(results, 'SMabs_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- '2018-04-15'
SMabs_T2model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_T2model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMabs_T2model_results_2018) <- list.files(file.path(results, 'SMabs_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMabs_T2model_results_2018)
names(SMabs_T2model_results_2018) <- c('peak2018')#c('Apr', 'Feb', 'Mar', 'May')
T2model_results_2018 <- lapply(list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
T2model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], T2model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(T2model_results_2018) <- list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(T2model_results_2018)
names(T2model_results_2018) <- "peak2018" #c('Apr', 'Feb', 'Mar', 'May')
SMabs_model_results_2018 <- lapply(list.files(file.path(results, 'SMabs_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
SMabs_model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMabs_model_results_2018) <- list.files(file.path(results, 'SMabs_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMabs_model_results_2018)
names(SMabs_model_results_2018) <- c('peak2018')#c('Apr', 'Feb', 'Mar', 'May')

#2018 plot
png(file = file.path(resultsFigures, 'finals', 'SMabs_T2_model', paste0('WY2018.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMabs_T2model_results_2018$peak2018$dates), SMabs_T2model_results_2018$peak2018$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2017-12-01', '2018-04-15')), pch = 1, col='black', cex.axis=1) #main=paste(depth, 'cm model results in', yr, '(wet year)')
lines(as.Date(T2model_results_2018$peak2018$dates), T2model_results_2018$peak2018$r2.model, col ='red', lty=2)
lines(as.Date(SMabs_model_results_2018$peak2018$dates), SMabs_model_results_2018$peak$r2.model, col ='blue', lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("peak forage vs. SMabs + poly(T, 2) model", 'peak forage vs. poly(T, 2) model', "peak forage vs. SMabs model")), lty=c(1, 2, 2), col=c('black', 'red', 'blue'), inset = 0.01, cex=1.1)
#axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'), cex.axis=0.9)
#mtext("mm precipitation per day", side=4, line=2.5)
#lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. / 100, type='s', col='lightblue', cex=0.5)
dev.off()

#make combined plots of r2 from SMabs+T2 model, T2model, and SMabs model for different dates in 2017
depth <- 7
yr <- 2017
SMabs_T2model_results_2017 <- lapply(list.files(file.path(results, 'SMabs_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2017 <- '2017-05-01'
SMabs_T2model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_T2model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMabs_T2model_results_2017) <- list.files(file.path(results, 'SMabs_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMabs_T2model_results_2017)
names(SMabs_T2model_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')
T2model_results_2017 <- lapply(list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
T2model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], T2model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(T2model_results_2017) <- list.files(file.path(results, 'T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(T2model_results_2017)
names(T2model_results_2017) <- "peak2017" #c('Apr', 'Feb', 'Mar', 'May')
SMabs_model_results_2017 <- lapply(list.files(file.path(results, 'SMabs_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
SMabs_model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMabs_model_results_2017) <- list.files(file.path(results, 'SMabs_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMabs_model_results_2017)
names(SMabs_model_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')

#2017 plot
png(file = file.path(resultsFigures, 'finals', 'SMabs_T2_model', paste0('WY2017.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMabs_T2model_results_2017$peak2017$dates), SMabs_T2model_results_2017$peak2017$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, col='black') #main=paste(depth, 'cm model results in', yr, '(wet year)')
lines(as.Date(T2model_results_2017$peak2017$dates), T2model_results_2017$peak2017$r2.model, col ='red', lty=2)
lines(as.Date(SMabs_model_results_2017$peak2017$dates), SMabs_model_results_2017$peak$r2.model, col ='blue', lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("peak forage vs. SMabs + poly(T, 2) model", 'peak forage vs. poly(T, 2) model', "peak forage vs. SMabs model")), lty=c(1, 2, 2), col=c('black', 'red', 'blue'), inset = 0.01, cex=1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'), cex.axis=0.9)
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. / 100, type='s', col='lightblue', cex=0.5)
dev.off()

#make combined plots of r2 from SMnorm+T model, Tmodel, and SMnorm model for different dates in 2018
depth <- 22
yr <- 2018
SMnorm_Tmodel_results_2018 <- lapply(list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- '2018-04-15'
SMnorm_Tmodel_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_Tmodel_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMnorm_Tmodel_results_2018) <- list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMnorm_Tmodel_results_2018)
names(SMnorm_Tmodel_results_2018) <- c('peak2018')#c('Apr', 'Feb', 'Mar', 'May')
Tmodel_results_2018 <- lapply(list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
Tmodel_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], Tmodel_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(Tmodel_results_2018) <- list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(Tmodel_results_2018)
names(Tmodel_results_2018) <- "peak2018" #c('Apr', 'Feb', 'Mar', 'May')
SMnorm_model_results_2018 <- lapply(list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
SMnorm_model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMnorm_model_results_2018) <- list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMnorm_model_results_2018)
names(SMnorm_model_results_2018) <- c('peak2018') #c('Apr', 'Feb', 'Mar', 'May')

#2018 plot
png(file = file.path(resultsFigures, 'finals', 'SMnorm_T_model', paste0('WY2018.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMnorm_Tmodel_results_2018$peak2018$dates), SMnorm_Tmodel_results_2018$peak2018$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2017-12-01', '2018-04-15')), pch = 1, col='black', cex.axis=0.9) #main=paste(depth, 'cm model results in', yr, '(wet year)')
lines(as.Date(Tmodel_results_2018$peak2018$dates), Tmodel_results_2018$peak2018$r2.model, col ='red', lty=2)
lines(as.Date(SMnorm_model_results_2018$peak2018$dates), SMnorm_model_results_2018$peak$r2.model, col ='blue', lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2018/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("peak forage vs. SMnorm + T model", 'peak forage vs. T model', "peak forage vs. SMnorm model")), lty=c(1, 2, 2), col=c('black', 'red', 'blue'), inset = 0.01, cex=1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'), cex.axis=0.9)
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. / 100, type='s', col='lightblue', cex=0.5)
dev.off()

#make combined plots of r2 from SMnorm+T model, Tmodel, and SMnorm model for different dates in 2017
depth <- 22
yr <- 2017
SMnorm_Tmodel_results_2017 <- lapply(list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2017 <- '2017-05-01'
SMnorm_Tmodel_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_Tmodel_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMnorm_Tmodel_results_2017) <- list.files(file.path(results, 'SMnorm_T_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMnorm_Tmodel_results_2017)
names(SMnorm_Tmodel_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')
Tmodel_results_2017 <- lapply(list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
Tmodel_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], Tmodel_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(Tmodel_results_2017) <- list.files(file.path(results, 'T_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(Tmodel_results_2017)
names(Tmodel_results_2017) <- "peak2017" #c('Apr', 'Feb', 'Mar', 'May')
SMnorm_model_results_2017 <- lapply(list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
SMnorm_model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMnorm_model_results_2017) <- list.files(file.path(results, 'SMnorm_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMnorm_model_results_2017)
names(SMnorm_model_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')

#2017 plot
png(file = file.path(resultsFigures, 'finals', 'SMnorm_T_model', paste0('WY2017.', depth, 'cm.model.r2.results.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMnorm_Tmodel_results_2017$peak2017$dates), SMnorm_Tmodel_results_2017$peak2017$r2.model, xaxt='n', type = 'l', xlab='', ylab = bquote('r'^2*' values'), ylim=c(0,1), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 1, cex.axis=0.9)#, main=paste(depth, 'cm model results in', yr, '(wet year)'), col='black')
lines(as.Date(Tmodel_results_2017$peak2017$dates), Tmodel_results_2017$peak2017$r2.model, col ='red', lty=2)
lines(as.Date(SMnorm_model_results_2017$peak2017$dates), SMnorm_model_results_2017$peak$r2.model, col ='blue', lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topright", legend=(c("peak forage vs. SMnorm + T model", 'peak forage vs. T model', "peak forage vs. SMnorm model")), lty=c(1, 2, 2), col=c('black', 'red', 'blue'), inset = 0.01, cex=1.1)
axis(side = 4, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = c('0', '10', '20', '30', '40'), cex.axis=0.9)
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. / 100, type='s', col='lightblue', cex=0.5)
dev.off()

#significant SM associations 2017
#make combined plots of significant 2017 associations
depth <- 7
SMnorm_T2model_results_2017 <- lapply(list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2017 <- '2017-05-01'
SMnorm_T2model_results_2017 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_T2model_results_2017, clipdates2017, SIMPLIFY = FALSE)
names(SMnorm_T2model_results_2017) <- list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2017*', depth, 'cm*')))
names(SMnorm_T2model_results_2017)
names(SMnorm_T2model_results_2017) <- c('peak2017')#c('Apr', 'Feb', 'Mar', 'May')
#min(unlist(lapply(model_results_2017, function(x) min_modified(x$slope.SM))), na.rm = TRUE)

#2017 SM plot
png(file = file.path(resultsFigures, 'finals', 'SMnorm_T2_model', paste0('WY2017.SM.', depth, 'cm.forage_SMnorm+T2model.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMnorm_T2model_results_2017$peak2017$dates[SMnorm_T2model_results_2017$peak2017$p.value.SM < 0.05 & SMnorm_T2model_results_2017$peak2017$p.value.model < 0.05]), SMnorm_T2model_results_2017$peak2017$slope.SM[SMnorm_T2model_results_2017$peak2017$p.value.SM < 0.05 & SMnorm_T2model_results_2017$peak2017$p.value.model < 0.05], xaxt='n', xlab='', ylab = bquote('kg ha'^-1*' association of +1 std dev soil moisture'), ylim=c(min(unlist(lapply(SMnorm_T2model_results_2017, function(x) min_modified(x$slope.SM))), na.rm=TRUE), max(unlist(lapply(SMnorm_T2model_results_2017, function(x) max_modified(x$slope.SM))), na.rm=TRUE)), xlim = as.Date(c('2016-12-01', '2017-05-01')), pch = 21, bg='grey')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2016/12/1'), to = as.Date('2017/5/1'), by='months'), format = '%m/%d/%y')
legend("topleft", legend='soil moisture association (p<0.05) with peak forage \nafter accounting for non-linear association with soil temperature', pch=21, pt.bg='grey', inset = 0.01, cex=1.1)
text(x=as.Date('2017/1/10'), y= -2000, label='relatively dry (and warm) during storms = \nmore peak forage', cex=1.2)
#abline(v=as.Date(c('2017-02-21', '2017-03-20'))) #only 2.8 mm precip over 30 days
axis(side = 4, at = c(-8000, -6000, -4000, -2000, 0), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 200 - 8000, type='s', col='lightblue', cex=0.5)
dev.off()

#significant SM associations 2018
depth <- 22
SMnorm_T2model_results_2018 <- lapply(list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- '2018-04-15'
SMnorm_T2model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMnorm_T2model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMnorm_T2model_results_2018) <- list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMnorm_T2model_results_2018)
names(SMnorm_T2model_results_2018) <- c('peak2018')

#2018 SM plot
png(file = file.path(resultsFigures, 'finals', 'SMnorm_T2_model', paste0('WY2018.SM.', depth, 'cm.forage_SMnorm+T2model.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMnorm_T2model_results_2018$peak2018$dates[SMnorm_T2model_results_2018$peak2018$p.value.SM < 0.05]), SMnorm_T2model_results_2018$peak2018$slope.SM[SMnorm_T2model_results_2018$peak2018$p.value.SM < 0.05], xaxt='n', xlab='', ylab = bquote('kg ha'^-1*' association of +1 std dev soil moisture'), ylim=c(min(unlist(lapply(SMnorm_T2model_results_2018, function(x) min_modified(x$slope.SM))), na.rm=TRUE), max(unlist(lapply(SMnorm_T2model_results_2018, function(x) max_modified(x$slope.SM))), na.rm=TRUE)), xlim = as.Date(c('2017-12-01', '2018-04-15')), pch = 21, bg='grey')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/4/15'), by='months'), format = '%m/%d/%y')
legend("topright", legend='soil moisture association (p<0.05) with peak forage \nafter accounting for non-linear association with soil temperature', pch=21, pt.bg='grey', inset = 0.01, cex=1.1)
text(x=as.Date('2018/1/15'), y= 700, label='relatively greater wet-up during January storms = more peak forage', cex=1.2)
text(x=as.Date('2018/2/15'), y= -500, label='relatively greater drawdown after storms = more peak forage', cex=1.2)
axis(side = 4, at = c(-1200, -500, 200, 900, 1600), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 70 - 1200, type='s', col='lightblue', cex=0.5)
dev.off()

#2018 SM plot from SMabs + T2 model
depth <- 22
SMabs_T2model_results_2018 <- lapply(list.files(file.path(results, 'SMabs_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')), full.names = TRUE), read.csv, stringsAsFactors=FALSE)
clipdates2018 <- '2018-04-15'
SMabs_T2model_results_2018 <- mapply(function(x, y) x <- x[as.Date(x$dates) <= y, ], SMabs_T2model_results_2018, clipdates2018, SIMPLIFY = FALSE)
names(SMabs_T2model_results_2018) <- list.files(file.path(results, 'SMnorm_T2_model_results'), pattern = glob2rx(paste0('*peak2018*', depth, 'cm*')))
names(SMabs_T2model_results_2018)
names(SMabs_T2model_results_2018) <- c('peak2018')
#plot it
png(file = file.path(resultsFigures, 'finals', 'SMabs_T2_model', paste0('WY2018.SM.', depth, 'cm.forage_SMabs+T2model.png')), family = 'Book Antiqua', width = 1200, height = 400, units = 'px', res=100)
par(mar=c(2, 4.5, 1, 4.5))
plot(as.Date(SMabs_T2model_results_2018$peak2018$dates[SMabs_T2model_results_2018$peak2018$p.value.SM < 0.05 & SMabs_T2model_results_2018$peak2018$p.value.model < 0.05]), SMabs_T2model_results_2018$peak2018$slope.SM[SMabs_T2model_results_2018$peak2018$p.value.SM < 0.05 & SMabs_T2model_results_2018$peak2018$p.value.model < 0.05] / 100, xaxt='n', xlab='', ylab = bquote('kg ha'^-1*' association of +0.01 vol. soil moisture'), ylim=c(min(unlist(lapply(SMabs_T2model_results_2018, function(x) min_modified(x$slope.SM) / 100)), na.rm=TRUE), max(unlist(lapply(SMabs_T2model_results_2018, function(x) max_modified(x$slope.SM) / 100)), na.rm=TRUE)), xlim = as.Date(c('2017-12-01', '2018-04-15')), pch = 21, bg='grey')
abline(1, 0, lty=2)
axis.Date(side = 1, at=seq.Date(from = as.Date('2017/12/1'), to = as.Date('2018/4/15'), by='months'), format = '%m/%d/%y')
legend("topleft", legend='soil moisture association (p<0.05) with peak forage \nafter accounting for non-linear association with soil temperature', pch=21, pt.bg='grey', inset = 0.01, cex=1.1)
text(x=as.Date('2018/2/15'), y= -30, label='lower volumetric soil moisture = more peak forage \n apparent trend in association suggests greater drawdown during dry period = more peak forage', cex=1.2)
axis(side = 4, at = c(-88, -71, -54, -37, -20), labels = c('0', '10', '20', '30', '40'))
mtext("mm precipitation per day", side=4, line=2.5)
lines(as.Date(precip_data$Date, '%m/%d/%Y'), precip_data$Rainfall..mm. * 1.7 - 88, type='s', col='lightblue', cex=0.5)
dev.off()
