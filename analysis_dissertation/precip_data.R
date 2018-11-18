library(extrafont)
library(extrafontdb)
loadfonts()
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
climateDir <- 'C:/Users/smdevine/Desktop/rangeland project/climate_data'
PRISM_precip <- read.csv(file.path(climateDir, 'PRISM_ppt_provisional_Camatta.csv'), stringsAsFactors = FALSE)
precip_data <- read.csv(file.path('C:/Users/smdevine/Desktop/rangeland project/climate_data/Camatta_precip_WY2017_2018.csv'), stringsAsFactors = FALSE)
#WY 2018 was 10/1/2017-4/25/2018 per weather station
#WY 2017 was 10/27/2017-4/20/2017 per weather station and so missed one mid-October precip event which was before soil moisture sensors were installed
#there were no late April rains in either year
precip_data$Date
precip_data$Date <- as.Date(precip_data$Date, format = '%m/%e/%Y')
precip_data$Month <- format.Date(precip_data$Date, '%b')
precip_data$Year <- format.Date(precip_data$Date, '%Y')
precip_data$WY <- ifelse(precip_data$Date < as.Date('2017-07-01'), 2017, 2018)
precip_summary <- as.data.frame(tapply(precip_data$Rainfall..mm., list(precip_data$WY, precip_data$Month), sum))
Oct_15_16_2016 <- 25.4 * (0.13 + 0.02) #from Paso Robles station
names(precip_summary)
precip_summary <- precip_summary[ ,c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')]
precip_summary$Oct[1] <- precip_summary$Oct[1] + Oct_15_16_2016
precip_summary$TOTAL <- apply(precip_summary, 1, sum)
precip_summary
tiff(file = file.path(results, 'figures', 'finals', 'precip_summary',  'WY2017_2018_precip.tif'), family = 'Times New Roman', pointsize = 11, width = 6.5, height = 4.5, units = 'in', res=150)
par(mar=c(3, 5, 1, 1))
barplot(as.matrix(precip_summary), beside = TRUE, col=c('blue', 'red3'), ylab = 'Precipitation (mm)', legend.text = c('WY2017 (wet)', 'WY2018 (dry)'), cex.axis = 1, cex.names = 1, cex.lab = 1, args.legend = list(x="topleft", inset=0.1, cex=1))
dev.off()
precip_summary

#PRISM precip
head(PRISM_precip)
lapply(PRISM_precip, class)
PRISM_precip$Date_false <- paste0(PRISM_precip$Date, '-01')
PRISM_precip$Date_false <- as.Date(x=PRISM_precip$Date_false, format = '%Y-%m-%d')
PRISM_precip$Month <- as.integer(format.Date(PRISM_precip$Date_false, '%m'))
PRISM_precip$Year <- as.integer(format.Date(PRISM_precip$Date_false, '%Y'))
PRISM_precip$WY <- ifelse(PRISM_precip$Month <= 6, PRISM_precip$Year, PRISM_precip$Year+1)
WY_summary <- data.frame(year=unique(PRISM_precip$WY), precip_WY_mm=as.numeric(tapply(PRISM_precip$ppt..inches., PRISM_precip$WY, function(x) sum(x) * 25.4)))
mean(WY_summary$precip_WY_mm[WY_summary$year %in% 2001:2018])
plot(WY_summary$year[-c(1,nrow(WY_summary))], WY_summary$precip_WY_mm[-c(1,nrow(WY_summary))], type = 'b')
abline(h=mean(WY_summary$precip_WY_mm), lty=2, col='red')
PRISM_precip_GS <- PRISM_precip[PRISM_precip$Month <= 5 | PRISM_precip$Month >= 10, ]
WY_summary_GS <- data.frame(year=unique(PRISM_precip_GS$WY), precip_GS_mm=as.numeric(tapply(PRISM_precip_GS$ppt..inches., PRISM_precip_GS$WY, function(x) sum(x) * 25.4)), mean_T_GS = as.numeric(tapply(PRISM_precip_GS$tmean..degrees.F., PRISM_precip_GS$WY, function(x) (mean(x) - 32)*(5/9))), max_T_GS = as.numeric(tapply(PRISM_precip_GS$tmax..degrees.F., PRISM_precip_GS$WY, function(x) (mean(x) - 32)*(5/9))), min_T_GS= as.numeric(tapply(PRISM_precip_GS$tmin..degrees.F., PRISM_precip_GS$WY, function(x) (mean(x) - 32)*(5/9))))
WY_summary_GS <- WY_summary_GS[-c(1,nrow(WY_summary_GS)), ]
tail(WY_summary_GS)
plot(WY_summary_GS$year, WY_summary_GS$precip_GS_mm, type = 'b')
abline(h=mean(WY_summary_GS$precip_GS_mm), lty=2, col='red')
plot(WY_summary_GS$year, WY_summary_GS$mean_T_GS, type='b')
plot(WY_summary_GS$year, WY_summary_GS$max_T_GS, type='b')
plot(WY_summary_GS$year, WY_summary_GS$min_T_GS, type='b')

#make P vs. T plot
tiff(file = file.path(results, 'figures', 'finals', 'precip_summary',  'WY1896_2018_PRISM_summary.v2.tif'), family = 'Times New Roman', pointsize = 11, width = 6.5, height = 6, units = 'in', res=150)
par(mar=c(4, 4, 1, 1))
plot(WY_summary_GS$mean_T_GS, WY_summary_GS$precip_GS_mm, col=ifelse(WY_summary_GS$year>=1980, 'red', 'blue'), pch=19, ylab = 'study site mean growing season precipitation, WY1896-WY2018 (Oct - May, mm)', xlab=expression('study site growing season mean temperature, WY1896-WY2018 (Oct-May '*degree*'C)'))
#abline(h=summary(WY_summary_GS$precip_GS_mm)[5], lty=2, col='grey')
abline(h=quantile(WY_summary_GS$precip_GS_mm, probs=c(0.1, 0.5, 0.9)), lty=2, col='grey')
abline(v=median(WY_summary_GS$mean_T_GS), lty=2, col='grey')
points(WY_summary_GS$mean_T_GS[WY_summary_GS$year %in% 2000:2018], WY_summary_GS$precip_GS_mm[WY_summary_GS$year %in% 2000:2018], cex=1.5)
text(WY_summary_GS$mean_T_GS[WY_summary_GS$year %in% 2011:2018], WY_summary_GS$precip_GS_mm[WY_summary_GS$year %in% 2011:2018], labels=2011:2018, pos=c(3, 3, 1, 3, 2, 2, 3, 4), offset = 0.4)
legend('topleft', legend=c('pre-WY1980', 'post-WY1980', 'post-WY2000'), pch=c(19, 19, 1), col = c('blue', 'red', 'black'), inset = 0.01, pt.cex=c(1, 1, 1.5))
text(8.8, 455, '90th percentile', adj=c(0,0))
text(8.8, 275, 'median precipitation', adj=c(0,0))
text(8.8, 170, '10th percentile', adj=c(0,0))
dev.off()

#make P vs. T plot v2 
tiff(file = file.path(results, 'figures', 'finals', 'precip_summary',  'WY1896_2018_PRISM_summary.tif'), family = 'Times New Roman', pointsize = 11, width = 6.5, height = 6, units = 'in', res=150)
par(mar=c(4, 4, 1, 1))
plot(WY_summary_GS$mean_T_GS, WY_summary_GS$precip_GS_mm, col=ifelse(WY_summary_GS$year < 1980, 'blue', WY_summary_GS$year < 2000, 'black', 'red'), pch=ifelse(WY_summary_GS$year < 1980, 1, ifelse(WY_summary_GS$year < 2000, 19, 21)), ylab = 'study site mean growing season precipitation, WY1896-WY2018 (Oct - May, mm)', xlab=expression('study site growing season mean temperature, WY1896-WY2018 (Oct-May '*degree*'C)'))
#abline(h=summary(WY_summary_GS$precip_GS_mm)[5], lty=2, col='grey')
abline(h=quantile(WY_summary_GS$precip_GS_mm, probs=c(0.1, 0.5, 0.9)), lty=2, col='grey')
abline(v=median(WY_summary_GS$mean_T_GS), lty=2, col='grey')
points(WY_summary_GS$mean_T_GS[WY_summary_GS$year %in% 2000:2018], WY_summary_GS$precip_GS_mm[WY_summary_GS$year %in% 2000:2018], cex=1.5)
text(WY_summary_GS$mean_T_GS[WY_summary_GS$year %in% 2011:2018], WY_summary_GS$precip_GS_mm[WY_summary_GS$year %in% 2011:2018], labels=2011:2018, pos=c(3, 3, 1, 3, 2, 2, 3, 4), offset = 0.4)
legend('topleft', legend=c('pre-WY1980', 'post-WY1980', 'post-WY2000'), pch=c(19, 19, 1), col = c('blue', 'red', 'black'), inset = 0.01, pt.cex=c(1, 1, 1.5))
text(8.8, 455, '90th percentile', adj=c(0,0))
text(8.8, 275, 'median precipitation', adj=c(0,0))
text(8.8, 170, '10th percentile', adj=c(0,0))
dev.off()

sum(WY_summary_GS$precip_GS_mm < mean(WY_summary_GS$precip_GS_mm)) / nrow(WY_summary_GS) #59% below average overall
sum(WY_summary_GS$precip_GS_mm[WY_summary_GS$year >=1970] < mean(WY_summary_GS$precip_GS_mm)) / length(WY_summary_GS$precip_GS_mm[WY_summary_GS$year >=1970]) #59% below average
count <- 0
drought.counter <- 1
start_yr = integer()
end_yr =integer()
drought.lengths <- integer()
drought.severity <- numeric()
P.deficit <- 0
for (i in seq_along(WY_summary_GS$year)) {
  if (count == 0 & WY_summary_GS$precip_GS_mm[i] < mean(WY_summary_GS$precip_GS_mm)) {
    print(WY_summary_GS$year[i])
    count <- count + 1
    start_yr[drought.counter] <- WY_summary_GS$year[i]
    drought.counter <- drought.counter + 1
    P.deficit <- mean(WY_summary_GS$precip_GS_mm) - WY_summary_GS$precip_GS_mm[i]
    next
  } else if (WY_summary_GS$precip_GS_mm[i] < mean(WY_summary_GS$precip_GS_mm)) {
      count <- count + 1
      P.deficit <- mean(WY_summary_GS$precip_GS_mm) - WY_summary_GS$precip_GS_mm[i] + P.deficit
      next
  } else if (count == 0) {
      next 
  } else {
      drought.lengths[drought.counter - 1] <- count
      end_yr[drought.counter - 1] <- WY_summary_GS$year[i]
      drought.severity[drought.counter - 1] <- P.deficit
      P.deficit <- 0
      count <- 0
  }
}
end_yr
start_yr
drought.lengths
start_yr <- start_yr[-length(start_yr)]
drought.severity
drought_summary <- data.frame(drought_start = start_yr, drought_end= end_yr, drought_length=drought.lengths, drought.severity=drought.severity)
drought_summary
plot(drought_summary$drought_end, drought_summary$drought.severity, type = 'p')
library(Kendall)
MannKendall(drought.severity)
MannKendall(drought.lengths)
