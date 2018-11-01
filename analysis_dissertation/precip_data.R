library(extrafont)
library(extrafontdb)
loadfonts()
results <- 'C:/Users/smdevine/Desktop/rangeland project/results'
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
png(file = file.path(results, 'figures', 'finals', 'precip_summary',  'WY2017_2018_precip.png'), family = 'Book Antiqua', width = 800, height = 650, units = 'px', res=100)
par(mar=c(3, 5, 1, 1))
barplot(as.matrix(precip_summary), beside = TRUE, col=c('blue', 'red3'), ylab = 'Precipitation (mm)', legend.text = c('WY2017 (wet)', 'WY2018 (dry)'), cex.axis = 1.3, cex.names = 1.3, cex.lab = 1.3, args.legend = list(x="topleft", inset=0.1, cex=1.4))
dev.off()
precip_summary
