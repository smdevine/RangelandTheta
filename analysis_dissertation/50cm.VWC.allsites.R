vwc.data.50cm <- read.csv('C:/Users/smdevine/Desktop/rangeland project/soilmoisture/all.sites.50cmVWC.csv', stringsAsFactors = FALSE)
lapply(vwc.data.50cm, class)
time.cols <- grepl('time', colnames(vwc.data.50cm))
vwc.cols <- grepl('VWC', colnames(vwc.data.50cm))
vwc.data.50cm[,time.cols] <- lapply(vwc.data.50cm[,time.cols], strptime, format="%m/%d/%Y %H:%M")
head(vwc.data.50cm$Adelaida.time)
lapply(vwc.data.50cm[,vwc.cols], summary)
vwc.data.50cm[,vwc.cols] <- lapply(vwc.data.50cm[,vwc.cols], function(x) ifelse(x < 0, NA, x))
lapply(vwc.data.50cm[,vwc.cols], summary)
plot(vwc.data.50cm$Adelaida.time, vwc.data.50cm$Adelaida.VWC.50cm) #full wet-up almost every winter
plot(vwc.data.50cm$Bitterwater.time, vwc.data.50cm$Bitterwater.VWC.50cm) #no wet-up until 2017
plot(vwc.data.50cm$Cambria.time, vwc.data.50cm$Cambria.VWC.50cm) #no wet-up until WY2016
plot(vwc.data.50cm$MorroBayN.time, vwc.data.50cm$MorroBayN.VWC.50cm) #may be erroneous during summer/fall 2016; wet-ups in 2015, 2016, and 2017
plot(vwc.data.50cm$Shandon.time, vwc.data.50cm$Shandon.VWC.50cm) #no wet-up until 2017
plot(vwc.data.50cm$SodaLake.time, vwc.data.50cm$SodaLake.VWC.50cm) #no wet-up until 2017

sum(vwc.data.50cm$Cambria.VWC.50cm < 0.05, na.rm = TRUE) #150
sum(vwc.data.50cm$Cambria.VWC.50cm < 0.1, na.rm = TRUE) #150
sum(vwc.data.50cm$Cambria.VWC.50cm < 0.13, na.rm = TRUE) #150
sum(vwc.data.50cm$Cambria.VWC.50cm < 0.15, na.rm = TRUE) #3010

#so, change less than
vwc.data.50cm$Cambria.time[which(vwc.data.50cm$Cambria.VWC.50cm < 0.05)] #2/16 to 2/26 in 2014 and 5/7 to 5/8 in 2015 
vwc.data.50cm$Cambria.VWC.50cm[5000:5300]
vwc.data.50cm$Cambria.VWC.50cm[4500:5000]
vwc.data.50cm$Cambria.VWC.50cm[5300:6000]#clearly there were problems
sum(vwc.data.50cm$MorroBayN.VWC.50cm < 0.05, na.rm = TRUE) #284
hist(vwc.data.50cm$MorroBayN.VWC.50cm)
which(vwc.data.50cm$MorroBayN.VWC.50cm < 0.05)
vwc.data.50cm$MorroBayN.time[18000:18500]
vwc.data.50cm$MorroBayN.VWC.50cm[18000:18500]
plot(vwc.data.50cm$MorroBayN.time, vwc.data.50cm$MorroBayN.VWC.50cm)

soil_moisture_dfs[[i]]$date <- format(soil_moisture_dfs[[i]]$Measurement.Time, "%Y%m%d")
