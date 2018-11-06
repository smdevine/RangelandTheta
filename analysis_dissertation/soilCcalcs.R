mainDir <- 'C:/Users/smdevine/Desktop/rangeland project'
soilCDir <- 'C:/Users/smdevine/Desktop/rangeland project/soils_data/soil C'
list.files(soilCDir)
soilmasses <- read.csv(file.path(soilCDir, 'Camatta_soilCN_masses.csv'), stringsAsFactors = FALSE)
dim(soilmasses)
head(soilmasses)
soilCN <- read.csv(file.path(soilCDir, 'Camatta_soilCN_data.csv'), stringsAsFactors = FALSE)
head(soilCN)
soilmasses$tray.ID <- paste0(soilmasses$tray, '-', soilmasses$well.plate)
soilCN <- merge(soilCN, soilmasses, by='tray.ID')
dim(soilCN) #372 rows from draft dataset
length(unique(soilCN$sample.ID)) #213 unique
water_contents <- read.csv(file.path(soilCDir, 'airdry_soilmoisture.csv'), stringsAsFactors = FALSE)
soilCN <- merge(soilCN, water_contents, by='sample.ID')
dim(soilCN) #369 (dropped analyses with W in sample.ID)
length(unique(soilCN$sample.ID)) #now 210 unique
soilCtotN <- soilCN[soilCN$analysis.type=='TC',]
soilCorgN <- soilCN[soilCN$analysis.type=='OC',]
dim(soilCtotN) #211, 1 more than expected
tapply(soilCtotN$C.mg, soilCtotN$sample.ID, length)#57_1 has two data points
soilCtotN$totC.percent <- 100 * soilCtotN$C.mg / soilCtotN$mass.mg
#soilCtotN$totC.percent <- round(100 * (soilCtotN$totC.percent / (100 - soilCtotN$H2O.perc.Adbasis)), 3)
hist(soilCtotN$totC.percent)
summary(soilCtotN$totC.percent)
soilCtotN$totN.percent <- 100 * soilCtotN$N.mg / soilCtotN$mass.mg
#soilCtotN$totN.percent <- round(100 * (soilCtotN$totN.percent / (100 - soilCtotN$H2O.perc.Adbasis)), 4)
soilCtotN[soilCtotN$sample.ID=='57_1',] #both ok
hist(soilCtotN$totN.percent)
summary(soilCtotN$totN.percent)
which(soilCtotN$totC.percent > 4)
soilCtotN[which(soilCtotN$totC.percent > 3),]
dim(soilCorgN) #158 from draft dataset
length(unique(soilCorgN$sample.ID)) #156 are unique, so 54 not yet analyzed
dim(soilCorgN[!grepl('W', soilCorgN$sample.ID), ]) #158 that were dried before weighing into tins
soilCorgN$orgC.percent <- round(100 * soilCorgN$C.mg / soilCorgN$mass.mg, 3)
soilCorgN$totN.percent.v2 <- round(100 * soilCorgN$N.mg / soilCorgN$mass.mg, 4)
hist(soilCorgN$orgC.percent)
summary(soilCorgN$orgC.percent)
soilCorgN$sample.ID
soilC_all <- merge(soilCtotN[,c('sample.ID', 'totC.percent', 'totN.percent')], soilCorgN[!grepl('W', soilCorgN$sample.ID),c('sample.ID', 'orgC.percent', 'totN.percent.v2')], by='sample.ID') #W in org C samples denoted those that had not been dried at 60C first before immediately weighing
dim(soilC_all) #159 rows
length(unique(soilC_all$sample.ID)) #156 unique
soilC_all$inorgC.percent <- soilC_all$totC.percent - soilC_all$orgC.percent
soilC_all$orgC.to.totC <- round(soilC_all$orgC.percent / soilC_all$totC.percent, 3)
summary(soilC_all$inorgC.percent)
hist(soilC_all$orgC.to.totC)
hist(soilC_all$orgC.to.totC[grepl('_1', soilC_all$sample.ID)])
hist(soilC_all$orgC.to.totC[grepl('_2', soilC_all$sample.ID)])
sum(soilC_all$inorgC.percent < 0)
soilC_all[soilC_all$inorgC.percent < 0,]
plot(soilC_all$totN.percent, soilC_all$totN.percent.v2)
summary(lm(totN.percent.v2 ~ totN.percent, data = soilC_all))
abline(0, 1, lty=2)
#abline(lm(totN.percent.v2 ~ totN.percent, data = soilC_all), lty=2)
text(soilC_all$totN.percent, soilC_all$totN.percent.v2, labels = soilC_all$sample.ID, pos=1, offset = 0.3)
soilC_all$N.error.percent <- round(100 * (soilC_all$totN.percent.v2 - soilC_all$totN.percent) / soilC_all$totN.percent, 2)
summary(soilC_all$N.error.percent)
hist(soilC_all$N.error.percent)
sum(abs(soilC_all$N.error.percent) > 20) #8 greater than 20% error
soilC_all[order(soilC_all$N.error.percent), ]
soilC_all$N.diff.abs <- abs(soilC_all$totN.percent - soilC_all$totN.percent.v2)
hist(soilC_all$N.diff.abs[soilC_all$N.diff.abs < 0.1])
mean(soilC_all$totN.percent) #mean is 0.115 %N
sum(soilC_all$N.diff.abs > 0.015) #24 greater
sum(soilC_all$N.diff.abs > 0.02) #16 greater than this
#N_QC_summary <- summary(lm(totN.percent.v2 ~ totN.percent, data = soilC_all))
#N_QC_summary$residuals[order(N_QC_summary$residuals)]
soilC_all$CaCO3.percent <- round((100/12) * soilC_all$inorgC.percent, 3)
summary(soilC_all$CaCO3.percent)
soilC_all[order(soilC_all$CaCO3.percent), ] #two are 45_2
soilC_all[soilC_all$sample.ID=='45_2', ]
soilCorgN[soilCorgN$sample.ID=='45_2', ]
soilC_reruns <- soilC_all[abs(soilC_all$N.error.percent) > 15,]
dim(soilC_reruns)
#soilC_reruns$N.diff.abs <- abs(soilC_reruns$totN.percent - soilC_reruns$totN.percent.v2)
soilC_reruns[order(abs(soilC_reruns$N.error.percent)),]
sum(soilC_reruns$N.diff.abs > 0.02) #9 are greater
sum(soilC_reruns$N.diff.abs > 0.015) #14 are greater

soilC_reruns.v2 <- soilC_all[soilC_all$N.diff.abs > 0.01 & abs(soilC_all$N.error.percent) > 15,] #if more than 15% relative difference with absolute difference at least 0.010 %N
dim(soilC_reruns.v2)
sum(abs(soilC_reruns.v2$N.error.percent) > 20)
soilC_reruns.v2
write.csv(soilC_reruns.v2, file.path(soilCDir, 'soilC_reruns.v2.csv'), row.names = FALSE)
summary(soilC_reruns.v2$CaCO3.percent)
summary(soilC_all$CaCO3.percent)
soilC_0_10cm <- soilC_all[grepl('_1', soilC_all$sample.ID), ]
soilC_10_30cm <- soilC_all[grepl('_2', soilC_all$sample.ID), ]
hist(soilC_0_10cm$orgC.percent)
dim(soilC_0_10cm)
length(unique(soilC_0_10cm$sample.ID))
tapply(soilC_0_10cm$totC.percent, soilC_0_10cm$sample.ID, length) #57_1 and 58_1 are reps
rownames(soilC_0_10cm) <- 1:nrow(soilC_0_10cm)
soilC_0_10cm[soilC_0_10cm$sample.ID=='57_1',]
soilC_0_10cm[soilC_0_10cm$sample.ID=='58_1',]
#temp fix to get rid of duplicate IDs
soilC_0_10cm <- soilC_0_10cm[-c(36, 39), ]
hist(soilC_10_30cm$orgC.percent)
write.csv(soilC_0_10cm, file.path(soilCDir, 'soilC_0_10cm.draft.csv'), row.names = FALSE)
write.csv(soilC_10_30cm, file.path(soilCDir, 'soilC_10_30cm.draft.csv'), row.names = FALSE)

#merge with spatial dataset
library(raster)
sampling_pts <- shapefile(file.path(mainDir, 'sampling points 2018', 'soil_sampling_points.shp'))
biomass_Apr2017 <- raster(file.path(mainDir, 'sampling strategy April 2018', 'Biomass_2017-04-10_APAR.tif'))
sampling_pts$point_no <- as.integer(gsub('point', '', sampling_pts$Comment))
soilC_0_10cm$point_no <- as.integer(gsub('_1', '', soilC_0_10cm$sample.ID))
soilC_0_10cm_shp <- merge(sampling_pts, soilC_0_10cm, by='point_no')
plot(biomass_Apr2017)
plot(soilC_0_10cm_shp, cex=soilC_0_10cm_shp$orgC.percent, pch=1, add=T)
soilC_0_10cm_shp$Apr2017biomass <- extract(biomass_Apr2017, coordinates(soilC_0_10cm_shp)[,1:2], buffer=3, fun=mean)
plot(soilC_0_10cm_shp$orgC.percent, soilC_0_10cm_shp$Apr2017biomass, col=ifelse(soilC_0_10cm_shp$N.error.percent > 15, 'red', 'black'))
text(soilC_0_10cm_shp$orgC.percent, soilC_0_10cm_shp$Apr2017biomass, labels = soilC_0_10cm_shp$point_no, pos=1, offset =0.3)
abline(lm(Apr2017biomass ~ orgC.percent, data = soilC_0_10cm_shp), lty=2)
summary(lm(Apr2017biomass ~ orgC.percent, data = soilC_0_10cm_shp))
