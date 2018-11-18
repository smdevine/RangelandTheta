mainDir <- 'C:/Users/smdevine/Desktop/rangeland project'
soilCDir <- 'C:/Users/smdevine/Desktop/rangeland project/soils_data/soil C'
list.files(soilCDir)
soilmasses <- read.csv(file.path(soilCDir, 'Camatta_soilCN_masses.csv'), stringsAsFactors = FALSE)
dim(soilmasses) #498
head(soilmasses)
soilCN <- read.csv(file.path(soilCDir, 'Camatta_soilCN_data.csv'), stringsAsFactors = FALSE)
head(soilCN)
tail(soilCN)
soilmasses$tray.ID <- paste0(soilmasses$tray, '-', soilmasses$well.plate)
dim(soilCN) #498 samples analyzed (15 extra)
soilCN <- merge(soilCN, soilmasses, by='tray.ID')
dim(soilCN) #498
length(unique(soilCN$sample.ID)) #213 unique
water_contents <- read.csv(file.path(soilCDir, 'airdry_soilmoisture.csv'), stringsAsFactors = FALSE)
soilCN <- merge(soilCN, water_contents, by='sample.ID')
dim(soilCN) #495 (dropped analyses with W in sample.ID, which were orgC that had not been pre-dried prior to weighing)
length(unique(soilCN$sample.ID)) #now 210 unique
soilCtotN <- soilCN[soilCN$analysis.type=='TC',]
soilCorgN <- soilCN[soilCN$analysis.type=='OC',]
dim(soilCtotN) #231, 21 dups
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
soilCtotN[which(soilCtotN$totC.percent > 3), c('sample.ID', 'totC.percent', 'totN.percent')]
sum(duplicated(soilCtotN$sample.ID)) #21 duplicated
duplicated_samples <- which(soilCtotN$sample.ID %in% soilCtotN$sample.ID[duplicated(soilCtotN$sample.ID)])
soilCtotN[duplicated_samples, c('sample.ID', 'totC.percent', 'totN.percent', 'tray.ID')] #all were ok except 14_1
soilCtotN[soilCtotN$sample.ID=='14_1',] #1-B10 was erroneous
soilCtotN <- soilCtotN[-which(soilCtotN$tray.ID=='1-B10'), ]
dim(soilCtotN) #230 now
dim(soilCorgN) #264 from draft dataset
length(unique(soilCorgN$sample.ID)) #210 unique as expected
soilCorgN$orgC.percent <- round(100 * soilCorgN$C.mg / soilCorgN$mass.mg, 3)
soilCorgN$totN.percent.v2 <- round(100 * soilCorgN$N.mg / soilCorgN$mass.mg, 4)
hist(soilCorgN$orgC.percent)
summary(soilCorgN$orgC.percent)
duplicated_samples_orgC <- which(soilCorgN$sample.ID %in% soilCorgN$sample.ID[duplicated(soilCorgN$sample.ID)])
soilCorgN[duplicated_samples_orgC, c('sample.ID', 'orgC.percent', 'totN.percent.v2', 'tray.ID')]
QC_orgC <- data.frame(sample.ID =as.character(unique(soilCorgN$sample.ID)), orgC.error = as.numeric(tapply(soilCorgN$orgC.percent, soilCorgN$sample.ID, function(x) round(100*(max(x) - min(x))/mean(x), 1))))
QC_orgC[QC_orgC$orgC.error > 15,]
samples_to_review <- QC_orgC[QC_orgC$orgC.error > 15,]
samples_to_review[order(samples_to_review$orgC.error),]
check <- soilCorgN[soilCorgN$sample.ID %in% samples_to_review$sample.ID, c('sample.ID', 'orgC.percent', 'totN.percent.v2', 'tray.ID')]
check[order(check$sample.ID, check$tray.ID),]
#soilCorgN$sample.ID
colnames(soilCtotN)
colnames(soilCtotN)[2] <- 'tray.ID.TC'
colnames(soilCorgN)
colnames(soilCorgN)[2] <- 'tray.ID.OC'
soilC_all <- merge(soilCtotN[,c('sample.ID', 'tray.ID.TC', 'totC.percent', 'totN.percent')], soilCorgN[ ,c('sample.ID', 'tray.ID.OC', 'orgC.percent', 'totN.percent.v2')], by='sample.ID') #W in org C samples denoted those that had not been dried at 60C first before immediately weighing !grepl('W', soilCorgN$sample.ID)
dim(soilC_all) #299 rows
length(unique(soilC_all$sample.ID)) #210 unique, so 11 duplicates (2 OC, 9 TC as of 11/8/18)
soilC_all$inorgC.percent <- soilC_all$totC.percent - soilC_all$orgC.percent
soilC_all$orgC.to.totC <- round(soilC_all$orgC.percent / soilC_all$totC.percent, 3)
summary(soilC_all$inorgC.percent)
hist(soilC_all$orgC.to.totC)
hist(soilC_all$orgC.to.totC[grepl('_1', soilC_all$sample.ID)])
hist(soilC_all$orgC.to.totC[grepl('_2', soilC_all$sample.ID)])
sum(soilC_all$inorgC.percent < 0) #11 less than 0
soilC_all[soilC_all$inorgC.percent < 0,]
plot(soilC_all$totN.percent, soilC_all$totN.percent.v2)
summary(lm(totN.percent.v2 ~ totN.percent, data = soilC_all))
abline(0, 1, lty=2)
#abline(lm(totN.percent.v2 ~ totN.percent, data = soilC_all), lty=2)
text(soilC_all$totN.percent, soilC_all$totN.percent.v2, labels = soilC_all$sample.ID, pos=1, offset = 0.3)
soilC_all$N.error.percent <- round(100 * (soilC_all$totN.percent.v2 - soilC_all$totN.percent) / soilC_all$totN.percent, 2)
summary(soilC_all$N.error.percent)
hist(soilC_all$N.error.percent)
sum(abs(soilC_all$N.error.percent) > 20) #9 greater than 20% error
sum(abs(soilC_all$N.error.percent) > 15) #27 greater than 20% error
sum(abs(soilC_all$N.error.percent) > 10) #59 greater than 10% error

soilC_all$N.diff.abs <- abs(soilC_all$totN.percent - soilC_all$totN.percent.v2)
summary(soilC_all$N.diff.abs)
hist(soilC_all$N.diff.abs)
soilC_all$N.diff <- soilC_all$totN.percent - soilC_all$totN.percent.v2
summary(soilC_all$N.diff)
hist(soilC_all$N.diff.abs[soilC_all$N.diff.abs < 0.1])
soilC_all[order(abs(soilC_all$N.error.percent)), c('sample.ID', 'N.error.percent', 'N.diff')]
mean(soilC_all$totN.percent) #mean is 0.115 %N
sd(soilC_all$totN.percent) #sd is 0.05
sum(soilC_all$N.diff.abs > 0.01) #72 greater than 0.01% difference
sum(soilC_all$N.diff.abs > 0.015) #35 greater
sum(soilC_all$N.diff.abs > 0.02) #22 greater than this
#N_QC_summary <- summary(lm(totN.percent.v2 ~ totN.percent, data = soilC_all))
#N_QC_summary$residuals[order(N_QC_summary$residuals)]
soilC_all$CaCO3.percent <- round((100/12) * soilC_all$inorgC.percent, 3)
summary(soilC_all$CaCO3.percent)
soilC_all[order(soilC_all$CaCO3.percent), ] #two are 45_2
soilC_all[soilC_all$sample.ID=='45_2', ]
soilCorgN[soilCorgN$sample.ID=='45_2', ]
soilC_reruns <- soilC_all[abs(soilC_all$N.error.percent) > 15,]
dim(soilC_reruns)
soilC_reruns$N.diff.abs <- abs(soilC_reruns$totN.percent - soilC_reruns$totN.percent.v2)
soilC_reruns[order(abs(soilC_reruns$N.error.percent)),]
sum(soilC_reruns$N.diff.abs > 0.02) #11 are greater
sum(soilC_reruns$N.diff.abs > 0.015) #18 are greater
plot(soilC_all$orgC.percent, soilC_all$N.diff.abs)
soilC_reruns.v2 <- soilC_all[soilC_all$N.diff.abs > 0.01 & abs(soilC_all$N.error.percent) > 10,] #if more than 15% relative difference with absolute difference at least 0.010 %N
dim(soilC_reruns.v2) #48 re-runs
soilC_reruns.v2[abs(soilC_reruns.v2$N.error.percent) > 20, ]
sum(abs(soilC_reruns.v2$N.error.percent) > 20) #8 have relative difference of 20% or more
sum(abs(soilC_reruns.v2$N.error.percent) > 15) #25 have relative difference of 15% or more
soilC_reruns.v2
soilC_reruns.v2$depth <- ifelse(grepl('_2', soilC_reruns.v2$sample.ID), 2, 1)
soilC_reruns.v2$location <- gsub('_2', '', soilC_reruns.v2$sample.ID)
soilC_reruns.v2$location <- gsub('_1', '', soilC_reruns.v2$location)
soilC_reruns.v2$location <- as.integer(soilC_reruns.v2$location)
soilC_reruns.v2 <- soilC_reruns.v2[order(soilC_reruns.v2$location),]
write.csv(soilC_reruns.v2, file.path(soilCDir, 'soilC_reruns.v2.csv'), row.names = FALSE)
summary(soilC_reruns.v2$CaCO3.percent)
summary(soilC_all$CaCO3.percent)
colnames(soilC_all)
soilC_0_10cm <- soilC_all[grepl('_1', soilC_all$sample.ID), c("sample.ID", "totC.percent", "totN.percent", "orgC.percent", "totN.percent.v2", "inorgC.percent", "orgC.to.totC", "CaCO3.percent", 'N.error.percent')]
soilC_0_10cm[which(soilC_0_10cm$sample.ID %in% soilC_0_10cm$sample.ID[duplicated(soilC_0_10cm$sample.ID)]),]
soilC_0_10cm <- data.frame(sample.ID=unique(soilC_0_10cm$sample.ID), totC.percent=as.numeric(tapply(soilC_0_10cm$totC.percent, soilC_0_10cm$sample.ID, mean)), totN.percent=as.numeric(tapply(soilC_0_10cm$totN.percent, soilC_0_10cm$sample.ID, mean)), orgC.percent=as.numeric(tapply(soilC_0_10cm$orgC.percent, soilC_0_10cm$sample.ID, mean)), inorgC.percent=as.numeric(tapply(soilC_0_10cm$inorgC.percent, soilC_0_10cm$sample.ID, mean)), CaCO3.percent=as.numeric(tapply(soilC_0_10cm$CaCO3.percent, soilC_0_10cm$sample.ID, mean)), orgC.to.totC=as.numeric(tapply(soilC_0_10cm$orgC.to.totC, soilC_0_10cm$sample.ID, mean)), N.error.percent=as.numeric(tapply(soilC_0_10cm$N.error.percent, soilC_0_10cm$sample.ID, mean)))
soilC_10_30cm <- soilC_all[grepl('_2', soilC_all$sample.ID), ]
soilC_10_30cm <- data.frame(sample.ID=unique(soilC_10_30cm$sample.ID), totC.percent=as.numeric(tapply(soilC_10_30cm$totC.percent, soilC_10_30cm$sample.ID, mean)), totN.percent=as.numeric(tapply(soilC_10_30cm$totN.percent, soilC_10_30cm$sample.ID, mean)), orgC.percent=as.numeric(tapply(soilC_10_30cm$orgC.percent, soilC_10_30cm$sample.ID, mean)), inorgC.percent=as.numeric(tapply(soilC_10_30cm$inorgC.percent, soilC_10_30cm$sample.ID, mean)), CaCO3.percent=as.numeric(tapply(soilC_10_30cm$CaCO3.percent, soilC_10_30cm$sample.ID, mean)), orgC.to.totC=as.numeric(tapply(soilC_10_30cm$orgC.to.totC, soilC_10_30cm$sample.ID, mean)), N.error.percent=as.numeric(tapply(soilC_10_30cm$N.error.percent, soilC_10_30cm$sample.ID, mean)))
mean(soilC_0_10cm$totN.percent)
sd(soilC_0_10cm$totN.percent)
mean(soilC_0_10cm$N.diff.abs)
sum(abs(soilC_0_10cm$N.error.percent) > 10) #22 greater than 10% rel. diff
sum(abs(soilC_0_10cm$N.error.percent) > 5) #56 greater than 5% rel. diff
mean(soilC_10_30cm$totN.percent)
sd(soilC_10_30cm$totN.percent)
mean(soilC_10_30cm$N.diff.abs)
sum(abs(soilC_10_30cm$N.error.percent) > 10) #32 greater than 10% rel. diff
hist(soilC_0_10cm$orgC.percent)
dim(soilC_0_10cm)
dim(soilC_10_30cm)
length(unique(soilC_0_10cm$sample.ID))
tapply(soilC_0_10cm$totC.percent, soilC_0_10cm$sample.ID, length) #57_1 and 58_1 are reps
# rownames(soilC_0_10cm) <- 1:nrow(soilC_0_10cm)
# soilC_0_10cm[soilC_0_10cm$sample.ID=='57_1',]
# soilC_0_10cm[soilC_0_10cm$sample.ID=='58_1',]
#temp fix to get rid of duplicate IDs
# soilC_0_10cm <- soilC_0_10cm[-c(36, 39), ]
hist(soilC_10_30cm$orgC.percent)
hist(soilC_10_30cm$orgC.percent)
write.csv(soilC_0_10cm, file.path(soilCDir, 'soilC_0_10cm.draft.csv'), row.names = FALSE)
write.csv(soilC_10_30cm, file.path(soilCDir, 'soilC_10_30cm.draft.csv'), row.names = FALSE)

#merge with spatial dataset
library(raster)
sampling_pts <- shapefile(file.path(mainDir, 'sampling points 2018', 'soil_sampling_points.shp'))
biomass_Apr2017 <- raster(file.path(mainDir, 'sampling strategy April 2018', 'Biomass_2017-04-10_APAR.tif'))
sampling_pts$point_no <- as.integer(gsub('point', '', sampling_pts$Comment))
soilC_0_10cm$point_no <- as.integer(gsub('_1', '', soilC_0_10cm$sample.ID))
soilC_10_30cm$point_no <- as.integer(gsub('_2', '', soilC_10_30cm$sample.ID))
soilC_0_10cm_shp <- merge(sampling_pts, soilC_0_10cm, by='point_no')
soilC_10_30cm_shp <- merge(sampling_pts, soilC_10_30cm, by='point_no')
plot(biomass_Apr2017)
plot(soilC_0_10cm_shp, cex=soilC_0_10cm_shp$orgC.percent, pch=1, add=T)
soilC_0_10cm_shp$Apr2017biomass <- extract(biomass_Apr2017, coordinates(soilC_0_10cm_shp)[,1:2], buffer=1.5, fun=mean)
soilC_10_30cm_shp$Apr2017biomass <- extract(biomass_Apr2017, coordinates(soilC_10_30cm_shp)[,1:2], buffer=1.5, fun=mean)
plot(soilC_0_10cm_shp$orgC.percent, soilC_0_10cm_shp$Apr2017biomass, col=ifelse(soilC_0_10cm_shp$N.error.percent > 10, 'red', 'black'))
text(soilC_0_10cm_shp$orgC.percent, soilC_0_10cm_shp$Apr2017biomass, labels = soilC_0_10cm_shp$point_no, pos=1, offset =0.3)
abline(lm(Apr2017biomass ~ orgC.percent, data = soilC_0_10cm_shp), lty=2)
summary(lm(Apr2017biomass ~ orgC.percent, data = soilC_0_10cm_shp))
lm.result <- lm(Apr2017biomass ~ orgC.percent, data = soilC_0_10cm_shp)
lm.result$residuals[order(lm.result$residuals)]
plot(lm.result)
summary(lm(Apr2017biomass ~ inorgC.percent, data = soilC_0_10cm_shp))
summary(lm(Apr2017biomass ~ orgC.percent, data = soilC_10_30cm_shp))
summary(lm(Apr2017biomass ~ inorgC.percent, data = soilC_10_30cm_shp))
summary(lm(Apr2017biomass ~ orgC.percent + inorgC.percent, data = soilC_10_30cm_shp))
plot(soilC_10_30cm_shp$orgC.percent, soilC_10_30cm_shp$Apr2017biomass, col=ifelse(soilC_10_30cm_shp$N.error.percent > 5, 'red', 'black'))
abline(lm(Apr2017biomass ~ orgC.percent, data = soilC_10_30cm_shp))
text(soilC_10_30cm_shp$orgC.percent, soilC_10_30cm_shp$Apr2017biomass, labels=soilC_10_30cm_shp$point_no, pos=1, offset=0.3, cex=0.8)
plot(biomass_Apr2017)
plot(soilC_10_30cm_shp, cex=soilC_10_30cm_shp$orgC.percent*2, pch=1, add=T)
plot(lm(Apr2017biomass ~ orgC.percent, data = soilC_10_30cm_shp))
plot(soilC_0_10cm_shp$orgC.percent, soilC_10_30cm_shp$orgC.percent)
abline(glm(soilC_10_30cm_shp$orgC.percent ~ soilC_0_10cm_shp$orgC.percent))
summary(lm(soilC_10_30cm_shp$orgC.percent ~ soilC_0_10cm_shp$orgC.percent))
abline(0.5179, 0.1365, col='red')
plot(lm(soilC_10_30cm_shp$orgC.percent ~ soilC_0_10cm_shp$orgC.percent))
