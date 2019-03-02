terrainDir <- 'C:/Users/smdevine/Desktop/rangeland project/results/terrain_characteristics'
list.files(terrainDir)
Mar2017_terrain <- read.csv(file.path(terrainDir, 'terrain_3m_filtered_Mar2017.csv'), stringsAsFactors = FALSE)
Mar2017_terrain
Nov2016_terrain <- read.csv(file.path(terrainDir, 'terrain_3m_filtered_Nov2016.csv'), stringsAsFactors = FALSE)
summary(lm(Mar2017_terrain$elevation ~ Nov2016_terrain$elevation)) #0.9943 
summary(lm(Mar2017_terrain$aspect ~ Nov2016_terrain$aspect)) #0.9991
summary(lm(Mar2017_terrain$slope ~ Nov2016_terrain$slope)) #0.9942
summary(lm(Mar2017_terrain$TCI ~ Nov2016_terrain$TCI)) #0.9547; p-value: 5.152e-11
summary(lm(Mar2017_terrain$curvature_mean ~ Nov2016_terrain$curvature_mean)) #0.9638
summary(lm(Mar2017_terrain$curvature_plan ~ Nov2016_terrain$curvature_plan)) #0.942
summary(lm(Mar2017_terrain$curvature_profile ~ Nov2016_terrain$curvature_profile)) #0.9436
