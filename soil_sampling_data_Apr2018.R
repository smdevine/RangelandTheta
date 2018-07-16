WorkDir <- 'C:/Users/smdevine/Desktop/rangeland project/soils_data'
soils.data <- data.frame(point_no=c(seq(from=1, to=105), seq(from=1, to=105)), depth_cm=c(rep('0_10', 105), rep('10_30', 105)), depth_code=as.integer(c(rep(1, 105), rep(2, 105))), stringsAsFactors = FALSE)
soils.data$sample_code <- paste0(soils.data$point_no, '_', soils.data$depth_code)
soils.data <- soils.data[order(soils.data$point_no, soils.data$depth_code),]
lapply(soils.data, class)
head(soils.data)
write.csv(soils.data, file.path(WorkDir, 'soils_datasheet.4.23.18.csv'), row.names = FALSE)
