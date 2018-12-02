forage_data_8018A <- read.csv('C:/Users/smdevine/Desktop/rangeland project/forage_data_8018Appendix/8018 Appendix A.csv', stringsAsFactors = FALSE, na.strings = c('n/a', 'n/a/'))
head(forage_data_8018A)
unique(lapply(forage_data_8018A[,2:ncol(forage_data_8018A)], class))
apply(forage_data_8018A[,2:ncol(forage_data_8018A)], 2, mean, na.rm=TRUE)
apply(forage_data_8018A, 2, function(x) sum(!is.na(x)))
lessthan10 <- which(as.numeric(apply(forage_data_8018A, 2, function(x) sum(!is.na(x)))) < 10)
forage_data_8018A_subset <- forage_data_8018A[,-lessthan10]
apply(forage_data_8018A_subset, 2, function(x) sum(!is.na(x)))
CV.forage.production <- as.data.frame(apply(forage_data_8018A_subset[,2:ncol(forage_data_8018A_subset)], 2, function(x) 100 * sd(x, na.rm = TRUE)/ mean(x, na.rm = TRUE)))
colnames(CV.forage.production) <- 'CV'
rownames(CV.forage.production)