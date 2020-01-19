library("ggplot2")
opt <- options(warn = -1)
rm(list = ls())

library("activityinfo")

# database.id <- 10899
database.id <- "ck2yrizmo2"

values <- getDatabaseRecordTable(database.id, as.single.table = TRUE)
View(values)

outfilname<- paste('../', Sys.Date(), "activityinfo_v4.csv", sep="")
write.csv(values, outfilname, row.names=FALSE)