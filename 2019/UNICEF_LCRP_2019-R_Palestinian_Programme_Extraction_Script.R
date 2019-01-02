
library("ggplot2")
opt <- options(warn = -1)
rm(list = ls())

library("activityinfo")

# Replace 'NA' with the numeric identifier of your database (e.g. 6352):
database.id <- 10167

# Uncomment the following command if you want to log in manually, leave commented
# out if you have stored your login credentials on your local machine.

if (is.na(database.id)) {
  stop("you forgot to set the database identifier at the top of this script!")
}
# values <- getDatabaseValueTable(database.id, col.names = c("Funded by" = "Funded_by"))
values <- getDatabaseValueTable(database.id)

# Generating values dataframe 
cat("Done. The results are in a data frame called 'values'.\n")

# Subsetting Values to UNICEF interventions dataset only 
values$start_date <-strftime(values$start_date,"%Y-%m")

# Creating a folder and export the extraction as a csv file to "DBs" folder after creating it in case it doesn't exist
db.all.lcrp <- values

outfilname<- paste('../', Sys.Date(), "_Sector_PPL.csv", sep="")
write.csv(db.all.lcrp, outfilname, row.names=FALSE)
