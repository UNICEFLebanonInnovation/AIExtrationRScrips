
library("ggplot2")
opt <- options(warn = -1)
rm(list = ls())

library("activityinfo")

# Replace 'NA' with the numeric identifier of your database (e.g. 6352):
database.id <- 10213

# Uncomment the following command if you want to log in manually, leave commented
# out if you have stored your login credentials on your local machine.

if (is.na(database.id)) {
  stop("you forgot to set the database identifier at the top of this script!")
}
values <- getDatabaseValueTable(database.id, col.names = c("Funded by" = "Funded_by"))

# Generating values dataframe 
cat("Done. The results are in a data frame called 'values'.\n")

# Subsetting Values to UNICEF interventions dataset only 
values$start_date <-strftime(values$start_date,"%Y-%m")
sector_values <- values
values <- values[values$Funded_by=="UNICEF",]

subset_data1 = list(
  values[["indicator.id"]],
  values[["indicator.name"]],
  values[["indicator.value"]],
  values[["indicator.units"]],
  values[["start_date"]],
  values[["partner.label"]],
  values[["partner.description"]],
  values[["location.adminlevel.governorate"]],
  values[["location.adminlevel.caza"]],
  values[["location.adminlevel.cadastral_area"]],
  values[["location.adminlevel.cadastral_area.code"]],
  values[["indicator.category"]],
  values[["location.latitude"]],
  values[["location.longitude"]],
  values[["location.alternate_name"]],
  values[["location.adminlevel.caza.code"]]
)

#  values[["Site Type"]]

names(subset_data1) <- c("Indicator_ID", "Indicator_Name", "Value", "Unit", "Month", "Partner_Name", "PartnerFullName", 
                         "Governorate", "Caza", "Cadaster", "CadCod","Indicator_Category", "Location_Latitude", 
                         "Location_Longitude", "locationName", "location.adminlevel.caza.code")
#"Site_Type"

subset_data2 = list(
  values[["indicator.id"]],
  values[["indicator.value"]],
  values[["location.adminlevel.governorate"]],
  values[["partner.label"]],
  values[["start_date"]],
  values[["indicator.name"]]
)

names(subset_data2) <- c("IndicatorID", "Value", "Governorate", "Partner_Name", "Month", "Indicator_Name")

# Creating a folder and export the extraction as a csv file to "DBs" folder after creating it in case it doesn't exist
db.all.lcrp <- sector_values

outfilname<- paste('../', Sys.Date(), "_Sector_SS.csv", sep="")
write.csv(db.all.lcrp, outfilname, row.names=FALSE)

# Creating a folder and export the extraction as a csv file to "UNICEF" folder after creating the folder in case it doesn't exist
db.12columns.lcrp <- subset_data1

outfilname<- paste('../', Sys.Date(), "_UNICEF_PBI_SS.csv", sep="")
write.csv(db.12columns.lcrp, outfilname, row.names=FALSE)
cat("Done. The results of UNICEF interventions are in a data frame called 'UNICEF'.\n")

db.6columns.lcrp <- subset_data2

outfilname<- paste('../', Sys.Date(), "_UNICEF_SS.csv", sep="")
write.csv(db.6columns.lcrp, outfilname, row.names=FALSE)
cat("Done. The results of UNICEF interventions are in a data frame called 'UNICEF'.\n")

