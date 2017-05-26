################################################################################
# Organization : UNICEF
# Author       : Raed Abdel Sater
# Title        : Information management officer
# Copyright (c) 2017 UNICEF
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################

opt <- options(warn = -1)
rm(list = ls())


library("activityinfo")
# library("xlsx")
# library("XLConnect")

# Replace 'NA' with the numeric identifier of your database (e.g. 1234):
database.id <- 6500

# Uncomment the following command if you want to log in manually, leave commented
# out if you have stored your login credentials on your local machine.



if (is.na(database.id)) {
  stop("you forgot to set the database identifier at the top of this script!")
}

values <- getDatabaseValueTable(database.id, 
                                col.names = c(database.id = "databaseId",
                                              indicator.id = "indicatorId",
                                              indicator.name = "indicatorName",
                                              indicator.value = "value",
                                              location.adminlevel.cadastral_area = "cadastral.area",
                                              location.adminlevel.governorate = "governorate",
                                              location.adminlevel.caza = "caza",
                                              location.adminlevel.cadastral_area.code = "cadCod",
                                              partner.label = "partnerName",
                                              partner.description = "partnerFullName",
                                              location.name = "locationName",
                                              location.code = "locationCode",
                                              start_date = "month",
                                              "Funded by" = "Funded.by"))

# Generating values dataframe 
cat("Done. The results are in a data frame called 'values'.\n")

# Subsetting Values to UNICEF interventions dataset only 
values$month <-strftime(values$month,"%Y-%m")
UNICEF <- values[values$Funded.by=="UNICEF",]

# Creating a folder and export the extraction as a csv file to "DBs" folder after creating it in case it doesn't exist
db.6500ba.lcrp <- values
#mainDir <-"Information Management/AI Extractions/"
#subDir <- "sector"
#ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
#setwd(file.path(mainDir, subDir))
outfilname<- paste(Sys.Date(), "_BA.csv", sep="")
write.csv(db.6500ba.lcrp,outfilname)

# Creating a folder and export the extraction as a csv file to "UNICEF" folder after creating the folder in case it doesn't exist
db.6500uniba.lcrp <- UNICEF
#mainDir <-"Information Management/AI Extractions/"
#subDir <- "UNICEF"
#ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
#setwd(file.path(mainDir, subDir))
outfilname<- paste(Sys.Date(), "_UNICEF_BA.csv", sep="")
write.csv(db.6500uniba.lcrp,outfilname)
cat("Done. The results of UNICEF interventions are in a data frame called 'UNICEF'.\n")

# Writing UNICEF extraction to the Dashboard
# XLConnect::writeWorksheetToFile("s://2- Programmes/SPPME/7.PME/21-DASHBOARDS_2017/DASHBOARD_2017- BA.xlsx",db.6500uniba.lcrp, "BADB",clearSheets = TRUE,styleAction = XLC$STYLE_ACTION.NONE)
# cat("UNICEF results are available on the Dashboard'.\n")
# Workbook <- loadWorkbook("s://2- Programmes/SPPME/7.PME/21-DASHBOARDS_2017/ADHOC_EXTRACTIONS/TEST/TEST.xlsx")
# writeData(Workbook,sheet = "MasterList",db.6500uniba.lcrp)
# saveWorkbook(Workbook,"s://2- Programmes/SPPME/7.PME/21-DASHBOARDS_2017/ADHOC_EXTRACTIONS/TEST/TEST.xlsx",overwrite = TRUE)

