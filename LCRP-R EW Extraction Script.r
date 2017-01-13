################################################################################
# The MIT License (MIT)
#
# Copyright (c) 2015 Maarten-Jan Kallen
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


database.id <- 6354

# Uncomment the following command if you want to log in manually, leave commented
# out if you have stored your login credentials on your local machine.

#activityInfoLogin()

#-------------------------------------------------------------------------------
# Function definitions
#-------------------------------------------------------------------------------

na.if.null <- function(x) {
  if (is.null(x)) NA else x
}

sanitizeNames <- function(s) {
  # convert strings to a format that's suitable for use as name
  gsub("\\s|-|_", ".", tolower(s))
}

translateFieldType <- function(typeClass) {
  switch(toupper(typeClass),
         REFERENCE  = "reference",
         LOCAL_DATE = "date",
         QUANTITY   = "indicator",
         CALCULATED = "calculated indicator",
         ENUMERATED = "attribute",
         NARRATIVE  =,
         FREE_TEXT  = "text",
         GEOAREA    = "geographic entity",
         "other")
}

getFormElements <- function(form, tree, name.prefix = NULL) {
  
  if (is.null(form$elements)) {
    NULL
  } else {
    do.call(rbind, lapply(form$elements, function(e) {
      fieldType <- translateFieldType(e$type$typeClass)
      if (fieldType == "reference") {
        # This form refers to one or more other forms
        do.call(rbind, lapply(e$type$parameters$range, function(refform) {
          getFormElements(tree$forms[[refform]],
                          tree,
                          ifelse(is.null(name.prefix),
                                 e$code,
                                 paste(name.prefix, e$code, sep = ".")))
        }))
      } else {
        fieldName <- ifelse(is.null(e$code), e$label, e$code)
        fieldLabel <- ifelse(is.null(e$label), e$code, e$label)
        fieldType <- if (fieldType == "attribute") {
          switch(e$type$parameters$cardinality,
                 SINGLE="single attribute",
                 MULTIPLE="multiple attribute",
                 stop("unknown cardinality"))
        } else {
          fieldType
        }
        data.frame(id = e$id,
                   name = ifelse(is.null(name.prefix),
                                 fieldName,
                                 paste(name.prefix, fieldName, sep = ".")),
                   label = fieldLabel,
                   type = fieldType,
                   stringsAsFactors = FALSE
        )
      }
    }))
  }
}

getFormTree <- function(activity) {
  
  prefix <- switch(as.character(activity$reportingFrequency),
                   "0"="a",
                   "1"="M",
                   stop("reporting frequency should be 0 (once) or 1 (monthly)")
  )
  
  tree <- getResource(sprintf("form/%s%s/tree", prefix, activity$id))
  
  form <- tree$forms[[tree$root]]
  
  elements <- getFormElements(form, tree)
  
  structure(elements, class = c("formtree", class(elements)), tree = tree)
}

queryForm <- function(form, queryType = c("rows", "columns"), ...) {
  
  formId <- if (inherits(form, "formtree")) {
    # query the root form of a tree contained in a formtree result
    attr(form, "tree")$root
  } else if (is.character(form)) {
    # query using a form identifier
    form
  } else {
    # query the root of a form tree
    form$root
  }
  
  getResource(sprintf("form/%s/query/%s", formId, match.arg(queryType)), ...)
}

extractOldId <- function(s) {
  if (all(grepl("^[[:alpha:]]0*", s))) {
    as.integer(sub("^[[:alpha:]]0*", "", s))
  } else {
    s
  }
}

determineMonth <- function(start, end) {
  start <- as.POSIXlt(start)
  end <- as.POSIXlt(end)
  if (start$year != end$year || start$mon != end$mon) {
    cat("Warning: found a start and end date in different months\n")
  }
  format(start, format = "%Y-%m")
}

getPartnersDataFrame <- function(formId) {
  partners <- getResource(sprintf("form/%s/query/rows", formId), id = "_id", name = "name")
  do.call(rbind, lapply(partners, function(p) {
    data.frame(id = p$id,
               name = p$name,
               oldId = extractOldId(p$id),
               stringsAsFactors = FALSE)
  }))
}

getLocationsDataFrame <- function(formIds) {
  do.call(rbind, lapply(formIds, function(formId) {
    locations <- getResource(sprintf("form/%s/query/rows", formId), id = "_id", name = "name", code = "axe")
    do.call(rbind, lapply(locations, function(p) {
      data.frame(id = p$id,
                 name = p$name,
                 code = na.if.null(p$code), # alternative name ("axe")
                 oldId = extractOldId(p$id),
                 stringsAsFactors = FALSE)
    }))
  }))
}

lookupName <- function(x, table, lookupCol = "oldId", outputCol = "name") {
  
  if (is.na(x) || is.character(x)) return(x)
  
  tableName <- deparse(substitute(table))
  
  if(is.null(table[[lookupCol]]) || is.null(table[[outputCol]])) {
    stop("'", tableName, "' must have columns '", lookupCol, "' and '", outputCol, "'")
  }
  
  row <- match(x, table[[lookupCol]])
  if (any(is.na(row))) {
    cat("Warning: no record(s) found with (old) identifier(s) ",
        paste(x[is.na(row)], collapse = ", "), " in '", tableName,
        "'\n", sep ="")
  }
  table[[outputCol]][row]
}

is.monthly <- function(formTree) {
  grepl("^M\\d*$", attr(formTree, "tree")$root)
}

# Send a "curl -I" request to the beta API to warm up the server:
invisible(HEAD("https://pivot-dot-activityinfoeu.appspot.com/login"))

#-------------------------------------------------------------------------------
# Script body
#-------------------------------------------------------------------------------

if (is.na(database.id)) {
  stop("you forgot to set the database identifier at the top of this script!")
}

# Use the new API (in beta)
activityInfoRootUrl("https://pivot-dot-activityinfoeu.appspot.com")

# Get the schema and retry a few times to allow the beta-api instance to warm up:
cat("Retrieving schema for database ", database.id, "...\n", sep ="")
retry <- 5
while (retry) {
  success <- TRUE
  tryCatch(schema <- getDatabaseSchema(database.id),
           error = function(e) {
             cat("Failed to retrieve the schema for database ", database.id,
                 ". Retrying...\n", sep = "")
             retry <<- retry - 1
             if (retry == 0) stop("Failed with the following error: ", e$message)
             success <<- FALSE
           },
           finally = if (success) {
             cat("Retrieved schema for database ", database.id,
                 ": ", schema$name, "\n", sep = "")
             retry <- 0
           }
  )
}

# Prepare a list with query parameters to get administrative level and
# geographic location data:
adminLevels <- getAdminLevels(schema$country$id)
adminLevelNames <- vapply(adminLevels, function(x) x$name, "character")
locationQueryParams <- local({
  tmp <- sprintf("[%s].name",vapply(adminLevelNames, URLencode, "character"))
  tmp <- as.list(tmp)
  names(tmp) <- make.names(adminLevelNames)
  tmp$id <- "_id"
  tmp$lat <- "location.latitude"
  tmp$lon <- "location.longitude"
  tmp
})

# Which fields are attributes?
attributeGroups <- unique(
  do.call(c, lapply(schema$activities, function(form) {
    sapply(form$attributeGroups, function(group) {
      group$name
    })
  }))
)

values <- NULL

# Loop over all forms in the database:
for (formIndex in seq(length(schema$activities))) {
  
  activity <- schema$activities[[formIndex]] # "activity" is the old name for a form
  indicator.metadata <- do.call(rbind, lapply(activity$indicators, function(indicator) {
    data.frame(oldId = indicator$id,
               units = na.if.null(indicator$units),
               category = na.if.null(indicator$category),
               stringsAsFactors = FALSE)
  }))
  
  cat("Processing activity ", activity$id, " (", activity$name, ")...\n", sep = "")
  formTree <- getFormTree(activity)
  
  #   partnerFormId <- grep("^P\\d*$", names(attr(formTree, "tree")$forms), value = TRUE)
  #   cat("Retrieving partners...\n")
  #   partners <- getPartnersDataFrame(partnerFormId)
  #   
  #   locationFormId <- grep("^L\\d*$", names(attr(formTree, "tree")$forms), value = TRUE)
  #   if (length(locationFormId) == 0L) {
  #     cat("Warning: no locations for form ", activity$id, ", skipping...\n", sep = "")
  #     next
  #   }
  #   cat("Retrieving locations...\n")
  #   locations <- getLocationsDataFrame(locationFormId)
  
  cat("Retrieving reported values...\n")
  retry <- 3
  while (retry) {
    success <- TRUE
    tryCatch(reports <- queryForm(formTree),
             error = function(e) {
               cat("Error: failed to retrieve reported values for form ", activity$id,
                   ". Retrying...\n", sep = "")
               retry <<- retry - 1
               if (retry == 0) {
                 stop("Failed with the following error: ", conditionMessage(e), call. = FALSE)
               }
               success <<- FALSE
             },
             finally = if (success) {
               retry <- 0
             }
    )
  }
  
  cat("Retrieving administrative levels...\n")
  success <- TRUE
  tryCatch(admin.levels <- queryForm(formTree, queryParams = locationQueryParams),
           error = function(e) {
             cat("Error: failed to retrieve administrative levels for form ", activity$id,
                 ", skipping...\n", sep = "")
             success <<- FALSE
           },
           finally = if (!success) next)
  
  # Merge/fuse the two lists together:
  reports <- mapply(c, reports, admin.levels, SIMPLIFY = FALSE)
  
  cat("Converting values to a tabular format...\n")
  values <- rbind(values, do.call(rbind, lapply(reports, function(report) {
    # Convert report to a data frame so we can merge with the form tree:
    reportTable <- data.frame(name = names(report),
                              values = unlist(report), stringsAsFactors = FALSE)
    reportTable <- merge(reportTable, formTree, by = "name")
    
    if (is.monthly(formTree)) {
      partnerLabel <- report$site.partner.label
      locationLabel <- if (is.null(report$site.location.label)) {
        "unknown"
      } else {
        report$site.location.label
      }
    } else {
      partnerLabel <- report$partner.label
      locationLabel <- if (is.null(report$location.label)) {
        "unknown"
      } else {
        report$location.label
      }
    }
    #     partnerId <- partners$oldId[match(partnerLabel, partners$name)]
    #     locationId <- if (!is.na(locationLabel)) {
    #       locations$oldId[match(locationLabel, locations$name)]
    #     } else {
    #       NA
    #     }
    is.indicator <- grepl("indicator", reportTable$type)
    n <- sum(is.indicator)
    
    if (n == 0L) {
      # The current report doesn't have any data on indicators
      return(NULL)
    } else {
      oldIndicatorId <- extractOldId(reportTable$id[is.indicator])
      values <- data.frame(
        entryId       = report[["@id"]], # entryId = either the site identifier or the identifier of the monthly report
        indicatorId   = oldIndicatorId,
        indicatorName = reportTable$label[is.indicator],
        units         = lookupName(oldIndicatorId, indicator.metadata, outputCol = "units"),
        indicatorCategory = lookupName(oldIndicatorId, indicator.metadata, outputCol = "category"),
        value         = as.numeric(reportTable$values[is.indicator]),
        stringsAsFactors = FALSE)
      
    }
    values$activityId   <- activity$id
    values$activityName <- activity$name
    values$activityCategory <- na.if.null(activity$category)
    values$month        <- determineMonth(report$date1, report$date2)
    
    # Add administrative level information:
    for (col in c(make.names(adminLevelNames), "lon", "lat")) {
      values[[col]] <- na.if.null(report[[col]])
    }
    
    if (is.monthly(formTree)) {
      # values$locationId   <- locationId
      # values$locationName <- lookupName(locationId, locations)
      # values$locationCode <- lookupName(locationId, locations, outputCol = "code")
      values$locationName <- locationLabel
      values$locationCode <- na.if.null(report$site.location.axe)
      # values$partnerId    <- partnerId
      values$partnerName  <- report$site.partner.label
      values$partnerFullName <- na.if.null(report[["site.partner.Full Name"]])
    } else {
      # values$locationId   <- locationId
      #       values$locationName <- report$location.label
      #       values$locationCode <- na.if.null(report$location.axe)
      values$locationName <- locationLabel
      values$locationCode <- na.if.null(report.location.axe)
      # values$partnerId    <- partnerId
      values$partnerName  <- report$partner.label
      values$partnerFullName <- na.if.null(report[["partner.Full Name"]])
    }
    for (col in attributeGroups) {
      if (is.monthly(formTree)) {
        values[[make.names(col)]] <- na.if.null(report[[paste("site", col, sep = ".")]])
      } else {
        values[[make.names(col)]] <- na.if.null(report[[col]])
      }
    }
    values
  })))
} # end of loop over forms

values$databaseId <- database.id
values$database <- schema$name

cat("Done. The results are in a data frame called 'values'.\n")

###add coordinates.
#AI_xy<- read.csv("d:\\R\\xy.csv")
#values<-merge(values,AI_xy,by.x="locationId", by.y="LocationID", all.x=TRUE)


### Clean unused elements
#get cad_cod
cad_cod<-getAdminLevelEntities(1522)
cad_cod<-data.frame(t(sapply(cad_cod,as.character)), stringAsFactors=FALSE)
cad_cod<- cad_cod[,c("X2","X3")]
#cad_cod$code<-factor(cad_cod$code)

#lists to char
#values$cadastral.area<- vapply(values$cadastral.area, paste, collapse= ",", character(1L))
#values$governorate<- vapply(values$governorate, paste, collapse= ",", character(1L))
#values$caza<- vapply(values$caza, paste, collapse= ",", character(1L))

#merge cad_code
values<-merge(values,cad_cod,by.x="Cadastral.Area", by.y="X2", all.x=TRUE)
###
colnames(values)[ncol(values)]<- "cadCod"
# You can do the following to keep just the object(s) that you want:
# rm(list = setdiff(ls(), "values"))
#options(opt)
###

db.5130ew.lcrp <- values
outfilname<- paste("C:\\Work/Information Management/AI Extractions/DBs/",paste(Sys.Date(), "_ew.csv", sep=""), sep="")
write.csv(db.5130ew.lcrp,outfilname)
outfilname4<- paste("S:/5- Emergency/Syrian Emergency/Information Management/UNICEF_LBN_Information-Management/05_Tools/14_Dashboards/Dashboard 2015/governorate/Dbs/",paste(Sys.Date(), "_ew.csv", sep=""), sep="")
write.csv(db.5130ew.lcrp,outfilname4)