library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Chronic-Absenteeism-by-Meal-Eligibility
# Created by Jenna Daly
# On 03/16/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
all_csvs <- dir(path, recursive=T, pattern = ".csv") 

chronic_absent_ME <- data.frame(stringsAsFactors = F)
chronic_absent_ME_noTrend <- grep("Trend", all_csvs, value=T, invert=T)
###Chronic Absenteeism All-Students###
for (i in 1:length(chronic_absent_ME_noTrend)) {
  current_file <- read.csv(paste0(path, "/", chronic_absent_ME_noTrend[i]), stringsAsFactors=F, header=F )
  current_file <- current_file[-c(1:2),]
  colnames(current_file) = current_file[1, ] # the first row will be the header
  current_file = current_file[-1, ]          # removing the first row.
  current_file <- current_file[, !(names(current_file) == "District Code")]
  #only grab rows with race/ethnicity categories
  blankFilter <- logical()
  for(j in 1:nrow(current_file)) {
    blankFilter <- append(blankFilter, all(is.na(current_file[j,])))
  }
  ME <- c("Eligible For Free Lunch", "Eligible For Reduced Lunch", "Not Eligible For Lunch")
  current_file <- current_file[!blankFilter & current_file$`Category` %in% ME,]
  #rename Category column
  names(current_file)[names(current_file) == 'Category'] <- 'Meal Eligibility'
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(chronic_absent_ME_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  chronic_absent_ME <- rbind(chronic_absent_ME, current_file)
}

#Add statewide data...

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

chronic_absent_ME_fips <- merge(chronic_absent_ME, districts, by.x = "District", by.y = "District", all=T)

chronic_absent_ME_fips$District <- NULL

chronic_absent_ME_fips<-chronic_absent_ME_fips[!duplicated(chronic_absent_ME_fips), ]####

#backfill year
years <- c("2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Meal Eligibility` = unique(chronic_absent_ME$`Meal Eligibility`),
  `Year` = years 
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_chronic_absent_ME <- merge(chronic_absent_ME_fips, backfill_years, by = c("FixedDistrict", "Meal Eligibility", "Year"), all=T)

complete_chronic_absent_ME <- complete_chronic_absent_ME[order(complete_chronic_absent_ME$FixedDistrict, complete_chronic_absent_ME$Year, complete_chronic_absent_ME$`Meal Eligibility`),]

#remove duplicated Year rows
complete_chronic_absent_ME <- complete_chronic_absent_ME[!with(complete_chronic_absent_ME, is.na(complete_chronic_absent_ME$Year)),]

#recode missing data with -6666
complete_chronic_absent_ME[["% Chronically Absent"]][is.na(complete_chronic_absent_ME[["% Chronically Absent"]])] <- -6666

#recode suppressed data with -9999
complete_chronic_absent_ME[["% Chronically Absent"]][complete_chronic_absent_ME$"% Chronically Absent" == "*"]<- -9999

#return blank in FIPS if not reported
complete_chronic_absent_ME$FIPS <- as.character(complete_chronic_absent_ME$FIPS)
complete_chronic_absent_ME[["FIPS"]][is.na(complete_chronic_absent_ME[["FIPS"]])] <- ""

#reshape from wide to long format
cols_to_stack <- c("% Chronically Absent")

long_row_count = nrow(complete_chronic_absent_ME) * length(cols_to_stack)

complete_chronic_absent_ME_long <- reshape(complete_chronic_absent_ME,
                                            varying = cols_to_stack,
                                            v.names = "Value",
                                            timevar = "Variable",
                                            times = cols_to_stack,
                                            new.row.names = 1:long_row_count,
                                            direction = "long"
)

#Rename FixedDistrict to District
names(complete_chronic_absent_ME_long)[names(complete_chronic_absent_ME_long) == 'FixedDistrict'] <- 'District'

#reorder columns and remove ID column
complete_chronic_absent_ME_long <- complete_chronic_absent_ME_long[order(complete_chronic_absent_ME_long$District, complete_chronic_absent_ME_long$Year),]
complete_chronic_absent_ME_long$id <- NULL

#Add Measure Type
complete_chronic_absent_ME_long$`Measure Type` <- "Percent"

#Rename Variable columns
complete_chronic_absent_ME_long$`Variable` <- "Chronically Absent Students"

#Order columns
complete_chronic_absent_ME_long <- complete_chronic_absent_ME_long %>% 
  select(`District`, `FIPS`, `Year`, `Meal Eligibility`, `Variable`, `Measure Type`, `Value`)

#Use this to find if there are any duplicate entires for a given district
test <- complete_chronic_absent_ME_long[,c("District", "Year", "Meal Eligibility")]
test2<-test[duplicated(test), ]

#Write CSV
write.table(
  complete_chronic_absent_ME_long,
  file.path(getwd(), "data", "chronic_absenteeism_by_meal_eligibility_2012-2016.csv"),
  sep = ",",
  row.names = F
)