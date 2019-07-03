# Data Prep R file 
# Adding the area for each GEO ID in the csv files for each city in the data prepped folder

#setwd("~/Desktop/info370/a2-zcolah")
library("rjson")
library("dplyr")
library("jsonlite")


# Referred to https://www.r-bloggers.com/perform-a-function-on-each-file-in-r/ 
# to learn how to apply functions on csvs and directly overwrite them

fileNames <- list.files(path = "./data/prepped", pattern = '*.csv')
areaNames <- list.files(path = "./data/shapefiles", pattern = '*.json',full.names=TRUE)
areaNames

area <- lapply(areaNames, function(x) fromJSON(file=x))

baltimore <- fromJSON("./data/shapefiles/baltimore_tracts.json")
charleston <- fromJSON("./data/shapefiles/charleston_tracts.json")
chicago <- fromJSON("./data/shapefiles/chicago_tracts.json")
columbus <- fromJSON("./data/shapefiles/columbus_tracts.json")
dayton <- fromJSON("./data/shapefiles/dayton_tracts.json")
denver <- fromJSON("./data/shapefiles/denver_tracts.json")
kc <- fromJSON("./data/shapefiles/kc_tracts.json")
memphis <- fromJSON("./data/shapefiles/memphis_tracts.json")
milwaukee <- fromJSON("./data/shapefiles/milwaukee_tracts.json")
ok_city <- fromJSON("./data/shapefiles/ok_city_tracts.json")
pittsburgh <- fromJSON("./data/shapefiles/pittsburgh_tracts.json")
st_louis <- fromJSON("./data/shapefiles/st_louis_tracts.json")
syracuse <- fromJSON("./data/shapefiles/syracuse_tracts.json")
wichita <- fromJSON("./data/shapefiles/wichita_tracts.json")



# for (fileName in fileNames) {
#   
#   # read city data:
#   city <- read.csv(fileName,stringsAsFactors = FALSE)
#   
#   
#   
#   #
#   #area <- fromJSON(file = )
#     
#   # add one to every widget value in every file:
#   
#   #sample$Widgets <- sample$Widgets + 1
#   
#   
#   
#   # overwrite old data with new data:
#   # write.table(sample, 
#   #             fileName,
#   #             append = FALSE,
#   #             quote = FALSE,
#   #             sep = ",",
#   #             row.names = FALSE,
#   #             col.names = TRUE)
#   
#   
# }
# 
