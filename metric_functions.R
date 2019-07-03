# Metric Functions
# contains the functions necessary for computing metrics of segregation
# tests for each of my functions are provided below each function
# they are commented out for now though

# DF for testing

#baltimore <-read.csv("data/prepped/baltimore_race.csv", stringsAsFactors = FALSE)
#View(baltimore)
#denver<-read.csv("data/prepped/denver_race.csv", stringsAsFactors = FALSE)
#View(denver)

# Each City Dataframe has an added column which contains the area in square metres for each geo id 
# please refer to data_prep_area to see how the data was modified

#setwd("~/Desktop/info370/a2-zcolah")

# Libraries

library(dplyr)
library(tidyr)

# returns a list of dataframes of each csv in the directory path given
get_cities <-function(dir_path){

  # Learnt how to to read csvs into R as a list of Dataframes from https://gist.github.com/apreshill/1d197a3403aa4483454f
    
  file_list <- list.files(path = dir_path, pattern = '*.csv')
  
  
  # City Data is a list of dataframes containing the pop details and geo id of each city
  # Note: The spec did not mention if we should consider the geoids marked with exclude or not. I decided to consider them.
  city_data <- lapply(file.path(dir_path, file_list), read.csv, stringsAsFactors = FALSE) %>% setNames(gsub('_race.csv', '', file_list, fixed = TRUE))

  return(city_data)
  
}

# Function to convert this first letter of each string in a vector to upper case 
# got from: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# old version to get city names
# returns a list of the city names
# it was not good as it required a lot of manual fixing for each city even after cleaning it 
# refer to knew function
get_city_names_old <-function(dir_path){
  
  # Learnt how to to read csvs into R as a list of Dataframes from https://gist.github.com/apreshill/1d197a3403aa4483454f
  
  file_list <- list.files(path = dir_path, pattern = '*.csv') %>%unlist(recursive = TRUE, use.names = TRUE) 
  
  # remove the last nine characters
  city_names <- gsub('.{9}$', '', file_list)
  # Capitalize the first letter 
  city_names <- firstup (city_names)
  
  
  return(city_names)
  
}

# returns a list of the city names
get_city_names <-function(dir_path){
  
  
  cities <- c( 'Baltimore', 'Charleston', 'Chicago', 'Columbus', 
                   'Dayton', 'Denver', 'Kansas City', 
                   'Memphis', 'Milwaukee', 'Oklahoma City', 'Pittsburgh', 
                   'St. Louis', 'Syracuse', 'Wichita')
  
  
  return(cities)
  
}


# get_city_names("./data/prepped")


# To better understand interaction and isolation index I refferred to https://www.dartmouth.edu/~segregation/IndicesofSegregation.pdf

######################################################################################################################################

# Interaction Index
interaction_index <- function (city) {
  
  # total blacks in the city
  X <- sum(city$pop.not.white)
  
  # xi is the number of blacks in the tract
  # yi is the number of whites in the tract
  
  city <- city %>% mutate(xi_div_X = pop.not.white / X,
                          yi_div_ti = pop.white / pop, 
                          interaction_sub = xi_div_X * yi_div_ti)
  
  # return interaction index for one city
  return(format(round(sum(city$interaction_sub), 2), nsmall = 2))
  
}

#interaction_index(baltimore)
#interaction_index(denver)

######################################################################################################################################

# Isolation Index
isolation_index <- function (city) {
  
  # total blacks in the city
  X <- sum(city$pop.not.white)
  
  
  
  # xi is the number of blacks in the tract
  
  city <- city %>% mutate(xi_div_X = pop.not.white / X,
                          xi_div_ti = pop.not.white / pop, 
                          isolation_sub = xi_div_X * xi_div_ti)
  
  # return isolation index for one city
  return (format(round(sum(city$isolation_sub), 2), nsmall = 2))
  
}

# isolation_index(baltimore)
# isolation_index(denver)

######################################################################################################################################``

# Correlation 
correlation_ratio <- function (city) {
  
  # total blacks in the city
  X <- sum(city$pop.not.white)
  
  
  
  # xi is the number of blacks in the tract
  
  city <- city %>% mutate(xi_div_X = pop.not.white / X,
                          xi_div_ti = pop.not.white / pop, 
                          isolation_sub = xi_div_X * xi_div_ti)
  
  iso_index <- sum(city$isolation_sub)

  total_pop <- sum(city$pop)

  # P is the Ratio of X to T (proportion of the metropolitan area’s population that is minority)
  
  P <- X / total_pop
  
  # return correlation ratio for one city
  return((format(round((iso_index - P)/(1-P), 2), nsmall = 2)))
  
}

######################################################################################################################################``

# Gini Coefficient

gini_coeff <- function (city) {
  
  # total blacks in the city
  X <- sum(city$pop.not.white)
  
  # total pop
  total_pop <- sum(city$pop)
  
  #the ratio of X to T (proportion of the metropolitan area’s population that is minority)
  P <- X/total_pop
  
  # n is the number of rows in our dataset
  n <- nrow(city)
  
  # we will calculate the numerator and denominator separately
  
  # numerator
  # initially wrote a nested for loop (see below), 
  # however it was taking a lot of time to compute
  # I knew there should be a faster way for this so I went to stackoverflow for help
  # Link to my question: https://stackoverflow.com/questions/53029084/what-is-a-better-way-to-write-this-nested-for-loop-in-r
  
  titj <- tcrossprod(city$pop)
  pipj <- outer(city$pct.not.white, city$pct.not.white, "-")
  numerator <- sum(titj * abs(pipj))
  
  
  # for(i in 1:n) {
  # 
  #   ti <- city$pop[i]
  #   pi<- city$pct.not.white[i]
  #   
  #   for(j in 1:n) {
  #   
  #     tj <- city$pop[j]
  #     pj <- city$pct.not.white[j]
  #     
  #     numerator = numerator + (ti * tj) * abs(pi -pj)
  #       
  #   }
  #     
  # }
  
  # denominator
  denominator <- (2*total_pop*total_pop*P*(1-P))
  gini <- numerator / denominator
  
  # return gini
  return (format(round(gini, 2), nsmall = 2))
}


######################################################################################################################################

# Delta Index

# Area Data is the land area in square metres for each geoid
# Needed to calculate the delta index
area_data <- read.delim("./data/gaz_tracts_national_area.txt") %>% select(GEOID, ALAND)

# testing for left join
# Rows with issues: 105, 121
# baltimore <-transform(baltimore, GEOID = as.numeric(GEOID)) 
# baltimore <- baltimore %>%  left_join(area_data)
# View(baltimore)

delta_index <- function (city) {

  city <- transform(city, GEOID = as.numeric(GEOID)) 
  city <- city %>% left_join(area_data)
  
  # adding in area data for each tract of the city
  city <- left_join(city, area_data)
  
  # total blacks in the city
  X <- sum(city$pop.not.white)
  
  # total area of the city
  A <- sum(as.numeric(city$ALAND))
  
  # xi is the number of blacks in the tract
  # ai is the area of the tract
  city <- city %>% mutate(xi_div_X = pop.not.white / X,
                          ai_div_A = ALAND / A,
                          delta_sub = abs(xi_div_X - ai_div_A))

  # return interaction index for one city
  return(format(round((sum(city$delta_sub)* 0.5), 2), nsmall = 2))
}



####################################################################

zo_index <- function (city) {
  
  city <- transform(city, GEOID = as.numeric(GEOID)) 
  city <- city %>% left_join(area_data)
  
  # adding in area data for each tract of the city
  city <- left_join(city, area_data)
  
  # total blacks in the city
  X <- sum(city$pop.not.white)
  
  # total area of the city
  A <- sum(as.numeric(city$ALAND))
  
  
  city <- city %>% mutate(zo_sub= (abs(pop.not.white - pop.white)/pop) * ALAND / A)
  
  # return interaction index for one city
  return(format(round((sum(city$zo_sub)), 2), nsmall = 2))
}