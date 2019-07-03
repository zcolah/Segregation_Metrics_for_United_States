# Analysis

# Libraries
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)

# THIS NEVER INSTALLS PROPERLY - I AM FRUSTRATED WITH THIS MAP THING 
#install.packages("geojsonio")
#library(geojsonio)


# We need to source metric_functions so that we can access each metric function
source('metric_functions.R')


cities <-get_cities("./data/prepped")

# We will call the data from each function only once 
# rather than running the functions for each dataframe we are making here
# This will improve efficiency and speed 

# Storing 
city_names <- get_city_names("./data/prepped")
correlation <- sapply(cities, correlation_ratio)
isolation <- sapply(cities, isolation_index)
interaction <- sapply(cities, interaction_index)
gini <- sapply(cities, gini_coeff)
delta <- sapply(cities, delta_index)
zo <- sapply(cities, zo_index)

#typeof(city_names)
#typeof(correlation)
#typeof(isolation)
#typeof(interaction)

# All Metrics 

# City Metrics contains all the metrics measured for each city
city_metrics <- as.data.frame(cbind(City = city_names,
                                    Gini = as.numeric(gini),
                                    Interaction_Index = as.numeric(interaction),
                                    Isolation_Index = as.numeric(isolation),
                                    Correlation_Ratio = as.numeric(correlation),
                                    Delta = as.numeric(delta), 
                                    Zo = as.numeric(zo)), 
                              stringsAsFactors = FALSE) %>% 
                              arrange(Gini)

#without zo-index
city_metrics_wo_zo <- city_metrics %>% select(-Zo)

# City Exposure Metrics
city_exposure_metrics <- city_metrics %>% select(City, Isolation_Index, Interaction_Index, Correlation_Ratio) %>% arrange(Correlation_Ratio)

# City isolation metrics contains all the isolation metrics for each city
city_isolation_metrics <- city_metrics %>% select (City, Isolation_Index) %>% arrange(Isolation_Index)
isolation_sd <- sd(city_isolation_metrics$Isolation_Index, na.rm = FALSE)

# City interaction metrics contains all the isolation metrics for each city
city_interaction_metrics <- city_metrics %>% select (City, Interaction_Index) %>% arrange(desc(Interaction_Index))
interaction_sd <- sd(city_interaction_metrics$Interaction_Index, na.rm = FALSE)

# City correlation ratio metrics contains all the isolation metrics for each city
city_correlation_ratio <- city_metrics %>% select (City, Correlation_Ratio) %>% arrange(Correlation_Ratio)
correlation_sd <- sd(city_correlation_ratio$Correlation_Ratio, na.rm = FALSE)

# City Metrics
city_gini <- city_metrics %>% select (City, Gini) %>% arrange(Gini)
gini_sd <- sd(city_gini$Gini, na.rm = FALSE)

# City Delta Index
city_delta <- city_metrics %>% select (City, Delta) %>% arrange(desc(Delta))
delta_sd <- sd(city_delta$Delta, na.rm = FALSE)

# City Zo Index
city_zo <- city_metrics %>% select (City, Zo) %>% arrange(Zo)
zo_sd <- sd(city_zo$Zo, na.rm = FALSE)

# City Ranks
ranks <- c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
city_ranks <- as.data.frame(cbind(Ranks = as.numeric(ranks),
                                  Gini = city_gini$City,
                                  Interaction_Index = city_interaction_metrics$City,
                                  Isolation_Index = city_isolation_metrics$City,
                                  Correlation_Ratio = city_correlation_ratio$City,
                                  Delta = city_delta$City), 
                            stringsAsFactors = FALSE)

# For Ranking Chart
city_metrics[] <- lapply(city_metrics, type.convert, as.is = TRUE)

#The reason I am making Gini and Zo as 1-Gini and 1-Zo is so that we get the right ranking order of least to most segregated by each Index
rank_metrics <- city_metrics %>% select(City, Gini, Interaction_Index, Delta) %>% mutate (Gini = (1 - Gini)) %>% melt() %>% rename(metric = variable) %>% dcast(metric ~ City)
rank_metrics_with_zo <- city_metrics %>% select(City, Gini, Interaction_Index, Delta, Zo) %>% mutate (Gini = (1 - Gini), Zo = (1 - Zo)) %>% melt() %>% rename(metric = variable) %>% dcast(metric ~ City)



########################################################################################################################

#baltimore.map.data <- file_to_geojson(input='data/shapefiles/baltimore_tracts.json', method='web', output = ":memory:")
