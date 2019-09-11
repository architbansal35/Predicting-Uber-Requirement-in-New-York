

library(lubridate) #for date modifications
library(ggmap)
library(plotly)
library(VIM) #for aggr
library(dplyr) #for left join, count_, 
library(DT) #for datatable
library(dbscan)
library(ggplot2)

######## DATA FOR 2014 ########

apr14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-apr14.csv")
may14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-may14.csv")
jun14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-jun14.csv")
jul14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-jul14.csv")
aug14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-aug14.csv")
sep14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-sep14.csv")

#### Bind all the files in one and then delete the individual files
apr_june14 <- bind_rows(apr14, may14, jun14)
july_sep14 <- bind_rows(jul14, aug14, sep14)
rm(apr14,may14,jun14,jul14,aug14,sep14)
gc()

#### Checking the data in the dataset
apr_june14 <- na.omit(apr_june14)
july_sep14 <- na.omit(july_sep14)
#summary(data_2014)
# #### Shows there are no more missing data (or na values)
# aggr(apr_june14)

#### Separate or mutate the Date/Time columns
#colnames(apr_june14)
colnames(apr_june14)[1] <- c("date_time")
colnames(july_sep14)[1] <- c("date_time")

# apr_june14$date_time <- mdy_hms(apr_june14$date_time)
# apr_june14$Day <- factor(day(apr_june14$date_time))
# apr_june14$Month <- factor(month(apr_june14$date_time))
# july_sep14$Month <- factor(month(july_sep14$date_time))
# apr_june14$Year <- factor(year(apr_june14$date_time))
# apr_june14$Weekday <- factor(wday(apr_june14$date_time))
# apr_june14$Hour <- factor(hour(apr_june14$date_time))
# apr_june14$Minute <- factor(minute(apr_june14$date_time))
# apr_june14$Second <- factor(second(apr_june14$date_time))

#### Plot the data of trips in NY city map
register_google(key = "AIzaSyAuDLT-9GhNpUtNjGB7ihukK0jkhB454Vs")
NYCMap <- get_map("New York", zoom = 10)
#ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat),data = data_2014)

test_april14 <- read.csv("C:/Users/archi/OneDrive/Desktop/Projec/Data/uber-raw-data-apr14.csv", nrows=10000)

######## DATA ANALYSIS - MODEL 1 ########

#### Making boroughs using DBSCAN
EPS <- 0.009
clusters_dbscan_train <- dbscan(select(test_april14, Lat, Lon), eps = EPS)
test_april14$cluster_db <- as.factor(clusters_dbscan_train$cluster)
groups <- test_april14 %>% filter(clusters_dbscan_train$cluster != 0)
noise_train <- test_april14 %>% filter(clusters_dbscan_train$cluster == 0)
noise_test <- july_sep14 %>% filter(clusters_dbscan_test$cluster == 0)

#### Making boroughs using DBSCAN
# EPS <- 0.2
# clusters_dbscan_train <- dbscan(select(apr_june14, Lat, Lon), eps = EPS)
# clusters_dbscan_test <- dbscan(select(july_sep14, Lat, Lon), eps = EPS)
# test_april14$cluster_db <- as.factor(clusters_dbscan$cluster)
# groups <- test_april14 %>% filter(clusters_dbscan$cluster != 0)
# noise_train <- apr_june14 %>% filter(clusters_dbscan_train$cluster == 0)
# noise_test <- july_sep14 %>% filter(clusters_dbscan_test$cluster == 0)

#### Making boroughs using KMeans
set.seed(20)
clusters_train <- kmeans(apr_june14[,2:3], 5)
clusters_test <- kmeans(july_sep14[,2:3], 5)
apr_june14$Borough <- as.factor(clusters_train$cluster)
july_sep14$Borough <- as.factor(clusters_test$cluster)

#### Making final data for 2014 to be used in Model 2
final_data14 <- bind_rows(apr_june14,july_sep14)
final_data14$date_time <- mdy_hms(final_data14$date_time)
final_data14$Month <- factor(month(final_data14$date_time))

#### Trying to plot the boroughs formed from KMean - Training Data
#ggplot(test_april14, aes(test_april14$Lon, test_april14$Lat, color = test_april14$cluster)) + geom_point()
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(Borough)),data = apr_june14) +ggtitle("NYC Boroughs using KMean - Train")

#### Trying to plot the boroughs formed from KMean - Test Data
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(Borough)),data = july_sep14) +
  #geom_point(aes(x = Lon, y = Lat, fill = "grey"), noise_test) + 
  ggtitle("NYC Boroughs using KMean - Test")


#### Trying to plot the boroughs formed from DBSCAN
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(cluster_db)),data = test_april14) +
  geom_point(aes(x = Lon, y = Lat, fill = "grey"), noise_train) +
  geom_point(aes(x = Lon, y = Lat, colour = as.factor(cluster_db)), groups)

#### Check the difference of number of rows between the orig table and after removing .na values
# data_2014_borough_omit = na.omit(data_2014_borough)
# outside_ny_pickup = nrow(data_2014_borough) - nrow(data_2014_borough_omit)
# outside_ny_pickup

######## Data prepartion ends for 2014 ########

final_data14$Month <- as.double(final_data14$Month)
month_borough_14 <- count_(final_data14, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)

## Monthly growth 2014
monthly_growth_all_14 <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth_all_14


