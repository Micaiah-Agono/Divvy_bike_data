library(tidyverse)
# Using the conflicted package to manage conflicts
library(conflicted)
# Setting dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


#=====================
# STEP 1: COLLECTING DATA
#=====================
# # Uploading Divvy datasets (csv files) here
q1_2019 <- read_csv("C:/Users/user/OneDrive/Desktop/My Project/Project 1/raw data/Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("C:/Users/user/OneDrive/Desktop/My Project/Project 1/raw data/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("C:/Users/user/OneDrive/Desktop/My Project/Project 1/raw data/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("C:/Users/user/OneDrive/Desktop/My Project/Project 1/raw data/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("C:/Users/user/OneDrive/Desktop/My Project/Project 1/raw data/Divvy_Trips_2020_Q1.csv")


#====================================================
# STEP 2: WRANGLING DATA AND COMBINING INTO A SINGLE FILE
#====================================================
# Comparing column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before
# we can use a command to join them into one file
colnames(q1_2019)
colnames(q1_2020)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)

# Renaming columns to make them consistent
# and same in all table (q12019, q1_2020, q2_2019, q3_2019, q4_2019)

q1_2019 <- rename(q1_2019
                  ,start_station_id = from_station_id
                  ,start_station_name = from_station_name
                  ,end_station_id = to_station_id
                  ,end_station_name = to_station_name
)
q1_2020 <- rename(q1_2020
                  ,trip_id = ride_id
                  ,bikeid = rideable_type
                  ,start_time = started_at
                  ,end_time = ended_at
                  ,usertype = member_casual
)
q2_2019 <- rename(q2_2019
                  ,start_station_id = '03 - Rental Start Station ID'
                  ,start_station_name = '03 - Rental Start Station Name'
                  ,end_station_id = '02 - Rental End Station ID'
                  ,end_station_name = '02 - Rental End Station Name'
                  ,trip_id = '01 - Rental Details Rental ID'
                  ,bikeid = '01 - Rental Details Bike ID'
                  ,start_time = '01 - Rental Details Local Start Time'
                  ,end_time = '01 - Rental Details Local End Time'
                  ,usertype = 'User Type'
)
q3_2019 <- rename(q3_2019
                  ,start_station_id = from_station_id
                  ,start_station_name = from_station_name
                  ,end_station_id = to_station_id
                  ,end_station_name = to_station_name
)
q4_2019 <- rename(q4_2019
                  ,start_station_id = from_station_id
                  ,start_station_name = from_station_name
                  ,end_station_id = to_station_id
                  ,end_station_name = to_station_name
)

# Inspecting the dataframes and looking for incongruencies

str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)

# Converting ride_id and bikeid to character so that they can stack correctly

q1_2019 <- mutate(q1_2019, trip_id = as.character(trip_id)
                  ,bikeid = as.character(bikeid)
                  )
q2_2019 <- mutate(q2_2019, trip_id = as.character(trip_id)
                  ,bikeid = as.character(bikeid)
)
q3_2019 <- mutate(q3_2019, trip_id = as.character(trip_id)
                  ,bikeid = as.character(bikeid)
)
q4_2019 <- mutate(q4_2019, trip_id = as.character(trip_id)
                  ,bikeid = as.character(bikeid)
)

# joining the individual quarter's data frames into one big data frame

all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019, q1_2020)

# Checking the column names for all_trips

colnames(all_trips)

#removing droped data
#There are some rides where tripduration shows up as negative, including several hundred
#rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to
#delete these rides.

all_trips <- all_trips %>%
  select(-c('tripduration', gender, birthyear, '01 - Rental Details Duration In Seconds Uncapped'
            ,`Member Gender`, '05 - Member Details Member Birthday Year', start_lat
            , start_lng, end_lat, end_lng))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)
str(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
summary(all_trips)



# There are a few problems we will need to fix:
# (1) In the "usertype" column, there are two names for members ("member" and "Subscriber")
#and two names for casual riders ("Customer" and "casual"). Let's consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular.
# Adding some additional columns of data -- such as day, month, year -- that provide additional
#opportunities to aggregate the data.
# (3) Adding a calculated field for length of ride since the 2020Q1 data did not have
#the "tripduration" column. Added "ride_length" to the entire dataframe for consistency.




# In the "usertype" column, replace "member" with "Subscriber" and "casual" with "Customer"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make
#our dataframe consistent
# Let's begin by seeing how many observations fall under each usertype 

table(all_trips$usertype)


# Reassigning to the desired values


all_trips <- all_trips %>%
  mutate(usertype = recode(usertype
                           , 'member' = 'Subscriber'
                           , 'casual' = 'Customer'))

# Checking to make sure the proper number of observations were reassigned
table(all_trips$usertype)

# Adding columns that list the date, month, day, and year of each ride

all_trips$date <- as.Date(all_trips$start_time) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


all_trips$ride_length <- difftime(all_trips$end_time, all_trips$start_time)

str(all_trips)
summary(all_trips)

# Converting "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)#check if the ride_length column is a factor
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))#convert to character first then numeric
is.numeric(all_trips$ride_length)

# Removing "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
#checked for quality by Divvy or ride_length was negative
# Creating a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCTED DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Condensing the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Comparing Subscribers and Customers
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)

# See the average ride time by each day for Subscribers vs Customers
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week,
          FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now let's run the average ride time by each day for Subscribers vs Customers again
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week,
          FUN = mean)


# analyzing ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>% #creates weekday field using  wday() 
  group_by(usertype, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
arrange(usertype, weekday)  # sorts
  
 # visaulizing the number of rides by usertype
 all_trips_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")




#=================================================
# STEP 5: EXPORTED SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Creating a csv file that we will visualize in Excel, Tableau, or my presentation software
avg_ride_length <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype +
                      all_trips_v2$day_of_week, FUN = mean)

number_of_rides <- aggregate(all_trips_v2$trip_id ~ all_trips_v2$usertype + all_trips_v2$day_of_week 
                         , FUN = length)
write.csv(avg_ride_length, file = "C:/Users/user/OneDrive/Desktop/My Project/Project 1/Finished/avg_ride_length.csv")
write.csv(number_of_rides, file = "C:/Users/user/OneDrive/Desktop/My Project/Project 1/Finished/number_of_rides.csv")