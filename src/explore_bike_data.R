#Explore bike data characteristics

#Explore relationship between temperature and number 
#of riders
#
#Step 1: Load the bike data and look at the metadata

library(tidyverse)

bike <- read_csv("data/daily_bike_data.csv")

head(bike)
str(bike)

sort(names(bike))

###  Time trend of ridership ----
ggplot(data=bike)+
        geom_line(aes(dteday, y=cnt))

ggplot(data=bike)+
        geom_point(aes(x=temp, y=cnt))

### Cleaning ----
# dplyr verbs for data transformation
#  select: select columns that we want to keep
#  filter: select rows that we want
#  mutate: transform data while keeping other columns
#  transmute: creates new columns and does not keep old columns
#  %>%: "pipe"  pipes the output from one command as the input
#        for the next command
bike %>% select(dteday, season, weathersit, temp, cnt)
select(bike,dteday, season, weathersit, temp, cnt)

# One way of selecting spring records and just a few col
spring_bike <- filter(bike, season =="spring")
spring_bike_temp_cnt <- select(spring_bike, temp, cnt)

spring_bike_temp_cnt2 <- bike %>%
        filter(season=="spring")%>%
        select(temp,cnt)

## Exercise: select weathersit and cnt for all winter records
winter_bike_temp_cnt2 <- bike %>%
        filter(season=="winter")%>%
        select(weathersit,cnt)

### Mutate and Transmute with Factors and Dates
summary(bike$weathersit)
unique(bike$weathersit)

#Can reference the data documentation
#https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

bike2 <- bike %>%
        dplyr::mutate(
                weather_fac = factor(weathersit,
                                     levels = c(1,2,3,4),
                                     labels = c("Clear","Cloudy","Rainy", "Heavy Rain")))

unique(bike2$weather_fac)

bike2 %>% select(dteday, weathersit, weather_fac)

### Converting to and from dates
sort(names(bike))
bike_dates <- bike %>% transmute(
        instant,
        date = dteday,
        date_num = as.numeric(dteday),
        date_char = as.character(dteday)
)

bike_dates %>%
        transmute(
                instant,
                date,
                date_num = as.Date(date_num, origin ="1970-01-01"),
                date_char = as.Date(date_char)
)