library(lubridate)

# Extreme example from lubridate package help
x <- c(20100101120101, 
       "2009-01-02 12-01-02", 
       "2009.01.03 12:01:03",
       "2009-1-4 12-1-4",
       "2009-1, 5 12:1, 5",
       "200901-08 1201-08",
       "2009 arbitrary 1 non-decimal 6 chars 12 in between 1 !!! 6",
       "OR collapsed formats: 20090107 120107 (as long as prefixed with zeros)",
       "Automatic wday, Thu, detection, 10-01-10 10:01:10 and p format: AM",
       "Created on 10-01-11 at 10:01:11 PM")
x_timestamp <- ymd_hms(x)
class(x_timestamp)
print(x_timestamp)


# Parsing mechanism
x <- c("23/12/2013", "07/10/2014")
dmy(x)

x <- c("2013/12/23", "2013/10/07")
x_date <- ymd(x)

class(x_date)

# Handling truncation
x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
ymd_hms(x, truncated = 3) #up to 3 elements may be missing



# covid dataset -----------------------------------------------------------

covid_raw <- readRDS("data/covid-ita-regions.Rds")

# Several Covid-19 indicators by date and region - Italy

# Quick data check
glimpse(covid_raw)
summary(covid_raw)

covid_raw %>% 
  select(time) %>% 
  mutate(datetime_tz = ymd_hms(time, tz = 'CET'),
         # get the Chicago time for each 'CET' entry
         chicago_time = with_tz(datetime_tz, tz = 'America/Chicago'),
         # if you calculate the Chicago time from Chicago time it keeps the same value.
         chicago_time2 = with_tz(chicago_time, tz = 'America/Chicago'))

# search for time zone name --------------------------------------------------------------

# you can manually search in here: 
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
#
# Otherwise you can find the time zones with the OlsonNames() function,
# and you can filter it with 'grep'
# this line returns the New York time zone
grep("York", OlsonNames(), value=TRUE)
grep("Chicago", OlsonNames(), value=TRUE)

covid_raw %>% 
  select(time) %>% 
  mutate(
    # Parse the time string
    time = ymd_hms(time),
    # Set the time time-zone as Central Europe Time
    time = force_tz(time, tz = 'CET'),
    # Calculate the time in Chicago
    chicago_time = with_tz(time, tz = 'America/Chicago'),
    # Calculate the time in New York
    ny_time = with_tz(time, tz = 'America/New_York')
  )


# Example: China --------------------------------------------------------------

# China has only one time zone: Beijing Time
# You can see from here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
# that it match with the 'Asia/Shanghai' time zone
covid_time <- covid_raw %>% 
  select(time) %>% 
  mutate(datetime_tz = ymd_hms(time, tz = 'CET')) %>% 
  select(-time)

covid_time %>% 
  mutate(
    # manually calculating the Chinese time as 7 hours forward
    china_time = datetime_tz + hours(7),
    # manually set the correct time zone
    real_china_time = force_tz(china_time, tz = 'Asia/Shanghai'),
    # calculate Chinese time (the previous two steps) with 'with_tz' function.
    calculated_china_time = with_tz(datetime_tz, tz = 'Asia/Shanghai')
  )



# other functions ---------------------------------------------------------
result <- 
  covid_time %>% 
  mutate(
    year = year(datetime_tz),
    month = month(datetime_tz, label = T),
    day = day(datetime_tz),
    hour= hour(datetime_tz),
    plus_seven = datetime_tz + hours(7) + days(1) + months(7),
    date_month = floor_date(datetime_tz, unit = 'month')
  ) %>% 
  group_by(date_month) 

