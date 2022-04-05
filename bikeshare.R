# Loading packages
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggsci)

# Loading data into environment
ny <- read.csv('new_york_city.csv')
chi  <- read.csv('chicago.csv')
wash  <-  read.csv('washington.csv')

# Adding a city column/variable
ny <- mutate(ny, City = 'New York')
chi <- mutate(chi, City = 'Chicago')
wash <- mutate(wash, City = 'Washington')

# Appending data frames for all cities
df <- rbind(ny[c(-8,-9)], chi[c(-8,-9)], wash)

# Setting data types
df$City <- as.factor(df$City)
df$Start.Time <- strptime(df$Start.Time, format = '%Y-%m-%d %H:%M:%S')
df$Start.Date <- as.Date(df$Start.Time)

# Question 1
# The most common day of the week in each city
## Creating day of the week column
df$Start.Dow <- as.factor(weekdays(df$Start.Date))
df$Start.Dow <- factor(df$Start.Dow,
                       levels = c('Monday','Tuesday', 'Wednesday', 'Thursday',
                                  'Friday', 'Saturday', 'Sunday'))

## Summary
by(df$Start.Dow, df$City, summary)

## Histogram
ggplot(df, aes(x = Start.Dow)) +
  geom_bar(aes(fill = City), color = 'black', lwd = 0.1) +
  scale_x_discrete(labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')) +
  geom_vline(xintercept = 5.5, alpha = 0.5, linetype = 2) +
  coord_cartesian(ylim = c(30000, 55000)) +
  labs(x = 'Day of the week',
      y = 'Number of travels',
      title = 'Number of travels by day of the week and city',
      subtitle = 'Data for Jan, 2017 - Jun, 2017. Source: https://www.motivateco.com/') +
  facet_wrap(~ City) +
  theme_few(base_size = 12) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'ivory'))

# Question 2
# The frequency of each type of trip for each day of the week in each city
## Adding a trip type column
df <- mutate(df, Trip.Type = ifelse(Start.Station == End.Station, 'Round trip', 'Trip'))
df$Trip.Type <- as.factor(df$Trip.Type)

## Summary
by(df$Start.Dow, list(df$Trip.Type, df$City), summary)

## Plot 1
## How does the occurrence of round trips compare to that of other type of trips?
ggplot(df, aes(x = Start.Dow)) +
  geom_bar(aes(fill = Trip.Type), color = 'black', lwd = 0.1) +
  scale_x_discrete(labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')) +
  coord_cartesian(ylim = c(30000, 55000)) +
  geom_vline(xintercept = 5.5, alpha = 0.5, linetype = 2) +
  labs(x = 'Day of the week',
       y = 'Number of travels',
       fill = 'Trip type',
       title = 'Type of trip by day of the week and city',
       subtitle = 'Data for Jan, 2017 - Jun, 2017. Source: https://www.motivateco.com/') +
  facet_wrap(~City)+
  theme_few(base_size = 12) +
  theme(panel.background = element_rect(fill = 'ivory'))

## Plot 2
## Absolute number of round trips
ggplot(subset(df, Trip.Type == 'Round trip'), aes(x = Start.Dow)) +
  geom_bar(aes(fill = City), color = 'black', lwd = 0.1) +
  scale_x_discrete(labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')) +
  scale_y_continuous(limits = c(0, 2800), breaks = seq(0, 2800, 700)) +
  geom_vline(xintercept = 5.5, alpha = 0.5, linetype = 2) +
  labs(x = 'Day of the week',
       y = 'Number of travels',
       title = 'Round trips by day of the week and city',
       subtitle = 'Data for Jan, 2017 - Jun, 2017. Source: https://www.motivateco.com/') +
  facet_wrap(~City)+
  theme_few(base_size = 12) +
  theme(legend.position = 'none', panel.background = element_rect(fill = 'ivory'))

# Prep for questions 3 and 4
# Appending data frames for NY and Chicago, where age is available
ny_chi <- rbind(ny, chi)

# Creating age column
# This is just an estimate since only the year of birth was provided
ny_chi$Age <- as.integer(2017-ny_chi$Birth.Year)

# Creating age categories
ny_chi <- mutate(ny_chi, Age.Cat =
                   case_when(Age < 15 ~ 'Children',
                             Age >= 15 & Age < 25 ~ 'Youth',
                             Age >= 25 & Age < 35 ~ 'Adults (25-34)',
                             Age >= 35 & Age < 45 ~ 'Adults (35-44)',
                             Age >= 45 & Age < 55 ~ 'Adults (45-54)',
                             Age >= 55 & Age < 65 ~ 'Adults (55-64)',
                             Age >= 65 ~ 'Seniors'))

# Setting age category as factor
ny_chi$Age.Cat <- as.factor(ny_chi$Age.Cat)

# Setting data types
ny_chi$City <- as.factor(ny_chi$City)
ny_chi$User.Type <- as.factor(ny_chi$User.Type)
ny_chi$Age.Cat <- factor(ny_chi$Age.Cat,
                           levels = c('Children', 'Youth', 'Adults (25-34)', 'Adults (35-44)',
                                      'Adults (45-54)', 'Adults (55-64)', 'Seniors'))

# Question 3
# Age distribution in cities
## Summary
by(ny_chi$Age.Cat, ny_chi$City, summary)

## Plot 1
ggplot(subset(ny_chi, Age.Cat != 'Children'), aes(x = Age.Cat)) +
  geom_bar(aes(fill = City), color = 'black', lwd = 0.1) +
  scale_x_discrete() +
  coord_cartesian() +
  labs(x = 'Age group',
       y = 'Number of travels',
       title = 'Number of trips by age group and city',
       subtitle = 'Data for Jan, 2017 - Jun, 2017. Source: https://www.motivateco.com/') +
  facet_wrap(~ City)+
  theme_few(base_size = 12) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        panel.background = element_rect(fill = 'ivory'))

# Question 4
# Age and trip duration
## Converting trip duration to minutes
ny_chi <- mutate(ny_chi, Trip.Duration = round(Trip.Duration/60, digits = 2))

## Summary
by(ny_chi$Trip.Duration, list(ny_chi$Age.Cat, ny_chi$City), summary)

## Scatter plot
ggplot(ny_chi, aes(x = Age, y = Trip.Duration)) +
  geom_jitter(aes(color = City), shape = 20, alpha = 0.025) +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(limits = c(12, 80)) +
  labs(x = 'Age (years)',
       y = 'Trip length (min)',
       title = 'Trip length versus Age',
       subtitle = 'Data for Jan, 2017 - Jun, 2017. Source: https://www.motivateco.com/') +
  facet_wrap(~City) +
  theme_few(base_size = 12) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'ivory'))
