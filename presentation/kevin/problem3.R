# Problem 3: Will COVID-19 cases continue to rise in the future or go down? 
# What about vaccine uptake? (Kevin)

# Set working directory
setwd("~/Documents/Programming/Repositories/covid-19-associations/presentation/kevin")

# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Load Texas cases data, extract daily cases
cases_tx <- read.csv("time_series_covid19_confirmed_US.csv") %>% 
  filter(Province_State == "Texas") %>%
  select(starts_with("X")) 

# Transpose data frame
cases <- as.data.frame(t(cases_tx))

# Add total column for daily Texas cases across all counties
cases <- cases %>% mutate(total = rowSums(cases))

# Add daily column for non-cumulative daily cases across all counties
cases$daily <- diff(c(0, cases$total))

# Create date column
dates <- rownames(cases)
dates <- sub('^X', '', dates)
dates <- mdy(dates)
cases$date <- dates

# Draw plots
ggplot(data = cases, mapping = aes(x = dates, y = daily)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(color = "red")) + 
  geom_smooth(method = "loess", aes(color = "blue")) + 
  labs(x = "Date", y = "Cases", 
       title = "Time Series Trend of Daily Cases in Texas") + 
  scale_color_manual(name = "Regression", values = c("blue", "red"),
                     labels = c("Local", "Linear"))
ggsave("daily_cases.png")

ggplot(data = cases, mapping = aes(x = dates, y = total)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(color = "red")) + 
  labs(x = "Date", y = "Cases", 
       title = "Time Series Trend of Total Cases in Texas") + 
  scale_color_manual(name = "Regression", values = c("red"),
                     labels = c("Linear"))
ggsave("total_cases.png")

# Load Texas vaccine data
#vax <- read.csv("time_series_covid19_vaccine_doses_admin_US.csv")
vax <- read.csv("vaccine_data_us_timeline.csv")

# Extract daily vaccine administrations
vax$Date <- ymd(vax$Date) # convert Date column to Date format
vax_tx <- vax %>%
  filter(Province_State == "Texas" & # extract Texas and All vaccine type rows
           Vaccine_Type == "All") %>% 
  select(Date, Doses_admin) %>% # extract Date and Doses Administered variables
  mutate(Doses_millions = Doses_admin/1000000) %>% # display doses by millions
  mutate(Doses_admin_daily = diff(c(0, Doses_admin)))

ggplot(data = vax_tx, mapping = aes(x = Date, y = Doses_admin_daily)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(color = "red")) + 
  geom_smooth(method = "loess", aes(color = "blue")) + 
  labs(x = "Date", y = "Doses Administered", 
       title = "Time Series Trend of Daily Vaccines Administered in Texas") +
  scale_color_manual(name = "Regression", values = c("blue", "red"),
                     labels = c("Local", "Linear"))
ggsave("daily_vax.png")

ggplot(data = vax_tx, mapping = aes(x = Date, y = Doses_millions)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(color = "red")) + 
  labs(x = "Date", y = "Doses Administered (in millions)", 
       title = "Time Series Trend of Total Vaccines Administered in Texas") +
  scale_color_manual(name = "Regression", values = c("red"),
                     labels = c("Linear"))
ggsave("total_vax.png")
