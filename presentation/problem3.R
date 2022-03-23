# Problem 3: Will COVID-19 cases continue to rise in the future or go down? 
# What about vaccine uptake? (Kevin)

# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Load data
cases <- read.csv("time_series_covid19_confirmed_US.csv")

cases_tx <- cases %>% filter(Province_State == "Texas") # subset by Texas

sums <- colSums(select(cases_tx, starts_with("X"))) # sum all case columns by day
sums <- as.data.frame((sums)) # convert to single column data frame
dates <- rownames(sums) # extract dates
sums <- mutate(sums, Date = dates) # add back as Date column
rownames(sums) <- NULL # delete row names
names(sums) <- c("Cases", "Date") # rename columns
sums <- sums[ , c("Date", "Cases")] # switch first and second columns
sums$Date <- sub('^X', '', sums$Date) # regex to remove X from all dates
sums$Date <- mdy(sums$Date) # convert dates to Date format

# Extract cases per day for whole US (non-cumulative)
cases_new <- cases %>% 
  select(Province_State, Confirmed, Deaths) %>%
  rename(State = Province_State)

cases_new$State <- factor(cases_new$State)

# Plot time series and fit linear regression
ggplot(data = cases_new, mapping = aes(x = reorder(State, Confirmed), 
                                       y = Confirmed,
                                       fill = Deaths)) + 
  geom_bar(stat = "identity", position = "stack") +
  xlab("State") +
  ylab("Confirmed Cases") +
  ggtitle("Confirmed Cases by State") +
  coord_flip() 

# Extract vaccine administrations per day (non-cumulative)