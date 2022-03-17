library(lubridate) # for working with dates
library(ggplot2) # for drawing plots
library(dplyr) # for data cleaning
library(stringr) # for column name handling

# 6. Is the rollout of vaccinations correlated with a decrease in 
# cases/hospitalizations/deaths when considering pre-existing health 
# conditions? What about mask usage?
vax <- read.csv("vaccine_data_us_timeline.csv") # load vax data
vax$Date <- ymd(vax$Date) # convert Date column to Date format
vax_tx <- vax %>%
  filter(Province_State == "Texas" & # extract Texas and All vaccine type rows
                           Vaccine_Type == "All") %>% 
  select(Date, Doses_admin) %>% # extract Date and Doses Administered variables
  mutate(Doses_millions = Doses_admin/1000000) # display doses by millions

ggplot(data = vax_tx, mapping = aes(x = Date, y = Doses_millions)) + 
  geom_line() +
  ylab("Total Doses Administered (in millions)") + 
  ggtitle("Total Vaccine Doses Administered to Date in Texas") +
  scale_x_date(breaks = "2 months") # scale x-axis by 2 months

cases <- read.csv("time_series_covid19_confirmed_US.csv") # load cases data
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

total <- left_join(sums, vax_tx, by = "Date")

ggplot(data = total) + 
  geom_line(mapping = aes(x = Date, y = Doses_admin, color = "blue")) + 
  geom_line(mapping = aes(x = Date, y = Cases)) + 
  ylab("Count") + 
  xlab("Date") + 
  ggtitle("Cases and Vaccine Doses by Date in Texas")

# 8. Is there a way to possibly predict the future course of the pandemic 
# (e.g. future case numbers, future deaths) based on modeling the cases and 
# deaths we have seen so far?
# 9. What models could be used to perform such inferences?
# 10. By analyzing the data, can we predict the trend in Texas? Is that going 
# to increase or decrease in the future?

ggplot(data = sums, mapping = aes(x = Date, y = Cases)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  ggtitle("Cumulative Cases by Date in Texas")


# 12. Does Texas ever appear to be spawning its own variant? That is, does 
# the virus seem more lethal here than elsewhere in the US or even the world? 

  