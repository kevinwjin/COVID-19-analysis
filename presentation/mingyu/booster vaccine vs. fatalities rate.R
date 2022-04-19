library(dplyr)
library(ggplot2)

tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)
ggplot(tx) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black")+
  coord_quickmap()

covid_vaccine <- read.csv("Vaccine.csv")
covid_vaccine <- select(covid_vaccine, 1, 7)
names(covid_vaccine) <- c("county", "vaccine")
covid_vaccine$county <- tolower(covid_vaccine$county)
covid_vaccine <- covid_vaccine[c(4: 257), ]
texas_new <- left_join(tx, covid_vaccine, by = "county")
texas_new$vaccine <- gsub(",", "",texas_new$vaccine)
texas_new$vaccine <- as.numeric(texas_new$vaccine)
texas_new$vaccine_rate <- texas_new$vaccine / texas_new$pop

covid_fatalities <- read.csv("fatalities.csv")
covid_fatalities <- select(covid_fatalities, 1, 4)
names(covid_fatalities) <- c("county", "fatalities")
covid_fatalities$county <- tolower(covid_fatalities$county)
covid_fatalities <- covid_fatalities[c(2: 255), ]
texas_new_fatalities <- left_join(tx, covid_fatalities, by = "county")
texas_new_fatalities$fatalities <- as.numeric(texas_new_fatalities$fatalities)
texas_new_fatalities$rate_fatalities <- texas_new_fatalities$fatalities / 
  texas_new_fatalities$pop

texas_new_vaccine_combine <- select(texas_new, 7, 10)
texas_new_combine <- left_join(texas_new_fatalities,texas_new_vaccine_combine, 
                               by = "county")
texas_new_combine <- texas_new_combine[!duplicated(texas_new_combine$county), ]
cor(texas_new_combine$rate_fatalities, texas_new_combine$vaccine_rate,
    use = "complete.obs", method = "spearman")





ggplot(data = texas_new_combine) +
  geom_point(mapping = aes(x = vaccine_rate, y = rate_fatalities), 
             color = "purple")+
  geom_smooth(mapping = aes(x = vaccine_rate, y = rate_fatalities), 
              color = "black", 
              method = "lm", se = FALSE)+
  labs(title = "Relationship Between Vaccine Rate and Fatality Rate in Texas", 
       x = "Vaccine Rate", y = "Fatality Rate")

model <- lm(texas_new_combine$rate_fatalities ~ texas_new_combine$vaccine_rate)
summary(model)