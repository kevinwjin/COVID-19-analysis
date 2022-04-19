library(dplyr)
library(ggplot2)

tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)
ggplot(tx) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black")+
  coord_quickmap()

covid_fatalities <- read.csv("fatalities.csv")
covid_fatalities <- select(covid_fatalities, 1, 4)
names(covid_fatalities) <- c("county", "fatalities")
covid_fatalities$county <- tolower(covid_fatalities$county)
covid_fatalities <- covid_fatalities[c(2: 255), ]
texas_new_fatalities <- left_join(tx, covid_fatalities, by = "county")
texas_new_fatalities$fatalities <- as.numeric(texas_new_fatalities$fatalities)
texas_new_fatalities$rate_fatalities <- texas_new_fatalities$fatalities / 
  texas_new_fatalities$pop


ggplot(subset(texas_new_fatalities, !is.na(rate_fatalities))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = rate_fatalities))+
  coord_quickmap()+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(fill = "rate", title = "Texas Covid Fatality Rate") 
  
