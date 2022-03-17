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
texas_new <- left_join(tx, covid_fatalities, by = "county")
texas_new$fatalities <- as.numeric(texas_new$fatalities)
range(texas_new$fatalities, na.rm = TRUE)
texas_new$Covid_Fatalities <- cut(log10(texas_new$fatalities), breaks = 0:5, 
                           labels = c( "<10", "10 - 99",
                                       "100 - 999", "1,000 - 9,999",
                                       ">10,000"))
ggplot(subset(texas_new, !is.na(Covid_Fatalities))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = Covid_Fatalities))+
  coord_quickmap()+
  scale_fill_brewer(palette = "Blues", direction = 1)

