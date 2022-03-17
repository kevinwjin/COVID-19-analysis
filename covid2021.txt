library(dplyr)
library(ggplot2)


tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)
ggplot(tx) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black")+
  coord_quickmap()

covid2021 <- read.csv("covid2021.csv")
covid2021 <- select(covid2021, 1, 366)
names(covid2021) <- c("county", "summary")
covid2021$county <- tolower(covid2021$county)
covid2021 <- covid2021[c(3: 256), ]
texas_new <- left_join(tx, covid2021, by = "county")
texas_new$summary <- as.numeric(texas_new$summary)
range(texas_new$summary, na.rm = TRUE)
texas_new$covid2021 <- cut(log10(texas_new$summary), breaks = 1:6, 
                           labels = c( "<100", "100 - 999",
                                       "1,000 - 9,999", 
                                       "10,000 - 99,999",
                                       ">100,000"))
ggplot(subset(texas_new, !is.na(covid2021))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = covid2021))+
  coord_quickmap()+
  scale_fill_brewer(palette = "Reds", direction = 1)

