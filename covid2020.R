library(dplyr)
library(ggplot2)


tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)
ggplot(tx) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black")+
  coord_quickmap()

covid2020 <- read.csv("covid2020.csv")
covid2020 <- select(covid2020, 1, 304)
names(covid2020) <- c("county", "summary")
covid2020$county <- tolower(covid2020$county)
covid2020 <- covid2020[c(3: 256), ]
texas_new <- left_join(tx, covid2020, by = "county")
texas_new$summary <- gsub(",", "",texas_new$summary)
texas_new$summary <- as.numeric(texas_new$summary)
range(texas_new$summary, na.rm = TRUE)

texas_new$covid2020 <- cut(log10(texas_new$summary), breaks = 1:6, 
                             labels = c( "<100", "100 - 999",
                                        "1,000 - 9,999", 
                                        "10,000 - 99,999",
                                        ">100,000"))
                                        
ggplot(subset(texas_new, !is.na(covid2020))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = covid2020))+
  coord_quickmap()+
  scale_fill_brewer(palette = "Reds", direction = 1)
