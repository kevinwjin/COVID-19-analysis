library(dplyr)
library(ggplot2)


tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)
ggplot(tx) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
                                        fill = "white", color = "black")+
  coord_quickmap()


Texas <- read.csv("Texas.csv")
Texas <- select(Texas, 1, 76)
names(Texas) <- c("county", "summary")
Texas$county <- tolower(Texas$county)
Texas <- Texas[c(3: 256), ]
texas_new <- left_join(tx, Texas, by = "county")
texas_new$summary <- as.numeric(texas_new$summary)
range(texas_new$summary, na.rm = TRUE)

texas_new$covid2022 <- cut(log10(texas_new$summary), breaks = 1:6, 
                             labels = c("<100", "100 - 1,000",
                                        "1,000 - 9,999", 
                                        "10,000 - 99,999",
                                        ">100,000"))
ggplot(subset(texas_new, !is.na(covid2022))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = covid2022))+
  coord_quickmap()+
  scale_fill_brewer(palette = "Reds", direction = 1)

