library(dplyr)
library(ggplot2)


tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)


texas_area <- read.csv("Area.csv", stringsAsFactors = FALSE)
texas_area$X <- gsub(",", "",texas_area$X)
names(texas_area) <- c("county", "area")
texas_area$county <- tolower(texas_area$county)
texas_area$area <- as.numeric(texas_area$area)

tx <- left_join(tx, texas_area, by = "county")
tx$population_density <- tx$pop / tx$area

ggplot(subset(tx, !is.na(log(population_density)))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = log(population_density)))+
  coord_quickmap()+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  labs(title = "Texas Population Density")
  