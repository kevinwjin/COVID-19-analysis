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

texas_new$rate <- texas_new$summary / texas_new$pop


ggplot(subset(texas_new, !is.na(rate))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = rate))+
  coord_quickmap()+
  scale_fill_distiller(palette = "Reds", direction = 1)

texas_new_data <- texas_new[!duplicated(texas_new$county), ]

ggplot(data = texas_new_data) +
  geom_point(mapping = aes(x = log10(pop), y = rate), color = "blue")+
  geom_smooth(mapping = aes(x = log10(pop), y = rate), color = "black", 
              method = "lm", se = FALSE)+
  labs(title = "Relationship Between Population and Covid Case Rate", 
       x = "Population(log10)", y = "Covid Case Rate" )

cor(texas_new_data$pop, texas_new_data$rate, use = "complete.obs", 
    method = "spearman")


cor(texas_new_data$pop, texas_new_data$rate, use = "complete.obs", 
    method = "spearman")
model_1 <- lm(texas_new_data$rate ~ texas_new_data$pop)
