# Relationship Between Population and Covid Case Rate
library(dplyr)
library(ggplot2)

# Read Texas dataset 
tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)


# Read covid case data and clean the data 
Texas <- read.csv("Texas.csv")
Texas <- select(Texas, 1, 76)
names(Texas) <- c("county", "summary")
Texas$county <- tolower(Texas$county)
Texas <- Texas[c(3: 256), ]
texas_new <- left_join(tx, Texas, by = "county")
texas_new$summary <- as.numeric(texas_new$summary)
range(texas_new$summary, na.rm = TRUE)

# Caculate the covid case rate
texas_new$rate <- texas_new$summary / texas_new$pop

# Delete the duplicate rows
texas_new_data <- texas_new[!duplicated(texas_new$county), ]

# Plot the map of the covid case rate
ggplot(data = texas_new_data) +
  geom_point(mapping = aes(x = log10(pop), y = rate), color = "blue")+
  geom_smooth(mapping = aes(x = log10(pop), y = rate), color = "black", 
              method = "lm", se = FALSE)+
  labs(title = "Relationship Between Population and Covid Case Rate", 
       x = "Population(log10)", y = "Covid Case Rate" )


# Find the correlation of the texas 
cor(texas_new_data$pop, texas_new_data$rate, use = "complete.obs", 
    method = "spearman")

# Applied Linear Model
model_1 <- lm(texas_new_data$rate ~ texas_new_data$pop)


# Texas Covid Case Rate Map
library(dplyr)
library(ggplot2)

# Read Texas dataset 
tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)


# Read covid case data and clean the data 
Texas <- read.csv("Texas.csv")
Texas <- select(Texas, 1, 76)
names(Texas) <- c("county", "summary")
Texas$county <- tolower(Texas$county)
Texas <- Texas[c(3: 256), ]
texas_new <- left_join(tx, Texas, by = "county")
texas_new$summary <- as.numeric(texas_new$summary)
range(texas_new$summary, na.rm = TRUE)

# Caculate the covid case rate
texas_new$rate <- texas_new$summary / texas_new$pop

# Plot the Texas covid case rate Map
ggplot(subset(texas_new, !is.na(rate))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = rate))+
  coord_quickmap()+
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Texas Covid Case Rate(2020 - 2022)")

# Texas Population Density Map
library(dplyr)
library(ggplot2)

# Read Texas dataset 
tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)

# Read Texas area dataset and clean the data 
texas_area <- read.csv("Area.csv", stringsAsFactors = FALSE)
texas_area$X <- gsub(",", "",texas_area$X)
names(texas_area) <- c("county", "area")
texas_area$county <- tolower(texas_area$county)
texas_area$area <- as.numeric(texas_area$area)
tx <- left_join(tx, texas_area, by = "county")
tx$population_density <- tx$pop / tx$area

# Plot Texas Population Density
ggplot(subset(tx, !is.na(log(population_density)))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = log(population_density)))+
  coord_quickmap()+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  labs(title = "Texas Population Density")

# Relationship Between Vaccine Rate and Fataility Rate in Texas
library(dplyr)
library(ggplot2)

# Read Texas dataset
tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)

# Read Texas covid vaccine rate dataset and clean the data
covid_vaccine <- read.csv("Vaccine.csv")
covid_vaccine <- select(covid_vaccine, 1, 7)
names(covid_vaccine) <- c("county", "vaccine")
covid_vaccine$county <- tolower(covid_vaccine$county)
covid_vaccine <- covid_vaccine[c(4: 257), ]
texas_new <- left_join(tx, covid_vaccine, by = "county")
texas_new$vaccine <- gsub(",", "",texas_new$vaccine)
texas_new$vaccine <- as.numeric(texas_new$vaccine)
texas_new$vaccine_rate <- texas_new$vaccine / texas_new$pop

# Read Texas fatalities rate dataset and clean the data
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

# Find the correlation between vaccine rate and fatalities rate 
cor(texas_new_combine$rate_fatalities, texas_new_combine$vaccine_rate,
    use = "complete.obs", method = "spearman")

# Plot the graph of correlation 
ggplot(data = texas_new_combine) +
  geom_point(mapping = aes(x = vaccine_rate, y = rate_fatalities), 
             color = "purple")+
  geom_smooth(mapping = aes(x = vaccine_rate, y = rate_fatalities), 
              color = "black", 
              method = "lm", se = FALSE)+
  labs(title = "Relationship Between Vaccine Rate and Fatality Rate in Texas", 
       x = "Vaccine Rate", y = "Fatality Rate")

# Applied the Linear Model
model <- lm(texas_new_combine$rate_fatalities ~ texas_new_combine$vaccine_rate)
summary(model)


# Texas Covid Fatality Rate
library(dplyr)
library(ggplot2)

# Read Texas dataset
tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)

# Read Texas fatalities rate dataset and clean data
covid_fatalities <- read.csv("fatalities.csv")
covid_fatalities <- select(covid_fatalities, 1, 4)
names(covid_fatalities) <- c("county", "fatalities")
covid_fatalities$county <- tolower(covid_fatalities$county)
covid_fatalities <- covid_fatalities[c(2: 255), ]
texas_new_fatalities <- left_join(tx, covid_fatalities, by = "county")
texas_new_fatalities$fatalities <- as.numeric(texas_new_fatalities$fatalities)
texas_new_fatalities$rate_fatalities <- texas_new_fatalities$fatalities / 
  texas_new_fatalities$pop

# Plot Covid Fatality Rate Map
ggplot(subset(texas_new_fatalities, !is.na(rate_fatalities))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = rate_fatalities))+
  coord_quickmap()+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(fill = "rate", title = "Texas Covid Fatality Rate") 

# Texas Covid Vaccine Rate
library(dplyr)
library(ggplot2)

# Read Texas dataset
tx <- read.csv("map_texas.csv", stringsAsFactors = FALSE)

# Read Texas vaccine dataset and clean the data
covid_vaccine <- read.csv("Vaccine.csv")
covid_vaccine <- select(covid_vaccine, 1, 7)
names(covid_vaccine) <- c("county", "vaccine")
covid_vaccine$county <- tolower(covid_vaccine$county)
covid_vaccine <- covid_vaccine[c(4: 257), ]
texas_new <- left_join(tx, covid_vaccine, by = "county")
texas_new$vaccine <- gsub(",", "",texas_new$vaccine)
texas_new$vaccine <- as.numeric(texas_new$vaccine)
texas_new$vaccine_rate <- texas_new$vaccine / texas_new$pop


# Plot Texas Covid Vaccine rate Map
ggplot(subset(texas_new, !is.na(vaccine_rate))) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = vaccine_rate)) +
  coord_quickmap()+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(fill = "rate", title = "Texas Covid Vaccine Rate")


