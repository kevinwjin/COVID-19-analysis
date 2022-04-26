# Library
library(UsingR)
library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(grid)
library(gapminder)



#---------------------------------------------------------------------------------------

# Data from https://dshs.texas.gov/coronavirus/additionaldata/  
# Case and Fatality Demographics Data -> Case and Fatality Demographics Data - Counts

# Create New Data Frame and replace the file location with your own
Coviddemo <- read_excel("COVIDdemo.xlsx")


#Data Cleaning
colnames(Coviddemo) <- c("Month Year", "<1", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+", "Total")
Coviddemo <- Coviddemo[-c(1, 2, 3, 15, 29, 32, 34, 35, 36, 37),]
Coviddemo <- Coviddemo[-c(11,24,26),]
Coviddemo <- Coviddemo[-24,]
Coviddemo <- Coviddemo[-12]
Demorownames <- Coviddemo$`Month Year`
Coviddemo <- Coviddemo[-1]
rownames(Coviddemo) <- Demorownames
Coviddemo <- t(Coviddemo)
lablist <- colnames(Coviddemo)

# Graph
barplot(as.matrix(Coviddemo),
        cex.names = 0.65,
        las = 2,
        legend = TRUE,
        ylim = c(0, 10000)
        )






#---------------------------------------------------------------------------------------

# Data from https://www.cdc.gov/pcd/issues/2021/21_0123.htm#tables
# Table 1

# Create New Data Frame and replace the file location with your own
CovidCDC <- read_excel("CDCcovid.xlsx")

# Data Cleaning
Preexisting <- data.frame(CovidCDC[4:11,])
Preexisting <- Preexisting[-c(1, 2, 3),]
rownames(Preexisting) <- Preexisting$Column1
Preexisting <- Preexisting[-c(1, 2)]
colnames(Preexisting) <- c("Full Sample", "ICU", "IMV", "Died")


#Convert to percentage

Preexisting$`Full Sample` <- c(5.1, 7.4, 39.3, 31.0, 17.3)
Preexisting$`ICU` <- c(2.9, 5.7, 37.8, 34.0, 19.6)
Preexisting$`IMV` <- c(1.5, 3.6, 35.7, 37.5, 21.6)
Preexisting$Died <- c(0.9,2.6, 32.3, 39.1, 25.1)

# Graph of Percent of Covid Patients with Underlying Health Conditions
barplot(as.matrix(Preexisting), cex.names = 0.8,
        xlab = "Hospitilized Adult Patients with COVID-19 March 2020 - March 2021",
        ylab = "Percent",
        legend = TRUE,
        xlim = c(0, 7),
        args.legend = list(title = "No. of Conditions"),
        main = "Percent of COVID Patients with Underlying Health Conditions",
        cex.main = 1,
        )


#---------------------------------------------------------------------------------------

# Data from https://www.cdc.gov/pcd/issues/2021/21_0123.htm#tables
# Table 1

# Alternate Graph of Percent of Covid Patients with Underlying Health Conditions

# Data Cleaning
Preexisting1 <- data.frame(CovidCDC[4:11,])
Preexisting1 <- Preexisting1[-c(1, 2),]
Preexisting1 <- Preexisting1[-c(3:6),]
rownames(Preexisting1) <- Preexisting1$Column1
greaterthan1 <- substring(rownames(Preexisting1)[1],1,2)
Preexisting1 <- Preexisting1[-c(1, 2)]
rownames(Preexisting1) <- c(greaterthan1, "0")
colnames(Preexisting1) <- c("Full Sample", "ICU", "IMV", "Died")


# Convert to percentage
Preexisting1$`Full Sample` <- c(94.9, 5.1)
Preexisting1$`ICU` <- c(97.1, 2.9)
Preexisting1$`IMV` <- c(98.5, 1.5)
Preexisting1$Died <- c(99.1, 0.9)

# Graph
barplot(as.matrix(Preexisting1), cex.names = 0.8,
        xlab = "Hospitilized Adult Patients with COVID-19 March 2020 - March 2021",
        ylab = "Percent",
        legend = TRUE,
        xlim = c(0, 7),
        args.legend = list(title = "No. of Conditions"),
        main = "Percent of COVID Patients with Underlying Health Conditions",
        cex.main = 1,)

#---------------------------------------------------------------------------------------

# Data from https://www.cdc.gov/pcd/issues/2021/21_0123.htm#tables
# Table 3

# Side by Side bar graph of frequency of underlying medical conditions of adults hospitalized with COVID


# Create New Data Frame and replace the file location with your own
Underlying <- read_excel("CDCunderlying.xlsx")


# Data cleaning
Underlying <- Underlying[-c(3, 4, 5)]
Underlying <- Underlying[-c(1, 2, 3, 4, 8, 9, 18, 19, 58, 59),]

AgeGroup18 <- Underlying[c(2:8),]
names(AgeGroup18) <- c("Underlying Condition", "Percent")
 
AgeGroup40 <- Underlying[c(10:25),]
names(AgeGroup40) <- c("Underlying Condition", "Percent")

AgeGroup65 <- Underlying[c(47:66),]
names(AgeGroup65) <- c("Underlying Condition", "Percent")

for(x in 1:nrow(AgeGroup18)){
  AgeGroup18$Percent[x] <- substr(AgeGroup18$Percent[x], 
                                              nchar(AgeGroup18$Percent[x]) - 4, 
                                                     nchar(AgeGroup18$Percent[x]) - 1)
}

for(x in 1:nrow(AgeGroup40)){
  AgeGroup40$Percent[x] <- substr(AgeGroup40$Percent[x], 
                                  nchar(AgeGroup40$Percent[x]) - 4, 
                                  nchar(AgeGroup40$Percent[x]) - 1)
}
  
for(x in 1:nrow(AgeGroup65)){
  AgeGroup65$Percent[x] <- substr(AgeGroup65$Percent[x], 
                                  nchar(AgeGroup65$Percent[x]) - 4, 
                                  nchar(AgeGroup65$Percent[x]) - 1)
}

# Create new data frames with a category column

G18 <- mutate(AgeGroup18, Group = rep("18-39", 7))
G40 <- mutate(AgeGroup40, Group = rep("40-64", 16))
G65 <- mutate(AgeGroup65, Group = rep("65+", 20))

# Combine into one data frame
combined <- rbind(G18,G40, G65)

# Graph (Use Zoom for True Graph)
ggplot(data = combined, mapping = aes(x = Group, y = as.numeric(Percent), fill = `Underlying Condition`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(legend.key.size = unit(0.5, "cm"), legend.position = "right", 
        legend.text = element_text(size=20), legend.title = element_text(size = 24)) +
  guides(fill = guide_legend(nrow = 22, byrow = TRUE)) + 
  ggtitle("Frequency of Underlying Medical Conditions in Adults Hospitalized with COVID-19") + 
  ylab("Percent") +
  xlab("Age Group (Years)")

