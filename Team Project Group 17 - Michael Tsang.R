library(UsingR)
library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)

#Set your own working directory here:
setwd("/Users/tsang/OneDrive/Desktop/Data")


#---------------------------------------------------------------------------------------

# Data from https://dshs.texas.gov/coronavirus/additionaldata/  
# Case and Fatality Demographics Data -> Case and Fatality Demographics Data - Counts

# Create New Data Frame and replace the file location with your own
Coviddemo <- read_excel("/Users/tsang/OneDrive/Desktop/Data/COVIDdemo.xlsx")


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

barplot(as.matrix(Coviddemo),
        cex.names = 0.65,
        las = 2,
        legend = TRUE,
        ylim = c(0, 10000)
        )

# Alternate code

# legend("topright", 
#       legend = c("<1", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
#       fill = )





#---------------------------------------------------------------------------------------

# Data from https://www.cdc.gov/pcd/issues/2021/21_0123.htm#tables

# Create New Data Frame
CovidCDC <- read_excel("/Users/tsang/OneDrive/Desktop/Data/CDCcovid.xlsx")

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

# Alternate Graph of Percent of Covid Patients with Underlying Health Conditions
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


barplot(as.matrix(Preexisting1), cex.names = 0.8,
        xlab = "Hospitilized Adult Patients with COVID-19 March 2020 - March 2021",
        ylab = "Percent",
        legend = TRUE,
        xlim = c(0, 7),
        args.legend = list(title = "No. of Conditions"),
        main = "Percent of COVID Patients with Underlying Health Conditions",
        cex.main = 1,)

#---------------------------------------------------------------------------------------



