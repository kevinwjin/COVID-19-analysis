# Problem 3: Will COVID-19 cases continue to rise in the future or go down? 
# What about vaccine uptake? (Kevin)

# Set working directory
setwd("~/Documents/Programming/Repositories/covid-19-associations/presentation/kevin")

# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tseries)
library(ggpubr)
library(gridExtra)

# Load Texas cases data, extract daily cases
cases_tx <- read.csv("time_series_covid19_confirmed_US.csv") %>% 
  filter(Province_State == "Texas") %>%
  select(starts_with("X")) 

# Transpose data frame
cases <- as.data.frame(t(cases_tx))

# Add columns for daily and total Texas cases across all counties
cases <- cases %>% 
  mutate(total = rowSums(cases)) %>%
  mutate(daily = diff(c(0, total))) %>%
  filter(daily >= 0) %>% # remove rows with negative cases
  mutate(daily_thousands = daily/1000) %>%
  mutate(total_millions = total/1000000)

# Create date column
dates <- rownames(cases)
dates <- sub('^X', '', dates)
dates <- mdy(dates)
cases$date <- dates

# Augmented Dickey-Fuller test shows non-stationary behavior
# p-value > 0.05; we reject the null hypothesis that the data is stationary
adf.test(cases$daily)

# Draw cases time series plots
cuts <- data.frame(Ref = c("Mask mandate\ndeclared \n(2020-07-02)",
                           "Alpha + Beta\ndesignated\n(2020-12-08)",
                           "Delta \ndesignated\n(2021-05-11)",
                           "Omicron \ndesignated \n(2021-11-26)"),
                   vals = c(as.Date("2020-07-02"), 
                            as.Date("2020-12-08"), 
                            as.Date("2021-05-11"),
                            as.Date("2021-11-26")),
                   stringsAsFactors = FALSE)
cd <- ggplot(data = cases, mapping = aes(x = dates, y = daily_thousands)) + 
      geom_line() +
      labs(x = "Date", y = "Cases (in thousands)", 
           title = "Daily Cases in Texas (Dickey-Fuller = -2.65, p = 0.3)") + 
      scale_y_continuous(breaks = seq(0, 100, by = 25)) + 
      scale_x_date(date_breaks = "2 months") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()) +
      geom_vline(data = cuts,
                mapping = aes(xintercept = vals,
                              color = Ref),
                linetype = 4, 
                size = 0.8,
                show.legend = FALSE) +
      geom_text(data = cuts,
                mapping = aes(x = vals,
                              y = 80,
                              label = Ref,
                              hjust = 1.1))
ct <- ggplot(data = cases, mapping = aes(x = dates, y = total_millions)) + 
      geom_line() + 
      labs(x = "Date", y = "Cases (in millions)", 
                 title = "Total Cases in Texas (Local Regression)") + 
      scale_x_date(date_breaks = "2 months") +
      stat_smooth(geom = "line",
                  method = "loess",
                  alpha = 0.5,
                  linetype = 1,
                  color = "blue") +
      geom_vline(data = cuts,
                 mapping = aes(xintercept = vals,
                               color = Ref),
                 linetype = 4, 
                 size = 0.8,
                 show.legend = FALSE) + # Abbott orders mask mandate 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
            #legend.position = c(0.90, 0.30),
            #legend.title = element_blank()) +
      #scale_color_discrete("Events", label = c("Mask Mandate"))
ggarrange(cd, ct, nrow = 2)
ggsave("cases.png")

# Load Texas vaccine data
vax <- read.csv("vaccine_data_us_timeline.csv")

# Extract daily vaccine administrations from cumulative sum
vax$Date <- ymd(vax$Date) # convert Date column to Date format
vax_tx <- vax %>%
  filter(Province_State == "Texas" & # extract Texas and All vaccine type rows
           Vaccine_Type == "All") %>% 
  select(Date, Doses_admin) %>% # extract Date and Doses Administered variables
  mutate(Doses_millions = Doses_admin/1000000) %>% # display total doses by millions
  mutate(Doses_admin_daily = diff(c(0, Doses_admin))) %>% # extract daily cases
  filter(Doses_admin_daily >= 0) %>% # remove rows with negative doses
  mutate(Doses_admin_daily_thousands = Doses_admin_daily/1000) # display in thousands

# Augmented Dickey-Fuller test shows non-stationary behavior
# p-value > 0.05; we reject the null hypothesis that the data is stationary
vax_tx <- vax_tx %>% dplyr::filter(!is.na(Doses_admin_daily)) # remove na rows
adf.test(vax_tx$Doses_admin_daily)

# Draw vaccine time series plots
eua <- data.frame(Ref = c("Pfizer EUA\nissued\n(2020-12-11)",
                           "Moderna EUA\nissued\n(2020-12-18)",
                           "J&J EUA\nissued\n(2021-02-27)",
                           "Boosters\nauthorized\n(2021-09-22)"),
                   vals = c(as.Date("2020-12-11"), 
                            as.Date("2020-12-18"), 
                            as.Date("2021-02-27"),
                            as.Date("2021-09-22")),
                   stringsAsFactors = FALSE)

vd <- ggplot(data = vax_tx, mapping = aes(x = Date, 
                                          y = Doses_admin_daily_thousands)) + 
      geom_line() +
      labs(x = "Date", 
           y = "Doses (in thousands)", 
           title = "Daily Vaccine Doses Administered in Texas (Dickey-Fuller = -2.65, p = 0.3)") +
      scale_x_date(date_breaks = "2 months") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()) +
      geom_vline(data = eua,
                 mapping = aes(xintercept = vals,
                               color = Ref),
                 linetype = 4, 
                 size = 0.8,
                 show.legend = TRUE) +
  guides(color = guide_legend(reverse = TRUE))
      # geom_text(data = eua,
      #           mapping = aes(x = vals,
      #                         y = 600,
      #                         label = Ref,
      #                         hjust = 0.5))
vt <- ggplot(data = vax_tx, mapping = aes(x = Date, y = Doses_millions)) + 
      geom_line() + 
      labs(x = "Date", y = "Doses (in millions)", 
           title = "Total Vaccine Doses Administered in Texas by Date") +
      scale_y_continuous(breaks = seq(0, 50, by = 10)) + 
      scale_x_date(date_breaks = "2 months") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(data = eua,
                 mapping = aes(xintercept = vals,
                               color = Ref),
                 linetype = 4, 
                 size = 0.8,
                 show.legend = TRUE) +
  guides(color = guide_legend(reverse = TRUE))
ggarrange(vd, vt, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave("vaccines.png")
