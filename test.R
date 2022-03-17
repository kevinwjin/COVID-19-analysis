library(ggplot2)
library(dplyr)

cases <- read.csv("03-09-2022.csv")

cases_new <- cases %>% 
  select(Province_State, Confirmed, Deaths) %>%
  rename(State = Province_State)

cases_new$State <- factor(cases_new$State)
# for (row in nrow(cases_new)) {
#   cases_new$State[row] <-  
# }

ggplot(data = cases_new, mapping = aes(x = reorder(State, Confirmed), 
                                      y = Confirmed,
                                      fill = Deaths)) + 
  geom_bar(stat = "identity", position = "stack") +
  xlab("State") +
  ylab("Confirmed Cases") +
  ggtitle("Confirmed Cases by State") +
  coord_flip() 


