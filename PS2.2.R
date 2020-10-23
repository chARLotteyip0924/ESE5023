library(tidyr)
library(dplyr)
library(ggplot2)


a <- read.csv("2281305.csv", header = T)
a_tbl <- as_tibble(a) %>%
  mutate(month = substr(DATE, 6,7))%>%
  mutate(year = as.numeric(substr(DATE, 1,4)))%>%
  mutate(speed = as.numeric(substr(WND, 9,12)))%>%
  select(month, year, speed)%>%
  group_by(month)

b <- aggregate(x = a_tbl$speed, by = list(a_tbl$year, a_tbl$month), FUN = mean)

names(b)[names(b) == "x"] <- "avewindspeed"
names(b)[names(b) == "Group.2"] <- "Month"
names(b)[names(b) == "Group.1"] <- "Year"
b_tbl <- as_tibble(b)
b_tbl%>%
  ggplot(aes(x= Year, y = avewindspeed, color = Month))+
  geom_line()+
  facet_wrap(~ Month)



 # ggplot(aes(x=year, y=speed, color=month)) + 
  #geom_line()+
  #facet_wrap(~ month)
?tapply(vector, index, function)