library(tidyr)
library(dplyr)
library(ggplot2)
a <- read.csv("sample.csv", header = T)
a_tbl <- as_tibble(a)%>%
  select(DATE, TMP)%>%
  mutate(temp = as.numeric(substr(TMP,1,5)))%>%
  mutate(DAY = as.numeric(substr(DATE,9,10)))%>%
  group_by(DAY)%>%
  summarise(day_min_temp = min(temp))

a_tbl%>%
  ggplot(aes(DAY, day_min_temp))+
  geom_line()
  