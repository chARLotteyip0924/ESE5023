library(tidyr)
library(dplyr)
library(ggplot2)

#1.1
Earthquake_Data <- read.delim ( file = "signif.txt", header = T,
                                sep = "\t", quote = "", dec = ".",
                                na.strings = "")
Sig_Eqs <- as_tibble(Earthquake_Data)

#1.2
Sig_Eqs_death <- Sig_Eqs%>%
  select(COUNTRY, DEATHS)%>%
  group_by( COUNTRY )%>%
  mutate(sum_death = sum(DEATHS, na.rm = T))%>%
  arrange(desc(sum_death))%>%
  select(COUNTRY, sum_death)
a <- Sig_Eqs_death[!duplicated(Sig_Eqs_death),]
a
  
#1.3
Sig_Eqs_mag <- Sig_Eqs%>%
  select(EQ_PRIMARY, YEAR)%>%
  group_by(YEAR)%>%
  filter(EQ_PRIMARY >= 6.0)
Sig_Eqs_mag[is.na(Sig_Eqs_mag)] <- 0

hist(Sig_Eqs_mag$YEAR)


#1.4
Sig_Eqs_country <- Sig_Eqs%>%
  select(COUNTRY)%>%
  group_by(COUNTRY)%>%
  count(COUNTRY)

Sig_Eqs_LargeEq <- Sig_Eqs%>%
  select(COUNTRY, EQ_PRIMARY, YEAR, MONTH, DAY)%>%
  group_by(COUNTRY)%>%
  summarise(max_EQ = max(EQ_PRIMARY, na.rm = T),
            max_YEAR = YEAR[which(EQ_PRIMARY == max(EQ_PRIMARY))],
            max_MONTH = MONTH[which(EQ_PRIMARY == max(EQ_PRIMARY))]  , 
            MAX_DAY = DAY[which(EQ_PRIMARY == max(EQ_PRIMARY))])

Sig_Eqs_both <- merge(Sig_Eqs_country, Sig_Eqs_LargeEq, by = "COUNTRY")
Sig_Eqs_both%>%
  arrange(desc(max_EQ))
class(Sig_Eqs_both)

CountEq_LargestEq <- function(){
  print.data.frame(Sig_Eqs_both)
 
}




  
  

  