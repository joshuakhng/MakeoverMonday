#---- Soccer - Makeover Mondy (Week 23)----
library(tidyverse)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
library(lubridate)
library(zoo)

#import data
soccer<-read_excel("week23.xlsx")

#remove everything after "--" (including "--")
x<-soccer$Sport
soccer$Sport<-gsub("\\--.*","",x)

#removing potential trailing and leading spaces
soccer$Sport<-as.factor(str_trim(soccer$Sport, c("both", "left", "right")))
soccer$State<-str_trim(soccer$State, c("both", "left", "right"))

#calculating percentages and cleaning dates
soccer_clean_new<-soccer%>%
  select(Year, State, Sport, `Girls Participation`)%>%
  mutate(year_cleaned=as.numeric(substr(Year, 1, 4)))%>%
  group_by(year_cleaned, State, Sport)%>%
  summarize(`Girls Participation`=sum(`Girls Participation`))%>%
  ungroup()%>%
  group_by(year_cleaned)%>%
  mutate(country_total_participation=sum(`Girls Participation`, na.rm=TRUE))%>%
  ungroup()%>%
  group_by(year_cleaned, State)%>%
  mutate(state_total_participation=sum(`Girls Participation`, na.rm=TRUE))%>%
  ungroup()%>%
  group_by(year_cleaned, State, Sport)%>%
  mutate(percent_participation=`Girls Participation`/state_total_participation)%>%
  filter(`Girls Participation`!=0)%>%
  ungroup()
soccer_clean_new$year_cleaned<-(ISOdate(soccer_clean_new$year_cleaned, 1, 1))

#changes in percentages over the last 5 and 10 years
change_1<-soccer_clean_new%>%
  filter(year(year_cleaned) %in% c(2008, 2013, 2018))%>%
  select(year_cleaned, State, Sport, percent_participation)%>%
  spread(year_cleaned, percent_participation)

#change names of columns
colnames(change_1)[3]<-"ten_year"
colnames(change_1)[4]<-"five_year"
colnames(change_1)[5]<-"today"

#replace all NAs with zeroes
#change_1[is.na(change_1)] <- 0

#calculating changes over the years
change_2<-change_1%>%
  filter(!is.na(ten_year), !is.na(five_year), !is.na(today))%>%
  mutate(five_year_change=(today-five_year)/five_year,
         ten_year_change=(today-ten_year)/ten_year)%>%
  select(State, Sport, five_year_change, ten_year_change)

#left join into the original dataset
final_df<-left_join(soccer_clean_new, change_2, by=c("State"="State", "Sport"="Sport"))
final_df
  
#select top 8 sports to use in visualization 
top_sports<-soccer%>%
  select(Year, State, Sport, `Girls Participation`)%>%
  group_by(Sport)%>%
  summarize(total_participation=sum(`Girls Participation`))%>%
  arrange(-total_participation)
top_sports<-top_sports[1:20,]

#limiting to the top 20 sports in the nation
final_df<-final_df%>%
  filter(Sport %in% top_sports$Sport)

write.csv(final_df, "soccer_cleaned.csv")



#--------Another dataset-------------
#remove everything after "--" (including "--")
x<-soccer$Sport
soccer$Sport<-gsub("\\--.*","",x)

#removing potential trailing and leading spaces
soccer$Sport<-as.factor(str_trim(soccer$Sport, c("both", "left", "right")))
soccer$State<-str_trim(soccer$State, c("both", "left", "right"))

#calculating percentages and cleaning dates
y<-soccer$Sport
soccer_n<-soccer
soccer_n$Sport<-y%>%
  gsub("Field Hockey", "Ice/Field Hockey", .)%>%
  gsub("Ice Hockey", "Ice/Field Hockey", .)%>%
  gsub("Softball", "Base/Softball", .)%>%
  gsub("Baseball", "Base/Softball",.)

soccer_new_non_ag<-soccer_n%>%
  select(Year, State, Sport, `Girls Participation`)%>%
  mutate(year_cleaned=as.numeric(substr(Year, 1, 4)))%>%
  filter(`Girls Participation`!=0)
soccer_new_non_ag$year_cleaned<-(ISOdate(soccer_new_non_ag$year_cleaned, 1, 1))
write.csv(soccer_new_non_ag, "soccer_cleaned_1.csv")


  