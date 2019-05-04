#################################### WQD7004: ASSIGNMENT 2-ANSWERS #######################################

# Name :
# Student ID :
myDF <- read.csv("2008.csv")

install.packages("dplyr")
library(dplyr)

# 1) Sort in decreasing order the ten most popular airports according to the number of flight departures 
# # (hint : use decreasing=T and matrix indexing )
sort(table(myDF$Origin), decreasing = T)[1:10]
sort(table(myDF$Origin), decreasing = T)[1:10]

# dplyr
myDF %>% 
  group_by(Origin) %>% 
  summarise(numOfFlight = n()) %>% 
  arrange(desc(numOfFlight)) %>% 
  slice(1:10)


# 2) Assign the names of the ten most popular airports according to the number of flight departures 
## variable called mostPopularOrg
# # (hint : use names() and matrix indexing)
mostpopularOrg<-names(sort(table(myDF$Origin), decreasing = T)[1:10])
mostpopularOrg

# dplyr
mostpopularOrg <- myDF %>% 
  group_by(Origin) %>% 
  summarise(numOfFlight = n()) %>% 
  arrange(desc(numOfFlight)) %>% 
  slice(1:10) %>% 
  pull(Origin)
mostpopularOrg

# 3) Assign the names of the ten most popular airports according to the number of flight arrivals 
## variable called mostPopularDes
mostpopularDes<-names(sort(table(myDF$Dest), decreasing = T)[1:10])
mostpopularDes
# dplyr
mostpopularDes <- myDF %>% 
  group_by(Dest) %>% 
  summarise(numOfFlight = n()) %>% 
  arrange(desc(numOfFlight)) %>% 
  slice(1:10) %>% 
  pull(Dest)
mostpopularDes

# 4)How many flights had their origin in one of these 10 most popular airports
## (hint : use %in%)
sum(myDF$Origin %in% mostpopularOrg)

# dplyr
myDF %>% 
  filter(Origin %in% mostpopularOrg) %>% 
  summarise(totalFlights = n())

# 5)How many flights had their destination in one of these 10 most popular airports
## (hint : use %in%)
sum(myDF$Dest %in% mostpopularDes)

# dplyr
myDF %>% 
  filter(Dest %in% mostpopularDes) %>% 
  summarise(totalFlights = n())

# 6) Find flights for which the origin and the destination
# were among the 10 most popular airports
## (hint : use %in%)
sum(myDF$Origin %in% mostpopularOrg & myDF$Dest %in% mostpopularDes)

# dplyr
myDF %>% 
  filter(Origin %in% mostpopularOrg, Dest %in% mostpopularDes) %>%
  summarise(totalFlights = n())

# 7) For the purposes of this question, treat the group 
# of the 200 least popular airports according to the 
# number of flights having these as the origins.
# How many flights had one of these 200 least popular 
# airports as their origin?
sort(table(myDF$Origin), decreasing = F)[1:200]
leastpopular<-names(sort(table(myDF$Origin), decreasing = F)[1:200])
leastpopular
sum(myDF$Origin %in% leastpopular)

# dplyr
leastpopular <- myDF %>% 
  group_by(Origin) %>% 
  summarise(numOfFlight = n()) %>% 
  arrange(numOfFlight) %>% 
  slice(1:200) %>% 
  pull(Origin)
leastpopular

myDF %>% 
  filter(Origin %in% leastpopular) %>%
  summarise(totalFlights = n())

# 8) We can index a vector according to the names of the elements in the vector
##8a) How many flights departed from "IND" ?
table(myDF$Origin)["IND"]

# dplyr
myDF %>% 
  filter(Origin == "IND") %>% 
  summarise(numOfFlight = n())

##8b) How many flights departed from "IND","ORD","JFK","EWR","IAD" ?
table(myDF$Origin)[c("IND","ORD","JFK","EWR","IAD")]
# dplyr
myDF %>%
  group_by(Origin) %>%
  filter(Origin %in% c("IND", "ORD", "JFK", "EWR", "IAD")) %>%
  summarise(numOfFlights = n())

##8c) How many flights departed from each of the 10 most popular airports ?
mostpopular <- mostpopularOrg
table(myDF$Origin)[mostpopular]
# dplyr

myDF %>% 
  group_by(Origin) %>%
  filter(Origin %in% mostpopular) %>%
  summarise(numOfFlights = n())

##8d) How many flights departed from each of 200 least popular airports ?
table(myDF$Origin)[leastpopular]
# dplyr
myDF %>% 
  group_by(Origin) %>% 
  filter(Origin %in% leastpopular) %>% 
  summarise(numOfFlights = n())

# 8e) How many flights landed at Ronald Reagan Washington
## National ("DCA") or Washington Dulles Airport ("IAD") in 2008? 
## (hint: Use just one command to get both of these counts 
## simultaneously)
table(myDF$Dest)[c("DCA","IAD")]

# dplyr
myDF %>%
  group_by(Dest) %>%
  filter(Dest %in% c("DCA", "IAD")) %>%
  summarise(numOfFlights = n())

# 9)Check the first 20 flights and see which one departed on time or early
sum(head(myDF$DepDelay <=0, n=20))
sum((myDF$DepDelay <=0)[1:20])
tapply(myDF$DepDelay <=0,myDF$Origin,sum,na.rm=T)[1:20]
# dplyr
myDF %>% 
  slice(1:20) %>% 
  filter(DepDelay <= 0) %>% 
  summarise(delayedFlights = n())

myDF %>% 
  group_by(Origin) %>% 
  filter(DepDelay <= 0) %>% 
  summarise(delayedFlights = n()) %>% 
  slice(1:20)

##9a) We restrict attention to only the 10 most popular airports 
##and see which one departed on time or early
depDelayMostPopAir<-tapply(myDF$DepDelay <=0,myDF$Origin,sum,na.rm=T)[mostpopular]
depDelayMostPopAir

# dplyr
depDelayMostPopAir<-  myDF %>% 
                      group_by(Origin) %>% 
                      filter(DepDelay <= 0, Origin %in% mostpopular) %>% 
                      summarise(delayedFlightsPopAir= n()) 
depDelayMostPopAir


##9b)Find the percentage of flights at each of the 10 most popular 
# airports that departed on time or early
OrgMostPopAir<-table(myDF$Origin)[mostpopular]
OrgMostPopAir
percentage<-depDelayMostPopAir/OrgMostPopAir
percentage

# dplyr
OrgMostPopAir <- myDF %>%
  group_by(Origin) %>% 
  filter(Origin %in% mostpopular) %>% 
  summarise(numOfFlightsOrg = n()) 
OrgMostPopAir

depDelayMostPopAir %>% 
  left_join(OrgMostPopAir, by = "Origin") %>% 
  mutate(percentage = delayedFlightsPopAir/numOfFlightsOrg) %>% 
  select(Origin, percentage)

# 9c) What percentage of flights departed from IND on time or early?
depDelayIND<-tapply(myDF$DepDelay <=0,myDF$Origin,sum,na.rm=T)["IND"]
OrgIND<-table(myDF$Origin)["IND"]
depDelayIND/OrgIND

# dplyr
depDelayIND <- myDF %>% 
  group_by(Origin) %>% 
  filter(DepDelay <= 0, Origin == "IND") %>% 
  summarise(numDepDelayIND = n())
depDelayIND

orgIND <- myDF %>% 
  group_by(Origin) %>% 
  filter(Origin == "IND") %>% 
  summarise(numOrgIND= n())
orgIND

depDelayIND %>% 
  left_join(orgIND, by = "Origin") %>% 
  mutate(percentageIND = numDepDelayIND / numOrgIND) %>% 
  select(Origin, percentageIND)

#10) Analyzing Flights by Origin Airport and Month of Departure
##10a) Break the data in the DepDelay vector according to which city of origin 
tapply(myDF$DepDelay,myDF$Origin,length)
# dplyr
myDF %>% 
  group_by(Origin) %>% 
  select(DepDelay) %>%
  summarise(numOfFlights = n())

##10b) Break the data in the DepDelay vector according to month
tapply(myDF$DepDelay,myDF$Month,length)
# dplyr
myDF %>% 
  group_by(Month) %>% 
  select(DepDelay) %>% 
  summarise(numOfFlights = n())

#11) How many flights delay occur from each airport in each month ?
tapply(myDF$DepDelay>0,list(myDF$Origin,myDF$Month),sum, na.rm=T)

# dplyr
myDF %>% 
  group_by(Origin, Month) %>% 
  filter(DepDelay > 0) %>% 
  summarise(numOfFlightDelay = n())

##11a) Extract the data from origin airport = "IND"
# and from the month of June
tapply(myDF$DepDelay>0,list(myDF$Origin,myDF$Month),sum,na.rm=T )["IND",6]

# dplyr
myDF %>% 
  group_by(Origin, Month) %>% 
  filter(DepDelay > 0, Origin == "IND", Month == 6) %>% 
  summarise(numOfFlightDelay = n())

##11b) Extract the data from origin airport = "ATL"
# and from the month of March
tapply(myDF$DepDelay>0,list(myDF$Origin,myDF$Month),sum,na.rm=T)["ATL",3]

# dplyr
myDF %>% 
  group_by(Origin, Month) %>% 
  filter(DepDelay > 0, Origin == "ATL", Month == 3) %>%
  summarise(numOfFlightDelay = n())

# 11c) The number of flights delay from 3 airports = "ATL","AUS","BDL"
# during the months of July through October
tapply(myDF$DepDelay >0,list(myDF$Origin,myDF$Month),sum,na.rm=T)[c("ATL","AUS","BDL"),c(7,8,9,10)]
tapply(myDF$DepDelay >0,list(myDF$Origin,myDF$Month),sum,na.rm=T)[c("ATL","AUS","BDL"),7:10]

# dplyr
myDF %>%
  group_by(Origin, Month) %>%
  filter(DepDelay > 0, Origin %in% c("ATL", "AUS", "BDL"), Month %in% c(7, 8, 9, 10)) %>%
  summarise(numOfFlightDelay = n())

# 11d) How many delayed departure flights altogether from ATL, AUS, and BDL during the months of 
#July 2008 through October 2008?
q<-tapply(myDF$DepDelay >0,list(myDF$Origin,myDF$Month),sum,na.rm=T)[c("ATL","AUS","BDL"),7:10]
q
sum(q)
colSums(q)
rowSums(q)

# dplyr
myDF %>%
  group_by(Origin, Month) %>%
  filter(DepDelay > 0, Origin %in% c("ATL", "AUS", "BDL"), Month %in% c(7:10)) %>% 
  summarise(numOfFlightDelay = n())

myDF %>%
  filter(DepDelay > 0, Origin %in% c("ATL", "AUS", "BDL"), Month %in% c(7:10)) %>% 
  summarise(numOfFlightDelay = n())

myDF %>%
  group_by(Month) %>% 
  filter(DepDelay > 0, Origin %in% c("ATL", "AUS", "BDL"), Month %in% c(7:10)) %>% 
  summarise(numOfFlightDelay = n())

myDF %>%
  group_by(Origin) %>% 
  filter(DepDelay > 0, Origin %in% c("ATL", "AUS", "BDL"), Month %in% c(7:10)) %>% 
  summarise(numOfFlightDelay = n())


# 11e) All the flight delays, month by month, frm IND airport
tapply(myDF$DepDelay>0,list(myDF$Origin,myDF$Month),sum,na.rm=T)["IND",1:12]
tapply(myDF$DepDelay>0,list(myDF$Origin,myDF$Month),sum,na.rm=T)["IND",]

# dplyr
myDF %>% 
  group_by(Origin, Month) %>% 
  filter(DepDelay > 0, Origin == "IND") %>% 
  summarise(numOfFlightDelay = n())

# 11f) All the flight delays, month by month, frm both IND and ORD at once
r<-tapply(myDF$DepDelay>0,list(myDF$Origin,myDF$Month),sum,na.rm=T)[c("IND","ORD"),]
r
class(r)
dim(r)

# dplyr
r<- myDF %>% 
    group_by(Origin, Month) %>% 
    filter(DepDelay > 0, Origin %in% c("IND", "ORD")) %>% 
    summarise(numOfFlightDelayr = n())


# 12) Calculating Percentages of Flights with delayed more than 30 minutes when departing
longdelayDF <- subset(myDF,myDF$DepDelay > 30)
dim(longdelayDF)
p<-tapply(longdelayDF$DepDelay,list(longdelayDF$Origin,longdelayDF$Month),length)[c("IND","ORD"),]
p
# dplyr
longdelayDF <- myDF %>% 
               filter(DepDelay > 30)
 
p<- longdelayDF %>% 
    group_by(Origin, Month) %>% 
    select(DepDelay) %>% 
    filter(Origin %in% c("IND", "ORD")) %>% 
    summarise(numOfFlightDelayp = n())
p
# 12a) find the percentage of flights with long delays and plot with dotchart()
m<-p/r
dotchart(m)

# dplyr
percentageM <- p %>% 
  left_join(r, by = c("Origin", "Month")) %>% 
  mutate(percentDelay30m = numOfFlightDelayp / numOfFlightDelayr) %>% 
  select(Origin, Month, percentDelay30m)
percentageM


# 12b) How many flights departed altogether from IND 
# or ORD in 2008 with a delay of more than 30 minutes each?
sum(p)

# dplyr
myDF %>%
  filter(DepDelay > 30, Origin %in% c("IND", "ORD"), Month %in% c(1:12)) %>% 
  summarise(numOfFlightDelay30m = n())

#12c) In which month of 2008 was the percentage of long delays 
#(i.e., flights with more than 30 minute delays) the highest?
colSums(m)

# dplyr
m_dplyr_IND <- percentageM %>% filter(Origin == "IND") %>% rename(pct_flights_depdelay_gt30_IND = pct_flights_depdelay_gt30)
m_dplyr_IND

m_dplyr_ORD <- percentageM %>% filter(Origin == "ORD") %>% rename(pct_flights_depdelay_gt30_ORD = pct_flights_depdelay_gt30)
m_dplyr_ORD

m_dplyr_IND_ORD <- m_dplyr_IND %>% 
  bind_cols(m_dplyr_ORD) %>% 
  mutate(pct_flights_depdelay_gt30_byMonth = pct_flights_depdelay_gt30_IND + pct_flights_depdelay_gt30_ORD) %>% 
  ungroup() %>% 
  select(Month, pct_flights_depdelay_gt30_byMonth)
m_dplyr_IND_ORD

# 13) Analyzing Flights by Time of Day for Departure
# Break the day into 4 parts:
# early morning (1) correspond to the times to 6 am
# late morning (2) correspond to the times to 6 am to 12 noon
# early evening (3) correspond to the times to 12 noon to 6 pm
# late evening (4) correspond to the times to 6 pm to 12 midnight
v<-ceiling(myDF$DepTime/600)
# dplyr
myDF_v_dplyr <- myDF %>% select(DepTime) %>% mutate(v_dplyr = ceiling(DepTime / 600)) %>% select(v_dplyr)

# build a vector called parts of the day
partsofday <- rep(NA, times=dim(myDF)[1])
partsofday
partsofday[v==1]<-"early morning"
partsofday[v==2]<-"late morning"
partsofday[v==3]<-"early evening"
partsofday[v==4]<-"late evening"
table(partsofday)
# dplyr
num_row <- myDF %>% dim() %>% nth(1)
num_row
partsofday_dplyr <- NA %>% rep(times = num_row)
partsofday_dplyr
partsofday_dplyr <- case_when(
  myDF_v_dplyr == 1 ~ "early morning",
  myDF_v_dplyr == 2 ~ "late morning",
  myDF_v_dplyr == 3 ~ "early evening",
  myDF_v_dplyr == 4 ~ "late evening"
)
# and we can create a new column in the myDF data frame called "timeofday"
# and we can store this information we just found into this column
myDF$timeofday <- partsofday
dim(myDF)
# dplyr
myDF_dplyr <- myDF_dplyr %>% mutate(timeofday_dplyr = partsofday_dplyr) 
myDF_dplyr %>% dim()

# just check to make sure that the first 6 flights were done properly
head(myDF$timeofday)
head(myDF$DepTime)
# dplyr
myDF_dplyr %>% select(timeofday_dplyr) %>% slice(1:6)
myDF_dplyr %>% select(DepTime) %>% slice(1:6)

# 13a) How many flights departed from IND early in the morning?
sum(myDF$Origin =="IND" & myDF$timeofday =="early morning",na.rm=T)
# dplyr
myDF_dplyr %>% 
  group_by(Origin, timeofday_dplyr) %>% 
  filter(Origin == "IND", timeofday_dplyr == "early morning") %>% 
  summarise(n_IND_earlymorning = n())


# 13b)  tabulate how many flights occur, by splitting the flights according to
# both the city of origin and also the time of the day when the flight departed
tapply(myDF$DepDelay, list(myDF$Origin,myDF$timeofday),length)[c("IND","CVG","JFK"),]
# dplyr
myDF_dplyr %>% 
  group_by(Origin, timeofday_dplyr) %>% 
  select(DepDelay) %>% 
  filter(Origin %in% c("IND","CVG","JFK")) %>% 
  summarise(n = n())
