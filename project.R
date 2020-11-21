library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
data <- read_csv("US_Accidents_June20.csv")

#============================================================== Data Preparation
#ID 
data %>% filter(is.na(ID)) ->dataNATest
# Result = ID all record have value in this column

#Source
data %>% filter(is.na(Source)) ->dataNATest
# Result = Source all record have value in this column

#TMC 700k
data %>% filter(is.na(TMC)) ->dataNATest
# Result = TMC some observation  missing value

#Severity
data %>% filter(is.na(Severity)) ->dataNATest
# Result = Severity all record have value in this column

#Start_Time
data %>% filter(is.na(Start_Time)) ->dataNATest
# Result = Start_Time all record have value in this column

#End_Time
data %>% filter(is.na(End_Time)) ->dataNATest
# Result = End_Time all record have value in this column

#Start_Lat 
data %>% filter(is.na(Start_Lat)) ->dataNATest
# Result = Start_Lat all record have value in this column

#Start_Lng
data %>% filter(is.na(Start_Lng)) ->dataNATest
# Result = Start_Lng all record have value in this column

#End_lat ALL
data %>% filter(is.na(End_Lat)) ->dataNATest
# Result = End_Lat most of observation have NA !!!

#End_Lng ALL
data %>% filter(is.na(End_Lng)) ->dataNATest
# Result = End_Lat most of observation have NA !!!

#Distance(mi)
data %>% filter(is.na(`Distance(mi)`)) ->dataNATest
# Result =  Distance(mi)  does not have Na in column

#Description
data %>%  filter(is.na(Description))->dataNATest
# Result =  Description  does not have Na in column

#Number 1.9M
data %>%  filter(is.na(Number))->dataNATest
# Result =  Number half of observation have NA

#Street 
data %>%  filter(is.na(Street))->dataNATest
# Result =  Street  does not have Na in column

#Side
data %>%  filter(is.na(Side))->dataNATest
# Result =  Side  does not have Na in column

#City
data %>%  filter(is.na(City))->dataNATest
# Result =  City  does not have Na in column

#County
data %>%  filter(is.na(County))->dataNATest
# Result =  County  does not have Na in column

#State
data %>%  filter(is.na(State))->dataNATest
# Result =  State  does not have Na in column

#Zipcode
data %>%  filter(is.na(Zipcode))->dataNATest
# Result =  ZipCode  does not have Na in column

#Country
data %>%  filter(is.na(Country))->dataNATest
# Result =  Country  does not have Na in column

#Timezone
data %>%  filter(is.na(Timezone))->dataNATest
# Result =  Timezone  does not have Na in column

#Airport_Code
data %>%  filter(is.na(Airport_Code))->dataNATest
# Result =  Airport_Code  does not have Na in column

#Weather_Timestamp
data %>%  filter(is.na(Weather_Timestamp))->dataNATest
# Result =  Weather_Timestamp  does not have Na in column

#Temperature(F) 50K
data %>% filter(is.na(`Temperature(F)`)) -> dataNATest
# Result = Temperature(F) contains some NULL value

#Wind_Chill(F) 1.8M
data %>% filter(is.na(`Wind_Chill(F)`)) -> dataNATest
# Result = Wind_Chill(F) contains some NULL value

#Humidity(%) 59K
data %>% filter(is.na(`Humidity(%)`)) -> dataNATest
# Result = Huminity(%) contains some NULL value

#Pressure(in) 48K
data %>% filter(is.na(`Pressure(in)`)) -> dataNATest
# Result = Pressure(in) contains some NULL value

#Visibility(mi) 65K
data %>% filter(is.na(`Visibility(mi)`)) -> dataNATest
# Result = Visibility(mi) contains some NULL value

#Wind_Direction 45K
data %>% filter(is.na(Wind_Direction)) -> dataNATest
# Result = Wind_Direction contains some NULL value

#Wind_Speed(mph) 440K
data %>% filter(is.na(`Wind_Speed(mph)`)) -> dataNATest
# Result = Wind_Speed(mph) contains some NULL value

#Precipitation(in) 2M
data %>% filter(is.na(`Precipitation(in)`)) -> dataNATest
# Result = Precipitation(in) contains some NULL value

#Weather_Condition 65K
data %>% filter(is.na(Weather_Condition)) -> dataNATest
# Result =  Weather_Condition contains some NULL value

#Amenity
data %>% filter(is.na(Amenity)) -> dataNATest
# Result = Amenity has no NULL value

#Bump
data %>% filter(is.na(Bump)) -> dataNATest
# Result = Bump has no NULL

#Crossing
data %>% filter(is.na(Crossing)) -> dataNATest
# Result = Crossing has no NULL value

#Give_Way
data %>% filter(is.na(Give_Way)) -> dataNATest
# Result = Give_Way has no NULL value

#Junction
data %>% filter(is.na(Junction)) -> dataNATest
# Result = Junction  has no NULL value

#No_Exit
data %>% filter(is.na(No_Exit)) -> dataNATest
# Result = No_exit has no NULL value

#Railway
data %>% filter(is.na(Railway)) -> dataNATest
# Result = Railway has no NULL value

#Roundabout
data %>% filter(is.na(Roundabout)) -> dataNATest
# Result = Roundabout has no NULL value

#Station
data %>% filter(is.na(Station)) -> dataNATest
# Result = Station has no NULL value

#Stop
data %>% filter(is.na(Stop)) -> dataNATest
# Result = Stop has no NULL value

#Traffic_Calming
data %>% filter(is.na(Traffic_Calming)) -> dataNATest
# Result = Traffic_Calming has no NULL value

#Traffic_Signal
data %>% filter(is.na(Traffic_Signal)) -> dataNATest
# Result =Traffic_Signal  has no NULL value

#Turning_Loop
data %>% filter(is.na(Turning_Loop)) -> dataNATest
# Result = Turning_Loop has no NULL value

#Sunrise_Sunset
data %>% filter(is.na(Sunrise_Sunset)) -> dataNATest
# Result = Sunrise_Sunset has no NULL value

#Civil_Twilight 93
data %>% filter(is.na(Civil_Twilight)) -> dataNATest
# Result = Civil_Twilight contains some NULL value

#Nautical_Twilight 93
data %>% filter(is.na(Nautical_Twilight)) -> dataNATest
# Result = Nautical_Twilight contains some NULL value

#Astronomical_Twilight 93
data %>% filter(is.na(Astronomical_Twilight)) -> dataNATest
# Result = Astronomical_Twilight contains some NULL value


data %>% 
  select(-End_Lat,
         -End_Lng,
         -ID,
         -Source,
         -TMC,
         -Zipcode,
         -Country,
         -Start_Lat,
         -Start_Lng,
         -Airport_Code,
         -`Wind_Chill(F)`,
         -Number,
         -Street,
         -Side,
         -City,
         -County,
         -Description,
         -End_Time,
         -Timezone,
         -Civil_Twilight,
         -Nautical_Twilight,
         -Astronomical_Twilight,
         -`Distance(mi)`,
         -`Precipitation(in)`)  %>% 
  drop_na() -> sData

sData$Severity<- as.factor(sData$Severity)
sData$Wind_Direction <-as.factor(sData$Wind_Direction)
sData$Weather_Condition<-as.factor(sData$Weather_Condition)
sData$Sunrise_Sunset<-as.factor(sData$Sunrise_Sunset)

#data prep for finding start_hour 
sData %>%  
  separate(Start_Time,c("Start_Date","Start_Time"),sep =" ") -> sData
sData$Start_Time
sData %>%  
  separate(Start_Time,c("Start_Hour","Start_Min","Start_Sec")) -> sData



#============================================================== Data Virtualization

# Temperature
sData %>% ggplot() + geom_boxplot(mapping=aes(x=Severity,y =`Temperature(F)`))
# all boxplot  overlap more than  50 percent,  Temperature might not have relationshilp with Severity

# Humidity
sData %>% ggplot() + geom_boxplot(mapping=aes(x=Severity,y =`Humidity(%)`))
# all boxplot overlap each other more than 50 percent, Humidity might not have relationship with Severity

# Pressure
sData %>% ggplot() + geom_boxplot(mapping=aes(x=Severity,y =`Pressure(in)`))
# some boxplot did not overlap more than  50 percent, Pressure might help predict Severity 

# visibility
sData %>% ggplot() + geom_boxplot(mapping=aes(x=Severity,y =`Visibility(mi)`))
# some boxplot did not overlap more than 50 percent , Visibility might help predict Severity

#Wind_Direction fisher exact test
chisq.test(x=sData$Severity,y=sData$Wind_Direction,simulate.p.value = TRUE)
# Result of p-value = 0.0004998 and x-square=7503 p-value less than 0.05 thus  wind_direction might help predict severity  but too much for predict 
 
#wind_speed
sData %>% ggplot() + geom_boxplot(mapping=aes(x=Severity,y =`Wind_Speed(mph)`))
# some boxplot did not overlap more than 50 percent , Wind_Speed might help predict Severity
 
#Precepitation
sData %>% ggplot() + geom_boxplot(mapping=aes(x=Severity,y =`Precipitation(in)`))
# all boxplot overlap each other more than 50 percent, Precipitation might not have relationship with Severity
 
#Amenity
chisq.test(x=sData$Severity,y=sData$Amenity,simulate.p.value = TRUE)
# X-squared = 2294.2, df = NA, p-value = 0.0004998 thus Amenity mush help predict severity

#Bump
chisq.test(x=sData$Severity,y=sData$Bump,simulate.p.value = TRUE)
#X-squared = 7.092, df = NA, p-value = 0.06597 p-value more than 0.05 thus this might not help predict
 
#Crossing
chisq.test(x=sData$Severity,y=sData$Crossing,simulate.p.value = TRUE)
#X-squared = 15901, df = NA, p-value = 0.0004998 p-value less than 0.05  thus this might help predict

#Give_way
chisq.test(x=sData$Severity,y=sData$Give_Way,simulate.p.value = TRUE)
#X-squared = 184.54, df = NA, p-value = 0.0004998 less than 0.05 thus this might help predict
 
#Junction
chisq.test(x=sData$Severity,y=sData$Junction,simulate.p.value = TRUE)
#X-squared = 184.54, df = NA, p-value = 0.0004998 less than 0.05 thus this might help predict
 
#No_Exit
chisq.test(x=sData$Severity,y=sData$No_Exit,simulate.p.value = TRUE)
#X-squared = 14.361, df = NA, p-value = 0.01199 less than 0.05 thus this might help predict
 
#Railway
chisq.test(x=sData$Severity,y=sData$Railway,simulate.p.value = TRUE)
#X-squared = 883.1, df = NA, p-value = 0.0004998 less than 0.05 thus this might help predict
 
#RoundAbout
chisq.test(x=sData$Severity,y=sData$Roundabout,simulate.p.value = TRUE)
#X-squared = 14.735, df = NA, p-value = 0.01649 less than 0.05 thus this might help predict
 
#Station
chisq.test(x=sData$Station,y=sData$Roundabout,simulate.p.value = TRUE)
#X-squared = 0.8362, df = NA, p-value = 0.6272 more than 0.05 thus this might not help predict
 
#Stop
chisq.test(x=sData$Station,y=sData$Stop,simulate.p.value = TRUE)
#X-squared = 839.18, df = NA, p-value = 0.0004998 less than 0.05 this might help predict
 
#Traffic_Calming
chisq.test(x=sData$Station,y=sData$Traffic_Calming,simulate.p.value = TRUE)
# X-squared = 14.336, df = NA, p-value = 0.001499 less than 0.05 this might help predict
 
#Traffic_Signal
chisq.test(x=sData$Station,y=sData$Traffic_Signal,simulate.p.value = TRUE)
#X-squared = 15120, df = NA, p-value = 0.0004998 less than 0.05 this might help predict
 
#Turning_Loop
chisq.test(x=sData$Station,y=sData$Turning_Loop,simulate.p.value = TRUE)
# there only have false in Turning thus this should be taken out
 
#Sunrise_Sunset
chisq.test(x=sData$Station,y=sData$Sunrise_Sunset,simulate.p.value = TRUE)
#X-squared = 13.797, df = NA, p-value = 0.0004998  less than 0.05 this might help predict
 
#Weather_Condition
chisq.test(x=sData$Severity,y=sData$Weather_Condition,simulate.p.value = TRUE)
#X-squared = 14078, df = NA, p-value = 0.0004998   p-value less than 0.05 thus weather_condition might help predict severity

sData %>% 
  group_by(State) %>% 
  summarise(total = n()) %>% 
  ggplot() + 
  geom_col(mapping=aes(x=State,y=total,fill=State))+ coord_flip()

sData %>% 
  filter(State == "CA") -> CAdata
# plot Start_hour which hour accident ouccur the most
CAdata %>% 
  group_by(Start_Hour) %>%  
  summarise(total=n()) %>% 
  ggplot() + 
  geom_col(mapping=aes(x=Start_Hour,y=total,fill=Start_Hour))

#plot Severity which severity is the highest
CAdata %>% 
  group_by(Severity) %>% 
  summarise(total=n()) %>% 
  ggplot() + 
  geom_col(mapping = aes(x=Severity,y=total,fill=Severity))

CAdata %>% 
  select(Severity,
         Start_Date,
         Start_Hour,
         Start_Min,
         Start_Sec,
         State,
         Weather_Timestamp,
         `Temperature(F)`,
         `Humidity(%)`,
         `Pressure(in)`,
         `Visibility(mi)`,
         Wind_Direction,
         `Wind_Speed(mph)`,
         Weather_Condition) -> CAdataNew

CAdata %>% 
  select(-Start_Date,
         -Start_Hour,
         -Start_Min,
         -Start_Sec,
         -State,
         -Weather_Timestamp,
         -`Temperature(F)`,
         -`Humidity(%)`,
         -`Pressure(in)`,
         -`Visibility(mi)`,
         -Wind_Direction,
         -`Wind_Speed(mph)`,
         -Weather_Condition) -> CAdataNewN
#============Failed Model
set.seed(555)
test_ind <- sample(nrow(CAdataNew), 0.3*nrow(CAdataNew))
test_ind
CAdataNew_training <- CAdataNew[-test_ind,]
CAdataNew_testing <- CAdataNew[test_ind,]
summary(CAdataNew_training)
summary(CAdataNew_testing)

CAtree <- rpart(Severity~ ., data = CAdataNew_training)
rpart.plot(CAtree,box.palette="green")


#=============Model 2
model <- glm(Severity ~ Amenity+Bump+Crossing+Give_Way+Junction+No_Exit+Railway+Roundabout+Station+Stop+Traffic_Calming+Traffic_Signal+Turning_Loop ,data = CAdataNewN, family = binomial)
summary(model)

model <-glm(Severity ~ Start_Date+Start_Hour+Start_Min+Start_Sec+State+Weather_Timestamp+`Temperature(F)`+`Humidity(%)`+`Pressure(in)`,+`Visibility(mi)`+`Wind_Speed(mph)`,data = CAdataNew, family = binomial)
summary(model)

res <- predict(model,CAdataNewN,type="response")
data %>% 
  group_by(Severity) %>% 
  summarise(n =n())
res_c <- factor(ifelse(res > 0.445, "TRUE","FALSE" ))
confusionMatrix(res_c,CAdataNewN$Severity,positive = "1",mode = "prec_recall")





