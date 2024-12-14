setwd("C:/Users/vk0589/OneDrive - UNT System/Documents/UNT/Courses/ADTA5130")
#install.packages("tidyverse")
library(readxl)
library(dplyr)
library(ggplot2)

rawdata <- read_excel("AirlineMarch2023.xlsx")
# Quick Look at the Data
head(rawdata)

#Selecting Data of Ohio City ######
ohio_data <- rawdata %>%
  filter(grepl(", OH", ORIGIN_CITY_NAME))
summary(as.data.frame(ohio_data))
# ohio_data_bkp <- ohio_data #Backup fro Main Data

#Adding Date (Fixing the existing Date)
ohio_data$DATE = paste(ohio_data$YEAR,ohio_data$MONTH,ohio_data$DAY_OF_MONTH, 
                       sep = "/")
ohio_data$DATE = as.Date(ohio_data$DATE, format = "%Y/%m/%d")

#Selecting Required Columns
ohdata2 <- ohio_data %>% 
  select(DATE, DAY_OF_WEEK, MKT_UNIQUE_CARRIER, MKT_CARRIER_FL_NUM, ORIGIN, 
         ORIGIN_CITY_NAME, DEST, DEST_CITY_NAME, DEP_TIME, DEP_DELAY, TAXI_OUT, 
         WHEELS_OFF, WHEELS_ON, TAXI_IN, ARR_TIME, ARR_DELAY, CANCELLED, 
         CRS_ELAPSED_TIME, ACTUAL_ELAPSED_TIME, AIR_TIME, DISTANCE)
# ohdata2 has required Columns that are needed for the Hypothesis
summary(ohdata2)
#ohdata2_bkp <- ohdata2

#Changing data type to factors - For Categorical Variables
ohdata2$DAY_OF_WEEK <- as.factor(ohdata2$DAY_OF_WEEK)
ohdata2$MKT_UNIQUE_CARRIER <- as.factor(ohdata2$MKT_UNIQUE_CARRIER)
ohdata2$MKT_CARRIER_FL_NUM <- as.factor(ohdata2$MKT_CARRIER_FL_NUM)
ohdata2$ORIGIN <- as.factor(ohdata2$ORIGIN)
ohdata2$ORIGIN_CITY_NAME <- as.factor(ohdata2$ORIGIN_CITY_NAME)
ohdata2$DEST <- as.factor(ohdata2$DEST)
ohdata2$DEST_CITY_NAME <- as.factor(ohdata2$DEST_CITY_NAME)
ohdata2$CANCELLED <- as.factor(ohdata2$CANCELLED)
summary(ohdata2)


### HYPOTHESIS TESTING ###
## Hypothesis 1 : ANOVA for DEP_DELAY vs ORIGIN #####
# Reference: https://statsandr.com/blog/anova-in-r/
# Graph for Ohio Data - Departure Delay vs Origin Airport
ggplot(ohdata2) +
  aes(x = ORIGIN, y = DEP_DELAY , color = ORIGIN) +
  geom_jitter() +
  theme(legend.position = "none")

#ANOVA for Departure Delay with Origin Airport
# 
#NULL and ALTERNATE Hypothesis:
# Ho : Average Flight Delays in all Airports of Ohio are not Different. 
#      Airport of Origin doesn't affect Flight Delays 
#      i.e.,  Mean(CAK)=Mean(CLE)=Mean(CMH)=Mean(CVG)=Mean(DAY)=Mean(LCK)=Mean(TOL)
# Ha: Average flight Delays is different for atleast 1 Airport in Ohio.
#     Origin Airport affects the Flight Delays
#     i.e., Mean Delay of 1 or more Airport(s) is different compared to others Airport

# ANOVA for Unclean Data (with Null Values and Outliers)
result1 <- aov(ohdata2$DEP_DELAY~ohdata2$ORIGIN)
summary(result1)


ggplot(ohdata2) +
  aes(x = ORIGIN, y = DEP_DELAY , xlab = "Origin Airport (ORIGIN)", ylab = "Departure Delay (DEP_DELAY)", color = ORIGIN) +
  geom_jitter() +
  theme(legend.position = "none") +
  labs(x = "Origin Airport (ORIGIN)", y = "Departure Delay (DEP_DELAY)", title = " Origin Airport vs Departure Delay")

# Selecting Columns reguired for Hypothesis 1
h1 <- ohdata2 %>% select(ORIGIN, ORIGIN_CITY_NAME ,DEP_DELAY)
summary(h1) #h1 

ggplot(ohdata2) +
  aes(x = ORIGIN, y = DEP_DELAY) +
  geom_boxplot() +
  theme(legend.position = "none")+
  labs(title = "ORIGIN vs DEP_DELAY (with Outliers)")

#Clean Data - Remove Outliers and Null Values for ANOVA
#Finding Mean and Standard Dev without Missing Value
#group_by(h1, ORIGIN) %>%
#  summarise(
#    mean = mean(DEP_DELAY, na.rm = TRUE),
#    sd = sd(DEP_DELAY, na.rm = TRUE))
na.omit(h1) -> h1t # Removed NAs; h1 with no Null Values - h1t
#Finding Q1, Q3 and IQR for Outliers 
Q1_1 <- quantile(h1t$DEP_DELAY, 0.25)
Q3_1 <- quantile(h1t$DEP_DELAY, 0.75)
IQR_1 <- IQR(h1t$DEP_DELAY)
h1t_no <- subset(h1t, 
                 h1t$DEP_DELAY > (Q1_1 - 1.5*IQR_1) & 
                   h1t$DEP_DELAY < (Q3_1 + 1.5*IQR_1)) 
# h1t with No Outliers - h1t_no

# Graph for a quick look on Data without Outliers
ggplot(h1t_no) +
  aes(x = ORIGIN, y = DEP_DELAY) +
  geom_boxplot() +
  theme(legend.position = "none")+
  labs(title = "ORIGIN vs DEP_DELAY (without Outliers)")


#Applying ANOVA for No Outlier Data
result1_no <- aov(DEP_DELAY ~ ORIGIN, data = h1t_no)
# result1_no has the results for Hypothesis 1 with h1t_no (No Outliers and NAs)
summary(result1_no) # Stats of result1_no


#install.packages("report")
library(report)
report(result1_no) # Final Report of our Results in Hypothesis 1


## Hypothesis 2 : ANOVA for TAXI_OUT vs MKT_UNIQUE_CARRIER #####
#Graph for Ohio Data - Departure Delay Time vs Fight Carrier
ggplot(ohdata2) +
  aes(x = MKT_UNIQUE_CARRIER, y = DEP_DELAY , color = MKT_UNIQUE_CARRIER) +
  geom_jitter() +
  theme(legend.position = "none") + 
  xlab("Airline Carrier (MKT_UNIQUE_CARRIER)") + 
  ylab("Departure Delay (DEP_DELAY)") +
  labs(title = "Airline Carriers vs Departure Delay")

#ANOVA for Departure Delay Time with Flight Carrier
# NULL and ALTERNATE HYPOTHESIS
##
# Ho : Mean Time out for all the Flight Carriers are not different (or) There is no 
#     difference in the average Departure Delay time for all the Flight Carrier Companies 
#      i.e.,  Mean(AA)=Mean(AS)=Mean(B6)=Mean(DL)=Mean(F9)=Mean(G4)=Mean(NK)=Mean(UA)
#              =Mean(WN)
# Ha: Average Departure Delay Time is not same for the Flight Carriers and atleast 1 Flight 
#     Carrier's Departure Delay Mean is different 
#     i.e., Mean Departure Delay Time of 1 or more Carrier(s) is different compared to others 
##

# ANOVA for Unclean Data (with Null Values and Outliers)
result2 <- aov(ohdata2$DEP_DELAY~ohdata2$MKT_UNIQUE_CARRIER)
summary(result2)

#Clean Data - Remove Outliers and Null Values for ANOVA
h2 <- ohdata2 %>% select(MKT_UNIQUE_CARRIER, MKT_CARRIER_FL_NUM, ORIGIN_CITY_NAME, DEP_DELAY)
summary(h2)
dim(h2) # h2 Dimensions
ggplot(ohdata2) +
  aes(x = MKT_UNIQUE_CARRIER, y = DEP_DELAY) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Airline Carriers vs Departure Delay: Boxplot (with Outliers)") +
  xlab("Airline Carrier (MKT_UNIQUE_CARRIER)") + ylab("Departure Delay (DEP_DELAY)")

#Removing Outliers and Missing Values 
na.omit(h2) -> h2t # Removed NAs; h2 with no Null Values - h2t
dim(h2t) #h2t Dimensions (After Removing NAs)
#Finding Q1, Q3 and IQR
Q1_2 <- quantile(h2t$DEP_DELAY, 0.25)
Q3_2 <- quantile(h2t$DEP_DELAY, 0.75)
IQR_2 <- IQR(h2t$DEP_DELAY)
h2t_no <- subset(h2t, 
                 h2t$DEP_DELAY > (Q1_2 - 1.5*IQR_2) & h2t$DEP_DELAY < (Q3_2 + 1.5*IQR_2)) 
dim(h2t_no)
# h2t with No Outliers - h2t_no
ggplot(h2t_no) +
  aes(x = MKT_UNIQUE_CARRIER, y = DEP_DELAY) +
  geom_boxplot() +
  theme(legend.position = "none")+
  labs(title = "Airline Carriers vs Departure Delay (without Outliers)") +
  xlab("Airline Carrier (MKT_UNIQUE_CARRIER)") + ylab("Departure Delay (DEP_DELAY)")

#Applying ANOVA for Hypothesis 2 with No Outlier Data
result2_no <- aov(DEP_DELAY ~ MKT_UNIQUE_CARRIER, data = h2t_no)
summary(result2_no)

library(report)
report(result2_no)

## Hypothesis 3 : Regression for CRS_ELAPSED_TIME vs ACTUAL_ELAPSED_TIME ####
#Graph for Ohio Data - Reservation System's Elapsed Time vs Actual Elapsed Time
ggplot(ohdata2) +
  aes(x = CRS_ELAPSED_TIME, y = ACTUAL_ELAPSED_TIME) +
  geom_point(color = "Light Green")+
  labs(title = "CRS_ELAPSED_TIME vs ACTUAL_ELAPSED_TIME") +
  xlab("Computer Reservation System's Elapsed Time") + ylab("Actual Elapsed Time")

#Regression for Reservation System's Elapsed Time vs Actual Elapsed Time 
# Elapsed Time = Diff b/w Departure Time and Arrival Time
#
# Ho: There is No Relation between CRS_ELAPSED_TIME and ACTUAL_ELAPSED_TIME 
#     i.e., B1 = 0 (B is Beta)
# Ha: CRS_ELAPSED_TIME and ACTUAL_ELAPSED_TIME are related to each other 
#     (positively or negatively)
#     i.e., B1 != 0
result3 <- lm(ohdata2$ACTUAL_ELAPSED_TIME ~ ohdata2$CRS_ELAPSED_TIME)
summary(result3)

hist(ohdata2$ACTUAL_ELAPSED_TIME, xlab = "Actual Elapsed Time (ACTUAL_ELAPSED_TIME)", 
     main = "Histogram of ACTUAL_ELAPSED_TIME (Skewed Distribution)")
# Cleaning / Correcting and Selecting appropriate data
h3 <- ohdata2 %>% select(ORIGIN, CRS_ELAPSED_TIME, ACTUAL_ELAPSED_TIME)
dim(h3)
summary(h3)
#Removing Null Values
h3t <-  na.omit(h3) #h3t is h3 after removing NAs
dim(h3t)
hist(h3t$ACTUAL_ELAPSED_TIME)
# Doesn't follow Normal Distribution. Deleting values after 180 to get NormDist
h3t_n <- h3t %>% filter(ACTUAL_ELAPSED_TIME <= 150) # h3t_n for NormDist Data
dim(h3t_n)
hist(h3t_n$ACTUAL_ELAPSED_TIME, xlab = "Actual Elapsed Time (ACTUAL_ELAPSED_TIME)",main = "Histogram of ACTUAL_ELAPSED_TIME (Normal Distribution)")
# h3t_n looks somewhat Normal Compared to h3t, So we will use h3t_n for Regression

result3_n <- lm(ACTUAL_ELAPSED_TIME ~ CRS_ELAPSED_TIME, data = h3t_n)
summary(result3_n)

ggplot(h3t_n, mapping = aes(CRS_ELAPSED_TIME, ACTUAL_ELAPSED_TIME)) +
  geom_point(color = "Grey")+
  geom_smooth(method = "lm",se =  FALSE ,color = "Red") + 
  title(main = "Linear Regression for CRS_ELAPSED_TIME vs ACTUAL_EAPSED_TIME") +
  annotate("text",x=70,y=160,label=(paste0("Slope / beta ==",round(coef(result3_n)[2],4))),parse=TRUE)

#Can add Equation on Graph here

library(report)
report(result3_n)

## Hypothesis 4 : Regression for DEP_DELAY vs ARR_DELAY ####
#Graph for Ohio Data - Departure Delay vs Arrival Delay
ggplot(ohdata2) +
  aes(x = DEP_DELAY, y = ARR_DELAY) +
  geom_point(color = "Dark Green")+
  labs(title = "DEP_DELAY vs ARR_DELAY") +
  xlab("Departure Delay (DEP_DELAY) (in min) ") + ylab("Arrival Delay (ARR_DELAY)(in min)")

#Regression for Departure Delay vs Arrival Delay 
#
# Ho: There is Positive or No Relation between DEP_DELAY and ARR_DELAY 
#     i.e., B1 >= 0
# Ha: DEP_DELAY and ARR_DELAY are related to each other(positively or negatively) 
# DEP_DELAY doesn't affect ARR_DELAY
#     i.e., B1 < 0
result4 <- lm(ohdata2$ARR_DELAY ~ ohdata2$DEP_DELAY)
summary(result4)

#Selecting Required Data
h4 <- ohdata2 %>% select(ORIGIN, DEP_DELAY, ARR_DELAY)
summary(h4)
dim(h4)
#Removing Null Values
h4t <- na.omit(h4)
hist(h4t$ARR_DELAY, xlab = "Arrival Delay (ARR_DELAY) - in min", main = "Histogram of ARR_DELAY (Not Normally Distributed)")
#Doesn't look NormDist; Removing Outliers to change it to NormDist
Q1_4 <- quantile(h4t$ARR_DELAY, 0.25)
Q3_4 <- quantile(h4t$ARR_DELAY, 0.75)
IQR_4 <- IQR(h4t$ARR_DELAY)
h4t_no <- subset(h4t,
                 h4t$ARR_DELAY > (Q1_4 - 1.5*IQR_4) & 
                   h4t$ARR_DELAY < (Q3_4 + 1.5*IQR_4)) 

hist(h4t_no$ARR_DELAY, xlab = "Arrival Delay (ARR_DELAY) - in min", main = "Histogram of ARR_DELAY (Normally Distributed)")
#Now it looks like Normal Distribution

#Applying Regression Analysis
result4_no <- lm(ARR_DELAY ~ DEP_DELAY, data = h4t_no)
summary(result4_no)
#Still getting -ve coeff even though Graph is +ve Linear 
# Working on DEP_DELAY Outliers
# Using Same data from Hypothesis 1
h4t_no2 <- subset(h4t_no, 
                  h4t_no$DEP_DELAY > (Q1_1 - 1.5*IQR_1) & 
                    h4t_no$DEP_DELAY < (Q3_1 + 1.5*IQR_1))

hist(h4t_no2$DEP_DELAY)
result4_no2 <- lm(ARR_DELAY ~ DEP_DELAY, data = h4t_no2)
summary(result4_no2)

ggplot(h4t_no2, mapping = aes(DEP_DELAY, ARR_DELAY)) +
  geom_point(color = "Light Blue")+
  geom_smooth(method = "lm",se = FALSE ,color = "Red") + 
  annotate("text",x = -17 ,y = 41,
           label=(paste0("Slope / beta ==",round(coef(result4_no2)[2],4))),
           parse = T)
#Can add Equation on Graph here

library(report)
report(result4_no2)

## Hypothesis 5 : Regression for TAXI_OUT vs TAXI_IN ####
#Graph for Ohio Data - Taxi Out Time vs Taxi In Time
ggplot(ohdata2) +
  aes(x = TAXI_OUT, y = TAXI_IN) +
  geom_point(color = "Orange") + xlab("Taxi Out Time (TAXI_OUT) in min") + ylab("Taxi In Time (TAXI_IN in min")+
  labs(title ="TAXI_OUT vs TAXI_IN")

# Graph doesn't have a Linear Relation
# Regression for Taxi Out Time vs Taxi In Time 
# Taxi Out Time: Time Diff b/w Departure Time from  Airport Gate till Wheel Off
# at DEP Airport
# Taxi In Time: Time Diff b/w Wheel On Ground and Arrival at Airport Gate at
# AVL Airport 
# Ho: There is No Relation between TAXI_OUT and TAXI_IN Time  
#     i.e., B1 = 0 (B is Beta)
# Ha: TAXI_OUT and TAXI_IN Time are related to each other 
#     (positively or negatively)
#     i.e., B1 != 0

#Selecting Required Data:
h5 <- ohdata2 %>% select(ORIGIN,TAXI_OUT, TAXI_IN)
dim(h5)
result5 <- lm(TAXI_IN ~ TAXI_OUT, data = h5)
summary(result5)

hist(h5$TAXI_OUT) # Not in NormDist
hist(h5$TAXI_IN, xlab = "Taxi In Time (in min)", main = "Histogram for TAXI_IN (with Outliers)") # Not in NormDist

#Removing Null Values:
h5t <- na.omit(h5) #h5t has no NA values
dim(h5t)

#Removing Outliers for TAXI_IN
Q1_5 <- quantile(h5t$TAXI_IN, 0.25)
Q3_5 <- quantile(h5t$TAXI_IN, 0.75)
IQR_5 <- IQR(h5t$TAXI_IN)
h5t_no <- subset(h5t,
                 h5t$TAXI_IN > (Q1_5 - 1.5*IQR_5) & 
                   h5t$TAXI_IN < (Q3_5 + 1.5*IQR_5)) 

hist(h5t_no$TAXI_IN, xlab = "Taxi In Time (in min)", main = "Histogram for TAXI_IN (without Outliers)")
# After Removing Outliers from Taxi In
ggplot(data = h5t_no, mapping = aes(x = TAXI_OUT, y = TAXI_IN)) +
  geom_point(color = "Orange") +
  geom_smooth(method = "lm", color = "Dark Blue")

#Using Data from h2 for TAXI_OUT Outliers
h5t_no2 <- subset(h5t_no, 
                  h5t_no$TAXI_OUT > (Q1_1 - 1.5*IQR_1) & 
                    h5t_no$TAXI_OUT < (Q3_1 + 1.5*IQR_1))

hist(h5t_no2$TAXI_OUT)
result5_no2 <- lm(TAXI_IN ~ TAXI_OUT, data = h5t_no2)
summary(result5_no2)

#After removing Outliers from Taxi Out and Using lm()
ggplot(data = h5t_no2, mapping = aes(x = TAXI_OUT, y = TAXI_IN)) +
  geom_point(color = "Orange") +
  geom_smooth(method = "lm", color = "Dark Blue") + 
  annotate("text",x = 3 ,y = 18,
           label=(paste0("Slope / beta ==",round(coef(result5_no2)[2],4))),
           parse = T)

library(report)
report(result5_no2)

## 
# Considering all the Results, we can say that all the Null Hypothesis are 
#  Rejected except for Hypothesis 4