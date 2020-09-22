# Read Task4 CSV into R
df <- read.csv(file="ua_analyst_task4_campaigns.csv", header=TRUE, sep=",")
head(df)
#Because it's really hard to compare the performance between different campaigns due to different size of spend and dau.
#We decide to define a campaign user quality score(CUQ).
#The campaign user quality score consists of CPM_Spend,CTR,Conversion_rate,Retention_Day_7,ARPDAU_D7,ROI_180,ROAS.
#Here are the scoring rules.
#We will use the weight score like below, it can adjust depend on our need.
#Fraud_metric(-):CPM_Spend:10%,CTR:10%,Conversion_rate:10%
#Key_index_metric(+~-):Retention_Day_7:10%,ARPDAU_D7:10%,ROI_180:20%,ROAS:20%
#campaign user quality score(CUQ)= 0 + Key_index_metric(+~-)+Fraud_metric(-)
#As a result, you will be labeled as "good" campaign if your CUQ>0, "average":CUQ=0,"bad":CUQ<0

#1.Day7 is the beginning of comparison, all campaign CUQ set to 10.
#2.volatility score=5x(this time period actual value - last time period actual value)/max(choose max value of time period)
#3.magnitud score=5 x campaign this time period actual value / sum of all campaigns this time period actual value(We only consider DAU and revenue in this score)
#Every metric score = score last time period + volatility score + magnitud score
#We will use the weight score like below, it can adjust depend on our need.
#Revenue:30%,DAU:20%,first_time_spenders vs.DAU:20%
#ARPDAU:15%,D7_Retention rate:15%

#Define Campaign_score()
#Campaign_score <-function(df, amount=T){
  #To realize volatility score
 # library(reshape)
  
#}

#Date overview
#Before we have a deep look on We will calculate CPM_Spend,CTR,Conversion_rate,ARPDAU_D7,Retention_Day_7,ROI_180,ROAS
#We will calculate CPM_Spend here.
df$CPM_Spend<- (df$spend*1000)/df$impressions
df$CTR<- df$clicks/df$impressions
df$Conversion_rate<- df$installs/df$clicks
df$CPI<-df$spend/df$installs
df$Retention_Day_7<-(df$d7_dau/df$installs)
df$ARPDAU_D7<-(df$d7_revenue/7/df$d7_dau)
#CPM_Spend
#Have an overview with summary()
summary(df$CPM_Spend)
#Using histogram to overview the distribution
hist(df$CPM_Spend)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
skewness(df$CPM_Spend)

#The skewness of sk is 3.764521. It indicates that the CPM_Spend distribution is skewed towards the right.
#The mean is on the right of the peak value.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
#boxplot(df$CPM_Spend,main="CPM_Spend Data")
adjbox(df$CPM_Spend,col="red",main="CPM_Spend Data")
adjboxStats(df$CPM_Spend)$out
#Have an outlier detection with adjusted boxplot
#boxplot(df$CPM_Spend,main="CPM_Spend Data")
length(unique(adjboxStats(df$CPM_Spend)$out))
l1<-as.numeric(length(unique(adjboxStats(df$CPM_Spend)$out)))
#Find out two campaigns CPM_spend lie outside Q3+1.5*exp(3M), 
#Where M is an index of skewness of the uncontaminated part of the data
#Details please refer user603 answer on https://stats.stackexchange.com/questions/13086/is-there-a-boxplot-variant-for-poisson-distributed-data
#And adjboxStats description https://rdrr.io/rforge/robustbase/man/adjboxStats.html
#Create function to filter those campaigns ID correspond values fall into outlier list 
CSO<-function(x){
  CPM_spend_outlier<-as.numeric(adjboxStats(df$CPM_Spend)$out)
  df[df$CPM_Spend == CPM_spend_outlier[x],1]
}
CSO(1)
CPM_outliier<-sapply(1:l1,CSO)
#Conclusion_1:
#It's not normal that CPM_Spend is so high(over 99)
#And Campaign ID 14 and 15 have CPM_Spend over Q3+1.5*exp(3M), which mean we should take them plus -10%.
df$campaign_quality_score<- rep(0,20)
CQS<-data.frame(df$campaign_id,df$campaign_quality_score)
CQS[which(CQS$df.campaign_id == CPM_outliier[1]),]
CQS$df.campaign_quality_score<-with(CQS,which)
CQS[CQS$df.campaign_id == CPM_outliier[1],2]
CQS<-(CQS[CQS$df.campaign_id == CPM_outliier,2] - 0.1)
CQS
#CTR
#Have an overview with summary()
summary(df$CTR)
#Using histogram to overview the distribution
hist(df$CTR)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
skewness(df$CTR)
#The skewness of sk is 3.817522. It indicates that the CTR distribution is skewed towards the right.
#The mean is on the right of the peak value.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
adjbox(df$CTR,col="red",main="CTR Data")
adjboxStats(df$CTR)
adjboxStats(df$CTR)$out
#Have an outlier detection with adjusted boxplot
#boxplot(df$CPM_Spend,main="CPM_Spend Data")
length(unique(adjboxStats(df$CTR)$out))
l2<-as.numeric(length(unique(adjboxStats(df$CTR)$out)))
#Find out two campaigns CTR lie outside Q3+1.5*exp(3M),another two campaigns CTR lie outside Q1-1.5*exp(3M) 
#Where M is an index of skewness of the uncontaminated part of the data
#Details please refer user603 answer on https://stats.stackexchange.com/questions/13086/is-there-a-boxplot-variant-for-poisson-distributed-data
#And adjboxStats description https://rdrr.io/rforge/robustbase/man/adjboxStats.html
CTO<-function(x){
  CTR_outlier<-as.numeric(adjboxStats(df$CTR)$out)
  df[df$CTR == CTR_outlier[x],1]
}
CTO(1)
sapply(1:l2,CTO)
#Conclusion_2:
#Since it's not normal that CTR over 100% but it's endurable for low CTR, we will catch for those suspicious high CTR Campaign.
#Campaign 14 and 15 have CTR over Q3+1.5*exp(3M) which mean we should take them plus -10%.
#Campaign 1 and 10 have CTR lower Q1-1.5*exp(3M) which mean we should take them plus -10%.

####Conversion_rate
#Have an overview with summary()
summary(df$Conversion_rate)
#Using histogram to overview the distribution
hist(df$Conversion_rate)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
skewness(df$Conversion_rate)
#The skewness of sk is 0.7519787. It indicates that the Conversion_rate distribution is skewed towards the right.
#The mean is on the right of the peak value.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
adjbox(df$Conversion_rate,col="red",main="Conversion_rate Data")
adjboxStats(df$Conversion_rate)
adjboxStats(df$Conversion_rate)$out
#Have an outlier detection with adjusted boxplot
length(unique(adjboxStats(df$Conversion_rate)$out))
l3<-as.numeric(length(unique(adjboxStats(df$Conversion_rate)$out)))
#Find out one campaigns Conversion_rate lie outside Q1-1.5*exp(3M) 
#Where M is an index of skewness of the uncontaminated part of the data
#Details please refer user603 answer on https://stats.stackexchange.com/questions/13086/is-there-a-boxplot-variant-for-poisson-distributed-data
#And adjboxStats description https://rdrr.io/rforge/robustbase/man/adjboxStats.html
CVO<-function(x){
  Conversion_rate_outlier<-as.numeric(adjboxStats(df$Conversion_rate)$out)
  df[df$Conversion_rate == Conversion_rate_outlier[x],1]
}
CVO(1)
sapply(1:l3,CVO)
#Conclusion_3:
#Since it's not normal that CVR too low
#Campaign 14 has Conversion_rate lower Q1-1.5*exp(3M) which mean we should take them plus -10%.




#CPI
#Have an overview with summary()
summary(df$CPI)
#Using histogram to overview the distribution
hist(df$CPI)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
skewness(df$CPI)
#The skewness of sk is 0.837968. It indicates that the CPI distribution is skewed towards the right.
#The mean is on the right of the peak value.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
adjbox(df$CPI,col="red",main="CPI Data")
adjboxStats(df$CPI)
adjboxStats(df$CPI)$out
#Have an outlier detection with adjusted boxplot
length(unique(adjboxStats(df$CPI)$out))
l4<-as.numeric(length(unique(adjboxStats(df$CPI)$out)))
#Conclusion_4:
#Find out 0 campaigns CPI lie outside Q1-1.5*exp(3M) and Q3+1.5*exp(3M)




#Retention_Day_7
#Have an overview with summary()
summary(df$Retention_Day_7)
#Using histogram to overview the distribution
hist(df$Retention_Day_7)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
skewness(df$Retention_Day_7)
#The skewness of sk is -1.116296. It indicates that the Conversion_rate distribution is skewed towards the left.
#The mean is on the left of the peak value.
#Since this is a left skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
adjbox(df$Retention_Day_7,col="red",main="Retention_Day_7 Data")
adjboxStats(df$Retention_Day_7)
adjboxStats(df$Retention_Day_7)$out
#Have an outlier detection with adjusted boxplot
length(unique(adjboxStats(df$Retention_Day_7)$out))
#l3<-as.numeric(length(unique(adjboxStats(df$Retention_Day_7)$out)))
#Conclusion_4:
#Find out 0 campaigns Retention_Day_7 lie outside Q1-1.5*exp(3M) and Q3+1.5*exp(3M)

#ARPDAU_D7
#Have an overview with summary()
summary(df$ARPDAU_D7)
#Using histogram to overview the distribution
hist(df$ARPDAU_D7)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ARPDAU_D7.
library(e1071)  
skewness(df$ARPDAU_D7)
#The skewness of sk is 1.624966. It indicates that the ARPDAU_D7 distribution is skewed towards the right.
#The mean is on the right of the peak value.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
adjbox(df$ARPDAU_D7,col="red",main="ARPDAU_D7 Data")
adjboxStats(df$ARPDAU_D7)
adjboxStats(df$ARPDAU_D7)$out
#Have an outlier detection with adjusted boxplot
length(unique(adjboxStats(df$ARPDAU_D7)$out))
l8<-as.numeric(length(unique(adjboxStats(df$ARPDAU_D7)$out)))
#Find out two campaigns ARPDAU_D7 lie outside Q1-1.5*exp(3M) and Q3+1.5*exp(3M)
#Where M is an index of skewness of the uncontaminated part of the data
#Details please refer user603 answer on https://stats.stackexchange.com/questions/13086/is-there-a-boxplot-variant-for-poisson-distributed-data
#And adjboxStats description https://rdrr.io/rforge/robustbase/man/adjboxStats.html
ARPD7O<-function(x){
  ARPDAU_D7_outlier<-as.numeric(adjboxStats(df$ARPDAU_D7)$out)
  df[df$ARPDAU_D7 == ARPDAU_D7_outlier[x],1]
}
ARPD7O(1)
sapply(1:l8,ARPD7O)
#Conclusion_5:
#Campaign 10 has ARPDAU_D7 higher Q3+1.5*exp(3M) which mean we should take them plus +10%.
#Campaign 2 has ARPDAU_D7 lower Q1-1.5*exp(3M) which mean we should take them multiply -10%.





CPM_spend_outlier<-as.numeric(boxplot.stats(df$CPM_Spend)$out)
CPM_spend_outlier
for (i in CPM_spend_outlier) {
  result<-df[df$CPM_Spend == i,1]
}
result
CPM_spend_outlier<-as.numeric(boxplot.stats(df$CPM_Spend)$out)
CPM_spend_outlier
df[df$CPM_Spend == CPM_spend_outlier[1],1]
df[df$CPM_Spend == CPM_spend_outlier[2],1]
#Campaign ID 14,15 has CPM_Spend over 1.5*IQR, which mean we should 

#1.Evaluate campaign performance from ROI_180-(LTV_180/CPI) perspective
#How is the each campaign current ROI and the future ROI performance?
#Existing ROI = LTV-180 compare to current campaign CPI?
#Firstly, we need to calculate the Retention_Rate
#add new column Retention_Day_1,Retention_Day_7,Retention_Day_14
#add new column ,Retention_Day_30,Retention_Day_60,Retention_Day_120
#add new column ,Retention_Day_150,Retention_Day_180
df$Retention_Day_1<-(df$d1_dau/df$installs)
df$Retention_Day_7<-(df$d7_dau/df$installs)
df$Retention_Day_14<-(df$d14_dau/df$installs)
df$Retention_Day_30<-(df$d30_dau/df$installs)
df$Retention_Day_60<-(df$d60_dau/df$installs)
df$Retention_Day_120<-(df$d120_dau/df$installs)
df$Retention_Day_150<-(df$d150_dau/df$installs)
df$Retention_Day_180<-(df$d180_dau/df$installs)
#Assume LTV-180 = ARPDAU_180*Life_time(1-180)
#This function got from Eric’s Seufert’s lecture from GDC (Retention Approach)
#https://mobiledevmemo.com/two-methods-modeling-ltv-spreadsheet/
#It assumes the retention function is a power function (y=a*x^b) and that ARPDAU is constant.
#Assume retention rate will follow the power function y=ax^b, x:days since install,y:retention rate
x<-c(1,7,14,30,60,120,150,180)
x
class(x)
r<-data.frame(df$campaign_id,df$Retention_Day_1,df$Retention_Day_7,df$Retention_Day_14,df$Retention_Day_30,df$Retention_Day_60,df$Retention_Day_120,df$Retention_Day_150,df$Retention_Day_180)
class(r)
as.numeric(r[r$df.campaign_id == 1,2:9])
#Write an retention funtion to fetch retention from day1 to day180 by campaignID
retention<- function(x){
  r<-data.frame(df$campaign_id,df$Retention_Day_1,df$Retention_Day_7,df$Retention_Day_14,df$Retention_Day_30,df$Retention_Day_60,df$Retention_Day_120,df$Retention_Day_150,df$Retention_Day_180)
  n<-as.numeric(r[r$df.campaign_id == x,2:9])
    return(n)
}
c<-retention(1)
c
#Write an LT function which can provide us the Life_time(1-180) by calculating its area of distribution
LT<- function(y){
  #Define days since install
  x<-c(1,7,14,30,60,120,150,180)
  r<-data.frame(df$campaign_id,df$Retention_Day_1,df$Retention_Day_7,df$Retention_Day_14,df$Retention_Day_30,df$Retention_Day_60,df$Retention_Day_120,df$Retention_Day_150,df$Retention_Day_180)
  #Write an retention funtion to fetch retention from day1 to day180 by campaignID
  n<-as.numeric(r[r$df.campaign_id == y,2:9])
  #Find coefficient a,b of power function
  lmResult<-lm(log(n)~log(x))
  i<-as.numeric(coef(lmResult)["(Intercept)"])
  a<-exp(i)
  b<-as.numeric(coef(lmResult)["log(x)"])
  f <-function(x) a*(x^(b))
  #Calculate the area under power function, we can get the estimate lifetime
  #To calculate, integration value of a function, we first define a function (with name f or some other name0 for the function as shown below.
  l<-integrate(f,1,180)$value
  return(l)
}
#Calculate campaign1 to campaign20 Life_time
sapply(1:20,LT)
#Add Life_time_180 into the original data set
df$Life_time_180<-sapply(1:20,LT)

#Calculate the ARPDAU_180 
df$ARPDAU_Day_180<-((df$d180_revenue)/180/df$d180_dau)
#Calculate the LTV_180 
df$LTV_180<-df$ARPDAU_Day_180*df$Life_time_180
#Calculate the CPI
df$CPI<-df$spend/df$installs
#Calculate the ROI
df$ROI_180<-df$LTV_180/df$CPI
df$ROI_180

#Here we will start to evaluate the performance of the campaigns and identify ‘good’, ‘average’ and ‘bad’ performing campaigns
#boxplot(df$ROI_180,main="ROI_180 Data")
#boxplot.stats(df$ROI_180)$out
#length(unique(boxplot.stats(df$ROI_180)$out))
#l5<-as.numeric(length(unique(boxplot.stats(df$ROI_180)$out)))
#Find out two campaigns CPM_spend lie outside 1.5*IQR, where IQR, the ‘Inter Quartile Range’ is the difference between 75th and 25th quartiles
#ROI180O<-function(x){
#  ROI180O_outlier<-as.numeric(boxplot.stats(df$ROI_180)$out)
#  df[df$ROI_180 == ROI180O_outlier[x],1]
#}
###ROI180O(1)
#sapply(1:l5,ROI180O)

#First of all, we will use histogram to overview the ROI_180 distribution
hist(df$ROI_180)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
sk = df$ROI_180
skewness(sk)

#The skewness of sk is 1.054106. It indicates that the ROI_180 distribution is skewed towards the right.
#The mean is on the right of the peak value.
#However how do we use standard deviation to intepret the data?
#To use log transformation on data
w<-log(sk)
shapiro.test(w)
#p-value = 0.1461 > 0.05, Accept H0 Log(ROI_180) is normally distributed.
log_ROI_180<- w
hist(log_ROI_180,col="red",freq=F,xlim=c(-5,5))
#Nevertheless, we find there some missing data on histogram, we decide to use right skewed distribution rather than normal distribution.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
#Since I can't find any good campaigns when coefficient = 1.5, I decide to use coefficient = 0.25 to find good and bad campaigns.
r_180<- data.frame(df$campaign_id,df$ROI_180)
adjbox(df$ROI_180,range = 0.25,col="red",main="ROI_180 Data")
adjboxStats(df$ROI_180,coef = 0.25)
adjboxStats(df$ROI_180,coef = 0.25)$out
#Have an outlier detection with adjusted boxplot
length(unique(adjboxStats(df$ROI_180,coef = 0.25)$out))
l6<-as.numeric(length(unique(adjboxStats(df$ROI_180,coef = 0.25)$out)))
#Find out 7 campaigns ROI_180 lie outside Q1-0.25*exp(3M) and Q3+0.25*exp(3M) 
#Where M is an index of skewness of the uncontaminated part of the data
#Details please refer user603 answer on https://stats.stackexchange.com/questions/13086/is-there-a-boxplot-variant-for-poisson-distributed-data
#And adjboxStats description https://rdrr.io/rforge/robustbase/man/adjboxStats.html
ROI_180_O<-function(x){
  ROI_180_outlier<-as.numeric(adjboxStats(df$ROI_180,coef = 0.25)$out)
  df[df$ROI_180 == ROI_180_outlier[x],1]
}
ROI_180_O(1)
sapply(1:l6,ROI_180_O)
#Conclusion_6:
#Campaign 13,14,16 have ROI_180 higher Q3+0.25*exp(3M) which mean we should take it plus +20%
#Campaign 3,7,9,18 have ROI_180 lower Q1-0.25*exp(3M) which mean we should take it plus -20%



#For skewed distributions, the standard deviation gives no information on the asymmetry. 
#It is better to use the first and third quartiles, since these will give some sense of the asymmetry of the distribution. 
#As a result, we uae Percentile to interpret the data
quantile(df$ROI_180, c(.25, .50, .75)) 
#25%       50%       75% 
#0.8489664 1.0262412 1.5594048 
#From Percentile persective, I will label those ROI>1.5594048 as good(Above 75th percentile),
#Label 0.8489664<ROI<1.5594048  as average(25th-75th percentile), it mean we still have some room to improve these "average" campaigns.
#and ROI<0.8489664 as bad(25th percentile below, Because it might make us lose money since the ROI below 1 )
#I will suggest we stop these "bad" campaigns if it doesn't.

#2.Evaluate campaign performance from ROAS perspective
#ROAS:Return of Ads Spend
#Definition:ROAS= Campaign_revenue_to_date / campaign_spend
df$ROAS<-df$revenue_to_date/df$spend
#Here, we will use histogram to overview the ROAS distribution
hist(df$ROAS)
#From plot, it looks more like the Skewed right (positive) distribution
#We apply the function skewness from the e1071 package to compute the skewness coefficient of ROI_180.
library(e1071)  
skewness(df$ROAS)
#The skewness of sk is 1.52701. It indicates that the ROAS distribution is skewed towards the right.
#The mean is on the right of the peak value.
#However how do we use standard deviation to intepret the data?
#To use log transformation on data
z<-log(df$ROAS)
shapiro.test(z)
#p-value = 0.583 > 0.05, Accept H0 Log(ROAS) is normally distributed.
log_ROAS<- z
hist(log_ROAS,col="red",freq=F,xlim=c(-5,5))
#Nevertheless, we still find out there some missing data on histogram, we decide to use right skewed distribution rather than normal distribution.
#Since this is a right skewed distribution, it doesn't recommend to use regular boxplot to detect outlier.
#Instead, we will use `adjbox` for Skew Distributions
#It's boxplot adjusted for skewed distributions as proposed in Hubert and Vandervieren (2004).
#Further information, please refer https://rdrr.io/cran/robustbase/man/adjbox.html and https://www.sciencedirect.com/science/article/pii/S0167947307004434
library(robustbase)
#Since I can't find any good campaigns when coefficient = 1.5, I decide to use coefficient = 1 to find good and bad campaigns.
adjbox(df$ROAS,range = 1,col="red",main="ROAS Data")
adjboxStats(df$ROAS,coef = 1)
adjboxStats(df$ROAS,coef = 1)$out
#Have an outlier detection with adjusted boxplot
length(unique(adjboxStats(df$ROAS,coef = 1)$out))
l7<-as.numeric(length(unique(adjboxStats(df$ROAS,coef = 1)$out)))
#Find out three campaigns ROAS lie outside Q1-1.5*exp(3M) and another one outside of Q3+1.5*exp(3M)
#Where M is an index of skewness of the uncontaminated part of the data
#Details please refer user603 answer on https://stats.stackexchange.com/questions/13086/is-there-a-boxplot-variant-for-poisson-distributed-data
#And adjboxStats description https://rdrr.io/rforge/robustbase/man/adjboxStats.html
ROAS_O<-function(x){
  ROAS_outlier<-as.numeric(adjboxStats(df$ROAS,coef = 1)$out)
  df[df$ROAS == ROAS_outlier[x],1]
}
ROAS_O(1)
sapply(1:l7,ROAS_O)
#Conclusion_7:
#Campaign 13 have ROAS higher Q3+1*exp(3M) which mean we should take it plus +20%
#Campaign 3,7,9 have ROAS lower Q1-1*exp(3M) which mean we should take it plus -20%

#Based on Conclusion_1 to 7

#Conclusion_1:
#It's not normal that CPM_Spend is so high(over 99)
#And Campaign ID 14 and 15 have CPM_Spend over Q3+1.5*exp(3M), which mean we should take them plus -10%.

#Conclusion_2:
#It's not normal that CTR over 100% but it's endurable for low CTR, we will catch for those suspicious high CTR Campaign.
#Campaign 14 and 15 have CTR over Q3+1.5*exp(3M) which mean we should take them plus -10%.

#Conclusion_3:
#Since it's not normal that CVR too low
#Campaign 14 has Conversion_rate lower Q1-1.5*exp(3M) which mean we should take them plus -10%.

#Conclusion_4:
#Find out 0 campaigns Retention_Day_7 lie outside Q1-1.5*exp(3M) and Q3+1.5*exp(3M)

#Conclusion_5:
#Campaign 10 has ARPDAU_D7 higher Q3+1.5*exp(3M) which mean we should take them plus +10%.
#Campaign 2 has ARPDAU_D7 lower Q1-1.5*exp(3M) which mean we should take them multiply -10%.

#Conclusion_6:
#Campaign 13,14,16 have ROI_180 higher Q3+0.25*exp(3M) which mean we should take it plus +20%
#Campaign 3,7,9,18 have ROI_180 lower Q1-0.25*exp(3M) which mean we should take it plus -20%

#Conclusion_7:
#Campaign 13 have ROAS higher Q3+1*exp(3M) which mean we should take it plus +20%
#Campaign 3,7,9 have ROAS lower Q1-1*exp(3M) which mean we should take it plus -20%

#Campaign score Calculation:
#Campaign ID 15:0-0.1 -0.1= -0.2
#Campaign ID 14:0-0.1 -0.1 -0.1 =-0.3
#Campaign ID 10:0+0.1=0.1
#Campaign ID 2:0-0.1 = -0.1
#Campaign ID 13:0+0.2+0.2=0.4
#Campaign ID 14:-0.3+0.2=-0.1
#Campaign ID 16:0+0.2=0.2
#Campaign ID 3:0-0.2-0.2=-0.4
#Campaign ID 7:0-0.2-0.2=-0.4
#Campaign ID 9:0-0.2-0.2=-0.4
#Campaign ID 18:0-0.2=-0.2

#In the summary, we can label campaign ID = 10,13,16 as "Good" campaign due to positive Campaign score.
#And campaign ID = 2,3,7,9,14,15,18 as "Bad" campaign due to negative Campaign score.
#Others will label as "Average" campaign due to 0 Campaign score.



#For skewed distributions, the standard deviation gives no information on the asymmetry. 
#It is better to use the first and third quartiles, since these will give some sense of the asymmetry of the distribution. 
#As a result, we uae Percentile to interpret the data
quantile(df$ROAS, c(.25, .50, .75)) 





#curve(dnorm(w, mean=mean(w), sd=sd(w)), y = 5, to = 15, add=T, col="blue")



shapiro.test(df$ROI_180)
df_roi<-data.frame(df$campaign_id,df$ROI_180)


x<-c(1,7,14,30,60,120,150,180)
y<-c(df$Retention_Day_1[1],df$Retention_Day_7[1],)

LTV <-function(m){
  x<-c(1,7,14,30,60,120,150,180)
  m=x^2
}
u<-LTV(2)
u
