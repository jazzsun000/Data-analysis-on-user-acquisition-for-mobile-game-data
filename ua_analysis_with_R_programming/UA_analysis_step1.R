
#In the overview of a game performance, we will take a look on daily install,D1_dau,Retention rate,ARPU and LTV
# Read CSV into R
df <- read.csv(file="ua_analyst_task3.csv", header=TRUE, sep=",")

#1
#We want to know the trend of new installs.
#Overview of Installs with line chart and linear fit line
library(ggplot2)    
library(scales)
df1 <- data.frame(df$registration_date,df$installs)
date <- as.Date(df$registration_date)
ggplot(data = df1, aes(x = date, y = df$installs,group = 1))+
  geom_line(color = "#00AFBB", size = 2)+
  geom_smooth(method = "lm")+
  labs(x = "registration_date", y = "Installs", 
       title = "The overview of new Installs")+
  scale_x_date(labels = date_format("%m-%Y"))
#From trend line, we can assume that installs is getting more and more since 01-2021

#2
#We want to know the trend of D1_DAU.
#Overview of D1_DAU with line chart and linear fit line
library(ggplot2)    
library(scales)
df2 <- data.frame(df$registration_date,df$d1_dau)
date2 <- as.Date(df$registration_date)
ggplot(data = df2, aes(x = date2, y = df$d1_dau,group = 1))+
  geom_line(color = "navy", size = 2)+
  geom_smooth(method = "lm")+
  labs(x = "registration_date", y = "D1_DAU", 
       title = "The overview of D1_DAU")+
  scale_x_date(labels = date_format("%m-%Y"))
#From trend line, we can assume that D1_DAU is getting more and more since 01-2021

#3 How is the campaign performance on Retention_rate?
#We want to know the performance of Retention_rate.
#Firstly, we need to calculate the Retention_Rate
#add new column Retention_Day_1,Retention_Day_3,Retention_Day_7
#add new column ,Retention_Day_14,Retention_Day_30,Retention_Day_60
df$Retention_Day_1<-(df$d1_dau/df$installs)
df$Retention_Day_3<-(df$d3_dau/df$installs)
df$Retention_Day_7<-(df$d7_dau/df$installs)
df$Retention_Day_14<-(df$d14_dau/df$installs)
df$Retention_Day_30<-(df$d30_dau/df$installs)
df$Retention_Day_60<-(df$d60_dau/df$installs)

#In the regular situation, retention_rate should not fluctuate a lot in the overall time period.
#As a result, we did the shapiro.test on d1,d3 and d7 retention_rate to know if they are normal distributed.
#We want to know if d1_retention is normally distributed.
#HO:Sample follow the normal distribution ,HA:Reject the normal distribution

shapiro.test(df$Retention_Day_1)

#W = 0.96684, p-value = 0.001052<0.05, reject H0 Retention_Day_1 is not normally distributed.

#We want to know if d3_retention is normally distributed.
#HO:Sample follow the normal distribution ,HA:Reject the normal distribution

shapiro.test(df$Retention_Day_3)

#W = 0.97297, p-value = 0.004504 < 0.05, reject H0 Retention_Day_1 is not normally distributed.


#We want to know if d7_retention is normally distributed.
#HO:Sample follow the normal distribution ,HA:Reject the normal distribution
shapiro.test(df$Retention_Day_7)
#W = 0.99344, p-value = 0.7262 > 0.05, Accept H0 Retention_Day_7 is normally distributed.

#Campaign d7_Retention_Rate performance overview
#Import the campaign d7 data
residentuser<-data.frame(df$registration_date,df$installs,df$d7_dau,df$Retention_Day_7)
#Using QCC AS Quality Control Charts to perform statistical quality control
library(qcc)
residentuser$date<-as.Date(df$registration_date)
attach(residentuser)
#type = "p" one-at-time data of a continuous process variable
sol<-qcc(df$d7_dau,df$installs,labels = date,
         type = "p",nsigmas = 3,
         title="Campaign d7_Retention_Rate performance overview",
         xlab="date",ylab="D7 Retention rate")
#There are 96 days(red points) have abnormal situation, and we should take care time periods on 2021-02-02 and 2021-04-07
#There are a lot of days d7_Retention_Rate perform below the three sigma low bar, we can suspect there might be some fraudulent behaviors happened.
summary(sol)
detach(residentuser)

#Overview of Retention_Day_7 with line chart and linear fit line
library(ggplot2)    
library(scales)
df3 <- data.frame(df$registration_date,df$Retention_Day_7)
date3 <- as.Date(df$registration_date)
ggplot(data = df3, aes(x = date3, y = df$Retention_Day_7,group = 1))+
  geom_line(color = "gray", size = 2)+
  geom_smooth(method = "lm")+
  labs(x = "registration_date", y = "Retention_Day_1", 
       title = "The overview of Retention_Day_7")+
  scale_x_date(labels = date_format("%m-%Y"))
#From trend line, we can assume that Retention_Day_1 is getting better since 01-2021



#We want to know if d14_retention is normally distributed.
#HO:Sample follow the normal distribution ,HA:Reject the normal distribution
shapiro.test(df$Retention_Day_14)

#W = 0.97726, p-value = 0.01323 < 0.05, reject H0 Retention_Day_14 is not normally distributed.

#We want to know if d30_retention is normally distributed.
#HO:Sample follow the normal distribution ,HA:Reject the normal distribution

shapiro.test(df$Retention_Day_30)

#W = 0.98956, p-value = 0.325 > 0.05, Accept H0 Retention_Day_30 is normally distributed.

#We want to know if d60_retention is normally distributed.
#HO:Sample follow the normal distribution ,HA:Reject the normal distribution

shapiro.test(df$Retention_Day_60)

#W = 0.98749, p-value = 0.1935 > 0.05, Accept H0 Retention_Day_60 is normally distributed.








#4
#We want to know the trend of ARPDAU_Day_1 to ARPDAU_Day_60 on daily campaign
#Firstly, we need to calculate the ARPDAU_Day_1
#We divide each accumulated revenue correspond with their days,then get to estimate that day revenue
df$ARPDAU_Day_1<-((df$d1_iap_revenue+df$d1_ads_revenue)/df$d1_dau)
df$ARPDAU_Day_3<-((df$d3_iap_revenue+df$d3_ads_revenue)/3/df$d3_dau)
df$ARPDAU_Day_7<-((df$d7_iap_revenue+df$d7_ads_revenue)/7/df$d7_dau)
df$ARPDAU_Day_14<-((df$d14_iap_revenue+df$d14_ads_revenue)/14/df$d14_dau)
df$ARPDAU_Day_30<-((df$d30_iap_revenue+df$d30_ads_revenue)/30/df$d30_dau)
df$ARPDAU_Day_60<-((df$d60_iap_revenue+df$d60_ads_revenue)/60/df$d60_dau)
df$ARPDAU_Day_1[2]
ar_date<-c(df$registration_date)
ar_date<-as.factor(ar_date)
head(ar_date)
ar_ARPDAU_Day_1<-c(df$ARPDAU_Day_1)
ar<- data.frame(df$registration_date,df$ARPDAU_Day_1,df$ARPDAU_Day_3,df$ARPDAU_Day_7,df$ARPDAU_Day_14,df$ARPDAU_Day_30,df$ARPDAU_Day_60)
head(ar)



#Overview of ARPDAU_Day_1 with line chart and linear fit line
library(ggplot2)    
library(scales)
df4 <- data.frame(df$registration_date,df$ARPDAU_Day_1)
date4 <- as.Date(df$registration_date)
ggplot(data = df4, aes(x = date4, y = df$ARPDAU_Day_1,group = 1))+
  geom_line(color = "brown", size = 2)+
  geom_smooth(method = "lm")+
  labs(x = "registration_date", y = "ARPDAU_Day_1", 
       title = "The overview of ARPDAU_Day_1")+
  scale_x_date(labels = date_format("%m-%Y"))
#From trend line, we can assume that ARPDAU_Day_1 is increasing slightly since 01-2021

#5
#We want to know the trend of LTV_Day_1.
#Firstly, we need to calculate the LTV_Day_1
#CLTV calculation from AppLovin
#LTV=ARPUx(1/churn_rate)+(referral value)
#churn_rate=1-Retention_rate,here we assume (referral value)=0
df$LTV_Day_1<-(df$ARPDAU_Day_1*(1/(1-df$Retention_Day_1)))
#Overview of LTV_Day_1 with line chart and linear fit line
library(ggplot2)    
library(scales)
df5 <- data.frame(df$registration_date,df$LTV_Day_1)
date5 <- as.Date(df$registration_date)
ggplot(data = df5, aes(x = date5, y = df$LTV_Day_1,group = 1))+
  geom_line(color = "steelblue", size = 2)+
  geom_smooth(method = "lm")+
  labs(x = "registration_date", y = "LTV_Day_1", 
       title = "The overview of LTV_Day_1")+
  scale_x_date(labels = date_format("%m-%Y"))
#From trend line, we can assume that LTV_Day_1 is increasing since 01-2021





library(ggplot2)
ggplot(df, aes(x=df$registration_date, y=df$installs)) + geom_bar(stat="identity")

ggplot(df, aes(x = df$registration_date, y = df$installs)) +
  geom_line() +
  geom_point()
#Overview of D1_DAU with barchart
# This gives us a raw barplot
df.bar <-barplot(df$d1_dau,names.arg=df$registration_date,main="The overview of new D1_DAU",col="navy")
lines(x = df.bar, y = df$d1_dau,col = 'red')
#points(x = df.bar, y = df$d1_dau)

#ggplot(df1, aes(x = date, y = df$installs) + geom_bar(stat = "identity", aes(fill = as.factor(df$installs >= 0))) + geom_smooth(method = "lm") + scale_fill_manual(values = c("red", "darkgreen")) + guides(fill = FALSE) )


#ts.Pharma <- ts(df$installs)
#plot(ts.Pharma,
 #    main="Actual Prices of Pharma Stock",
  #   col="navy", 
   #  ylab="Pharma (actual)")
#par(mfrow=c(1,2))
plot(df$installs~df$registration_date,
     type="l",
     lwd=2,
     col="navy",
     xlab="registration_date", ylab="Installs",
     main="The overview of new installs",
     ylim=c(0,70000))
par(new=TRUE)
plot(df$d1_dau~df$registration_date,
     type="l",
     lwd=2,
     ann=FALSE,
     col="brown",
     ylim=c(0,70000))

#Rention_rate is one of the most important key metrics we want to observe
#add new column Retention_Day_1,Retention_Day_3,Retention_Day_7
#add new column ,Retention_Day_14,Retention_Day_30,Retention_Day_60
df$Retention_Day_1<-(df$d1_dau/df$installs)
df$Retention_Day_3<-(df$d3_dau/df$installs)
df$Retention_Day_7<-(df$d7_dau/df$installs)
df$Retention_Day_14<-(df$d14_dau/df$installs)
df$Retention_Day_30<-(df$d30_dau/df$installs)
df$Retention_Day_60<-(df$d60_dau/df$installs)
#Assumption:We want to know about the new installs campaign impact on other metrics
#Plot the scatter plot and add the linear fitting line
#d1_dau vs installs 
plot(df$d1_dau~df$installs,col="violetred2",pch=16,main="Daily_Active_user_D1 VS Daily_new_install")
abline(lm(df$d1_dau~df$installs),col="blue",lwd=2,lty=2)
#From scatter plot,we can assume there some positive correlation between new_install and d1_dau
# Basic Scatterplot Matrix
pairs(~df$installs+df$d1_dau+df$d3_dau+df$d7_dau+df$d14_dau+df$d30_dau+df$d60_dau,data=df,main="Daily_Active_user VS Daily_new_install")

# Scatterplot Matrices from the glus Package 
install.packages("gclus")
library(gclus)
dta <- data.frame(df$installs,df$d1_dau,df$d3_dau,df$d7_dau,df$d14_dau,df$d30_dau,df$d60_dau)
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="The correlation between new installs and DAU" )

#IAP Revenue
dta1 <- data.frame(df$installs,df$d1_iap_revenue,df$d3_iap_revenue,df$d7_iap_revenue,df$d14_iap_revenue,df$d30_iap_revenue,df$d60_iap_revenue)
dta1.r <- abs(cor(dta1)) # get correlations
dta1.col <- dmat.color(dta1.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta1.o <- order.single(dta1.r) 
cpairs(dta1, dta1.o, panel.colors=dta1.col, gap=.5,
       main="The correlation between new installs and IAP Revenue" )

#Ads Revenue
dta2 <- data.frame(df$installs,df$d1_ads_revenue,df$d3_ads_revenue,df$d7_ads_revenue,df$d14_ads_revenue,df$d30_ads_revenue,df$d60_ads_revenue)
dta2.r <- abs(cor(dta2)) # get correlations
dta2.col <- dmat.color(dta2.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta2.o <- order.single(dta2.r) 
cpairs(dta2, dta2.o, panel.colors=dta2.col, gap=.5,
       main="The correlation between new installs and Ads Revenue" )

#first_time_spenders
dta3 <- data.frame(df$installs,df$d1_first_time_spenders,df$d3_first_time_spenders,df$d7_first_time_spenders,df$d14_first_time_spenders,df$d30_first_time_spenders,df$d60_first_time_spenders) # get data 
dta3.r <- abs(cor(dta3)) # get correlations
dta3.col <- dmat.color(dta3.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta3.o <- order.single(dta3.r) 
cpairs(dta3, dta3.o, panel.colors=dta3.col, gap=.5,
       main="The correlation between new installs and first_time_spenders" )

dta1 <- data.frame(df$installs,df$d1_iap_revenue,df$d3_iap_revenue,df$d7_iap_revenue,df$d14_iap_revenue,df$d30_iap_revenue,df$d60_iap_revenue,df$d1_ads_revenue,df$d3_ads_revenue,df$d7_ads_revenue,df$d14_ads_revenue,df$d30_ads_revenue,df$d60_ads_revenue,df$d1_first_time_spenders,df$d3_first_time_spenders,df$d7_first_time_spenders,df$d14_first_time_spenders,df$d30_first_time_spenders,df$d60_first_time_spenders) # get data 
dta1.r <- abs(cor(dta1)) # get correlations
dta1.col <- dmat.color(dta1.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dt1a.o <- order.single(dta1.r) 
cpairs(dta1, dta1.o, panel.colors=dta1.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )




#Plot line chart
# Basic line plot
date<-df$registration_date
ggplot(data=df, aes(x=date, y=df$Retention_Day_3))+geom_line()

library(ggplot2)
theme_set(theme_classic())

# Allow Default X Axis Labels
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=df$Retention_Day_3)) + 
  labs(title="Time Series Chart", 
       subtitle="Retention_rate Percentage from 'rovio' Dataset", 
       caption="Source: Rovio", 
       y="Retention_rate_d3 %")
