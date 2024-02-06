Mydata <- read.csv("Screen Time.csv")
Mydata$Prop.ST <- Mydata$Social.ST.min/Mydata$Total.ST.min
Mydata$Duration <- Mydata$Total.ST.min/Mydata$Pickups
head(Mydata)



par(mfrow=c(1,2))
ts.plot(Mydata$Total.ST.min,type="o",ylab="Total Screen Time(ST)")
ts.plot(Mydata$Social.ST.min,type="o",ylab="Total Social ST")
par(mfrow=c(1,2))
ts.plot(Mydata$Pickups,type="o",ylab="Total Number of Pickups")
par(mfrow=c(1,2))
ts.plot(Mydata$Prop.ST,type="o",ylab="Daily Proportion of Social ST")
ts.plot(Mydata$Duration,type="o",ylab="Daily Duration Per Use")



pairs(Mydata[,c(3,5,6,8,9)], pch = 19)
cor(Mydata[,c(3,5,6,8,9)])



OTC <- function(n,max,vec){
  prob <- c()
  threshold <- seq(0,max,length.out= n)
  for (i in 1:n){
    prob[i] <- sum(vec>=threshold[i])/length(vec)
  }
  df <- data.frame(prob=prob,threshold=threshold)
  df <- df[!duplicated(df$prob), ]
  
  return(list(prob=df$prob,
              threshold=df$threshold))
}




attach(Mydata)



par(mfrow=c(1,2))
Total_OTC <- OTC(1000,max(Total.ST.min),Total.ST.min)
plot(Total_OTC$threshold,Total_OTC$prob,type="l",lwd=2,ylab="P(X>c)",
     xlab="Threshold for Total ST")
Social_OTC <- OTC(1000,max(Social.ST.min),Social.ST.min)
plot(Social_OTC$threshold,Social_OTC$prob,type="l",lwd=2,ylab="P(X>c)",
     xlab="Threshold for Social ST")




par(mfrow=c(1,2))
Pickup_OTC <- OTC(1000,max(Pickups),Pickups)
plot(Pickup_OTC$threshold,Pickup_OTC$prob,type="l",lwd=2,ylab="P(X>c)",
     xlab="Threshold for Pickups")




par(mfrow=c(1,2))
Prop.ST_OTC <- OTC(1000,max(Prop.ST),Prop.ST)
plot(Prop.ST_OTC $threshold,Prop.ST_OTC $prob,type="l",lwd=2,ylab="P(X>c)",
     xlab="Threshold for Prop ST")
Duration_OTC <- OTC(1000,max(Duration),Duration)
plot(Duration_OTC$threshold,Duration_OTC$prob,type="l",lwd=2,ylab="P(X>c)",
     xlab="Threshold for Duration")


detach(Mydata)



# (d)

acf(Mydata$Total.ST.min,plot=FALSE)
acf(Mydata$Social.ST.min,plot=FALSE)
acf(Mydata$Pickups,plot=FALSE)
acf(Mydata$Prop.ST,plot=FALSE)
acf(Mydata$Duration,plot=FALSE)

par(mfrow=c(1,2))
acf(Mydata$Total.ST.min)
acf(Mydata$Social.ST.min)
par(mfrow=c(1,2))
acf(Mydata$Pickups)
acf(Mydata$Prop.ST)
par(mfrow=c(1,2))
acf(Mydata$Duration)


# Question 3

library(circular)
library(tidyverse)

Mydata$Pickup.1s <- gsub(" AM", "", Mydata$Pickup.1s)
Mydata$Pickup.1s <- sub("12", "0", Mydata$Pickup.1s)

pickup1 <- strptime( Mydata$Pickup.1s, "%H:%M")
Mydata <- Mydata %>%
  mutate(Pickup.1st.Angular=(hour(pickup1)*60 + minute(pickup1))/(24*60)*360)

first.pickup.cir = circular(Mydata$Pickup.1st.Angular, 
                            units = "degrees", template = "clock24")
plot(first.pickup.cir, col = "blue")


par(mfrow=c(1,2))
plot(first.pickup.cir, stack = TRUE, bins = 144, col = "blue")
plot(first.pickup.cir, stack = TRUE, bins = 72, col = "blue")
par(mfrow=c(1,2))
plot(first.pickup.cir, stack = TRUE, bins = 48, col = "blue")
plot(first.pickup.cir, stack = TRUE, bins = 24, col = "blue")







# Question 4

# In hour
Mydata$St <- Mydata$Total.ST.min/60
fit <- glm(Pickups ~ 1, offset = log(Mydata$St), family = poisson(link = "log"),
           data = Mydata)
summary(fit)



## (c)

Mydata$Date <- as.Date(Mydata$Date, format = "%m/%d/%Y" )
Mydata$weekday <- weekdays(Mydata$Date , abbreviate = T)
Mydata <- Mydata %>% mutate ( if_weekend = weekday %in% c("Sun", "Sat"))
Mydata$Xt <- ifelse(Mydata$if_weekend==T,1,0)
Mydata$Zt <- ifelse( Mydata$Date < Mydata$Date[4],0,1)



fit2 <- glm(Pickups ~ Xt+Zt, offset = log(Mydata$St), family = poisson(link = "log"),
            data = Mydata)
summary(fit2)




#  Question 5

mle.vonmises(first.pickup.cir)

1-pvonmises(circular(8.5/24*360-180), mu=circular(35.02), kappa=circular(2.14))



