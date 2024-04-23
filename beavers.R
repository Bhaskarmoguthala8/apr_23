?beavers
str(beaver2)
View(beaver2)
#H0: Body temperature is not affected by activity
#H1: Body temperature is affected by activity
beavers_data=beaver2
beavers_data
beavers_data$activ=factor(beavers_data$activ, labels=c("no","yes"))
str(beavers_data)
beavers_data

install.packages("ggplot2")
library(ggplot2)
#histogram of body temperature
windows(16,10)
ggplot(beavers_data,aes(x=temp))+geom_histogram()+theme_bw()
ggplot(beavers_data,aes(x=temp))+geom_histogram(breaks=seq(36,38,2))+theme_bw()+labs(x="temp",y="Activity")+scale_y_continuous(breaks=seq(0,60,5))
install.packages("lattice")
library("lattice")
window(16,10)
attach(beavers_data)
histogram(~temp| activ,data= beavers_data,main="Distribution of beaver activity data",xlab="Temperature (degrees)",ylab="Activity %")
detach(beavers_data)
ggplot(beavers_data,aes(x=temp))+geom_histogram()+theme_bw()

ggplot(beavers_data,aes(x=temp))+geom_histogram(breaks=seq(36,38,.2))
+theme_bw()+labs(x="temp",y="Activity")
+scale_y_continuous(breaks=seq(0,60,5))

opar=par(no.readonly= TRUE)
windows(20,10)
install.packages("lattice")
library(lattice)

window(20,10)
attach(beavers_data)
histogram(~temp | activ,
          data=beavers_data,main="distribution of beavers activity data",
          xlab="temparatures(degrees)",ylab="Activity %")
detach(beavers_data)
attach(beavers_data)
windows(16,10)
qqnorm(temp)
qqline(temp,col="red")

opar<-par(no.readonly = TRUE)
#window(20,10)

par(mfrow=c(1,2))

with(beavers_data,{
  qqnorm(temp[activ=="yes"],
         main="Beavers active data")
  qqline(temp[activ=="yes"])
  })
with(beavers_data,{
  qqnorm(temp[activ=="no"],
         main = "Beavers inactive data")
  qqline (temp[activ=="no"]
  )
})
normality_test=shapiro.test(beavers_data$temp)
normality_test
