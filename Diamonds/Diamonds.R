# import libraries
library(MASS)
source("~/Desktop/36401/panelfxns.R")

# import data
data<-read.table("~/Desktop/36401/exam2-87.txt")

# get vars
price<-data[,1]
carat<-data[,2]
cut<-data[,3]
color<-data[,4]
clarity<-data[,5]
x<-data[,6]
y<-data[,7]
z<-data[,8]
top<-data[,9]
body<-data[,10]

n = nrow(data)

# Univariate EDA

# Continuous EDA
get_eda<-function(data){
  result<-c(min(data),max(data),median(data),mean(data),sd(data))
  return(result)
}
get_eda(price)
get_eda(carat)
get_eda(x)
get_eda(y)
get_eda(z)
get_eda(top) 
get_eda(body)

par(mfrow = c(3,3))

# Histograms
hist(price,col=2,breaks=10,main="")
hist(carat,col=3,breaks=10,main="")
hist(x,col=4,breaks=20,main="")
hist(y,col=5,breaks=20,main="")
hist(z,col=6,breaks=20,main="")
hist(top,col=7,breaks=10,main="")
hist(body,col=8,breaks=10,main="")

# Categorical EDA
getCategoricalEDA<-function(vector,categories)
{
  index = 1
  result<-c(0)
  for (c in categories)
  {
    num<-length(which(vector==c))
    perc<-100*num/n
    result[index]=perc
    index = index + 1
  }
  return(result)
}

getCategoricalEDA(cut,c("Fair", "Good", "Very Good", "Ideal", "Premium"))
getCategoricalEDA(color,c("D","E","F","G","H","I","J"))
getCategoricalEDA(clarity,c("I1","SI1","SI2","VS1","VS2","VVS1","VVS2","IF"))

# Bar Graphs
par(mfrow = c(3,3))
barplot(table(cut),main="cut")
barplot(table(color),main="color")
barplot(table(clarity),main="clarity")

# Remove bad data
badIndex<-which(x==0 & y==0 & z==0)
data<-data[-badIndex,]

# get vars again
price<-data[,1]
carat<-data[,2]
cut<-data[,3]
color<-data[,4]
clarity<-data[,5]
x<-data[,6]
y<-data[,7]
z<-data[,8]
top<-data[,9]
body<-data[,10]

cut = factor(cut,c("Fair","Good","Very Good","Premium","Ideal"))
clarity = factor(clarity,c("I1","SI1","SI2","VS1","VS2","VVS1","VVS2","IF"))

# adjust n
n<-n-1

# Continuous Bivariate EDA
vars<-cbind(price,carat,x,y,z,top,body)
pairs(vars,lower.panel=panel.cor)

# Categorical Bivariate EDA
par(mfrow=c(2,2))
boxplot(price~cut,ylab="Price ($)")
boxplot(price~color,ylab="Price ($)")
boxplot(price~clarity,ylab="Price ($)")

# Model

# coding categorical variables

model.cut<-lm(price~cut)
summary(model.cut)

fairAndGood<-as.numeric(cut=="Fair" | cut=="Good")
veryGood<-as.numeric(cut=="Very Good")
premium<-as.numeric(cut=="Premium")
ideal<-as.numeric(cut=="Ideal")

Cut<-rep(NA,length(cut))
Cut[cut=="Fair"] = 0; Cut[cut=="Good"] = 0
Cut[cut=="Very Good"] = 1; Cut[cut=="Premium"] = 2
Cut[cut=="Ideal"] = 3;

Color<-rep(NA,length(color))
Color[color=="D"] = 0; Color[color=="E"] = 1
Color[color=="F"] = 2; Color[color=="G"] = 3
Color[color=="H"] = 4; Color[color=="I"] = 5
Color[color=="J"] = 6

poorClarity<-as.numeric(clarity=="I1" | clarity=="SI1")
SI2<-as.numeric(clarity=="SI2")
VS1<-as.numeric(clarity=="VS1")
VS2<-as.numeric(clarity=="VS2")
bestClarity<-as.numeric(clarity=="VVS1" | clarity=="VVS2" | clarity=="IF")


model.size<-lm(price~x+y+z)
aov(model.size)
model.size<-lm(price~x+z+y)
aov(model.size)
model.size<-lm(price~y+x+z)
aov(model.size)
model.size<-lm(price~y+z+x)
aov(model.size)
model.size<-lm(price~z+x+y)
aov(model.size)
model.size<-lm(price~z+y+x)
aov(model.size)

model<-lm(price~carat+Cut+Color+SI2+VS1+VS2+bestClarity+x+top+body)
summary(model)

model.2<-lm(price~carat+veryGood+premium+ideal+Color+SI2+VS1+VS2+bestClarity+x+y+z+top+body)
summary(model.2)

par(mfrow = c(1,1))
plot(body,price,pch=16,col=as.numeric(clarity),main="Price vs. Body")

abline(lm(price[poorClarity==1]~body[poorClarity==1]),col=1)
abline(lm(price[SI2==1]~body[SI2==1]),col=2)
abline(lm(price[VS1==1]~body[VS1==1]),col=3)
abline(lm(price[VS2==1]~body[VS2==1]),col=4)
abline(lm(price[bestClarity==1]~body[bestClarity==1]),col=5)
legend("topright",c("I1 or SI1","SI2","VS1","VS2","VVS1 or VVS2 or IF"),col=c(1,2,3,4,5),lwd=1,pch=16,cex=.5)

model<-lm(price.t~carat+Cut+Color+SI2+VS1+VS2+bestClarity+x+top+body)
par(mfrow=c(2,2))
qqnorm(model$res,main="Before transformation"); qqline(model$res)
boxcox(model)

price.t<-log(price)
model.t<-lm(price.t~carat+Cut+Color+SI2+VS1+VS2+bestClarity+x+top+body)
qqnorm(model.t$res,main="After transformation"); qqline(model.t$res)
boxcox(model.t)

plot(model.t$fitted,model.t$res,xlab="Fitted Values",ylab="Residuals",main=""); abline(h=0,lty=2)

layout(matrix(c(1,2,3,4,5,6),ncol=2))
title("Poor Clarity \n vs. Residuals")
plot(poorClarity,model.t$res,ylab="Residuals")
plot(SI2,model.t$res,ylab="Residuals")
plot(VS1,model.t$res,ylab="Residuals")
plot(VS2,model.t$res,ylab="Residuals")
plot(bestClarity,model.t$res,ylab="Residuals")
plot(model.t$res,pch=16,ylab="Residuals",main="Independence")
abline(h=0, lty=2)

summary(model.t)
aov(lm(price.t~carat+Cut+Color+SI2+VS1+VS2+bestClarity+x+body+top))
ssr.top = .00536
aov(lm(price.t~carat+Color+SI2+VS1+VS2+bestClarity+x+body+top+Cut))
ssr.Cut = .07959
aov(lm(price.t~carat+Color+SI2+VS1+VS2+bestClarity+x+top+Cut+body))
ssr.body = .61961
sse = 7.53497

f = ((ssr.top + ssr.Cut)/2)/(sse/(n-10))
f
f.star = qf(.95,df1=2,df2=n-10)
f.star
f > f.star

model.final<-lm(price.t~carat+Color+SI2+VS1+VS2+bestClarity+x+body)
summary(model.final)