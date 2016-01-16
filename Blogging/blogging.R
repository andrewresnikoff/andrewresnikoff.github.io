# import libraries
library(MASS)
library(tree)

# source helping scripts
source("~/Desktop/36401/panelfxns.R")

# import data
data<-read.table("~/Desktop/36401/final-87.txt")
names(data)
attach(data)

n<-nrow(data)

# Univariate EDA
get.eda<-function(data){
  result<-c(min(data),max(data),median(data),mean(data),sd(data))
  return(result)
}

get.eda.cat<-function(vector,categories)
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

# print eda
get.eda(comments.next24)
get.eda(comments.prebasetime)
get.eda(comments.prev24)
get.eda(comments.first24)
get.eda(source.avg.prebasetime)
get.eda(source.avg.prev24)
get.eda(source.avg.first24)
get.eda(links.prebasetime)
get.eda(links.prev24)
get.eda(links.first24)
get.eda(length)
get.eda(post.age)
get.eda(n.parents)
get.eda(parents.ave)
get.eda(parents.min)
get.eda(parents.max)
get.eda.cat(basetime.day,c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
get.eda.cat(post.day,c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
get.eda.cat(word1,c(0,1))
get.eda.cat(word2,c(0,1))
get.eda.cat(word3,c(0,1))
get.eda.cat(word4,c(0,1))
get.eda.cat(word5,c(0,1))
get.eda.cat(word6,c(0,1))
get.eda.cat(word7,c(0,1))
get.eda.cat(word8,c(0,1))
get.eda.cat(word9,c(0,1))
get.eda.cat(word10,c(0,1))

par(mfrow=c(4,3))
hist(comments.next24,breaks=40)
hist(comments.prebasetime)
hist(comments.prev24)
hist(comments.first24)
hist(source.avg.prebasetime)
hist(source.avg.prev24)
hist(source.avg.first24)
hist(length)
hist(post.age)
hist(n.parents)
hist(parents.ave)
hist(parents.max)
title("Figure 1: Histograms for Continuous Variables",outer=TRUE)

basetime.day<-factor(basetime.day,c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
post.day = factor(post.day,c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

par(mfrow=c(3,3))
title("Figure 2: Barplots for categorical variables",outer=TRUE)
barplot(table(basetime.day),main="basetime.day")
barplot(table(post.day),main="post.day")
barplot(table(word1),main="word1")
barplot(table(word4),main="word4")
barplot(table(word5),main="word5")
barplot(table(word7),main="word7")
barplot(table(word8),main="word8")
barplot(table(word10),main="word10")

par(mfrow=c(1,1))
# Multivariate EDA
vars.cont<-cbind(comments.next24,comments.prebasetime,comments.prev24,comments.first24,
             source.avg.prebasetime,source.avg.prev24,source.avg.first24,
             links.prebasetime,links.prev24,links.first24,length,post.age,
             n.parents,parents.ave,parents.max)
pairs(vars.cont,upper.panel=panel.smooth,lower.panel=panel.cor,main="Figure 3: Pairs Plots")

par(mfrow=c(2,2))
boxplot(comments.next24~basetime.day,names=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), outline=TRUE,main="Comments after 24hrs vs. Basetime day")
boxplot(comments.next24~post.day,names=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),outline=TRUE,main="Comments after 24hrs vs. Post day")
boxplot(comments.next24~basetime.day,names=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), outline=FALSE,main="Comments after 24hrs vs. Basetime day (no outliers)")
boxplot(comments.next24~post.day,names=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),outline=FALSE,main="Comments after 24hrs vs. Post day (no outliers)")
title("Figure 4: Categorical boxplots",outer=TRUE)

# categorical coding
Post.day<-rep(NA,length(post.day))
Post.day[post.day=="Sunday" | post.day=="Monday"] = 1
Post.day[post.day!="Sunday" & post.day!="Monday"] = 0


# weekday vs. weekend model

Basetime.day<-rep(NA,length(basetime.day))
Basetime.day[basetime.day=="Sunday" | basetime.day=="Friday" | basetime.day=="Saturday"] = 1
Basetime.day[basetime.day=="Monday" | basetime.day=="Tuesday" | basetime.day=="Wednesday" | basetime.day=="Thursday"] = 0

# boxplot inference model

Basetime.Day<-rep(NA,length(basetime.day))
Basetime.Day[basetime.day=="Friday" | basetime.day=="Saturday"] = 0
Basetime.Day[basetime.day=="Monday" | basetime.day=="Sunday" | basetime.day=="Wednesday" | basetime.day=="Thursday"] = 1
Basetime.Day[basetime.day=="Tuesday"] = 2

summary(lm(comments.next24~basetime.day))
summary(lm(comments.next24~Basetime.day))
summary(lm(comments.next24~Basetime.Day))
summary(lm(comments.next24~Post.day))

# initial model
model.init<-lm(comments.next24~comments.prebasetime+comments.prev24+comments.first24+
               source.avg.prebasetime+source.avg.prev24+source.avg.first24+
               links.prebasetime+links.prev24+links.first24+length+
               post.age+Basetime.day+Post.day+word1+word4+word5+
               word7+word8+word10+n.parents+parents.ave+parents.max)

# mulitcollinearity

# partial f test
aov(lm(comments.next24~comments.prebasetime+comments.prev24+
         source.avg.prebasetime+source.avg.prev24+source.avg.first24+
         links.prebasetime+links.prev24+links.first24+length+
         post.age+Basetime.day+Post.day+word1+word4+word5+
         word7+word8+word10+n.parents+parents.ave+parents.max+comments.first24))
c.f.ssr<-15781.3
aov(lm(comments.next24~comments.prebasetime+comments.prev24+
         source.avg.prebasetime+source.avg.prev24+
         links.prebasetime+links.prev24+links.first24+length+
         post.age+Basetime.day+Post.day+word1+word4+word5+
         word7+word8+word10+n.parents+parents.ave+parents.max+comments.first24+source.avg.first24))
s.a.f.ssr<-1626.1
aov(lm(comments.next24~comments.prebasetime+comments.prev24+
         source.avg.prebasetime+source.avg.prev24+
         links.prebasetime+links.prev24+length+
         post.age+Basetime.day+Post.day+word1+word4+word5+
         word7+word8+word10+n.parents+parents.ave+parents.max+comments.first24+
         source.avg.first24+links.first24))
l.f.ssr<-92.2
sse<-2583093.4
f.stat.num<-(c.f.ssr+s.a.f.ssr+l.f.ssr+35357.4)/4
f.stat.den<-(sse)/(n-23)
f.stat<-f.stat.num/f.stat.den
f.stat > qf(.95,df1=3,df2=n-(22+1))
# False, therefore we can exclude these variables

model.mult1<-lm(comments.next24~comments.prebasetime+comments.prev24+
                  source.avg.prebasetime+source.avg.prev24+
                  links.prebasetime+links.prev24+length+
                  post.age+Basetime.day+Post.day+word1+word4+word5+
                  word7+word8+word10+n.parents+parents.ave+parents.max)
summary(model.mult1)


# interactions
colors<-rep(1,length(Basetime.day))
colors[Basetime.day==0]<-2
colors[Basetime.day==1]<-3
plot(comments.prev24,comments.next24,pch=16,xlab="# of Comments 24 hrs Before Basetime",
     ylab="# of Comments 24 hrs After Basetime",col=colors)
abline(lm(comments.next24[Basetime.day==0]~comments.prev24[Basetime.day==0]),col=2)
abline(lm(comments.next24[Basetime.day==1]~comments.prev24[Basetime.day==1]),col=3)
title("Figure 5: # of Comments 24 hrs After Basetime vs. # of Comments 24 hrs Before Basetime \n Conditioned on Basetime Day")
legend("topright",c("Weekday","Weekend"),col=c(2,3),lwd=1,pch=16)
int.test1<-chisq.test(table(comments.prev24,Basetime.day))
int.test1$p.value #.5929 

findInteractions<-function()
{
  pred.vars<-cbind(comments.prebasetime,comments.prev24,comments.first24,
    source.avg.prebasetime,source.avg.prev24,source.avg.first24,
    links.prebasetime,links.prev24,links.first24,length,
    post.age,Basetime.day,Post.day,word1,word4,word5,
    word7,word8,word10,n.parents,parents.ave,parents.max)
  names<-c("comments.prebasetime","comments.prev24","comments.first24","source.avg.prebasetime","source.avg.prev24","source.avg.first24","links.prebasetime","links.prev24","links.first24","length","post.age","Basetime.day","Post.day","word1","word4","word5","word7","word8","word10","n.parents","parents.ave","parents.max")
  result<-cbind()
  for (i in 1:length(names))
  {
    for (j in 1:length(names))
    {
      test<-chisq.test(table(pred.vars[i,],pred.vars[j,]))
      if (test$p.value < 4e-11)
      {
        if ((i != j) & (!(paste(names[j],names[i]) %in% result)))
        {
          result<-append(result,paste(names[i],names[j]))
        }
      }
    }
  }
  return(result)
}

findInteractions()

# lowest test p-values
c.f.l.p<-comments.first24*links.prebasetime
c.f.B.d<-comments.first24*Basetime.day
l.p.l.p24<-links.prebasetime*links.prev24
l.p.P.d<-links.prebasetime*Post.day
l.p.w8<-links.prebasetime*word8
P.d.w7<-Post.day*word7
P.d.w8<-Post.day*word8
w7.w8<-word7*word8

int.model<-lm(comments.next24~comments.prebasetime+comments.prev24+
                source.avg.prebasetime+source.avg.prev24+
                links.prebasetime+links.prev24+length+
                post.age+Basetime.day+Post.day+word1+word4+word5+
                word7+word8+word10+n.parents+parents.ave+parents.max+
                c.f.l.p+c.f.B.d+l.p.l.p24+l.p.P.d+l.p.w8+P.d.w7+
                P.d.w8+w7.w8)
summary(int.model)
aov(lm(comments.next24~comments.prebasetime+comments.prev24+
         source.avg.prebasetime+source.avg.prev24+
         links.prebasetime+links.prev24+length+
         post.age+Basetime.day+Post.day+word1+word4+word5+
         word7+word8+word10+n.parents+parents.ave+parents.max+
         c.f.l.p+l.p.l.p24+l.p.w8+
         w7.w8+P.d.w8+P.d.w7+l.p.P.d+c.f.B.d))
w7.w8.ssr<-7747.1
P.d.w8.ssr<-761.9
P.d.w7.ssr<-1140.2
l.p.P.d.ssr<-1767.7
c.f.B.d.ssr<-3719.9
sse<-2398569.1
f.num<-(w7.w8.ssr+P.d.w8.ssr+P.d.w7.ssr+l.p.P.d.ssr+c.f.B.d.ssr)/5
f.den<-sse/(n-28)
f.stat<-f.num/f.den
f.stat > qf(.95,df1=5,df2=n-(27+1))
# False, therefore we can exclude these variables from the model

model<-lm(comments.next24~comments.prebasetime+comments.prev24+
            source.avg.prebasetime+source.avg.prev24+
            links.prebasetime+links.prev24+length+
            post.age+Basetime.day+Post.day+word1+word4+word5+
            word7+word8+word10+n.parents+parents.ave+parents.max+
            c.f.l.p+l.p.l.p24+l.p.w8)

summary(model)

# remove outliers
X<-cbind(1,comments.prebasetime,comments.prev24,
           source.avg.prebasetime,source.avg.prev24,
           links.prebasetime,links.prev24,length,
           post.age,Basetime.day,Post.day,word1,word4,word5,
           word7,word8,word10,n.parents,parents.ave,parents.max,
           c.f.l.p,l.p.l.p24,l.p.w8)
H<-X%*%solve(t(X)%*%X)%*%t(X)

n<-nrow(X)
p<-ncol(X)
SSE<-sum((comments.next24-model$fitted.values)^2)
MSE<-SSE/(n-p)
res<-model$residuals; del.res<-res*sqrt((n-p-1)/(SSE*(1-diag(H))-res^2))
sort(del.res)[1:10]; sort(del.res)[(n-10):n]
alpha<-0.05; qt(1-alpha/(2*n),n-p-1) # 605, 112, 534, 50. 592, 128, 96, 312, 365
mean.h<-p/n; which(diag(H)>2*mean.h); sort(diag(H))[(n-10):n]; order(diag(H))[(n-10):n] # 442, 418, 630, 583, 498, 188, 412, 348, 533, 71, 267
DFFITS<-del.res*(diag(H)/(1-diag(H)))^0.5; 2*sqrt(p/n); 
sort(DFFITS[which(DFFITS > 2*sqrt(p/n))]) # 365, 96, 71, 128, 498, 442, 64, 592, 27, 436
D<-(res^2/(p*MSE))*(diag(H)/(1-diag(H))^2)
perc<-pf(D,p,n-p); tail(sort(perc)) # 365, 96, 312, 71, 82, 605
out<-c(605, 112, 534, 50, 592, 128, 96, 312, 365) # removing five observations
n<-(n-9)
comments.next24[out];order(comments.next24)[(n-10):n]


len<-length # change length to run boxcox
model.2<-lm(comments.next24[-out]+1~comments.prebasetime[-out]+comments.prev24[-out]+
              source.avg.prebasetime[-out]+source.avg.prev24[-out]+
              links.prebasetime[-out]+links.prev24[-out]+len[-out]+
              post.age[-out]+Basetime.day[-out]+Post.day[-out]+word1[-out]+word4[-out]+word5[-out]+
              word7[-out]+word8[-out]+word10[-out]+n.parents[-out]+parents.ave[-out]+parents.max[-out]+
              c.f.l.p[-out]+l.p.l.p24[-out]+l.p.w8[-out])
summary(model.2)

par(mfrow=c(2,2))
qqnorm(model.2$res,main="Before transformation"); qqline(model.2$res)
boxcox(model.2); title("Boxcox before transformation")

model.t<-lm((comments.next24[-out]+1)^-1~comments.prebasetime[-out]+comments.prev24[-out]+
              source.avg.prebasetime[-out]+source.avg.prev24[-out]+
              links.prebasetime[-out]+links.prev24[-out]+len[-out]+
              post.age[-out]+Basetime.day[-out]+Post.day[-out]+word1[-out]+word4[-out]+word5[-out]+
              word7[-out]+word8[-out]+word10[-out]+n.parents[-out]+parents.ave[-out]+parents.max[-out]+
              c.f.l.p[-out]+l.p.l.p24[-out]+l.p.w8[-out])
qqnorm(model.t$res,main="After transformation"); qqline(model.t$res)
boxcox(model.t); title("Boxcox after transformation")
title("Figure 6: qqnorm and boxcox",outer=TRUE)
layout(matrix(c(1,2,3,4),ncol=2))
plot(model.2$fitted,model.t$res,xlab="Fitted Values",ylab="Residuals",main="Residual plot"); abline(h=0,lty=2)
plot(model.t$fitted,model.t$res,xlab="Fitted Values",ylab="Residuals",main="Residual plot (transform)"); abline(h=0,lty=2)
plot(model.2$res,pch=16,ylab="Residuals",main="Independence"); abline(h=0, lty=2)
plot(model.t$res,pch=16,ylab="Residuals",main="Independence (transform)"); abline(h=0, lty=2)
title("Figure 7: Residual plots and independence tests",outer=TRUE)
layout(matrix(c(1,2,3,4,5,6,7,8),ncol=4))
plot(word1[-out],model.2$res,ylab="Residuals",main="Residuals vs. word1"); abline(h=0, lty=2)
plot(word4[-out],model.2$res,ylab="Residuals",main="Residuals vs. word4"); abline(h=0, lty=2)
plot(word5[-out],model.2$res,ylab="Residuals",main="Residuals vs. word5"); abline(h=0, lty=2)
plot(word7[-out],model.2$res,ylab="Residuals",main="Residuals vs. word7"); abline(h=0, lty=2)
plot(word8[-out],model.2$res,ylab="Residuals",main="Residuals vs. word8"); abline(h=0, lty=2)
plot(word10[-out],model.2$res,ylab="Residuals",main="Residuals vs. word10"); abline(h=0, lty=2)
plot(Post.day[-out],model.2$res,ylab="Residuals",main="Residuals vs. Post.day"); abline(h=0, lty=2)
plot(Basetime.day[-out],model.2$res,ylab="Residuals",main="Residuals vs. Basetime.day"); abline(h=0, lty=2)
title("Figure 8: Residual plots for categorical variables",outer=TRUE)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),ncol=7))
plot(comments.prebasetime[-out],model.2$res,ylab="Residuals",main="Res vs. comments.prebasetime"); abline(h=0, lty=2)
plot(comments.prev24[-out],model.2$res,ylab="Residuals",main="Res vs. comments.prev24"); abline(h=0, lty=2)
plot(source.avg.prebasetime[-out],model.2$res,ylab="Residuals",,main="Res vs. source.avg.prebasetime"); abline(h=0, lty=2)
plot(source.avg.prev24[-out],model.2$res,ylab="Residuals",main="Res vs. source.avg.prev24"); abline(h=0, lty=2)
plot(links.prebasetime[-out],model.2$res,ylab="Residuals",main="Res vs. links.prebasetime"); abline(h=0, lty=2)
plot(links.prev24[-out],model.2$res,ylab="Residuals",main="Res vs. links.prev24"); abline(h=0, lty=2)
plot(len[-out],model.2$res,ylab="Residuals",,main="Res vs. length"); abline(h=0, lty=2)
plot(n.parents[-out],model.2$res,ylab="Residuals",main="Res vs. n.parents"); abline(h=0, lty=2)
plot(parents.ave[-out],model.2$res,ylab="Residuals",main="Res vs. parents.ave"); abline(h=0, lty=2)
plot(parents.max[-out],model.2$res,ylab="Residuals",main="Res vs. parents.max"); abline(h=0, lty=2)
plot(c.f.l.p[-out],model.2$res,ylab="Residuals",main="Res vs. comments.first24*links.prebasetime"); abline(h=0, lty=2)
plot(l.p.l.p24[-out],model.2$res,ylab="Residuals",main="Res vs. links.prebasetime*links.prev24"); abline(h=0, lty=2)
plot(l.p.w8[-out],model.2$res,ylab="Residuals",main="Res vs. links.prebasetime*word8"); abline(h=0, lty=2)
title("Figure 9: Residual plots for continous variables",outer=TRUE)

# Model Selection
null<-lm(comments.next24[-out]~1)
full<-model.2
model.b<-step(model.2,scope=list(lower=null,upper=model.2),method="backward")
summary(model.b)

summary(full)
aov(lm(comments.next24[-out]~comments.prebasetime[-out]+comments.prev24[-out]+
         source.avg.prebasetime[-out]+source.avg.prev24[-out]+
         post.age[-out]+word1[-out]+
         word7[-out]+c.f.l.p[-out]+l.p.w8[-out]+n.parents[-out]+
         l.p.l.p24[-out]+parents.max[-out]+parents.ave[-out]+word4[-out]+word5[-out]
        +word8[-out]+word10[-out]+Basetime.day[-out]+Post.day[-out]+len[-out]+links.prev24[-out]+links.prebasetime[-out]))
n.parents.ssr<-252.15
l.p.l.p24.ssr<-242.06
parents.max.ssr<-89.01
parents.ave.ssr<-88.81
word4.ssr<-239.36
word5.ssr<-160.71
word8.ssr<-484.75
word10.ssr<-723.04
Basetime.day.ssr<-125.92
Post.day.ssr<-252.26
len.ssr<-526.53
links.prev24.ssr<-700.41
links.prebasetime.ssr<-1775.5
sse<-224004.21

f.stat.num<-(n.parents.ssr+l.p.l.p24.ssr+parents.max.ssr+parents.ave.ssr+
               word4.ssr+word5.ssr+word8.ssr+word10.ssr+Basetime.day.ssr
               +Post.day.ssr+len.ssr+links.prev24.ssr+links.prebasetime.ssr)/13
f.stat.den<-sse/(n-p)
f.stat<-f.stat.num/f.stat.den
f.stat > qf(.95,df1=13,df2=n-p)
# FALSE - we can exclude these variables from the model

model.final<-lm(comments.next24[-out]~comments.prebasetime[-out]+comments.prev24[-out]+
                  source.avg.prebasetime[-out]+source.avg.prev24[-out]+
                  post.age[-out]+word1[-out]+
                  word7[-out]+c.f.l.p[-out]+l.p.w8[-out])
summary(model.final)