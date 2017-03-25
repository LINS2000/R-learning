

#讀取匯率檔
x <- read.table('fx_rate-01.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)
x <- read.table('fx_rate-02.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)
head(x)
str(x)
summary(x)
x$tran_date <- as.Date(x$tran_date)
x$datetime <- format(x$tran_date,format='%Y/%m/%d %H:%M:%S')  #格式化日期
x.factor <- levels(factor(x$curr_code,ordered=TRUE))    #群組--幣別

#install.packages("tidyr")   #安裝
library(tidyr)

w <- spread(x,curr_code,std_rate)    #長資料轉寬資料 !!!重要!!!
head(w)
class(w)
str(w)

#install.packages("zoo")   #安裝
library(zoo)
z <- w[, c(2:7)]
head(z)
class(z)
z <- read.zoo(z, format="%Y/%m/%d %H:%M:%S", FUN=as.POSIXct)

# 繪圖
plot(z, main="2015~2016 Exchage", xlab="Date")

index(z)




plot(scale(w[c(3:3)]),type='l')
lines(scale(w[c(4:4)]),col='red')

aud_scale <- scale(w[c(3:3)])
class(aud_scale)

aud_scale2<-aud_scale[order(aud_scale[,1]),]
plot(as.matrix(aud_scale2),type='l')
class(aud_scale2)

plot(w$USD,type='l',ylim=c(20,40))
lines(w$AUD,col='red')

index(x)

usd <- x[which(x$curr_code=='USD'),]   #篩選USD
aud <- x[which(x$curr_code=='AUD'),]   #篩選AUD
summary(usd)
(usd<-usd[,c(4,3)])
(aud<-aud[,c(4,3)])
class(usd)

plot(usd,type='l',col='Black')
plot(aud,type='l',col='Red')
lines(aud$std_rate)
grid()

plot(x$std_rate, type='l')
#plot(x[,3]), type='l')

x[order('tdate')]



x1 <- x$std_rate
mean(x1)

write.table(x,'a.csv',sep=',',row.names=FALSE)

save(x,file='aa.rdata')
load(file='aa.rdata')
