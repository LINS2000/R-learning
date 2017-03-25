


x <- read.table('fx_rate-01.csv',header=TRUE,sep=',')
str(x)
summary(x)
x$tdate <- as.Date(x$tran_date)
x$curr_code
x[1:2,1:3]
y <- x$curr_code='USD'
summary(x)
(y<-x[,c(4,3,2)])
class(y)

plot(y,type='l',col='Black')
grid()

plot(x$std_rate, type='l')
#plot(x[,3]), type='l')

x[order('tdate')]



x1 <- x$std_rate
mean(x1)

write.table(x,'a.csv',sep=',',row.names=FALSE)

save(x,file='aa.rdata')
load(file='aa.rdata')