# Individual analysis
str(test_data)
new_date<-strptime(test_data$date, format="%d/%m/%Y")
test_data$date<-as.Date(new_date, format="%y/%m/%d")
str(test_data)

# Correlation between temperature and property price
# leave only year and month
test_data$year_month<-format(as.Date(test_data$date), "%y-%m")

# mean value of temperature
month.avg<-aggregate(test_data$avg_temp, by=list(test_data$year_month), mean)
head(month.avg)

# mean value of price
price.avg<-aggregate(test_data$price, by=list(test_data$year_month), mean)
head(price.avg)

# square root price twice (because it's too big)
price.avg$x<-sqrt(price.avg$x)
head(price.avg)
price.avg$x<-sqrt(price.avg$x)
head(price.avg)

# merge two data frames
avg_temp_price<-merge(month.avg, price.avg, by=c("Group.1"))
colnames(avg_temp_price)<-c("date","avg_temp","avg_price")
head(avg_temp_price)

# create line plot
date1<-avg_temp_price$date
avg_temp1<-avg_temp_price$avg_temp
avg_price1<-avg_temp_price$avg_price
df1<-data.frame(x=date1, y=avg_temp1, z=avg_price1)
ggplot(df1, aes(x, group=1))+
  geom_line(size=1, aes(y=y, colour="temperature"))+
  geom_line(size=1, aes(y=z, colour="double square rooted property price"))+
  labs(x="Date",
       y="Average Value",
       title="Fluctuations of Monthly Average Temperature and Property Price by Date")

# simple regression model - 1
plot(avg_price~avg_temp, data=avg_temp_price)
model1<-lm(avg_price~avg_temp, avg_temp_price)
model1
plot(avg_price~avg_temp, data=avg_temp_price, xlab="Temperature", ylab="Double SQRT Property Price",
     main="Correlation between Temperature and Property Price")
abline(coef(model1))

# predict property price
avg_temp=15
avg_price=(-0.0208)*avg_temp+32.3744
print(avg_price)
print(avg_price*avg_price*avg_price*avg_price)

# compare actual and predicted property price
avg_temp<-avg_temp_price[,2]
avg_temp
pred<-(-0.0208)*avg_temp+32.3744
pred<-(pred*pred*pred*pred)
pred
compare<-cbind(pred, avg_temp_price[,3], abs(pred-avg_temp_price[,3]))
compare
colnames(compare)<-c("pred","temp","actual")
compare
