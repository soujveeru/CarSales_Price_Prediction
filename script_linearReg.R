library(readr)
CarSales_original <- read_csv("C:/Users/11200745/Desktop/Manufacturing/CarSales_original.csv")
#View(CarSales_original)
nrow(CarSales_original)
#1 observation lost: 371539 remaining
#Create a copy
CarSales<-CarSales_original
#Price outlier on high side
CarSales1<- CarSales[which(CarSales$price!=2147483647), ]
nrow(CarSales1)
#2 observations removed; 371537 remaining
#cross verify
nrow(CarSales[which(CarSales$price==2147483647), ])
#1 observation; 1 number not matching
#Missing value treatment
CarSales<-na.omit(CarSales1)
nrow(CarSales)
#299873 obs
#Price outliers when price=o
CarSales<-CarSales[which(CarSales$price!=0), ]
nrow(CarSales)
#294829 obs remaining
#Re-Coding variables


CarSales$gearbox_n <- NA

CarSales$gearbox_n[CarSales$gearbox  == "automatik"]   <- 1
CarSales$gearbox_n[CarSales$gearbox  == "manuell"]   <- 2 

table(CarSales$gearbox_n)
table(CarSales$fuelType)

CarSales$fuelType_n <-NA
CarSales$fuelType_n[CarSales$fuelType  == "elektro"]   <- 1
CarSales$fuelType_n[CarSales$fuelType  == "andere"]   <- 2 
CarSales$fuelType_n[CarSales$fuelType  == "hybrid"]   <- 3
CarSales$fuelType_n[CarSales$fuelType  == "cng"]   <- 4 
CarSales$fuelType_n[CarSales$fuelType  == "lpg"]   <- 5
CarSales$fuelType_n[CarSales$fuelType  == "diesel"]   <- 6 
CarSales$fuelType_n[CarSales$fuelType  == "benzin"]   <- 7

table(CarSales$fuelType_n)
table(CarSales$vehicleType)

CarSales$vehicleType_n <-NA
CarSales$vehicleType_n[CarSales$vehicleType  == "andere"]   <- 1
CarSales$vehicleType_n[CarSales$vehicleType  == "suv"]   <- 2 
CarSales$vehicleType_n[CarSales$vehicleType  == "coupe"]   <- 3
CarSales$vehicleType_n[CarSales$vehicleType  == "cabrio"]   <- 4 
CarSales$vehicleType_n[CarSales$vehicleType  == "bus"]   <- 5
CarSales$vehicleType_n[CarSales$vehicleType  == "kombi"]   <- 6 
CarSales$vehicleType_n[CarSales$vehicleType  == "kleinwagen"]   <- 7
CarSales$vehicleType_n[CarSales$vehicleType  == "limousine"]   <- 8 

table(CarSales$vehicleType_n)
nrow(CarSales)
a<-as.data.frame(table(CarSales$brand))
#View(a)
b<-a[order(a$Freq), ]
b

CarSales$brand_n<-NA
CarSales$brand_n[CarSales$brand == "lada"] <- 1
CarSales$brand_n[CarSales$brand == "trabant"] <- 2
CarSales$brand_n[CarSales$brand == "rover"] <- 3
CarSales$brand_n[CarSales$brand == "lancia"] <- 4
CarSales$brand_n[CarSales$brand == "daewoo"] <- 5
CarSales$brand_n[CarSales$brand == "saab"] <- 6
CarSales$brand_n[CarSales$brand == "jaguar"] <- 7
CarSales$brand_n[CarSales$brand == "daihatsu"] <- 8
CarSales$brand_n[CarSales$brand == "subaru"] <- 9
CarSales$brand_n[CarSales$brand == "jeep"] <- 10
CarSales$brand_n[CarSales$brand == "land_rover"] <- 11
CarSales$brand_n[CarSales$brand == "dacia"] <- 12
CarSales$brand_n[CarSales$brand == "chrysler"] <- 13
CarSales$brand_n[CarSales$brand == "chevrolet"] <- 14
CarSales$brand_n[CarSales$brand == "suzuki"] <- 15
CarSales$brand_n[CarSales$brand == "alfa_romeo"] <- 16
CarSales$brand_n[CarSales$brand == "porsche"] <- 17
CarSales$brand_n[CarSales$brand == "kia"] <- 18
CarSales$brand_n[CarSales$brand == "honda"] <- 19
CarSales$brand_n[CarSales$brand == "mitsubishi"] <- 20
CarSales$brand_n[CarSales$brand == "volvo"] <- 21
CarSales$brand_n[CarSales$brand == "mini"] <- 22
CarSales$brand_n[CarSales$brand == "hyundai"] <- 23
CarSales$brand_n[CarSales$brand == "smart"] <- 24
CarSales$brand_n[CarSales$brand == "toyota"] <- 25
CarSales$brand_n[CarSales$brand == "nissan"] <- 26
CarSales$brand_n[CarSales$brand == "citroen"] <- 27
CarSales$brand_n[CarSales$brand == "mazda"] <- 28
CarSales$brand_n[CarSales$brand == "skoda"] <- 29
CarSales$brand_n[CarSales$brand == "seat"] <- 30
CarSales$brand_n[CarSales$brand == "fiat"] <- 31
CarSales$brand_n[CarSales$brand == "peugeot"] <- 32
CarSales$brand_n[CarSales$brand == "renault"] <- 33
CarSales$brand_n[CarSales$brand == "ford"] <- 34
CarSales$brand_n[CarSales$brand == "audi"] <- 35
CarSales$brand_n[CarSales$brand == "mercedes_benz"] <- 36
CarSales$brand_n[CarSales$brand == "opel"] <- 37
CarSales$brand_n[CarSales$brand == "bmw"] <- 38
CarSales$brand_n[CarSales$brand == "volkswagen"] <- 39


table(CarSales$brand_n)
nrow(CarSales)
#294829 final obs

names(CarSales)
#Drop unnecessary columns
model1_ds<-CarSales[ , c("price","vehicleType_n", "yearOfRegistration","gearbox_n","powerPS","model","brand_n","kilometer","fuelType_n","postalCode")]
#View(model1_ds)
sum(is.na(model1_ds))





m_train1<-model1_ds
m_train1$pricecat<-NA
m_train1$pricecat[m_train1$price>100000]<-"high"
m_train1$pricecat[(m_train1$price<100000) & (m_train1$price>999)]<-"medium"
m_train1$pricecat[m_train1$price<999]<-"low"

m_t<-m_train1[which(m_train1$pricecat == "medium"), ]

nrow(m_t)
m_t$pcode<-NA
m_t$pcode[substr(m_t$postalCode,1,1)==1]<-1
m_t$pcode[substr(m_t$postalCode,1,1)==2]<-2
m_t$pcode[substr(m_t$postalCode,1,1)==3]<-3
m_t$pcode[substr(m_t$postalCode,1,1)==4]<-4
m_t$pcode[substr(m_t$postalCode,1,1)==5]<-5
m_t$pcode[substr(m_t$postalCode,1,1)==6]<-6
m_t$pcode[substr(m_t$postalCode,1,1)==7]<-7
m_t$pcode[substr(m_t$postalCode,1,1)==8]<-8
m_t$pcode[substr(m_t$postalCode,1,1)==9]<-9
table(m_t$pcode)

m_t<-m_t[which(m_t$yearOfRegistration<=2012), ]


nrow(m_t)

0.8*232264
model1_ds_train<-m_t[1:185811, ]
model1_ds_test<-m_t[185812:232264, ]
nrow(model1_ds_train)
nrow(model1_ds_test)



summary(model1_ds_train$price)
par(mfrow=c(2,2)) 
hist(model1_ds_train$price)

hist(model1_ds_train$price,breaks=15,col="red",xlab="price",main="Colored histogram with 15 bins")


hist(model1_ds_train$price,freq=FALSE,breaks=12,col="red", xlab="Price",main="Histogram,rug_plot,densitycurve") 
rug(jitter(model1_ds_train$price)) 
lines(density(model1_ds_train$price), col="blue",lwd=2) 

x <- model1_ds_train$price 
h<-hist(x,breaks=12,col="red",xlab="Price",main="Histogram with normal curve and box") 
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2) 
box() 

#KERNEL DENSITY PLOTS
plot(density(x)) 

par(mfrow=c(2,1)) 
d <- density(model1_ds_train$price)       
plot(d)                         
d <- density(model1_ds_train$price)
plot(d, main="Kernel Density of Price")
polygon(d, col="red", border="blue")
rug(CarSales_outlier$x, col="brown")


#quantile plot of price

qqnorm(model1_ds_train$price)
qqline (model1_ds_train$price, col=1)

#Anderson Darling test
install.packages("nortest")
library(nortest)
ad.test(model1_ds_train$price)
#data is non normal

#Shapiro Wilk test
#sample size has to be between 3 and 5000
sw<-model1_ds_train[sample(nrow(model1_ds_train), 4999), ]
shapiro.test(sw$price)
#data is non normal


#dropping x: none of the x can be dropped
kruskal.test(price ~ vehicleType_n, data = model1_ds_train)
kruskal.test(price ~ fuelType_n, data = model1_ds_train)
kruskal.test(price ~ brand_n, data = model1_ds_train)
kruskal.test(price ~ gearbox_n, data = model1_ds_train)



#box cox
library(MASS)
m3<-lm(price~kilometer+powerPS+yearOfRegistration+vehicleType_n+gearbox_n+fuelType_n+brand_n+model+pcode, model1_ds_train)                            
summary(m3)

boxcox(m3)
#refine boxcox
boxcox(m3, lambda = seq(-0.5, 0.2, 0.1))

par(mfrow=c(2,1)) 
y<-log(model1_ds_train$price)
hist(y)

qqnorm(y)
qqline (y, col=1)


#adding log data
names(model1_ds_train)

Yprime<-log(model1_ds_train$price)
model1_ds_train<-cbind(model1_ds_train, Yprime)
names(model1_ds_train)

#Linear Regression post boxcox
m4<-lm(Yprime~kilometer+yearOfRegistration+vehicleType_n+gearbox_n+fuelType_n+brand_n+model+pcode, model1_ds_train)                            
summary(m4)
a<-as.data.frame(m4$coefficients)
View(a)
b<-as.data.frame(m4$model)
View(b)
#Residuals plot
plot(m4$residuals)

pred1<-predict(m4,model1_ds_test)
plot(pred1)
plot(model1_ds_test$price,pred1)
price_predicted<-exp(pred1)
ds_after_pred1<-cbind(model1_ds_test,pred1, price_predicted)
View(ds_after_pred1)
nrow(ds_after_pred1)



write.csv(x = ds_after_pred1, file = "r.csv")
          
