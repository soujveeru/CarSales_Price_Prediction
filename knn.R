m_t$pricecategory<-NA
m_t$pricecategory[m_t$price>999 & m_t$price<=10000]<-1
m_t$pricecategory[m_t$price>10000 & m_t$price<=20000]<-2
m_t$pricecategory[m_t$price>20000 & m_t$price<=30000]<-3
m_t$pricecategory[m_t$price>30000 & m_t$price<=40000]<-4
m_t$pricecategory[m_t$price>40000 & m_t$price<=50000]<-5
m_t$pricecategory[m_t$price>50000 & m_t$price<=60000]<-6
m_t$pricecategory[m_t$price>60000 & m_t$price<=70000]<-7
m_t$pricecategory[m_t$price>70000 & m_t$price<=80000]<-8
m_t$pricecategory[m_t$price>80000 & m_t$price<=90000]<-9
m_t$pricecategory[m_t$price>90000 & m_t$price<=100000]<-10
m_t$pricecategory[m_t$price>100000]<-11

0.8*232264
model1_ds_train<-m_t[1:185811, ]
model1_ds_test<-m_t[185812:232264, ]
nrow(model1_ds_train)
nrow(model1_ds_test)

table(m_t$pricecategory)


sqrt(185811)
#knn
install.packages("class")
library(class)


names(model1_ds_train)
kn_train_labels <- model1_ds_train[ , 13]
kn_test_labels <- m_t[185812:232264, 13]

kn_train<-model1_ds_train[ , c(-1,-6,-10,-11)]
nrow(kn_train)
nrow(kn_train_labels)
ncol(kn_train_labels)
ncol(kn_train)
names(kn_train)
kn_test<-model1_ds_test[ , c(-1,-6,-10,-11)]

knn_outds<-knn(train= kn_train, test= kn_test,cl=kn_train_labels$pricecategory, k=100)

#Evaluating performance of knn
install.packages("gmodels")
library(gmodels)
Exhibit2<-CrossTable(x=kn_test_labels$pricecategory, y=knn_outds)
View(Exhibit2)
#*******************************************************************************************

m_t_small<-m_t[which(m_t$pricecategory == 1 | m_t$pricecategory==2), ]
nrow(m_t)
nrow(m_t_small)
m_t_small$pricecategory[m_t_small$price>999 & m_t_small$price<=2000]<-1
m_t_small$pricecategory[m_t_small$price>2000 & m_t_small$price<=3000]<-2
m_t_small$pricecategory[m_t_small$price>3000 & m_t_small$price<=4000]<-3
m_t_small$pricecategory[m_t_small$price>4000 & m_t_small$price<=5000]<-4
m_t_small$pricecategory[m_t_small$price>5000 & m_t_small$price<=6000]<-5
m_t_small$pricecategory[m_t_small$price>6000 & m_t_small$price<=7000]<-6
m_t_small$pricecategory[m_t_small$price>7000 & m_t_small$price<=8000]<-7
m_t_small$pricecategory[m_t_small$price>8000 & m_t_small$price<=9000]<-8
m_t_small$pricecategory[m_t_small$price>9000 & m_t_small$price<=10000]<-9
m_t_small$pricecategory[m_t_small$price>10000 & m_t_small$price<=11000]<-10
m_t_small$pricecategory[m_t_small$price>11000 & m_t_small$price<=12000]<-11
m_t_small$pricecategory[m_t_small$price>12000 & m_t_small$price<=13000]<-12
m_t_small$pricecategory[m_t_small$price>13000 & m_t_small$price<=14000]<-13
m_t_small$pricecategory[m_t_small$price>14000 & m_t_small$price<=15000]<-14
m_t_small$pricecategory[m_t_small$price>15000 & m_t_small$price<=16000]<-15
m_t_small$pricecategory[m_t_small$price>16000 & m_t_small$price<=17000]<-16
m_t_small$pricecategory[m_t_small$price>17000 & m_t_small$price<=18000]<-17
m_t_small$pricecategory[m_t_small$price>18000 & m_t_small$price<=19000]<-18
m_t_small$pricecategory[m_t_small$price>19000 & m_t_small$price<=20000]<-19

table(m_t_small$pricecategory)
nrow(m_t_small)
0.8*222638
model1_ds_train<-m_t[1:178110, ]
model1_ds_test<-m_t[178111:222638, ]
nrow(model1_ds_train)
nrow(model1_ds_test)

sqrt(185811)
#knn
install.packages("class")
library(class)


names(model1_ds_train)
kn_train_labels <- model1_ds_train[ , 13]
kn_test_labels <- m_t[178111:222638, 13]

kn_train<-model1_ds_train[ , c(-1,-6,-10,-11)]
nrow(kn_train)
nrow(kn_train_labels)
ncol(kn_train_labels)
ncol(kn_train)
names(kn_train)
kn_test<-model1_ds_test[ , c(-1,-6,-10,-11)]

knn_outds<-knn(train= kn_train, test= kn_test,cl=kn_train_labels$pricecategory, k=100)

#Evaluating performance of knn
install.packages("gmodels")
library(gmodels)
nrow(kn_test)
nrow(kn_test_labels)

Exhibit3<-CrossTable(x=kn_test_labels$pricecategory, y=knn_outds)
View(Exhibit3)
