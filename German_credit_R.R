library(tidyr)
library(tidyverse)
library(ggpubr)
library(corrplot)
library(randomForest)
library(cluster)
library(fpc)


data<-read.csv("german_credit_data.csv")

### Drop off the NA

data<-data[!is.na(data$Saving.accounts)==TRUE&!is.na(data$Checking.account)==TRUE,]
### Drop off the Column "X"
data<-data[,-1]

ggplot(data=data)+aes(x=Age)+geom_histogram(bins=45,fill="#6baed6")+
labs(x="Age",y="Count")+
theme_gray()

ggplot(data=data)+aes(x=Purpose)+geom_bar(fill="#6baed6")+
labs(x="Age",y="Count")+
theme_gray()+
theme(axis.text.x=element_text(size=rel(0.9)))


ggplot(data=data,aes(x=Credit.amount,y=Age,col=Sex))+geom_point()+
labs(y="Age",x="Credit amount")+
theme_gray()

ggplot(data=data,aes(x=Credit.amount,y=Duration,col=Sex))+geom_point()+
labs(y="Duration",x="Credit amount")+
theme_gray()

ggplot(data=data)+
aes(x=Purpose,y=Age,fill=Sex)+
geom_boxplot()+
theme_grey()+
theme(axis.text.x=element_text(size=rel(0.9)))


st1<-data%>%
  group_by(Purpose,Sex)%>%
  summarise(Average_Credit_amount=mean(Credit.amount),
            Average_duration=mean(Duration))
ggballoonplot(st1, x = "Sex", y = "Purpose", size = "Average_duration",fill = "Average_Credit_amount",
              ggtheme = theme_bw()) +gradient_fill(c("Grey", "Red"))

st2<-data%>%
  group_by(Saving.accounts,Checking.account,Sex)%>%
  summarise(Average_Credit_amount=mean(Credit.amount),
            Average_duration=mean(Duration))
ggballoonplot(st2, x = "Saving.accounts", y = "Checking.account", size = "Average_duration",fill = "Average_Credit_amount",facet.by = c("Sex"), 
              ggtheme = theme_bw()) +gradient_fill(c("Grey", "Red"))

st3<-data%>%
  group_by(Purpose,Age)%>%
  summarise(Average_Credit_amount=mean(Credit.amount),
            Average_duration=mean(Duration))
st3%>%ggplot(aes(x=Age, y=Average_duration, size=Average_Credit_amount, color=Purpose)) +
  geom_point(alpha=0.5) 

st4<-data%>%
  group_by(Sex,Job)%>%
  summarise(Average_Credit_amount=mean(Credit.amount),
            Average_duration=mean(Duration))
ggballoonplot(st4, x = "Sex", y = "Job", size = "Average_duration",fill = "Average_Credit_amount",
              ggtheme = theme_bw()) +gradient_fill(c("Grey", "Red"))

## Coreplot transformation
cor_data<-data%>%
  select(Age,Job,Credit.amount,Duration)
##
sex_male<-ifelse(data$Sex=="male",1,0)
sex_female<-ifelse(data$Sex=="female",1,0)

housing_free<-ifelse(data$Housing=="free",1,0)
housing_own<-ifelse(data$Housing=="own",1,0)
housing_rent<-ifelse(data$Housing=="rent",1,0)

Checking.account_little<-ifelse(data$Checking.account=="little",1,0)
Checking.account_moderate<-ifelse(data$Checking.account=="moderate",1,0)
Checking.account_rich<-ifelse(data$Checking.account=="rich",1,0)

Saving.accounts_little<-ifelse(data$Saving.accounts=="little",1,0)
Saving.accounts_moderate<-ifelse(data$Saving.accounts=="moderate",1,0)
Saving.accounts_quite_rich<-ifelse(data$Saving.accounts=="quite rich",1,0)
Saving.accounts_rich<-ifelse(data$Saving.accounts=="rich",1,0)

Purpose_business_business<-ifelse(data$Purpose=="business",1,0)
Purpose_business_car<-ifelse(data$Purpose=="car",1,0)
Purpose_business_domestic_appliances<-ifelse(data$Purpose=="domestic appliances",1,0)
Purpose_business_education<-ifelse(data$Purpose=="education",1,0)
Purpose_business_furniture_equipment<-ifelse(data$Purpose=="furniture/equipment",1,0)
Purpose_business_radio_TV<-ifelse(data$Purpose=="radio/TV",1,0)
Purpose_business_repairs<-ifelse(data$Purpose=="repairs",1,0)
Purpose_business_vacation_others<-ifelse(data$Purpose=="vacation/others",1,0)

cor_data<-cbind(cor_data,
                sex_male,sex_female,
                housing_free,
                housing_own,
                housing_rent,
                Checking.account_little,
                Checking.account_moderate,
                Checking.account_rich,
                Saving.accounts_little,
                Saving.accounts_moderate,
                Saving.accounts_quite_rich,
                Saving.accounts_rich,
                Purpose_business_business,
                Purpose_business_car,
                Purpose_business_domestic_appliances,
                Purpose_business_education,
                Purpose_business_furniture_equipment,
                Purpose_business_radio_TV,
                Purpose_business_repairs,
                Purpose_business_vacation_others)
##


corrplot(cor(cor_data),method = "color",addCoef.col="black",number.cex= 0.5,tl.cex = 0.5,type = "upper")

# Decision tree classifier
dt_data<-data%>%
  select(Age,Sex,Credit.amount)

tot_within_ss <- sapply(1:10, function(k){
  cl <- kmeans(dt_data[-2], k, nstart = 10)
  cl$tot.withinss
})
plot(1:10, tot_within_ss, type = "b")

cluster<-kmeans(data[,c(1,3)],2,nstart = 20)
clusplot(dt_data, cluster$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
##
dt2_data<-data
dt2_data<-lapply(dt2_data[,-2],as.numeric)
dt2_data<-data.frame(dt2_data,Sex=data$Sex)

tot_within_ss <- sapply(1:10, function(k){
  cl <- kmeans(dt2_data[-9], k, nstart = 10)
  cl$tot.withinss
})
plot(1:10, tot_within_ss, type = "b")
cluster2<-kmeans(dt2_data[-9],2,nstart=20)
clusplot(dt2_data, cluster2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

