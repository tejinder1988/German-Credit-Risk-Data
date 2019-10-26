library(tidyr)
library(tidyverse)
library(ggpubr)


cosetwd("C:/Users/Teji/Desktop/Project/German-Credit-Risk-Data")
data<-read.csv("german_credit_data.csv")

### Drop off the NA

data<-data[!is.na(data$Saving.accounts)==TRUE&!is.na(data$Checking.account)==TRUE,]
### Drop off the Column "X"
data<-data[,-1]
esquisse::esquisser()

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

subset(data,data$Sex=="female"&data$Purpose=="vacation/others")


# Decision tree classifier

