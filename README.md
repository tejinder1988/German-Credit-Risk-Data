# German-Credit-Risk-Data

```{r}
library(tidyr)
library(tidyverse)
library(ggpubr)
library(corrplot)
library(randomForest)
library(cluster)
library(fpc)
```
```{r}
ggplot(data=data)+aes(x=Age)+geom_histogram(bins=45,fill="#6baed6")+
  labs(x="Age",y="Count")+
  theme_gray()
```

```{r}
ggplot(data=data)+aes(x=Purpose)+geom_bar(fill="#6baed6")+
  labs(x="Purpose",y="Count")+
  theme_gray()+
  theme(axis.text.x=element_text(size=rel(0.9)))
```

```{r}
ggplot(data=data,aes(x=Credit.amount,y=Age,col=Sex))+geom_point()+
  labs(y="Age",x="Credit amount")+
  theme_gray()
```
  
```{r}
ggplot(data=data,aes(x=Credit.amount,y=Duration,col=Sex))+geom_point()+
  labs(y="Duration",x="Credit amount")+
  theme_gray()
```
  
```{r}
ggplot(data=data)+
  aes(x=Purpose,y=Age,fill=Sex)+
  geom_boxplot()+
  theme_grey()+
  theme(axis.text.x=element_text(size=rel(0.9)))
```
