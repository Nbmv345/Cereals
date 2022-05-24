Cereals <- read.csv("Cereals.csv")

#inciso a)
head(Cereals)

#inciso b)
CSUM<-data.frame(mean=sapply(Cereals[,4:16],mean,na.rm=TRUE)
                ,median=sapply(Cereals[,4:16],median,na.rm=TRUE)
                ,min=sapply(Cereals[,4:16],min,na.rm=TRUE)
                ,max=sapply(Cereals[,4:16], max,na.rm=TRUE)
                ,sd=sapply(Cereals[,4:16], sd,na.rm=TRUE))
colnames(CSUM)=c("media","Mediana","Min","Max","Desviacion Estandar")
CSUM

#inciso c)
library(tidyr)
library (ggplot2)

Cereals %>% gather() %>% head()
ggplot(gather(Cereals[,4:16]),aes(value))+
geom_histogram(bins=10)+facet_wrap(~key,scales="free_x")

#inciso d)
boxplot(calories~type,data=Cereals,main="Cereales Cold VS Hot",
        xlab="Tipos de cereales",ylab="numero de calorías",
        col="blue",medcol="red",boxlty=0,border="black",
        whisklty=1,staplelwd=4,outpch=13,outcex=1,outcol="green")

#inciso e
boxplot(rating~shelf,data=Cereals,main="Calificación del cliente",
        xlab="Ratings",ylab="Shelf",horizontal=TRUE, 
        col="blue",medcol="red",boxlty=0,border="black",
        whisklty=1,staplelwd=4,outpch=13,outcex=1,outcol="green")

#inciso f
#1
cmCereal=cor(Cereals[,4:16],use="complete.obs")
round(cmCereal,2)

#2
library(corrplot)
library(RColorBrewer)

corrplot(cmCereal,type="lower",main="Matriz de correlación",mar=c(0,0,1,0),tl.cex=0.8,tl.col="black", tl.srt=45,
         col=brewer.pal(n=8, name="PuOr"))

#3
library(caret)
normCereal=preProcess(Cereals[,4:16],method=c("center","scale"),na.rm=TRUE)
NormcmCereal=predict(normCereal,Cereals[,4:16])

cmCereal2=cor(NormcmCereal,use="complete.obs")
round(cmCereal2,2)

corrplot(cmCereal2,type="lower",main="Matriz de normalización",mar=c(0,0,1,0),tl.cex=0.8,tl.col="black", tl.srt=45,
         col=brewer.pal(n=8, name="PuOr"))
