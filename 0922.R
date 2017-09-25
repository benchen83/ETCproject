library(dplyr)
library(reshape2)
library(randomForest)
library(ggplot2)
dataOringin <- read.csv("D:/vargrant/project/m1.csv",header = TRUE)

dataOringin<-mutate(dataOringin,date=weekdays(as.Date(dataOringin$RecordT)))

head(data)
data <- local({
  dataOringin %>%
    mutate("F233N" = as.character((dataOringin$end=="01F0233N")),
           "F256N" = as.character((dataOringin$end=="01F0256N")),
           "F293N" = as.character((dataOringin$end=="01F0293N")),
           "F339N" = as.character((dataOringin$end=="01F0339N")),
           "F376N" = as.character((dataOringin$end=="01F0376N")),
           "F413N" = as.character((dataOringin$end=="01F0413N")),
           "F467N" = as.character((dataOringin$end=="01F0467N")),
           "F509N" = as.character((dataOringin$end=="01F0509N")),
           "F532N" = as.character((dataOringin$end=="01F0532N")),
           "F557N" = as.character((dataOringin$end=="01F0557N")),
           "F584N" = as.character((dataOringin$end=="01F0584N")),
           "F633N" = as.character((dataOringin$end=="01F0633N")),
           "F664N" = as.character((dataOringin$end=="01F0664N")),
           "F681N" = as.character((dataOringin$end=="01F0681N")),
           "F750N" = as.character((dataOringin$end=="01F0750N")),
           "F880N" = as.character((dataOringin$end=="01F0880N")),
           "F928N" = as.character((dataOringin$end=="01F0928N")))
})
#建立轉換模式
trans<-local({
  trans<-c("1","0")
  names(trans)<-c(TRUE,FALSE)
  trans
})

#轉換成1 0
data$F256N<-trans[data$F256N]
data$F928N<-trans[data$F928N]


data1<-local(
  data %>%
    mutate(hour = substring(data$RecordT,12,13) ,date=substring(data$RecordT,1,10)) %>%
    select(-X,-RecordT,-start,-end,-type)
)
#日期轉換星期
data1$date<-as.Date(data1$date)
data1$date<-weekdays(data1$date)
#chr轉換成factor
for(i in c(3:21)){
data1[[i]]<-as.factor(data1[[i]])
}
write.csv(x=data1,file = "D:/vargrant/project/friday0922.csv")


smap <- sample(x = 1:nrow(data1),size = nrow(data1)*0.01)
train <- data1[smap,]
test <- data1[-smap,-1]
testans <- data1[-smap,1]
a <- randomForest(time~.-time,data = train,mtry=3,importance=TRUE, na.action=na.omit)

store <- predict(object = a,test)
store <- as.data.frame(store)
testans <- as.data.frame(testans)
table <- cbind(testans,store)
table <- local({
  table %>%
    mutate(differ = round(abs(store -testans),2))
})
nrow(table)
nrow(table[table$differ > 1000,])
View(dataOringin[table$differ>1000,c("RecordT","time","date")])

dataOringin[dataOringin$RecordT ==""]

g<-ggplot(data=data1[data1$dateM == "2017/02/27",],aes(x=number,y=time))
g+geom_point(aes(color=hour))
