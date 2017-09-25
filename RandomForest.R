library(dplyr)
library(reshape2)
library(randomForest)
library(ggplot2)
dataOringin <- read.csv("D:/vargrant/project/raw312015-16.csv",header = FALSE)
#給欄位名稱
names(dataOringin) <- c("RecordT","start","end","type","time","number")
#建立國定假日向量
holidayc <- c("2015/01/01","2015/01/02","2015/01/03","2015/01/04","2015/02/18","2015/02/19",
              "2015/02/20","2015/02/21","2015/02/22","2015/02/23","2015/02/27","2015/02/28",
              "2015/03/01","2015/04/03","2015/04/04","2015/04/05","2015/04/06","2015/05/01",
              "2015/05/02","2015/05/03","2015/06/19","2015/06/20","2015/06/21","2015/09/26",
              "2015/09/27","2015/09/28","2015/10/09","2015/10/10","2015/10/11",
              "2016/01/01","2016/01/02","2016/01/03","2016/02/06","2016/02/07","2016/02/08",
              "2016/02/09","2016/02/10","2016/02/11","2016/02/12","2016/02/13","2016/02/14",
              "2016/02/27","2016/02/28","2016/02/29","2016/04/02","2016/04/03","2016/04/04",
              "2016/04/05","2016/06/09","2016/06/10","2016/06/11","2016/06/12","2016/09/15",
              "2016/09/16","2016/09/17","2016/09/18","2016/10/09","2016/10/10","2016/10/11")

#新增站名欄位
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
           "F880N" = as.character((dataOringin$end=="01F0880N")))
})
#轉換模式
trans <- local({
  trans <- c("1","0")
  names(trans) <- c(TRUE,FALSE)
  trans
})
#station <- c("F233N","F256N","F293N","F339N","F376N","F413N","F467N","F509N","F532N","F557N","F584N","F633N","F664N","F681N","F750N","F880N","F928N")
#轉換成1 0

data$F233N <- trans[data$F233N]
data$F256N <- trans[data$F256N]
data$F293N <- trans[data$F293N]
data$F339N <- trans[data$F339N]
data$F376N <- trans[data$F376N]
data$F413N <- trans[data$F413N]
data$F467N <- trans[data$F467N]
data$F509N <- trans[data$F509N]
data$F532N <- trans[data$F532N]
data$F557N <- trans[data$F557N]
data$F584N <- trans[data$F584N]
data$F633N <- trans[data$F633N]
data$F664N <- trans[data$F664N]
data$F681N <- trans[data$F681N]
data$F750N <- trans[data$F750N]
data$F880N <- trans[data$F880N]


data1 <- local(
  data %>%
    mutate(hour = substring(data$RecordT,12,13),
           date=substring(data$RecordT,1,10),
           holiday=ifelse(date %in% holidayc,1,0)) %>%
    select(-RecordT,-start,-end,-type) %>%
    arrange(date)
)

#日期轉換星期
data1$date <- as.Date(data1$date)
data1$date <- weekdays(data1$date)

#chr轉換成factor
for(i in c(3:21)){
data1[[i]]<-as.factor(data1[[i]])
}
data$
#檢查
str(dataOringin)
unique(data1$date)
#匯出
write.csv(x=data1,file = "D:/vargrant/project/monday0925.csv")

#切割訓練資料與測試資料
smap <- sample(x = 1:nrow(data1),size = nrow(data1)*0.02)
train <- data1[smap,]
test <- data1[-smap,-1]
testans <- data1[-smap,1]
#建模
modelrandomfroest <- randomForest(time~.-time,data = train,mtry=3,importance=TRUE, na.action=na.omit)

#帶入測試資料
pred <- predict(object = modelrandomfroest,test)
pred <- as.data.frame(pred)
testans <- as.data.frame(testans)
#測試結果與測試資料答案合併
table <- cbind(testans,pred)
table <- local({
  table %>%
    mutate(differ = round(abs(pred -testans),2))
})
nrow(table)
View(table)
nrow(table[table$differ > 600,])
View(dataOringin[table$differ>1000,c("RecordT","time")])



