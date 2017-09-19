#讀資料
testone <- read.table("D:/Shared/M04A-mon.csv",header = FALSE ,sep = ",")
#給欄位名稱
names(testone) <- c("RecordT","start","end","type","time","number")
library(dplyr)
#建立想要的路段 並變成dataframe 
station <- c("01F0928S","01F0880S","01F0750S","01F0681S","01F0664S","01F0633S","01F0578S","01F0557S","01F0532S","01F0509S","01F0467S","01F0413S","01F0376S","01F0339S","01F0293S","01F0264S")
x <- data.frame(station)
names(x) <- "start"

#篩選變數end開頭為01 與 type為小客車
data1 <- local({
  testone[grep("^01",testone$end),] %>%
    filter(type=="31") 
})

#把資料與路段合併篩出 想要的路段資料 並篩選時間
inn <- inner_join(data1,x)
inn <- inn[grep("S$",inn$end),]
inn <- inn[grep("[2345].$",inn$RecordT),]
#篩出起始路段為xxx 並新增一個欄位只有時分
x <- local({
  inn%>%
    filter(start=="01F0264S") %>%
     arrange(start) %>%
      mutate(date=substring(RecordT,1,10),dateT=substring(RecordT,12,16)) %>% 
       select(date,dateT,time,number)
})

#pivot資料 把資料整理成模型可用的
library(reshape2)
xt1 <- dcast(x,date~dateT ,value.var = "time")
xt2 <- dcast(x,date~dateT,value.var = "number")
finalT <- left_join(x=xt1,y=xt2,by="date")

#正規化function
nor <- function(x){
  return((x-min(x) )/ (max(x)-min(x)))
}

#先把資料正規化
finaluse <- as.data.frame(lapply(finalT[,-1],nor))

#選出訓練資料 與 測試資料 並把答案分開
train.ans <-finaluse[c(1:49),c(16)]
train <-finaluse[c(1:49),-c(8,16)]
test <- finaluse[c(50:55),-c(8,16)]
test.ans <-finaluse[c(50:55),c(16)]
library(class)
y<-knn(train=train,test=test,cl = train.ans,k=1)
y



library(ggplot2)
g<-ggplot(x,aes(dateT,number))
g+geom_line(aes(color=date,group=date))
g+geom_bar(stat = "identity")


