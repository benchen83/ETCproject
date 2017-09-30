library(dplyr)
library(ggplot2)
library(class)
dataknn <- read.csv("D:/vargrant/project/try.csv",header = TRUE)
dataknn <- dataknn[,-1]
head(dataknn)

# dataknn1 <- local({
#   dataknn %>%
#     group_by(date,hour,miniute,F233N,F256N,F293N,F339N,F376N,F413N,F467N,F509N,F532N,F557N,F584N,F633N,F664N,F681N,F750N,F880N) %>%
#     summarise(meannumber=mean(number))
# }) 
tranend <- local({
  tranend <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  names(tranend) <- c("F233N","F256N","F293N","F339N","F376N","F413N","F467N","F509N","F532N","F557N","F584N","F633N","F664N","F681N","F750N","F880N")
  tranend
  })

trandate <- local({
    trandate <- c(1,2,3,4,5,6,7) 
    names(trandate) <- c("星期一","星期二","星期三","星期四","星期五","星期六","星期日")
    trandate
    })
#轉換日期
dataknn$date <- trandate[dataknn$date] 
dataknn$end <- tranend[dataknn$end]

dataknn1 <- local({
  dataknn %>% 
    group_by(date,hour,miniute,end)%>%
    summarise(meannumber=mean(number))
})
dataknn1 <-as.data.frame(dataknn1)
write.csv(x = dataknn1,file ="D:/vargrant/project/serverknnuse.csv")
train <- dataknn1[,-5]
train.ans <- dataknn1[,5]


test <- data.frame(date=integer(),hour=integer(),minute=integer(),end=integer())
test1 <- data.frame(date=1,hour=1,minute=0,end=1)
test <- rbind(test,test1)
#建模
output <- knn1(train = train ,test = test ,cl = train.ans )
as.numeric(output)
class(output)
output <- as.data.frame(output)

dataknn <- read.csv("D:/vargrant/project/serverknnuse.csv",header = TRUE)
train <- dataknn[,-c(1,6)]
train.ans <- dataknn[,6]
