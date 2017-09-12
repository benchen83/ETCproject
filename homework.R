library(class)
#讀資料
data = read.csv("")
head(data,5)
data = data[,-1]
head(data,5)
#把目標變數分開
data.input=data[,-ncol(data)]
#正規化
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
set.seed(107)
#每欄都正規化
data.input <- as.data.frame(lapply(data.input, normalize))
#分訓練資料和測試資料
dataselect=sample(1:nrow(data),nrow(data)*0.8)
train.data=data.input[dataselect,]
test.data=data.input[-dataselect,]
train.ydata=data[dataselect,ncol(data)]
test.ydata=data[-dataselect,ncol(data)]

ROC=data.frame()
#分別看當k為1到8時 測試資料的精確度
for(i in seq(from = 1, to = 8, by = 1)){
y_hat <- knn(train = train.data, test = test.data, cl=train.ydata, k=i)
accuracy.knn <- sum(y_hat==test.ydata)/length(test.ydata)
out <- data.frame(i,accuracy.knn)
ROC <- rbind(ROC,out)
}

names(ROC) <- c("n","accuracy")
ROC
num_k <- ROC[(ROC$accuracy == max(ROC$accuracy)),1]
best_k <- max(num_k)
best_k
