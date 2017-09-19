library(rvest)
library(dplyr)

#先建立個空data.frame
dfurl<-data.frame(stringsAsFactors = FALSE)


for(i in 1:24){
hour<-c("00/","01/","02/","03/","04/","05/","06/","07/","08/","09/","10/","11/","12/","13/","14/","15/","16/","17/","18/","19/","20/","21/","22/","23/")

url="http://tisvcloud.freeway.gov.tw/history/TDCS/M04A/20170816/"
url=paste0(url,hour[i])
#找出網頁內放檔案超聯結的位置
html.page <- read_html(url)
route <- html_nodes(html.page,"tr > td[valign=top] >a")
target <- html_attrs(route)
target <- sapply(target,"[[",1)
target <- target[-1]

#完整路徑
download.url <- paste0(url,target)

#建立data.frame來儲存下載網址
dfurl<- rbind(dfurl,data.frame(url=download.url,stringsAsFactors = FALSE)) 
}

#新增欄位放置檔案要取的名稱
dfurl<- local({ 
  dfurl %>%
   mutate(name = substring(dfurl$url,75,91))        
    })

#下載檔案
for(i in c(1:4896)){
download.file(url=dfurl[i,1],dfurl[i,2],method = 'auto',mode = 'wb')}



