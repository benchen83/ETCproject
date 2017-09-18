library(rvest)
url="http://tisvcloud.freeway.gov.tw/history/TDCS/M04A/20170917/"
url=paste0(url,"00/")

html.page <- read_html(url)
route <- html_nodes(html.page,"tr > td[valign=top] >a")

hour<-c(00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

target <- html_attrs(route)
target <- sapply(target,"[[",1)
target <- target[-1]

download.url <- paste0(url,target)

w <- data.frame(url=download.url,stringsAsFactors = FALSE) 
    
w["name"]<- local({substring(download.url,75,91)})


for(i in c(1:10)){
download.file(url=w[i,1],w[i,2],method = 'auto',mode = 'wb')}

download.file(url=w[1,1],destfile = substring(url,13,16),method = 'auto',mode = 'wb')

?download.file


seq(from=4,to = 26,by = 2)
View(w)
