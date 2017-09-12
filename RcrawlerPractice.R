
library(rvest)
html <- read_html("https://tw.movies.yahoo.com/")
catst <- html_nodes(html,"li > span")
#編碼問題
iconv(html_text(catst),"utf8")

