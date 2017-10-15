library(shiny)
library(leaflet)
library(dplyr)
library(class)
library(randomForest)
etcdoor <- read.csv("C:/Users/Student/Desktop/Rwebserver/etcdoor1.csv",header = TRUE,stringsAsFactors = FALSE)
dataknn <- read.csv("C:/Users/Student/Desktop/Rwebserver/serverknnuse.csv",header = TRUE)
modelfroest <- readRDS("C:/Users/Student/Desktop/Rwebserver/modeltest.rds")
# channel <- read.csv("C:/Users/Student/Desktop/Rwebserver/channelall.csv",header = TRUE,stringsAsFactors = FALSE)
channel1 <- read.csv("C:/Users/Student/Desktop/Rwebserver/join.csv",header = TRUE,stringsAsFactors = FALSE)

str(channel1)

shinyApp(
  ui = fluidPage(
    leafletOutput('myMap'),
    selectInput("channel", label = h3(""),choices = unique(channel1$type)),
    selectInput("select", label = h3("起點"),choices = channel1$name),
    selectInput("out", label = h3("終點"),choices = channel1$name),
    tableOutput("aa"),
    dateInput("date", label = h3("Date input"), value = "2017-10-18",language = "zh-TW"),
    selectInput("selectminute", label = h3("Select box"), 
                choices = list("00:00" = "00:00", 
                               "00:30" = "00:30", 
                               "01:00" = "01:00", 
                               "01:30" = "01:30",
                               "02:00" = "02:00", 
                               "02:30" = "02:30",
                               "03:00" = "03:00", 
                               "03:30" = "03:30",
                               "04:00" = "04:00", 
                               "04:30" = "04:30",
                               "05:00" = "05:00", 
                               "05:30" = "05:30",
                               "06:00" = "06:00", 
                               "06:30" = "06:30",
                               "07:00" = "07:00", 
                               "07:30" = "07:30",
                               "08:00" = "08:00", 
                               "08:30" = "08:30",
                               "09:00" = "09:00", 
                               "09:30" = "09:30",
                               "10:00" = "10:00", 
                               "10:30" = "10:30",
                               "11:00" = "11:00", 
                               "11:30" = "11:30",
                               "12:00" = "12:00", 
                               "12:30" = "12:30",
                               "13:00" = "13:00", 
                               "13:30" = "13:30",
                               "14:00" = "14:00", 
                               "14:30" = "14:30",
                               "15:00" = "15:00", 
                               "15:30" = "15:30",
                               "16:00" = "16:00", 
                               "16:30" = "16:30",
                               "17:00" = "17:00", 
                               "17:30" = "17:30",
                               "18:00" = "18:00", 
                               "18:30" = "18:30",
                               "19:00" = "19:00", 
                               "19:30" = "19:30",
                               "20:00" = "20:00", 
                               "20:30" = "20:30",
                               "21:00" = "21:00", 
                               "21:30" = "21:30",
                               "22:00" = "22:00", 
                               "22:30" = "22:30",
                               "23:00" = "23:00", 
                               "23:30" = "23:30" ),selected = 1)
    ),
  server = function(input, output,session) {
    

  
  randomforestdata <- renderTable(pred <- local({
    
    etc <- local({ 
      etcdoor[grep("01F",etcdoor$ID),c(1,4)] %>%
        mutate(mile = substring(ID,4,7))  %>% 
        select(ID,mile)
    })
    etc$mile <- as.numeric(etc$mile) / 10
    
    stationselect <- local({ 
      if(channel1[channel1$name==input$select,"mile"]> channel1[channel1$name==input$out,"mile"]){
        judge <- channel1[channel1$name==input$select,"mile"] > etc$mile
        judge1 <- channel1[channel1$name==input$out,"mile"] < etc$mile
      }else{
        judge <- channel1[channel1$name==input$select,"mile"] < etc$mile
        judge1 <- channel1[channel1$name==input$out,"mile"] > etc$mile
      }
    
    stationselect <- as.data.frame(etc[judge & judge1,1])
    names(stationselect) <- c("station")
    stationselect$station <- as.character(stationselect$station)
    stationselect
    })
    transformstation <- local({
    #各站轉換
    tranend <- local({
      tranend <- c(1:161)
      names(tranend) <- etcdoor$ID
      tranend
    })
    transformstation <- tranend[stationselect$station]
    transformstation <- as.data.frame(transformstation)
    names(transformstation) <- c("route")
    transformstation
    })
    #knn  
    outputdata <-  local({
    
    train <- dataknn[,-5]
    train.ans <- dataknn[,5]
    trandate <- local({
      trandate <- c(1,2,3,4,5,6,7) 
      names(trandate) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
      trandate
    })
    dayget <- as.data.frame(trandate[weekdays(as.Date(input$date))])
    names(dayget) <- c("day")
    test <- data.frame(date = dayget$day,
                       hour = substring(input$selectminute,1,2),
                       minute = substring(input$selectminute,4,5),
                       end = transformstation$route)
    outputdata <- knn(train = train ,test = test ,cl = train.ans ,k = 1)
    outputdata <- as.character.default(outputdata)
    outputdata <- as.integer(outputdata)
    outputdata <- as.data.frame(outputdata)
    names(outputdata) <- c("number")
    outputdata
    })
    #randomforest
    dataOringin <- data.frame(end = stationselect$station) 
    data <- local({ dataOringin %>%
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
                            "F880N" = as.character((dataOringin$end=="01F0880N")))})
    trans <- local({
      trans <- c("1","0")
      names(trans) <- c(TRUE,FALSE)
      trans
    })
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
    data1 <- data.frame(hour = substring(input$selectminute,1,2),
                        miniute = substring(input$selectminute,4,5),
                        date = weekdays(as.Date(input$date)))
    data <- cbind(data[,-1],data1)
    data <- cbind(outputdata,data)
    #轉換成FACTOR 並加入新的levels
    for(i in c(2:20)){
      data[[i]] <- as.factor(data[[i]])
    }
    for(i in c(2:17)){
      data[,i] <- factor(x = data[,i], levels = c("0","1"))
    }
    data$hour <- factor(x = data$hour, levels = c("00","01","02","03","04","05","06","07","08"
                           ,"09","10","11","12","13","14","15","16","17"
                           ,"18","19","20","21","22","23"))
    data$miniute <- factor(x = data$miniute ,levels =  c("00","30"))
    data$date <- factor(x = data$date,
                        levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
     
     pred <- predict(object = modelfroest,data)
     sum(pred)
    }))

    output$aa <- randomforestdata
    observe({
      updateSelectInput(session,"select",
                        choices = channel1[channel1$type==input$channel,"name"])
      updateSelectInput(session,"out",
                        choices = channel1[channel1$type==input$channel,"name"])
    }) 
}
)  

