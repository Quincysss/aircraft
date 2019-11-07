library(mongolite)
library(shiny)
library(ggplot2)
library(ggmap)
library(sp)
library(maptools)
library(shinyWidgets)
library(dplyr)
library(rAmCharts)
library(lubridate)# date transfer
library(forecast) #predict data
allData = mongo(collection="mainData",db="FIT5141_Ass3_wanzi")
airAccidentDB <- allData$find()
blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)
  x[z==""] <- NA 
  return(x)
}# transfer the empty to NA,refer to https://codeday.me/bug/20190324/823548.html
airAccidentDB <-data.frame(sapply(airAccidentDB,blank2na))
NaairAccidentDB<-airAccidentDB[complete.cases(airAccidentDB[,2:16]),]
NaairAccidentDB$Longitude<-as.character(NaairAccidentDB$Longitude)
NaairAccidentDB$Latitude<-as.character(NaairAccidentDB$Latitude)
NaairAccidentDB$Latitude<-as.numeric(NaairAccidentDB$Latitude)
NaairAccidentDB$Longitude<-as.numeric(NaairAccidentDB$Longitude)
NaairAccidentDB<-NaairAccidentDB[complete.cases(NaairAccidentDB[,22]),]
NaairAccidentDB<-NaairAccidentDB[complete.cases(NaairAccidentDB[,28]),]
NaairAccidentDB<-NaairAccidentDB[complete.cases(NaairAccidentDB[,30]),]
ui<-fluidPage(
  fluidRow(
    column(3,
           wellPanel(
             radioGroupButtons(
               inputId = "selection",
               label = "Choose investigation type:", 
               choices = c("Incident", "Accident"),
               status = "danger"
             ),
             #  
             pickerInput(
               inputId = "selectWeather",
               label = "Select Weather:", 
               choices = levels(NaairAccidentDB$Weather.Condition),
               options = list(
                 style = "btn-danger"
               ),width = 220
             ),
             pickerInput(
               inputId = "selectDamage",
               label = "Select the damage level:", 
               choices = levels(NaairAccidentDB$Aircraft.Damage),
               options = list(
                 style = "btn-danger"),width = 220
             )
           ),
           hr(),
           wellPanel(
             pickerInput(
               inputId = "selectInjury",
               label = "Choose Injury Severity:", 
               choices = levels(NaairAccidentDB$Injury.Severity),
               options = list(
                 style = "btn-danger",
                 `live-search` = TRUE
               )
             ),
             #pickerInput(
             #  inputId = "selectAircraft",
             #  label = "Select Aircraft Category:", 
             # choices = levels(NaairAccidentDB$Aircraft.Category),
             # options = list(
             #   style = "btn-danger"
             # ),width = 220
             # ),
             uiOutput("choiceAircraft"),
             uiOutput("choiceMake")
             
             
           ),
           hr(),
           wellPanel(
             switchInput(
               inputId = "Showpredict",
               label = "Show Prediction",
               onStatus = "success", 
               offStatus = "danger"
             ) 
           ),
           column(9,mainPanel(
             fixedPanel( top=0,right = 250,plotOutput("map", height = 500,width = 900)),
             fixedPanel(top = 550,right = 650, amChartsOutput("barplot",width = 600)),
             fixedPanel(top = 550,right = 100, plotOutput("predictplot",width = 600))
             
           ))
           
    ))
)
server <- function(input, output){
  output$choiceMake <- renderUI({
    selectInput("selectMake", "Select Make:", choices = as.character(NaairAccidentDB[which(NaairAccidentDB$Aircraft.Category %in% input$selectAircraft & NaairAccidentDB$Injury.Severity %in% input$selectInjury),
                                                                                     "Make"]),width = 220) #add the selection which under the condition
  }) 
  output$choiceAircraft <- renderUI({
    selectInput("selectAircraft", "Select Aircraft:", choices = as.character(NaairAccidentDB[which(NaairAccidentDB$Injury.Severity %in% input$selectInjury),
                                                                                             "Aircraft.Category"]),width = 220) 
  })
  output$map <-renderPlot({
    mapworld<-borders("world",colour = "gray50",fill="white")
    mp<-ggplot()+mapworld+ylim(-60,120)
    if(input$selection == "Incident"){
      tmpMapData<-NaairAccidentDB %>%filter(Investigation.Type %in% 'Incident',Weather.Condition %in% input$selectWeather, Aircraft.Damage %in% input$selectDamage)
      mp+geom_point(aes(y=tmpMapData$Latitude,x=tmpMapData$Longitude),color="darkorange")+scale_size(range=c(1,1))
      
      
    }
    else if(input$selection == "Accident")
    {
      tmpMapData<-NaairAccidentDB %>%filter(Investigation.Type %in% 'Accident',Weather.Condition %in% input$selectWeather, Aircraft.Damage %in% input$selectDamage)
      mp+geom_point(aes(y=tmpMapData$Latitude,x=tmpMapData$Longitude),color="red")+scale_size(range=c(1,1))
    }
  })
  output$barplot <- renderAmCharts({
    NaairAccidentDB[,15:16] <- lapply(NaairAccidentDB[,15:16],as.character) #transfer column 15 and 16 to character
    tmpData <- NaairAccidentDB %>% filter(NaairAccidentDB$Injury.Severity %in% input$selectInjury, NaairAccidentDB$Aircraft.Category %in% input$selectAircraft,NaairAccidentDB$Make %in% input$selectMake)
    tmpFrame<-rename(count(tmpData,tmpData$Model))
    amBarplot(x="tmpData$Model",y="n",data = tmpFrame,main = "Injury Frequence", mainColor = "#68838B",horiz = TRUE, zoom = TRUE,xlab = "Model",ylab = "Frequence")
    
  })
  output$predictplot <- renderPlot({
    NaairAccidentDB$Event.Date <- as.Date(NaairAccidentDB$Event.Date , "%d/%m/%y")
    if(input$Showpredict == TRUE){
      print(head(NaairAccidentDB))
      accident_df <- subset(NaairAccidentDB, Investigation.Type == "Accident" )
      accident_df_last_three_years <- subset(accident_df, Event.Date > as.Date("2015-12-31") )
      accident_df_last_three_years <- subset(accident_df_last_three_years, Event.Date < as.Date("2019-01-01") )
      list2 <- rep(1,length(accident_df_last_three_years$Event.Id))
      acc_final = cbind(accident_df_last_three_years, occurence = list2)
      acc_final_grp = acc_final %>% group_by(Event.Date=floor_date(Event.Date, "month")) %>%
        summarize(occurence=sum(occurence))
      ts_acc = ts(acc_final_grp$occurence, start = 2016, frequency = 12)
      auto_acc = auto.arima(ts_acc)#解释arima
      forecasts <- predict(auto_acc, n.ahead=12)
      series <- c(acc_final_grp$occurence, forecasts$pred)
      print(series)
      print(ts_acc)
      seq_months = seq(as.Date("2016-01-01"),length=48,by="months")
      plot(seq_months, series, type="l", xaxt="n", ann=FALSE,ylab="Number of Accidents")
      axis.Date(1,at=seq_months,labels=format(seq_months,"%b-%Y"),las=2)
    } 
  })
}

shinyApp(ui = ui, server = server)