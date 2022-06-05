library(shiny)
#library(reticulate)
library(shinythemes)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(plotly)

#setwd("C:/Users/jacki/Documents/Testing/")
#use_python("C:/Users/jacki/anaconda3/")
#np <- import("numpy", convert = FALSE)
#pd <-import("pandas", convert = FALSE)
#source_python("data/job.py")

#Initial Data
data<-read.csv("data/MatchData.csv")
data$HomeResult<-as.factor(data$HomeResult)
data$VAR<-ifelse(data$VAR==1,"VAR","No VAR")
data$VAR<-as.factor(data$VAR)
data$Fans_Present<-ifelse(data$Attendance!=0,"Fans","No Fans")
data$Fans_Present<-as.factor(data$Fans_Present)
data$GoalScored=data$HomeGoal+data$AwayGoal
data$HomePassAccuracy=data$HomePassesCompleted/data$HomePassesAttempts
data$HomeShotAccuracy=data$HomeShots/data$HomeShotsonTarget
data$AwayPassAccuracy=data$AwayPassesCompleted/data$AwayPassesAttempts
data$AwayShotAccuracy=data$AwayShotsonTarget/data$AwayShots
data<-data[,which(colnames(data) %in% c("Season","League","Home","HomeResult", "Stadium_Capacity" ,"VAR","GoalScored","HomePassAccuracy","HomeShotAccuracy","HomeTackles","HomeInterceptions","HomeYellow","HomeRed","AwayPassAccuracy","AwayShotAccuracy","AwayTackles","AwayInterceptions","AwayYellow", "AwayRed","Fans_Present"))]

#Make model
modeling<-read.csv("data/Modeling_Data.csv")
model<-readRDS("data/finalmodel.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("superhero"),
    # Application title
   fluidRow(
   column(6,div(style="display:inline;text-align:left",h4("Professional Soccer Stadium Capacity Analysis Tool",style='padding-left: 5px;font-weight: bold;')),           tags$img(src = "Picture1.png", width = "50px", height = "50px"), 
          tags$img(src = "Picture2.png", width = "50px", height = "50px"), 
          tags$img(src = "Picture3.png", width = "50px", height = "50px"), 
          tags$img(src = "Picture4.png", width = "50px", height = "50px"), 
          tags$img(src = "Picture5.png", width = "75px", height = "50px")),
   column(3,div(style="display:inline;text-align:right",h5("Created by Team Analytica",style='padding-right: 5px;font-weight: bold;'))),
   column(style = "border: 4px double white;",3,div(style="display:inline;text-align:left",h6("Notes:",style='text-decoration: underline;')),
          div(style="display:inline;text-align:left",h6("-Date Refreshed 6/4/22")),
          div(style="display:inline;text-align:left",h6("-Data from https://fbref.com/en/")))),
    # Sidebar with a slider input for number of bins 
    h4("Exploring the Game Data"),
    sidebarLayout(
       sidebarPanel(
         selectInput("year","Year", choices=unique(data$Season),multiple=TRUE,selected=unique(data$Season)),
         selectInput("league","League", choices=unique(data$League),multiple=TRUE,selected=unique(data$League)),
         br(),
         selectInput("variable","X-Variable", choices=colnames(data)[c(4,7:14,17:20)],multiple=FALSE,selected="Stadium_Capacity"),
         selectInput("third_dim", "Slice Data by:", choices = colnames(data)[c(5,15)],multiple=FALSE,selected="VAR")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot")
        )
    ),
   tags$hr(style="border-color:white;height=20px"),
   h4("Future Capacity Planning & Predictions"),
   sidebarLayout(
     sidebarPanel(
       selectInput("variable2","X-Variable", choices=colnames(modeling)[c(3,4,10,11,13,16:19)],multiple=FALSE,selected="Stadium_Capacity"),
       sliderInput("stadcap",
                   "Stadium_Capacity:",
                   min = min(modeling$Stadium_Capacity),
                   max = max(modeling$Stadium_Capacity),
                   value = mean(modeling$Stadium_Capacity)),
       sliderInput("stadutl",
                   "Stadium Utilization:",
                   min =round(min(modeling$StadiumUtilization),0),
                   max =round(max(modeling$StadiumUtilization),0),
                   value = round(mean(modeling$StadiumUtilization),2)),
       sliderInput("awayfouls",
                   "Away Fouls:",
                   min =round(min(modeling$AwayFouls),0),
                   max =round(max(modeling$AwayFouls),0),
                   value = round(mean(modeling$AwayFouls),0)),
       sliderInput("awaytackles",
                   "Away Tackles:",
                   min =round(min(modeling$AwayTackles),0),
                   max =round(max(modeling$AwayTackles),0),
                   value = round(mean(modeling$AwayTackles),0)),
       sliderInput("awayyellow",
                   "Away Yellow:",
                   min =round(min(modeling$AwayYellow),0),
                   max =round(max(modeling$AwayYellow),0),
                   value = round(mean(modeling$AwayYellow),0)),
       sliderInput("homepassacc",
                   "Home Pass Accuracy:",
                   min =0,
                   max =1,
                   value = round(mean(modeling$HomePassAccuracy),2)),
       sliderInput("homeshotacc",
                   "Home Shot Accuracy:",
                   min =0,
                   max =1,
                   value = round(mean(modeling$HomeShotAccuracy),2)),
       sliderInput("awaypassacc",
                   "Away Pass Accuracy:",
                   min =0,
                   max =1,
                   value = round(mean(modeling$AwayPassAccuracy),2)),
       sliderInput("awayshotacc",
                   "Away Shot Accuracy:",
                   min =0,
                   max =1,
                   value = round(mean(modeling$AwayShotAccuracy),2))
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       h4("Final Model:"),
       h5("Win Percentage = 0.093 + 0.099*StadiumUtilization + 0.0000012*Stadium_Capacity - 0.0145*AwayFouls - 0.007*AwayTackles + 0.054*AwayYellow + 1.486*HomePassAccuracy + 1.356*HomeShotAccuracy - 1.110*AwayPassAccuracy - 0.889*AwayShotAccuracy"),
       br(),
       span(textOutput("Pred"), style="font-weight: bold;text-align:center"),
       br(),
       plotlyOutput("predPlot")
     )
   )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  new_data<-reactive({
    data<-data[which(data$Season %in% input$year),]
    data<-data[which(data$League %in% input$league),]
    data
  })
  
  total2<-reactive({
    data3<-new_data()
    data3 %>%
    group_by_(input$variable,"HomeResult",input$third_dim) %>%
    summarise(total=n())
  })

  output$scatterPlot <- renderPlot({
    data2<-new_data()
    if(input$variable %in% c("HomeYellow","HomeRed","AwayYellow","AwayRed")){
      totals<-total2()
      p<-ggplot(data2)+geom_bar(aes_string(x=input$variable,fill="HomeResult"),position = "dodge")+
        geom_text(as.data.frame(totals),mapping=aes_string(x=input$variable, label="total", y="total", group = "HomeResult", fill=NULL), position = position_dodge(width = .9))+
        ylab("Number of Games")+facet_wrap(~get(input$third_dim), scales="free")
    }else{
      p<-ggplot(data2)+geom_boxplot(aes_string(x=input$variable,y="HomeResult",fill="HomeResult"))+facet_wrap(~get(input$third_dim), scales="free")
    }
    p
  })
  
  model_data <- reactive({
    data.frame("StadiumUtilization"=input$stadutl,
               "Stadium_Capacity"=input$stadcap,
               "AwayFouls"=input$awayfouls,
               "AwayTackles"=input$awaytackles,
               "AwayYellow"=input$awayyellow,
               "HomePassAccuracy"=input$homepassacc,
               "HomeShotAccuracy"=input$homeshotacc,             
               "AwayPassAccuracy"=input$awaypassacc,
               "AwayShotAccuracy"=input$awayshotacc)
  })
  
  pred <- reactive({
    predict(model,model_data())
  })
  
  output$Pred <- renderText(paste0("Predicted Win Percentage: ",round(pred(),2)*100,"%"))
  
  output$predPlot <- renderPlotly({
    modeling2<-modeling[,c(3,4,10,11,13,16:20)]
    modeling2$Type<-"History"
    point<-model_data()
    point$HomeResult<-pred()
    #point <- point %>% relocate(HomeResult, .before = Stadium_Capacity)
    point$Type<-"Prediction"
    modeling2<-rbind(modeling2,point)
    modeling2$Type<-as.factor(modeling2$Type)
    modeling2<- modeling2 %>% mutate_if(is.numeric, round, digits=2)
    colnames(modeling2)[10]="Win_Percent"
    p<-ggplot(modeling2)+geom_point(aes_string(y="Win_Percent",x=input$variable2,col="Type"))+ylab("Win Percentage")+ theme(legend.title = element_blank())
    p <- ggplotly(p)
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
