

library(corrplot)
library(corrgram)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(reshape2)

#---------------Model Preparation------------------


data <- read.csv("D:\\class bimtech\\R\\R shiny Projects\\Leslie_Salt.csv")
numericData <- data[sapply(data, is.numeric)]
r <- cor(numericData)
corrplot(r, method = "circle", type = "upper")
corrplot.mixed(r)
bp<- ggplot(data, aes(x="", y=data$Price, fill=data$county))+
  geom_bar(width = 1, stat = "identity")


#---------Model------------
fit6 <- step(lm(Price ~ ., data= data[-c(26,6,4,21),]))
summary(fit6)
pd <- predict(fit6, newdata = data[-c(26,6,4,21),])

1 - mean(abs((data[-c(26,6,4,21),1]-pd)/data[-c(26,6,4,21),1]))

newdata <- data.frame(0,as.factor("Santa Clara"),246.8,0,0,6,"No",0)
colnames(newdata) <- c("Price", "county","size", "elevation","sewer", "date", "flood", "distance")



#-------Dashboard----------------------




ui <- shinyUI(
  
  dashboardPage(
    dashboardHeader( title = "Leslie Property Analysis"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Prediction",tabName="Result"),
      menuItem("Detailed Analysis", tabName ="Relation"),
         menuSubItem("Correlation", tabName = "Detailed"),
      menuItem("Insights"),
       menuSubItem("Price & Place", tabName = "insights1"),
      menuSubItem("Price & Sewer Distance", tabName = "insights2")
      
    )),
    dashboardBody(
      # fluidRow(plotOutput("Correlation"))
      
      
      
      tabItems(
        tabItem(tabName = "Result",
                fluidRow( 
                 column(4,
                   selectInput("county", 'County', c("San Clara"=1,"Santa Mateo"=0)),
                 numericInput("s","Enter Size of Property in acers","246.8"),
                  numericInput("e","Enter Elevation","0"),
                  numericInput("s","Enter Distance (in feet) to nearest sewer connection","0"),
                  numericInput("date","Date of sale counting backward from current time (in months)","6"),
                  selectInput('flood', 'Flood', c("No"=0,"Yes"=1)),
                  numericInput("d","Enter Distance in miles from Leslie Property","0"),
                 actionButton("Run_model", "Predict")),
                  
                 column(8, br(), br(),box( title = h2("Predicted Price in $000 per acre"), width = 10, solidHeader = TRUE,
                  h3(tableOutput("Price"))
                               )
                               )
                         )
                ),

        tabItem(tabName ="Relation",       
                  fluidRow(
          column(6,
                 h2("Correlation between Variables"),plotOutput("Correlation")),
          column(6,
                 br(),
                 br(),
                 h3("Size and Distance Variables are having positive crrelation, 
                 Elevation and sewer are having negative correlation, Distance and Elevation are 
                 also possessing negative correlation."))
                          )
                ),
        
        tabItem(tabName = "Detailed",
                  fluidRow(
                  h2("Relation between Price and No. of Moths since sale"),
                  box(plotOutput("CorrelationD")),
                  box(h3("no of months counting backward from current time contibutes positively to 
                         the Price ie., Older Land costed lesser in price."))
                          ),
                  fluidRow(
                  h2("Relation between Price and Elevation"),
                  box(plotOutput("CorrelationE")),
                  box(h3("Price is higher where elevation is more in value."))
                          )
                ),
        tabItem(tabName = "insights1",
                  fluidRow(
                  h2(" Elevation Vs Flood Vs Price"), 
                  box(plotOutput("SS")),
                  box(h3("Places with elevation equal or less than 2 has higher chances of flood"))
                          )
                ),
        
        tabItem(tabName = "insights2",
                fluidRow(
                  h2("Distance from sewer Vs Price"), 
                  box(plotOutput("SP")),
                  box(h3("As the Distance to nearest sewer connection increases,
                         Price decrease, This means Places which are quite far away from the sewer are cheaper."))
                        )
                )
            )
        )
    )
)

library(scales)
server <- function(input, output, session) {
  # output$hist <- renderPlot({
  #   hist(faithful$eruptions)
  # })
  
  #--------------------------
  
  output$Correlation <- renderPlot({
    corrplot(r, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
  })

  output$CorrelationD <- renderPlot({
    ggplot(data, aes(x = Price, y = date), xlab = "Price", ylab = "Months counting backward") + geom_line()
    # ggplot(data, aes(data$Price,(-1)*data$date)) + 
    #   geom_line(aes(colour = "data$sewer"))
      #geom_line(aes(y = data$elevation, colour = "data$elevation"))
  })

  output$CorrelationE <- renderPlot({
    ggplot(data, aes(x = Price, y = elevation)) + geom_line()
           
  })
  output$SS <- renderPlot({
    df1 <- data.frame(data$Price, data$elevation, data$flood)
    ggplot(df1, aes(x=data$elevation, y=data$Price, fill = data$flood)) +
      geom_bar(stat='identity', position='dodge')
  
  })
  output$SP <- renderPlot({
    df1 <- data.frame(data$Price, data$sewer, data$flood)
    ggplot(df1, aes(x=data$sewer, y=data$Price)) +
      geom_line(stat='identity', position='dodge')
  })
  
  
  
  
  test1 <- reactive({
    # this is how you fetch the input variables from ui component
    
    Price <- as.numeric(0)
    county <- as.numeric(input$county)
    size <- as.numeric(input$size)
    elevation <- as.integer(input$e)
    sewer <- as.numeric(input$s)
    date <- as.numeric(input$date)
    flood <- as.numeric(input$flood)
    distance <- as.numeric(input$d)
    
    test1 <- cbind( Price,county, size, elevation, sewer,date,flood,distance)
 
    test1 <- as.data.frame(test1)
   
  })
  
 
  observeEvent(input$Run_model, {
    pred <-   predict(fit6, newdata = test1())
    pri <- pred[[1]]
    output$Price <- renderPrint({(pri)})
    
  })
  
  
  #pd1 <- predict(fit6, newdata = test)
  #colnames(leslie_salt) <- c("Price", "county","size", "elevation","sewer", "date", "flood", "distance")
  #data<- reactive({rbind(data,new())})
  #pd1 <- predict(fit6, newdata = data[30,])
  # data[32,2] <- reactive({as.factor(input$c)})
  # data[32,3] <- reactive({input$size})
  # data[32,4] <- reactive({input$e})
  # data[32,5] <- reactive({input$s})
  # data[32,6] <- reactive({input$date}) 
  # data[32,7] <- reactive({as.factor(input$f)})
  # data[32,8] <- reactive({input$d})  
  #leslie_salt_price <- reactive({predict(fit6, newdata = data[32,])}) 
  #output$Price <- renderTable({(pred)})
  
}

shinyApp(ui=ui, server=server)