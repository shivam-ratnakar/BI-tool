# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

#recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

#head(recommendation)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "BI Tool")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)

frow1 <- fluidRow(
  fileInput('file1', 'Choose CSV File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
  )

frow2 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value0")
)

frow3 <- fluidRow(
  
  box(
    title = "Revenue per Account"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "300px")
  )
  
  ,box(
    title = "Revenue per Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegion", height = "300px")
  ) 
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Business Intelligence tool', header, sidebar, body, skin='purple')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  yo1 <-  reactive({
  
  if (is.null(input$file1))
    return(NULL)
  
  recommendation <- read.csv(input$file1$datapath,stringsAsFactors = F,header=T)
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- sum(recommendation$Revenue)
  total.revenue
  
  })

  
  yo2 <-  reactive({
    
    if (is.null(input$file1))
      return(NULL)
    
    recommendation <- read.csv(input$file1$datapath,stringsAsFactors = F,header=T)
    
    #some data manipulation to derive the values of KPI boxes
    sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    sales.account
  })
  
  yo3 <-  reactive({
    
    if (is.null(input$file1))
      return(NULL)
    
    recommendation <- read.csv(input$file1$datapath,stringsAsFactors = F,header=T)
    
    #some data manipulation to derive the values of KPI boxes
    prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    prof.prod
  })
  
  yo4 <-  reactive({
    
    if (is.null(input$file1))
      return(NULL)
    
    recommendation <- read.csv(input$file1$datapath,stringsAsFactors = F,header=T)
    
    recommendation
  })
  
    
  output$value0 <- reactive({renderValueBox({
    
    valueBox(
      formatC(yo1(), format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })})
       
      #creating the valueBoxOutput content
      output$value1 <- renderValueBox({
        valueBox(
          formatC(yo2()$value, format="d", big.mark=',')
          ,paste('Top Account:',yo2()$Account)
          ,icon = icon("stats",lib='glyphicon')
          ,color = "purple")
        
        
      })
      
      
      
      
      output$value2 <- renderValueBox({
        
        valueBox(
          formatC(yo1(), format="d", big.mark=',')
          ,'Total Expected Revenue'
          ,icon = icon("gbp",lib='glyphicon')
          ,color = "green")
        
      })
      
      
      
      output$value3 <- renderValueBox({
      
        valueBox(
          formatC(yo3()$value, format="d", big.mark=',')
          ,paste('Top Product:',yo3()$Product)
          ,icon = icon("menu-hamburger",lib='glyphicon')
          ,color = "yellow")
        
      })
      
      #creating the plotOutput content
      
      output$revenuebyPrd <- renderPlot({
        bf <- yo4()
        if(!is.null(bf))
        {ggplot(data = yo4(), 
               aes(x=yo4()$Product, y=yo4()$Revenue, fill=factor(yo4()$Region))) + 
          geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
          xlab("Product") + theme(legend.position="bottom" 
                                  ,plot.title = element_text(size=15, face="bold")) + 
          ggtitle("Revenue by Product") + labs(fill = "Region")
      }})
      
      output$revenuebyRegion <- renderPlot({
        bf <- yo4()
        if(!is.null(bf))
        {ggplot(data = yo4(), 
               aes(x=yo4()$Account, y=yo4()$Revenue, fill=factor(yo4()$Region))) + 
          geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
          xlab("Account") + theme(legend.position="bottom" 
                                  ,plot.title = element_text(size=15, face="bold")) + 
          ggtitle("Revenue by Region") + labs(fill = "Region")
      }})
      
    
  
  
  
  
  
  
  
}


shinyApp(ui, server)