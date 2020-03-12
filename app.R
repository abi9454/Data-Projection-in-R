

pacman::p_load('ggplot2','shinythemes','shiny' , 'shinyjs', 'tidyverse','readr', 'highcharter','Hmisc')




# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  conditionalPanel(
    condition = "input.password != 'password'",  
    passwordInput("password", "Type the password here", value="")
  ),
  conditionalPanel(
    condition = "input.password == 'password'", 
    tags$head(
      tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #ad1d28;
        
          
      }

    "))
    ),
    
    headerPanel("Marks By Student"),
    
    # Application title
    #titlePanel("Marks by Student"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        tags$style('.well {background-color: #2A9FBC}'),
        fileInput("myData", label = h3("File input")),
        downloadButton("download_myData", "Download from web Browser"),
        actionButton("action", label = "save Data"),
        selectInput("Subject", "Select the subject:",
                    choices = c("CMPS401","Math201"),
                    selected = "CMPS401"),
        h4('User may load data, download data, save data and switch subgroups of data'),
        radioButtons("color", "Select the colour", choices=c("Red", "Purple","Orange"),selected=("Red")
        ),
        shinythemes::themeSelector(),
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          type = "tabs",
          tabPanel('Plot histogram',
                   plotOutput('histogram')),
          tabPanel('Plot barchart',
                   plotOutput('barplot')),
          tabPanel('Mathematical Calculation',
                   br(),
                   actionButton("Mean", "Find mean"),
                   hidden(
                     div(id='text_div',
                         verbatimTextOutput("Mean")
                     )
                   ),
                   
                   br(),
                   br(),
                   br(),
                   actionButton("Median", "Find Median"),
                   hidden(
                     div(id='text_div1',
                         verbatimTextOutput("Median")
                     )
                   ),
                   
                   br(),
                   br(),
                   br(),
                   actionButton("SD", "Find S.D"),
                   hidden(
                     div(id='text_div2',
                         verbatimTextOutput("SD")
                     )
                   ),
                   
                   br(),
                   
                   
                   
          ),
          tabPanel('Data',
                   tableOutput('horsepower')),
          tabPanel('Global Data',
                   h4('*Data from global.R file'),
                   tableOutput('myUsers'))
        ) 
      )
    )
  )
)
#access the global.R file
source("Global.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  myData <- reactive({
    print(input$myData$name)
    read_csv(input$myData$datapath)
  })
  
  output$horsepower <- renderTable({
    if (is.null(input$myData)){
      return()
    } else if(!is.null(input$myData)){
      
      if(input$Subject == 'All'){
        myData()
      } else if ((input$Subject != '')){
        myData() %>% filter(Subject == input$Subject)
      }
    }
  })
  
  output$histogram <- renderPlot({
    if(is.null(input$myData)){
      return ()
    }else if (!is.null(input$myData)){
      myData() %>% filter(Subject == input$Subject)%>%
        ggplot(aes(marks))+ geom_histogram(bins = 10,fill=input$color)+ labs(y = "Number Of Student")
    }
  })
  output$barplot <-renderPlot({
    if(is.null(input$myData)){
      return ()
    }else if (!is.null(input$myData)){
      myData() %>% filter(Subject == input$Subject)%>%
        ggplot(aes(Grade))+ geom_bar(bins = 10,fill=input$color)+ labs(y = "Number Of Student")
    }
  })
  
  observeEvent(input$Mean, {
    toggle('text_div')
    output$Mean <- renderText({
      
      if(is.null(input$myData)){
        return ()
      }else if (!is.null(input$myData)){
        mydata <- myData()
        x<- mean(mydata$marks) 
        paste("The mean =" , x)
      }
      
    })
  })
  observeEvent(input$Median, {
    toggle('text_div1')
    output$Median <- renderText({
      
      if(is.null(input$myData)){
        return ()
      }else if (!is.null(input$myData)){
        mydata <- myData()
        x<- median(mydata$marks) 
        paste("The Median =" , x)
      }
      
    })
  })
  
  observeEvent(input$SD, {
    toggle('text_div2')
    output$SD <- renderText({
      
      if(is.null(input$myData)){
        return ()
      }else if (!is.null(input$myData)){
        mydata <- myData()
        x<- sd(mydata$marks) 
        paste("The S.D. =" , x)
      }
      
    })
  })
  
  
  
  
  
  
  output$download_myData <- downloadHandler(
    filename = function(){
      print(input$myData$name)
      paste(input$myData$name, ".csv", sep = "")
    },
    content = function(filename){
      print(filename)
      write_csv(myData(),filename)
    }
  )
  
  observeEvent(input$action,{
    write_csv(myData(),input$myData$name)
  })
  output$myUsers <- renderTable({
    return(myUsers)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

