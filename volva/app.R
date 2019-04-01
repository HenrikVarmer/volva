library(shiny)
library(prophet)
library(lubridate)
library(dplyr)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Instantly Forecast Anything"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("datafile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("contents")),
                  tabPanel("Plot", plotOutput("plot"))
                  
      )
      
    )
  )
)    

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$datafile will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$datafile)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$datafile$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })

  output$plot <- renderPlot({
    
    df <- read.csv(input$datafile$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    df[,1] <- as.character(df[,1])
    df[,2] <- as.integer(df[,2])
    
    #some required data cleaning
    colnames(df) <- c("ds", "y")
    
    df$ds <- as.Date(df[,1], format = "%Y-%m-%d")
    
    df <- df %>% 
      select(ds, y)

    #Fit the prophet model to input data
    m <- prophet(df)
    
    #Create a dataframe for the future
    future <- make_future_dataframe(m, periods = 365)
    
    #Make a future prediction based on the fitted model
    forecast <- predict(m, future)
    
    #Here, we plot known data points along with the model fit and future predictions.
    return(plot(m, forecast, xlabel = "Dates", ylabel = "Forecasted Value"))
      
    })
    
  
}

# Create Shiny app ----
shinyApp(ui, server)

