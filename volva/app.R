# volva - Instantly Forecast Anything
#
# A Shiny app that forecasts a user-uploaded CSV (date, value) time series
# using Facebook's prophet package.
#
# Live demo: https://varmer.shinyapps.io/volva/

library(shiny)
library(prophet)
library(lubridate)
library(dplyr)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- fluidPage(

  # App title ----
  titlePanel("Instantly Forecast Anything"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      strong("Upload a .csv file. File must contain two columns."),
      p("The first column must be a date value in the format YYYY-MM-DD."),
      p("The second column must be a numeric or integer value containing the values you wish to forecast."),

      # Input: Select a file ----
      fileInput("datafile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

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

      tags$hr(),

      # Input: Forecast horizon (days into the future) ----
      sliderInput("horizon", "Forecast horizon (days)",
                  min = 1, max = 730, value = 365, step = 1),

      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),

      tags$hr(),

      # Output: Download the full forecast ----
      downloadButton("downloadForecast", "Download forecast (CSV)")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ table, plot and predictions ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("contents")),
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Predictions", tableOutput("prediction"))
      )

    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output) {

  # Read the uploaded CSV exactly once and validate it. The result is shared
  # by the table, plot and predictions tabs via Shiny's reactive caching.
  rawData <- reactive({
    req(input$datafile)

    # When reading semicolon separated files, having a comma separator
    # causes `read.csv` to error, so surface a friendly message instead.
    df <- tryCatch(
      read.csv(input$datafile$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote),
      error = function(e) stop(safeError(e))
    )

    # Input validation with friendly messages.
    validate(
      need(ncol(df) >= 2,
           "The file must contain at least two columns: a date and a value."),
      need(!all(is.na(as.Date(as.character(df[, 1]), format = "%Y-%m-%d"))),
           "The first column could not be parsed as dates. Use the format YYYY-MM-DD.")
    )

    df
  })

  # Clean and aggregate the data into the (ds, y) frame prophet expects.
  # Aggregation is keyed on the parsed `ds` date, not the raw text column.
  cleanData <- reactive({
    df <- rawData()

    ds <- as.Date(as.character(df[, 1]), format = "%Y-%m-%d")
    y  <- as.numeric(df[, 2])

    agg <- aggregate(y, by = list(ds = ds), FUN = sum)
    colnames(agg) <- c("ds", "y")

    agg %>%
      arrange(ds) %>%
      select(ds, y)
  })

  # Fit the prophet model and compute the forecast a single time. Every tab
  # that needs the model/forecast reads this reactive instead of re-fitting.
  forecastData <- reactive({
    df <- cleanData()

    withProgress(message = "Fitting prophet model", value = 0.1, {

      # Fit the prophet model to the input data.
      m <- prophet(df)
      incProgress(0.5)

      # Build a future data frame and predict over the chosen horizon.
      future   <- make_future_dataframe(m, periods = input$horizon)
      forecast <- predict(m, future)
      incProgress(0.4)

      list(model = m, forecast = forecast)
    })
  })

  # Tab: raw (cleaned) input table.
  output$contents <- renderTable({
    df <- cleanData()
    df$ds <- format(df$ds, "%Y-%m-%d")
    if (input$disp == "head") head(df) else df
  })

  # Tab: prophet forecast plot.
  output$plot <- renderPlot({
    fc <- forecastData()
    plot(fc$model, fc$forecast, xlabel = "Dates", ylabel = "Forecasted Value")
  })

  # Predictions data frame: ds + point forecast and uncertainty interval.
  predictionTable <- reactive({
    fc <- forecastData()
    out <- fc$forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")]
    out$ds <- format(as.Date(out$ds), "%Y-%m-%d")
    out
  })

  # Tab: predictions table.
  output$prediction <- renderTable({
    predictionTable()
  })

  # Download handler: export the full forecast as a CSV.
  output$downloadForecast <- downloadHandler(
    filename = function() {
      paste0("volva-forecast-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(predictionTable(), file, row.names = FALSE)
    }
  )

}

# Create Shiny app ----
shinyApp(ui, server)
