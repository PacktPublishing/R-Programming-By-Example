
library(shiny)
library(ggplot2)
library(lubridate)
library(shinythemes)

source("../chapter-08/cryptocurrencies/utilities/time-stamp.R")
source("../chapter-09/sma-delegated.R", chdir = TRUE)
source("../chapter-09/sma-efficient.R")
source("./functions.R")

ORIGINAL_DATA <- read.csv("../chapter-09/data.csv", stringsAsFactors = FALSE)
ORIGINAL_DATA$time <- timestamp_to_time.TimeStamp(ORIGINAL_DATA$timestamp)
DATA_ASSETS <- unique(ORIGINAL_DATA$name)
DATE_MIN <- timestamp_to_date_string.TimeStamp(min(ORIGINAL_DATA$timestamp))
DATE_MAX <- timestamp_to_date_string.TimeStamp(max(ORIGINAL_DATA$timestamp))
DATE_END <- timestamp_to_date_string.TimeStamp(time_to_timestamp.TimeStamp(
    timestamp_to_time.TimeStamp(min(ORIGINAL_DATA$timestamp)) + days(2)))

ui <- fluidPage(
    titlePanel("Cryptocurrency Markets"),
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                dateRangeInput(
                    "date_range",
                    label = paste("Date range:"),
                    start = DATE_MIN,
                    end = DATE_END,
                    min = DATE_MIN,
                    max = DATE_MAX,
                    separator = " to ",
                    format = "yyyy-mm-dd",
                    weekstart = 1,
                    startview = "year"
                ),
                htmlOutput("select_asset")
            ),
            conditionalPanel(
                condition = "input.tab_selected == 1",
                wellPanel(
                    radioButtons(
                        "sma_implementation",
                        "Implementation:",
                        choices = list(
                            "C++" = "sma_delegated_cpp",
                            "Fortran" = "sma_delegated_fortran",
                            "Efficient 1" = "sma_efficient_1",
                            "Efficient 2" = "sma_efficient_2"
                        ),
                        selected = "sma_delegated_cpp"
                    ),
                    sliderInput(
                        "sma_period",
                        "Period:",
                        min = 5,
                        max = 200,
                        value = 30
                    )
                )
            )
        ),
        mainPanel(
            tabsetPanel(
                id = "tab_selected",
                tabPanel(
                    "Simple Moving Averages",
                    value = 1,
                    fluidRow(plotOutput("graph_top", brush = brushOpts("graph_brush"))),
                    fluidRow(plotOutput("graph_bottom"))
                ),
                tabPanel(
                    "Data Overview",
                    value = 2,
                    fluidRow(tableOutput("summary_table")),
                    fluidRow(DT::dataTableOutput("table"))
                )
            )
        )
    )
)

server <- function(input, output) {

    data <- reactive({
        data  <- ORIGINAL_DATA
        start <- input$date_range[1]
        end   <- input$date_range[2]
        if (input$asset != "All") {
            data <- data[data$name == input$asset, ]
        }
        if (time_to_date_string.TimeStamp(start) != DATE_MIN |
            time_to_date_string.TimeStamp(end) != DATE_MAX) {
            data <- data[
                data$timestamp >= time_to_timestamp.TimeStamp(start) &
                data$timestamp <= time_to_timestamp.TimeStamp(end), ]
        }
        return(data)
    })

    sma <- reactive({
        return(do.call(
            input$sma_implementation,
            list(input$sma_period, data()[1, "symbol"], data())
        ))
    })

    ranges <- reactive({
        if (!is.null(input$graph_brush)) {
            return(list(
                x = c(as.POSIXct(input$graph_brush$xmin, origin = "1970-01-01"),
                      as.POSIXct(input$graph_brush$xmax, origin = "1970-01-01")),
                y = c(input$graph_brush$ymin, input$graph_brush$ymax)
            ))
        }
        return(list(x = NULL, y = NULL))
    })

    output$table <- DT::renderDataTable(DT::datatable({return(data())}))

    output$select_asset <- renderUI({
        assets <- DATA_ASSETS
        if (input$tab_selected == 2) {
            assets <- c("All", assets)
        }
        return(selectInput("asset", "Asset:", assets))
    })

    output$summary_table <- renderTable(data.frame(
        Minimum = min(data()$price_usd),
        Median = mean(data()$price_usd),
        Mean = mean(data()$price_usd),
        Max = max(data()$price_usd)
    ))

    output$graph_top <- renderPlot({
        return(sma_graph(data(), sma()))
    })

    output$graph_bottom <- renderPlot({
        return(sma_graph(data(), sma()) +
               coord_cartesian(xlim = ranges()$x,
                               ylim = ranges()$y, expand = FALSE))
    })
}

shinyApp(ui, server, options = list(port = 6924))
