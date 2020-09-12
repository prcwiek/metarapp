library(DT)
library(pmetar)
library(shiny)
library(shinyjs)

# Change the maximum upload file size
options(shiny.maxRequestSize=20*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    shinyjs::useShinyjs(),
    
    fluidRow(
        column(2,
               br(),
               actionButton("decodeMETAR", "Decode single METAR")
               ),
        
        column(8,
               br(),
               textInput("textMETAR", label = NULL, value = "EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG", width = "100%")
               )
    ),

    hr(),
    
    fluidRow(
        column(2,
               br(),
               actionButton("historicalMETAR", "Decode historical METAR")
               ),
        column(2,
               #br(),
               textInput("airport", label = "ICAO or IATA code", value = "EPWA")
               ),
        column(2,
               dateInput(
                   "sdate",
                   "Start date:",
                   value = "2020-01-01",
                   format = "yyyy-mm-dd",
                   min = "2000-01-01",
                   max = Sys.Date() - 1
                   )
               ),
        column(2,
               dateInput(
                   "edate",
                   "End date:",
                   value = Sys.Date(),
                   format = "yyyy-mm-dd",
                   min = "2000-01-02",
                   max = Sys.Date()
                   )
        )
    ),
    
    hr(),
    
    fluidRow(
        column(2,
               checkboxInput("metric_decode", "Metric values", TRUE)),
        column(4,
               downloadButton("metar_download", label = "Download decoded METAR(s)"))
    ),
    
    hr(),
    
    dataTableOutput("decodedMETAR")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    decoded_METAR <- reactiveValues(data = NULL)

    observeEvent(input$decodeMETAR,{
        req(input$decodeMETAR, input$textMETAR)
        decoded_METAR$data <- metar_decode(input$textMETAR, metric = input$metric_decode)
        shinyjs::enable("metar_download")
    })
    
    observeEvent(input$historicalMETAR,{
        req(input$historicalMETAR, input$airport, input$sdate, input$edate)
        historical_metar <- metar_get_historical(input$airport, input$sdate, input$edate)
        decoded_METAR$data <- metar_decode(historical_metar, metric = input$metric_decode)
        shinyjs::enable("metar_download")
    })


    output$decodedMETAR <- renderDataTable({
        decoded_METAR$data
    })

    output$metar_download <- downloadHandler(
        filename = function() {
            paste("METAR_decoded", "csv", sep = ".")
        },
        content = function(file) {
            # Write to a file specified by the 'file' argument
            #write.table(exclusions$data, file, sep = "\t", row.names = FALSE, quote = FALSE, col.names = FALSE)
            write.csv(decoded_METAR$data, file, row.names = FALSE)
        }
    )
    
    
    # Disable buttons at start
    shinyjs::disable("metar_download")
    
    # End application when a window or a tab is closed
    session$onSessionEnded(stopApp)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
