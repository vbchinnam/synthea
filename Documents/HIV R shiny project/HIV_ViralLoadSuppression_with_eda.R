rm(list=ls())
set.seed(1997)

# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("boxuancui/DataExplorer")
# library("DataExplorer")

print("============================================================================")
print("............ Running App ............")
print("============================================================================")
if (!require("shiny")) install.packages('shiny')
if (!require("shiny")) install.packages('DT')
if (!require("shinydashboard")) install.packages('shinydashboard')
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("reactlog")) install.packages("reactlog")
if (!require("caret")) install.packages("caret", dependencies = c("Depends", "Suggests"))
if (!require("skimr")) install.packages("skimr")
if (!require("mltools")) install.packages("mltools")
if (!require("data.table")) install.packages("data.table")
if (!require("earth")) install.packages("earth")
if (!require("kernlab")) install.packages("kernlab")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
reactlog_enable()

ui <- dashboardPage(
  dashboardHeader(
    title = "HIV Viral Load Suppression",
    titleWidth = 275,
    tags$li(
      class = "dropdown", style = "padding: 8px;"
    )
  ),
  dashboardSidebar(width = 275, disable = T
  ),
  dashboardBody(width = 100, height = 100,
                useShinyjs(),
                # tabItems(
                # tabItem(tabName = "dataset",
                navbarPage(title = "", id = "new",
                           tabPanel("Select DataSet", value = "main",
                                    box(width =12,  collapsible = T,  solidHeader = T, title = "Select Data", status = "primary",
                                        fluidRow(column(3, selectInput(inputId = "selectDataset", label = "Select Source Dataset:", choices = c("Simulate Dataset", "Upload Dataset"),  multiple = F) )),
                                        tags$div(id = 'uploadOptions', 
                                                 fluidRow(
                                                   column(3, fileInput(inputId = "idfile", label = "Upload: (.csv only)", accept=c("text/csv","text/comma-separated-values,text/plain",".csv")))
                                                 ),
                                                 fluidRow(
                                                   column(3, actionButton(inputId = "loadFileButton", label = "Load File"))
                                                 )
                                        ),
                                        tags$div(id = 'simulateOptions', 
                                                 fluidRow(
                                                   column(3, numericInput(inputId = "numRows", label = "Number of Rows", value = 1000, min = 1000, step =1000, max = 10000))),
                                                 fluidRow(
                                                   column(2, 
                                                          sliderInput(inputId = 'perentMales',label = "% of Males", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                          sliderInput(inputId = 'perentFemales',label = "% of Females", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                          sliderInput(inputId = 'perentOthers',label = "% of Others", min = 0, max = 100, value = 20, step = 1, ticks = T, animate = F)
                                                   ),
                                                   column(2, 
                                                          sliderInput(inputId = 'perentHIVTrue',label = "% of HIV = 1", min = 0, max = 100, value = 30, step = 1, ticks = T, animate = F),
                                                          sliderInput(inputId = 'perentHIVFalse',label = "% of HIV = 0", min = 0, max = 100, value = 70, step = 1, ticks = T, animate = F)                                                             
                                                   ),
                                                   column(2, 
                                                          sliderInput(inputId = 'perentARTTrue',label = "% of ART = 1", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                          sliderInput(inputId = 'perentARTFalse',label = "% of ART = 0", min = 0, max = 100, value = 60, step = 1, ticks = T, animate = F)                                                             
                                                   ),
                                                   column(2, 
                                                          sliderInput(inputId = 'perentOnArtTrue',label = "% of On Art = 1", min = 0, max = 100, value = 50, step = 1, ticks = T, animate = F),
                                                          sliderInput(inputId = 'perentOnArtFalse',label = "% of On Art = 0", min = 0, max = 100, value = 50, step = 1, ticks = T, animate = F)                                                             
                                                   ),
                                                   column(2, 
                                                          sliderInput(inputId = 'perentUnawareTrue',label = "% of Unaware = 1", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                          sliderInput(inputId = 'perentUnawareFalse',label = "% of Unaware = 0", min = 0, max = 100, value = 60, step = 1, ticks = T, animate = F)                                                             
                                                   )
                                                   
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(3, actionButton(inputId = "simulateDataButton", label = "Simulate"))
                                                 )
                                        )
                                        ,
                                        tags$div(id = 'viewSampleData', DT::dataTableOutput(outputId = "DataTable", width = 1000)))
                           ),
                           tabPanel("EDA - Columns", value = "screen1",
                                    tags$div(id='edaColumnsTab',
                                             fluidRow(DT::dataTableOutput(outputId = "DataTableColumnsSummary"))
                                    )
                                    
                           ),
                        
                           tabPanel("EDA - Plots", value = "screen2",
                                    tags$div(id='edaPlotTab',
                                             fluidRow(
                                               box(width =12, collapsible = F,  solidHeader = T, title = "Single Variable Plots", status = "primary",
                                                   fluidRow(
                                                     column(width = 4, box( collapsible = F, solidHeader = F, status = "primary",
                                                                            selectInput(inputId = 'xSingle', label = "Select Variable", choices = c(), multiple = F)
                                                     )
                                                     ),
                                                     column(width = 8, plotOutput(outputId = 'plotSingle'))
                                                   )
                                               )
                                             ),
                                             fluidRow(
                                               box(width =12, collapsible = F,  solidHeader = T, title = "Two Variable Plots", status = "primary",
                                                   fluidRow(
                                                     column(width = 4, box( collapsible = F, solidHeader = F, status = "primary",
                                                                            selectInput(inputId = 'x', label = "Select X Variable", choices = c(), multiple = F),
                                                                            selectInput(inputId = 'y', label = "Select Y Variable", choices = c(), multiple = F)
                                                     )
                                                     ),
                                                     column(width = 8, plotOutput(outputId = 'plot'))
                                                   )
                                               )
                                             )
                                             
                                             
                                    )
                                    
                           )
                           
)
)
)

server <- function(input, output, session) {
  
  numRows <- reactive(input$numRows)
  
  observeEvent(input$perentMales, {
    val1 = round((input$perentOthers / (input$perentOthers + input$perentFemales)) *  (100 - input$perentMales))
    val2 = 100 - val1 - input$perentMales
    
    updateSliderInput(session = session, inputId = 'perentOthers',label = "% of Others", min = 0, max = 100, value = val1, step = 1 )
    updateSliderInput(session = session, inputId = 'perentFemales',label = "% of Females", min = 0, max = 100, value = val2, step = 1 )
  })
  observeEvent(input$perentFemales, {
    val1 = round((input$perentOthers / (input$perentOthers + input$perentMales)) *  (100 - input$perentFemales))
    val2 = 100 - val1 - input$perentFemales
    
    updateSliderInput(session = session, inputId = 'perentOthers',label = "% of Others", min = 0, max = 100, value = val1, step = 1 )
    updateSliderInput(session = session, inputId = 'perentMales',label = "% of Males", min = 0, max = 100, value = val2, step = 1 )
  })
  observeEvent(input$perentOthers, {
    val1 = round((input$perentMales / (input$perentMales + input$perentFemales)) *  (100 - input$perentOthers))
    val2 = 100 - val1 - input$perentOthers
    
    updateSliderInput(session = session, inputId = 'perentMales',label = "% of Males", min = 0, max = 100, value = val1, step = 1 )
    updateSliderInput(session = session, inputId = 'perentFemales',label = "% of Females", min = 0, max = 100, value = val2, step = 1 )
  })
  
  observeEvent(input$perentHIVTrue, {updateSliderInput(session = session, inputId = 'perentHIVFalse',label = "% of HIV = 0", min = 0, max = 100, value = (100-input$perentHIVTrue), step = 1 )})
  observeEvent(input$perentHIVFalse, {updateSliderInput(session = session, inputId = 'perentHIVTrue',label = "% of HIV = 1", min = 0, max = 100, value = (100-input$perentHIVFalse), step = 1 )})
  
  observeEvent(input$perentARTFalse, {updateSliderInput(session = session, inputId = 'perentARTTrue',label = "% of ART= 1", min = 0, max = 100, value = (100-input$perentARTFalse), step = 1 )})
  observeEvent(input$perentARTTrue, {updateSliderInput(session = session, inputId = 'perentARTFalse',label = "% of ART = 0", min = 0, max = 100, value = (100-input$perentARTTrue), step = 1 )})
  
  
  observeEvent(input$perentOnArtFalse, {updateSliderInput(session = session, inputId = 'perentOnArtTrue',label = "% of On Art = 1", min = 0, max = 100, value = (100-input$perentOnArtFalse), step = 1 )})
  observeEvent(input$perentOnArtTrue, {updateSliderInput(session = session, inputId = 'perentOnArtFalse',label = "% of On Art = 0", min = 0, max = 100, value = (100-input$perentOnArtTrue), step = 1 )})
  
  observeEvent(input$perentUnawareFalse, {updateSliderInput(session = session, inputId = 'perentUnawareTrue',label = "% of Unaware = 1", min = 0, max = 100, value = (100-input$perentUnawareFalse), step = 1 )})
  observeEvent(input$perentUnawareTrue, {updateSliderInput(session = session, inputId = 'perentUnawareFalse',label = "% of Unaware = 0", min = 0, max = 100, value = (100-input$perentUnawareTrue), step = 1 )})
  
  
  
  
  
  dataset <- eventReactive((input$simulateDataButton | input$loadFileButton), {
    if (input$selectDataset == "Simulate Dataset") {
      n = numRows()
      data.frame( vls = round(runif(n,50,300), 2), 
                  age  = sample(seq(20, 70),size = n,replace = TRUE), 
                  sex  =  factor(sample(c("Male", "Female", "Others"),size = n,replace = TRUE,prob = c(input$perentMales/100, input$perentFemales/100, input$perentOthers/100) )), 
                  hiv =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentHIVFalse/100, input$perentHIVTrue/100))), 
                  art =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentARTFalse/100, input$perentARTTrue/100))), 
                  on_art  =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentOnArtFalse/100, input$perentOnArtTrue/100))), 
                  unware  =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentUnawareFalse/100, input$perentUnawareTrue/100))), 
                  country =  factor(sample(c("India", "USA", "Germany", "Italy", "Australia"),size = n,replace = TRUE)))
    }
    else{
      
      read.csv(file = input$idfile$datapath)
    }
  })
  
  
  numeric_columns <- reactive({
    dataset() %>% filter(FALSE) %>% select_if(is.numeric) %>% colnames()
  }) 
  factor_columns <- reactive({
    dataset() %>% filter(FALSE) %>% select_if(is.factor) %>% colnames()
  })
  
  
  
  skimmed_Dataset <- reactive({
    cols <- c('skim_variable', 'skim_type', 'n_missing')
    if(length(factor_columns() > 0)){cols <- append(cols, c('factor.n_unique', 'factor.top_counts'))}
    if(length(numeric_columns() > 0)){cols <- append(cols, c('numeric.mean', 'numeric.sd', 'numeric.p0', 'numeric.p25', 'numeric.p50', 'numeric.p75', 'numeric.p100', 'numeric.hist'))}
    
    skim(dataset()) %>% 
      select(cols) %>% 
      rename(Variable = skim_variable, Type=skim_type)
    
  }) 
  
 
  displayDataTrigger <- reactive({
    paste(input$simulateDataButton , input$loadFileButton)
  })
  observeEvent(displayDataTrigger(), {
    
    output$DataTable  <- DT::renderDataTable(dataset(), options = list(lengthMenu = c(10, 15), pageLength = 10))
    output$DataTableColumnsSummary <- DT::renderDataTable(skimmed_Dataset(), options = list(scrollX = TRUE))
    
    
    ## plotting
    updateSelectInput(session = session,inputId = 'xSingle', label = "Select Variable", choices = colnames(dataset()))
    
    updateSelectInput(session = session,inputId = 'x', label = "Select X Variable", choices = colnames(dataset()))
    updateSelectInput(session = session,inputId = 'y', label = "Select Y Variable", choices = colnames(dataset()))
    
    
    shinyjs::showElement(id = "viewSampleData", animType = 'slide', time = 0.2 )
    shinyjs::showElement(id = "edaColumnsTab", animType = 'slide', time = 0.2 )
    
    shinyjs::showElement(id = "edaPlotTab", animType = 'slide', time = 0.2 )
  })
  
  makeSinglePlotTrigger <- reactive({input$xSingle})
  
  observeEvent( makeSinglePlotTrigger(), {
    
    
    if (input$xSingle %in% numeric_columns()){
      output$plotSingle <- renderPlot(
        ggplot(data = dataset(), aes_string(x=input$xSingle)) +
          geom_histogram()
      )
    }
    else if (input$xSingle %in% factor_columns()){
      output$plotSingle <- renderPlot(
        ggplot(data = dataset(), aes_string(x=input$xSingle)) +
          geom_bar(stat="count")
      )
    }
  })
  
  
  makeMultiplePlotTrigger <- reactive({
    paste(input$x , input$y)
  })
  
  
  observeEvent( makeMultiplePlotTrigger(), {
    
    
    if ((input$x %in% numeric_columns()) & (input$y %in% numeric_columns())){
      output$plot <- renderPlot(
        ggplot(data = dataset(), aes_string(x=input$x, y= input$y)) +
          geom_point()
      )
    }
    else if (((input$x %in% factor_columns()) & (input$y %in% numeric_columns())) | ((input$x %in% numeric_columns()) & (input$y %in% factor_columns()))){
      output$plot <- renderPlot(
        ggplot(data = dataset(), aes_string(x=input$x, y= input$y)) +
          geom_boxplot()
      )
    }
    # else if ((input$x %in% factor_columns()) & (input$y %in% factor_columns())){
    #   output$plot <- renderPlot(
    #     ggplot(data = dataset(), aes_string(x=input$x, y= input$y)) +
    #       geom_point()
    #   )
    # }
  })
  
  observeEvent(input$selectDataset, {
    shinyjs::hideElement(id = "viewSampleData", animType = 'slide', time = 0.2 )
    
    shinyjs::hideElement(id = "edaColumnsTab", animType = 'slide', time = 0.2 )
    
    shinyjs::hideElement(id = "edaPlotTab", animType = 'slide', time = 0.2 )
    
    
    if(input$selectDataset == "Upload Dataset"){
      shinyjs::hideElement(id = "simulateOptions", animType = 'slide', time = 0.2 )
      shinyjs::showElement(id = "uploadOptions", animType = 'slide', time = 0.2 )
    }
    else{
      shinyjs::hideElement(id = "uploadOptions", animType = 'slide', time = 0.2 )
      shinyjs::showElement(id = "simulateOptions", animType = 'slide', time = 0.2 )
    }
    
  })
  
}
shinyApp(ui = ui, server = server)

