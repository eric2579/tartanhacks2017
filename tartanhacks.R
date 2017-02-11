library(shiny)
library(datasets)
library(scatterplot3d)
library(knitr)

ui <- shinyUI(fluidPage(
  titlePanel("The Fire Ferrets 2.0 (created by Anonymous)"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose A File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv','.txt')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Summary",
             verbatimTextOutput("summary")
    ),
    tabPanel("Plots",
             pageWithSidebar(
               headerPanel('Plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('x', 'X Variable', ""),
                 selectInput('y', 'Y Variable (Optional)', "", selected = ""),
                 selectInput('x2', 'X2 Variable (Optional)', "", selected = "")
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    ),
    tabPanel("Assumptions",
             pageWithSidebar(
               headerPanel('Linear Regression Assumptions'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
               ),
               mainPanel(
                 plotOutput('MyPlot1'),
                 plotOutput("MyPlot2"),
                 plotOutput("MyPlot3"),
                 plotOutput("MyPlot4")
               )
             )
    )
    
  )
)
)

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    if (substr(inFile, nchar(inFile)-3, nchar(inFile)) == ".csv") {
      df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                     quote = input$quote)
    }
    else {
      df <- read.table(inFile$datapath)
    }
    
    updateSelectInput(session, inputId = 'x', label = 'X Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'y', label = 'Y Variable',
                      choices = c(names(df), "n/a"), selected = "n/a")
    updateSelectInput(session, inputId = 'x2', label = 'X2 Variable',
                      choices = c(names(df), "n/a"), selected = "n/a")
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$summary <- renderPrint({
    summary(data())
    
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$MyPlot <- renderPlot({

    if (input$y == "n/a" && input$x2 == "n/a") {
      x.histogram <- data()[, input$x]
      bins <- nrow(data())
      hist(x.histogram, breaks = bins, col = 'gray', border = 'white')
    }
    
    if (input$y != "n/a" && input$x2 == "n/a") {
      xvar <- data()[, input$x]
      yvar <- data()[, input$y]
      plot(xvar, yvar)
      
      abline(lm(yvar ~ xvar), lty = 2, col = "red", lwd = 2)
    }
    
    else {
      if (input$y != "n/a" && input$x2 != "n/a") {
        xvar <- data()[, input$x]
        yvar <- data()[, input$y]
        x2var <- data()[, input$x2]
        s3d <- scatterplot3d(xvar, yvar, x2var, main = "3D Scatterplot")
        fit <- lm(yvar ~ xvar + x2var)
        s3d$plane3d(fit)
      }
    }
    
  })
  
  
  output$MyPlot1 <- renderPlot({

    x <- data()[, input$xcol]
    y <- data()[, input$ycol]
    line <- lm(y~x)
    plot(line$fit,line$res,main="Residuals vs Fitted Values")
    abline(a = 0, b = 0, col = "red", lty = 2, lwd = 2)
  })
  
  output$MyPlot2 <- renderPlot({

    x <- data()[, input$xcol]
    y <- data()[, input$ycol]
    line <- lm(y~x)
    plot(na.omit(x),line$res,main="Model Residuals: Expectation Zero, Constant Variance")
    abline(a = 0, b = 0, col = "red", lty = 2, lwd = 2)
  })
  
  output$MyPlot3 <- renderPlot({

    x <- data()[, input$xcol]
    y <- data()[, input$ycol]
    line <- lm(y~x)
    plot(line$res,main="Model Residuals: Independence")
    abline(a = 0, b = 0, col = "red", lty = 2, lwd = 2)
  })
  
  output$MyPlot4 <- renderPlot({

    x <- data()[, input$xcol]
    y <- data()[, input$ycol]
    line <- lm(y~x)
    qqnorm(line$res,main="QQ Plot")
    qqline(line$res, col = "red", lwd = 2)
    
  })
})

shinyApp(ui, server)