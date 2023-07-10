library(shiny)
runExample("01_hello")

# hello_shiny(ui.R)
shinyUI(pageWithSidebar(
  headerPanel("Hello shiny!"), # 타이틀
  sidebarPanel(
    sliderInput("obs", #inputID
                "Number of observations: ", # 컴포넌트
                min = 1, # 최소값
                max = 1800, # 최대값
                value = 500)), # 기본값
  mainPanel(
    plotOutput("distPlot")) # 플롯 이름
))

# hello_shiny(server.R)
shinyServer(function(input, output){ # input과 output에 대한 함수
  output$distPlot = renderPlot({ # 랜덤 값을 dist 변수에 넣어 출력
    dist = rnorm(input$obs)
    hist(dist)
  })
})

setwd("C:/shiny") # 디렉토리
runApp()

# 1. Input과 Output 사용
# ui.R
shinyUI(pageWithSidebar(
  headerPanel("Miles Per Gallon"),
  sidebarPanel(
    selectInput("variable", "variable: ",
                list("Cylinders" = "cyl",
                     "Transmission" = "am",
                     "Geers" = "gear")),
    checkboxInput("outliers", "Show outliers", FALSE)
  ),
  mainPanel()
))
# 수정
shinyUI(pageWithSidebar(
  headerPanel("Miles Per Gallon"),
  sidebarPanel(
    selectInput("variable", "variable: ",
                list("Cylinders" = "cyl",
                     "Transmission" = "am",
                     "Geers" = "gear")),
    checkboxInput("outliers", "Show outliers", FALSE)
  ),
  mainPanel(h3(textOutput("caption")),
            plotOutput("mpgPlot")
            )
))
library(datasets)
mpgData = mtcars
# server.R
mpgData$am = factor(mpgData$am, labels = c("Automatic", "Manual"))
shinyServer(function(input, output){
  formulaText = reactive({
    paste("mpg ~", input$variable)
  })
  output$caption = renderText({
    formulaText()
  })
  output$mpgPlot = renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers)
  })
})
runApp()

# 2. Slider
# ui.R
shinyUI(pageWithSidebar(
  headerPanel("Sliders"),
  sidebarPanel(
    sliderInput("integer", "integer: ",
                min = 0, max = 1000, value = 500),
    sliderInput("decimal", "Decimal: ",
                min = 0, max = 1, value = .5, step = .1),
    sliderInput("range", "Range: ",
                min = 1, max = 1000, value = c(200, 500)),
    sliderInput("format", "Custom Format: ",
                min = 0, max = 10000, value = 0, step = 2500,
                animate = TRUE),
    sliderInput("animation", "Looping animation: ", 1, 2000, 1,
                animate = animationOptions(interval = 300, loop = T))
  ),
  mainPanel(
    tableOutput("values")
  )
))
# server.R
shinyServer(function(input, output){
  SliderValues = reactive({
    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = F)
  })
  output$values = renderTable({
    SliderValues()
  })
})
runApp()

# 3. Tabsets
# ui.R
shinyUI(pageWithSidebar(
  headerPanel("Tabsets"),
  sidebarPanel(
    radioButtons("dist", "Distribution type: ",
                 list("Normal" = "norm",
                      "Uniform" = "unif",
                      "Log-normal" = "lnorm",
                      "Expotential" = "exp")),
    br(),
    sliderInput("n",
                "Number of observations :",
                value = 500,
                min = 1,
                max = 1000)),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("table"))
    )
  )
))
# server.R
shinyServer(function(input, output){
  data <- reactive({
    dist <- switch(input$dist,
                  norm = rnorm,
                  unif = runif,
                  lnorm = rlnorm,
                  exp = rexp,
                  rnorm)
    dist(input$n)
  })
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(data(),
         main = paste("r", dist, "(", n, ")", sep = ""))
  })
  output$summary <- renderPrint({
    summary(data())
  })
})
runApp()

# 4. dataTable
runApp(list(
  ui = basicPage(
    h2("The mtcars data"),
    dataTableOutput("mytable")
  ),
  server = function(input, output){
    output$mytable = renderDataTable({
      mtcars
    })
  }
))
# ui.R
library(ggplot2)
shinyUI(pageWithSidebar(
  headerPanel("Examples of DataTables"),
  sidebarPanel(
    checkboxGroupInput("show_vars", "Columns in diamonds to show:", names(diamonds),
                       selected = names(diamonds)),
    helpText("For the diamonds data, we can select variables to show in the table;
               for the mtcars example, we use bSortClasses = TRUE so that sorted 
               columns are colored since they have special CSS classes attached;
               for the iris data, we customize the length menu so we can display 
               5 rows per page.")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("diamonds",
               dataTableOutput("mytable1")),
      tabPanel("mtcars",
               dataTableOutput("mytable2")),
      tabPanel("iris",
               dataTableOutput("mytable3"))
    )
  )
))
# server.R
shinyServer(function(input, output){
  output$mytable1 = renderDataTable({
    library(ggplot2)
    diamonds[, input$show_vars, drop = F]
  })
  output$mytable2 = renderDataTable({
    mtcars
  }, options = list(bSortClasses = T))
  output$mytable3 = renderDataTable({
    iris
  }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength =5))
})
runApp()

# 5. MoreWidget
# ui.R
shinyUI(pageWithSidebar(
  headerPanel("More Widgets"),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset: ",
                choices = c("rock", "pressure", "cars")),
    numericInput("obs", "Number of observations to view: ", 10),
    helpText("Note: While the data view will show only the specified",
             "number of observations, the summary will still be based",
             "on the full dataset."),
    submitButton("Update View")
  ),
  mainPanel(
    h4("Summary"),
    verbatimTextOutput("summary"),
    h4("Observations"),
    tableOutput("view"))
))
# server.R
shinyServer(function(input, output){
  datasetInput = reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  output$summary = renderPrint({
    dataset = datasetInput()
    summary(dataset)
  })
  output$view = renderTable({
    head(datasetInput(), n = input$obs)
  })
})
runApp()

# 6. uploading_Files
shinyUI(pageWithSidebar(
  headerPanel("CSV viewer"),
  sidebarPanel(
    fileInput("file1", "Choose CSV File",
              accept = c("text/csv", "tect/comma-separated-values,text/plain", ".csv")),
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    radioButtons("sep", "Separator",
                 c(Comma = ",",
                   Semicolon = ";",
                   Tab = "\t"),
                 "Comma"),
    radioButtons("quote", "Quote",
                 c(None = "",
                   "Double Quote" = '"',
                   "Single Quote" = "'"),
                 "Double Quote")
  ),
  mainPanel(
    tableOutput("contents")
  )
))
# server.R
shinyServer(function(input, output){
  output$contents = renderTable({
    inFile = input$file1
    if(is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
  })
})
runApp()

# 7. HTML_ui
# server.R
server = function(input, output) {
  d = reactive({
    dist = switch(input$dist,
                  norm = rnorm,
                  unif = runif,
                  lnorm = rlnorm,
                  exp = rexp,
                  rnorm)
    dist(input$n)
  })
  output$plot = renderPlot({
    dist = input$dist
    n = input$n
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  output$summary = renderPrint({
    summary(d())
  })
  output$table = renderTable({
    head(data.frame(x = d()))
  })
}
shinyApp(ui = htmlTemplate("C:/index.html"), server) # 디렉토리
runApp()
