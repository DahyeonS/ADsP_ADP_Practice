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