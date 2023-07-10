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
shinyApp(ui = htmlTemplate("C:/자료실/서다현/R 스크립트/shiny/www/index.html"), server)