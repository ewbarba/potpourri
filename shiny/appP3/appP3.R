library(shiny)
library(wesanderson)

data("faithful") #  Built-in eruption dataset
head(faithful)

ui = fluidPage(titlePanel("Old Faithful Eruptions"),
               sidebarLayout(sidebarPanel(
                 sliderInput(
                   inputId = "bins",
                   #  Bins for our inputs
                   label = "Number of bins:",
                   min = 5,
                   max = 20,
                   value = 10
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "distPlot"),
                 #  We want to output a histogram
                 img(
                   src = "bison.jpg",
                   height = 170,
                   width = 296
                 ),
                 p("bison beware")
               )))

print (ui)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <-
      seq(min(x), max(x), length.out = input$bins + 1) # Hint: Style rule #5
    hist(
      x,
      breaks = bins,
      col = wes_palette("GrandBudapest"),
      border = "white",
      xlab = "Waiting time to next eruption (mins)",
      main = "Histogram of waiting times"
    )
  })
  
}

shinyApp(ui = ui, server = server)