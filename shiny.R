## 4/3: Collaboration mini project I: shiny
## shiny tutorial adapted from various sites, primarily http://shiny.rstudio.com/tutorial/ and https://www.r-bloggers.com/building-shiny-apps-an-interactive-tutorial/ 
## Package developed by: Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). 
## shiny: Web Application Framework for R. R package version 1.0.5. https://CRAN.R-project.org/package=shiny

### PART I ###

### Installation ###

install.packages('shiny')
library(shiny)
#  Launch a pre-built example app in R Studio and (if you want) your web browser
runExample('11_timer') #  All good? Keep going...

### Create a shiny app template (an empty shiny app), method #1 ###

# DO NOT!! run the following script- instead, copy and paste into a new R script. 
# It should be saved as: "appP2.R" or "appP3.R" or "blah.R", whatever. 
# It's critical that app.R is saved in its own special folder separate from other .R files 
# (unless those other files are used by your app). Don't confuse Shiny!

library(shiny)
ui <- fluidPage()
server <- function(input, output, session) {}
shinyApp(ui = ui, server = server)

# After saving this ^, R Studio will recognize that this is a Shiny app.
# Green triangle + Run  will appear on the righthand side of your app.R script in R Studio. Click it.
# Not much happens, as it's an empty app, but you'll see something like "Listening on...".
# Click the red button or Escape to exit.
# You can also run the app with runApp("appP3.R").

### Create a shiny app template, method #2 ###

# You can also create a new Shiny app using R Studio's menu. 
# File > New Project > New Directory > Shiny Web Application
# RStudio will create a new folder and initialize a simple Shiny app there.
# There won't be an "app.R" file, but you will see ui.R (all code assigned to the ui object, as in the script above, goes here) and server.R (same, pertaining to the server object).

# I prefer the single-file method #1, but you do you.

# From Shiny's developer site, here is a list of the types of built-in examples:
runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer

### PART II ###

# We will also be using these libraries:

library(dplyr)
library(ggplot2)

bcl <- read.csv('bclData.csv') #  Briefly explore the data in your R Studio console.
# Don't forget to set your wd so R can find this file.

## Read in the data

# In 'appP2.R' (or whatever you named it), type the following code, do NOT just run it here- won't work:
# Place this line in your app as the second line, just after calling the library. 

bcl <- read.csv('bclData.csv', stringsAsFactors=F) #  Hint: Style rule #5


## Ensure the app can read your data

# Replace the entire server object assignment line with the following:
  
server = function(input, output, session) { #  Hint: Style rule #8
  print(str(bcl))
  }


## Add elements to the UI

# Here, we'll render the text by placing some strings inside fluidPage().

fluidPage("BC Liquor Store", "prices")

# Save and run the app. Looking good? Add a few more strings (aka, column headers from the worksheet).
# Run the app again.

## Format the text in the UI

fluidPage(
h1("BC liquor product app"), "BC", "Liquor", br(), "Store", strong("prices") #  Hint: Style rule #4
)

?builder #  Experiment with some of the tags listed here (i.e., replace h1 with h4, etc.) and run the app.

# Overwrite your fluidPage() text with the following:

fluidPage(
  titlePanel("BC Liquor Store prices")
)

## Add some structure

# At this point, you may notice the app window looks a little messy. sidebarLayout() will help.
# Inputs will be on the left, results will be in the main panel.
# Paste this inside fluidPage(), after titlePanel(). Don't forget a , after titlePanel(). 

sidebarLayout(
  sidebarPanel("our inputs will go here"),
mainPanel("the results will go here")
  )

# You can also control the minutiae of the display in fluidPage(). 

?column

# Replace fluidPage() with the following:

fluidRow(
  titlePanel("BC Liquor Store prices"),
  column(width = 4,
         column(width = 3, offset = 2,
                "Another formatting example"
      )
    )
  )

# Add some UI into sidebarPanel and mainPanel, run the app, notice the changes.
# All UI functions are HTML wrappers in diguise. Shiny lets you run these without prior knowledge of HTML.
# In your R console, run the following:

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"), 
  sidebarLayout(
    sidebarPanel("our inputs will go here"),
    mainPanel("the output will go here")
  )
)

print (ui) #  Hint: Style rule #5

# See how much uglier HTML is compared to R?
# Go ahead and paste this ^ for ui, overwriting the previous code.


## Add a numbers widget

?sliderInput

# Replace your sidebarPanel() with the following code. Keep mainPanel() as-is.
# These values are from the price data in the .csv.
sidebarPanel(sliderInput("priceInput", "Price", 
  min = 0, max = 100;value = c(25, 40), pre = "$"), #  Hint: Style rule #9
  
# Hold off on running the app as we continue to add stuff here- it won't work yet.


## Add buttons

?radioButtons

# Add this input code inside sidebarPanel(), after the previous input:

radioButtons("typeInput", "Product type",
  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
  selected = "WINE"),

# Now we want to choose the data we display in the app.

# After the radioButtions() code, paste:

uiOutput("countryOutput")), #  This helps set up for reactive programming. See ?uiOutput


## Output a plot and a table summary of results

# Replace mainPanel with the following text: 
mainPanel(
  plotOutput("cool_plot"), #  Hint: Style rule #2
  br(), br(), #  Coding in some breaks
  tableOutput("results")
    )
  )
)

# ^ What did we do here? We want to show results in a plot and a table.
# Now that we've coded two output types, we need to tell Shiny what kind of graphics to display.

?renderPlot()

## Reactivity in Shiny

# Create a list of selected inputs
# Replace your server assignment line with this:

server <- function(input, output, session) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)), selected = "CANADA")
  })

# Why did we do ^this? 
# We have to 1) assign our output object to a list (output$countryOutput) and 
# 2) build the object with render* (<- a render function).
# You don't have to deeply understand these rules, but Shiny requires you to follow this protocol.
  
# Using library(dplyr) to filter the data based on price, type, country (our inputs)
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL) #  If country input doesn't = CANADA, don't include
    }
    bcl %>%
      filter(Price >= input$priceInput[1], #  our minimum value
             Price <= input$priceInput[2], #  our maximum value
             Type == input$typeInput,
             Country == input$countryInput
  )
})
  
# Time to add in ggplot():
  
output$coolplot <- renderPlot({
  if (is.null(filtered())) {
    return() #  If our filtered list has nulls, don't include those
  }
  ggplot(filtered(), aes(Alcohol_Content)) +
    geom_histogram()
})

# We also want to add code for our table.
# Paste the following after the previous code:

output$results <- renderTable({
  filtered <-
    bcl %>%
    filter(Price >= input$priceInput[1],
           Price <= input$priceInput[2],
           Type == input$typeInput,
           Country == input$countryInput)
  filtered
  })
}

# Run the app and see how it looks*
# We've just created a dependency tree.
# This tells Shiny where to look/when to react to variable value changes.

# * If it fails to run, check your code against mine:

library(dplyr)
library(ggplot2)
library(shiny)

bcl <- read.csv("bclData.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"), 
  sidebarLayout(
    sidebarPanel(sliderInput("priceInput", "Price", 
                             min = 0, max = 100, value = c(25, 40), pre = "$"),
                 radioButtons("typeInput", "Product type",
                              choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                              selected = "WINE"),
                 uiOutput("countryOutput")), 
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)), selected = "CANADA")
}) 
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    } # Hint: Style rule #6
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
  )
})
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
})
  
  
  output$results <- renderTable({
    filtered <-
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
    filtered
  })
}


shinyApp(ui = ui, server = server)

# ^ All of this should be in "app.R".
# Great job- return to the worksheet.

### PART III ###

data("faithful") #  Built-in eruption dataset
head(faithful)

## Add to the ui (the user interface object)
# Let's start by making the ui variable fancier. Replace ui <- fluidPage() with:

ui = fluidPage( #  Hint: Style rule #8
  titlePanel("Old Faithful Eruptions"), #  The title of our app goes here
  sidebarLayout( #  Sidebar layout with spaces for inputs and outputs
    sidebarPanel( #  for inputs
      sliderInput(inputId = "bins", #  Bins for our inputs
                  label = "Number of bins:",
                  min = 5,
                  max = 20,
                  value = 10)
  ),
    mainPanel( # for outputs
      plotOutput(outputId = "distPlot") #  We want to output a histogram, eventually
    )
  )
)

# Run the app. Ooh, nice slider. 
# Now paste this code ^ into your R console, and then paste this into the console (not the app):

print (ui) #  Hint: Style rule #5

# By printing the ui here, we can see how ugly HTML would be to type raw. Thanks, Shiny!

## Reactivity in Shiny: add to the server function

# Let's feed the histogram our data.
# Replace server <- function(input, output, session) {} with the following code:

server <- function(input, output) { 
  # Define server logic required to draw a histogram
  # Calling renderPlot tells Shiny this plot is
  # 1. reactive, and will auto re-execute when inputs (input$bins) change
  # 2. the output type we want
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <-seq(min(x), max(x), length.out = input$bins + 1) # Hint: Style rule #5
    hist(x, breaks = bins, col = heat.colors(10, alpha = 1), border = "white",
         xlab = "Waiting time to next eruption (mins)",
         main = "Histogram of waiting times")
  })} # Hint: Style rule #6

# Make your edits, save, and run the app again. Play with your slider. Pretty.

# heat.colors() can be adjusted to make the outcome more sensible. Try a different number.
# Hint for above = type ?heat.colors


## Improving the ui

# Let's mess with the text. Replace your mainPanel() assignment with:

mainPanel(
  plotOutput(outputId = "distPlot"), 
  p("p creates a paragraph of text."),
  p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
  strong("strong() bolds your text."),
  em("em() creates italicized (i.e, EMphasized) text.")
  
# Run the app, see how your commands added and formatted your text.

## Add an image

# To add an image, Shiny requires that you
  # 1. Create a folder in your app repo named "www"
  # 2. Place the image in this folder with nothing else added

# I am using a scenic bison-filled photo for this tutorial.

# Replace your mainPanel() with:
mainPanel(
  plotOutput(outputId = "distPlot"), #  We want to output a histogram
  img(src = "bison.jpg", height = 170, width = 296),
  p("bison beware")
  
## Check your code against mine:

library(shiny)

ui <- fluidPage( 
  titlePanel("Old Faithful Eruptions"), 
  sidebarLayout( # Sidebar layout with spaces for inputs and outputs
    sidebarPanel( # for inputs
      sliderInput(inputId = "bins", 
        label = "Number of bins:",
min = 5, #  Hint: Style rule #4
                  max = 20,
                  value = 10)
    ),
    mainPanel( # for outputs
      plotOutput(outputId = "distPlot"), 
      img(src = "bison.jpg", height = 170, width = 296),
      p("bison beware")
    )
  )
)

server <- function(input, output, session) { 
  
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = heat.colors(20, alpha = 1), border = "white",
      xlab = "Waiting time to next eruption (mins)",
      main = "Histogram of waiting times")
  })
}
shinyApp(ui = ui, server = server)

# ^ All of this should be in your "app.R" file.
# Return to the worksheet.