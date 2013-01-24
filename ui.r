library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Choose some telemetry sites"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    checkboxGroupInput("dataset", "Choose telemetry sites:", 
                choices = c('Hurricane Ridge', 'Mount Baker', 'Washington Pass', 'Mazama', 'Mission Ridge', 'Stevens 2', 'Stevens Ski', 'Alpental', 'Snoqualmie')),
    uiOutput("datasetControls"),
    checkboxInput('smooth', 'Kill spikes?', value=TRUE),
      wellPanel(
    p('Source code on ', a('github', href='https://github.com/amcdavid/nwac_tool'))
    )
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
 mainPanel(
    conditionalPanel(condition = "output.hastemp_precip",
                     h4('Temp/Precip'),
      plotOutput(outputId = "plot_temp")),
 
    conditionalPanel(condition = "output.haswind",
                     h4('Wind'),
      plotOutput(outputId = "plot_wind")),
 
    conditionalPanel(condition = "output.hassnowbase",
                     h4('Snow'),
       plotOutput(outputId = "plot_snow")),

   conditionalPanel(condition = "output.hasother",
                     h4('Other'),
       plotOutput(outputId = "plot_other"))
 
    ##, conditionalPanel(condition = "output.rh",
    ##   plotOutput(outputId = "plot_rh"))
 
  )
  

))
