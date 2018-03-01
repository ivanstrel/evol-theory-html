library(shiny)
library(rmarkdown)

source("functions/SimDrift.r")
source("functions/BotNeckSqr.r")
source("functions/BotNeckSim.r")
source("functions/NatSelect.r")
source("functions/HWCommonCase.r")
source("functions/HW_NatSelect.r")
source("functions/HW_inbreeding.r")
source("functions/HW_isolation.r")

server <- function(input, output, session) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Drift plot output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$Drift_plot <- renderPlot({
    if (input$driftPlotButton == 0)
    return()
    
    isolate({
      NGen = input$NGen
      Freq = input$Freq
      PopSize = input$PopSize
      PopN = input$PopN
    })
    SimDrift(NGen = NGen, Freq = Freq, PopSize = PopSize, PopN = PopN, mode = "plot")
  }
  )

  output$Drift_hist <- renderPlot({
    if (input$driftHistButton == 0)
    return()
    
    isolate({
      NGen1 = input$NGen1
      Freq1 = input$Freq1
      PopSize1 = input$PopSize1
    })
    SimDrift(NGen = NGen1, Freq = Freq1, PopSize = PopSize1, PopN = 300, mode = "hist")
  }
  )
}


shinyApp(ui = htmlTemplate("www/index.html"), server)