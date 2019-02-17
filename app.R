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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bottle neck plot output 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$bot_neck_sqr_plot <- renderPlot({
    if (input$bot_neck_sqr_button == 0)
    return()

    isolate({
      p_a <- input$P_a
      p_b <- input$P_b
      p_c <- input$P_c
      p_d <- input$P_d

      pop_size <- input$PopSize2
      bot_size <- input$BotSize
    })
    bot_neck_sqr_tab <<- BotNeckSqr(pop_size = pop_size, bot_size = bot_size,
                                    p_a = p_a, p_b = p_b, p_c = p_c, p_d = p_d)
  }
  )

  output$bot_neck_sqr_tab <- renderTable({
    if (input$bot_neck_sqr_button == 0)
    return()

    isolate({
      p_a <- input$P_a
      p_b <- input$P_b
      p_c <- input$P_c
      p_d <- input$P_d

      pop_size <- input$PopSize2
      bot_size <- input$BotSize
    })
    bot_neck_sqr_tab
  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bottle neck plot output 2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$bot_neck_sim_plot <- renderPlot({
    if (input$bot_neck_sim_button == 0)
    return()

    isolate({
      freq <- input$Freq2
      pop_size <- input$PopSize3
      bot_size <- input$BotSize1
    })
    BotNeckSim(pop_size = pop_size, bot_size = bot_size,
               f_a = freq, f_b = freq, f_c = freq, f_d = freq, f_e = freq)
  }
  )
}


shinyApp(ui = htmlTemplate("www/index.html"), server)