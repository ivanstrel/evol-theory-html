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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Natural selection plot output 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nat_select_plot <- renderPlot({
    if (input$nat_select_plot_button == 0)
    return()

    isolate({
      n_gen <- input$NGen2
      pa <- input$PA
      pop_size <- input$PopSize4
      pop_n <- input$PopN1
      fAA <- input$fAA
      fAa <- input$fAa
      faa <- input$faa
    })
    nat_select(n_gen = n_gen, pa = pa, pop_size = pop_size, pop_n = pop_n, fAA = fAA, fAa = fAa, faa = faa)
  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Plot 4.1: Hardy Wainberg common case
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$hw_common_plot <- renderPlot({
    if (input$hw_common_plot_button == 0)
    return()

    isolate({
      pop_size <- input$pop_size_5
    })
    hw_common_case(pop_size = pop_size)
  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot 4.2: Hardy Wainberg natural selection
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$hw_nat_select_plot <- renderPlot({
    if (input$hw_nat_select_plot_button == 0)
    return()

    isolate({
      n_gen <- input$n_gen_2
      fp <- input$pa_1
      pop_size <- input$pop_size_6
      fAA <- input$fAA_1
      fAa <- input$fAa_1
      faa <- input$faa_1
    })
    hw_nat_select(n_gen = n_gen, fp = fp, pop_size = pop_size, fAA = fAA, fAa = fAa, faa = faa)
  }
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot 5.1: Hardy Wainberg inbreeding
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$hw_inbrieeding_plot <- renderPlot({
    if (input$hw_inbrieeding_plot_button == 0)
    return()

    isolate({
      self_poll_ratio <- input$self_poll_ratio
    })
    hw_inbrieeding_tab <<- hw_inbrieeding(self_poll_ratio = self_poll_ratio)
  }
  )

  output$hw_inbrieeding_tab <- renderTable({
    if (input$hw_inbrieeding_plot_button == 0)
    return()
    hw_inbrieeding_tab
  },
  rownames = TRUE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot 6.1: Hardy Wainberg inbreeding
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$hw_isolate_plot <- renderPlot({
    if (input$hw_isolate_plot_button == 0)
    return()

    isolate({
      fp <- input$fp_t6
      isolate <- input$brink
    })
    hw_isolate_tab <<- hw_isolateion(fp = fp, isolate = isolate)
  }
  )

  output$hw_isolate_tab <- renderTable({
    if (input$hw_isolate_plot_button == 0)
    return()
    hw_isolate_tab
  },
  rownames = TRUE
  )
}


shinyApp(ui = htmlTemplate("www/index.html"), server)