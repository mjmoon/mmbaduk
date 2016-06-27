#############################################################
# mmbaduk v0.1 - Baduk (Go) player by michael moon          #
#                                                           #
# Shiny server                                              #
#                                                           #
#############################################################
source("mmbaduk.shiny.R")

shinyServer(function(input, output, session) {
  
  output$game <- renderPlot({
    plotboard(19)
  })
  output$status <- renderText({
    "Click start"
  })
  waitplay()
  resetscores(output)
  
  observeEvent(input$start, {
    startplay()
    mmbaduk <<- initgame(input$mode, input$size, input$komi)
    output$game <- renderPlot({
      plotgame(mmbaduk$play, mmbaduk$size, mmbaduk$i, plotscrs = FALSE)
    })
    output$status <- renderText({
      paste("Black's turn")
    })
    playrandom(output)
    inplay()
    resetscores(output)
    resetinput(session)
  })
  
  observeEvent(input$play, {
    waitplay()
    resetinput(session)
    playuser(input$pos, output)
    playrandom(output)
    inplay()
  })
  
  observeEvent(input$skip, {
    waitplay()
    playmove(c(0,0), output)
    playrandom(output)
    inplay()
  })
  
  observeEvent(input$quit, {
    waitplay()
    resetinput(session)
    
    output$game <- renderPlot({
      plotterr(mmbaduk$play, mmbaduk$size, plotscrs = FALSE)
    })
    
    output$status <- renderText({
      paste(ifelse(winner(mmbaduk$play, mmbaduk$komi) == 1,
                   "Black", "White"), 
            "Wins")
    })
    
    printscores(output)
    endplay()
    mmbaduk$i <<- mmbaduk$i + 1
  })
})