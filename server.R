#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# Shiny server                                              #
# rsconnect::deployApp(".") to deploy                       #
#############################################################
shinyServer(function(input, output, session) {
  source("mmbaduk.shiny.R", local = TRUE)
  endplay()
  waitplay()
  resetscores(output)
  shinyjs::disable("lastplay")
  shinyjs::disable("currplay")
  printinst(output)
  output$status <- renderText({"Click start"})
  
  observeEvent(input$start, {
    startplay()
    initgame(input$mode, input$size, input$komi, output, session)
    playrandom(output, session)
    inplay()
  })
  
  observeEvent(input$clickpos, {
    mmbaduk$on <<- getxy(input$clickpos)
    shinyjs::enable("currplay")
    updateTextInput(session, "currplay", value = printpos(mmbaduk$on))
    shinyjs::disable("currplay")
    plotpos(mmbaduk$on, output)
  })
  
  observeEvent(input$play, {
    waitplay()
    playuser(mmbaduk$on, output)
    shinyjs::enable("lastplay")
    updateTextInput(session, "lastplay", value = input$currplay)
    shinyjs::disable("lastplay")
    shinyjs::enable("currplay")
    updateTextInput(session, "currplay", value = "")
    shinyjs::disable("currplay")
    playrandom(output, session)
    inplay()
  })
  
  observeEvent(input$skip, {
    waitplay()
    playmove(c(0,0), output)
    shinyjs::enable("lastplay")
    updateTextInput(session, "lastplay", value = "Skipped")
    shinyjs::disable("lastplay")
    playrandom(output, session)
    inplay()
  })
  
  observeEvent(input$quit, {
    waitplay()
    quitplay(output)
    resetgame(output, session)
    printscores(output)
    endplay()
  })
})