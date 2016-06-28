#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# Shiny server                                              #
# rsconnect::deployApp(".") to deploy                       #
#############################################################
source("mmbaduk.shiny.R")

shinyServer(function(input, output, session) {
  endplay()
  waitplay()
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
    initgame(input$mode, input$size, input$komi, output, session)
    quitplay(output)
    printscores(output)
    endplay()
  })
})