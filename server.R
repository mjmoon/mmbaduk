#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# Shiny server                                              #
# rsconnect::deployApp(".") to deploy                       #
#############################################################
shinyServer(function(input, output, session) {
  source("mmbaduk.shiny.R", local = TRUE)
  vars = reactiveValues(counter = 0)
  output$status <- renderText({"Click start"})
  endplay()
  waitplay()
  resetscores(output)
  printinst(output)
  
  observeEvent(input$start, {
    startplay(output)
    initgame(input$mode, input$size, input$komi, output, session)
    playrandom(output, session)
    inplay()
  })
  
  observeEvent(input$clickpos, {
    mmbaduk$on <<- getxy(input$clickpos)
    plotpos(mmbaduk$on, output)
  })
  
  observeEvent(input$play, {
    waitplay()
    playuser(mmbaduk$on, output, session)
    playrandom(output, session)
    inplay()
  })
  
  observeEvent(input$skip, {
    waitplay()
    playmove(c(0,0), output)
    if(mmbaduk$p == 2) {
      waitplay()
      quitplay(output)
      printscores(output)
      endplay()
    } else {
      playrandom(output, session)
      inplay()
      if(mmbaduk$p == 2) {
        waitplay()
        quitplay(output)
        printscores(output)
        endplay()
      }
    }
  })
  
  observeEvent(input$printinst, {
    isolate({
      vars$counter <- vars$counter + 1
    })
    if(vars$counter%%2 == 1){
      shinyjs::hide("game")
      shinyjs::show("inst")  
    } else {
      shinyjs::hide("inst")
      shinyjs::show("game")  
    }
  })
  
  label <- reactive({
    if(is.null(input$printinst))
      return("Show instruction")
    else{
      if(vars$counter%%2 == 1){
        return("Show board")
      } else {
        return("Show instruction")
      }  
    }
  })
  
  observeEvent(input$quit, {
    waitplay()
    quitplay(output)
    output$status <- renderText({
      paste(ifelse(mmbaduk$i %% 2 == 0,
                   "Black", "White"), 
            "Wins")
    })
    printscores(output)
    endplay()
  })
})