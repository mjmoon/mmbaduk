#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# game interface (Shiny)                                    #
#                                                           #
#############################################################
source("mmbaduk.R")
mmbaduk <- NULL
#############################################################
# initgame: initialize a game                               #
#           return a list of an empty game state, mode,     #
#           and komi                                        #
#   dep: initboard                                          #
#                                                           #
# mode 1: player vs. player                                 #
# mode 2: player vs. computer                               #
# mode 3: computer vs. player                               #
#############################################################
initgame <- function(mode, size, komi, output, session){
  mmbaduk <<- list(play = initboard(as.numeric(size)),
                   size = as.numeric(size),
                   mode = as.numeric(mode), 
                   komi = as.numeric(komi),
                   i = 1)
  
  output$game <- renderPlot({
    plotgame(mmbaduk$play, mmbaduk$size, mmbaduk$i, plotscrs = FALSE)
  })
  output$status <- renderText({
    paste("Black's turn")
  })
  
  output$on.b <- renderText({0})
  output$on.w <- renderText({0})
  output$terr.b <- renderText({0})
  output$terr.w <- renderText({0})
  output$cap.b <- renderText({0})
  output$cap.w <- renderText({0})
  
  shinyjs::enable("lastplay")
  updateTextInput(session, "lastplay", value = "")
  shinyjs::disable("lastplay")
  shinyjs::enable("currplay")
  updateTextInput(session, "currplay", value = "")
  shinyjs::disable("currplay")
  return(NULL)
}

#############################################################
# playuser: play at clicked position if among valid moves   #
#   dep: playmove; validmoves                               #
#############################################################
playuser <- function(pos, output){
  if(any(apply(validmoves(), 1, 
               function(x, want) isTRUE(all.equal(x, want)), 
               pos))
  ) {
    playmove(pos, output)
  }
  else output$status <- renderText({"Invalid move"})
  return(NULL)
}

#############################################################
# playrandom: play a random move among valid moves          #
#   dep: playmove; validmoves                               #
#############################################################
playrandom <- function(output, session){
  if((mmbaduk$mode == 2 & mmbaduk$i %% 2 == 0) |
     (mmbaduk$mode == 3 & mmbaduk$i %% 2 == 1) ) {
    vmoves <- validmoves()
    if(nrow(vmoves) > 0){
      randi <- floor(runif(1,min = 1, max = nrow(vmoves)))
      on <- vmoves[randi,]
      shinyjs::enable("lastplay")
      updateTextInput(session, "lastplay", value = printpos(on))
      shinyjs::disable("lastplay")
    }
    else on <- c(0,0)
    playmove(on, output)
  }
  return(NULL)
}

#############################################################
# playmove: play a given move and plot the resulting board  #
#           pass if the given move is c(0,0)                #
#   dep: playstone; plotgame                                #
#############################################################
playmove <- function(on, output){
  stone <- (mmbaduk$i+1) %% 2 +1
  if(all(on > 0)){
    mmbaduk$play <<- playstone(stone, on, mmbaduk$play, mmbaduk$i, mmbaduk$size) 
    output$game <- renderPlot({
      plotgame(mmbaduk$play, mmbaduk$size, mmbaduk$i, plotscrs = FALSE)
    })
    printscores(output)
  }
  mmbaduk$i <<- mmbaduk$i + 1
  output$status <- renderText({
    paste(ifelse(mmbaduk$i %% 2 == 1, "Black", "White"), "'s turn", sep = "")
  })
  return(NULL)
}

#############################################################
# quitplay: end the current game and plot the territories   #
#   dep: playterr; winner                                   #
#############################################################
quitplay <- function(output){
  output$game <- renderPlot({
    plotterr(mmbaduk$play, mmbaduk$size, plotscrs = FALSE)
  })
  
  output$status <- renderText({
    paste(ifelse(winner(mmbaduk$play, mmbaduk$komi) == 1,
                 "Black", "White"), 
          "Wins")
  })
  return(NULL)
}

#############################################################
# validmoves: return a matrix of valid move coordinates     #
#############################################################
validmoves <- function(){
  board <- subset(mmbaduk$play$board, 
                  mmbaduk$play$board[, (mmbaduk$i+1) %% 2 + 10]  
                  & !mmbaduk$play$board[, (mmbaduk$i+1) %% 2 + 12]
                  & mmbaduk$play$board$kocount != 2 )
  return(cbind(board$x, board$y))
}

#############################################################
# startplay: disable game selectors                         #
#############################################################
startplay <- function(){
  shinyjs::disable("start")
  shinyjs::disable("mode")
  shinyjs::disable("size")
  shinyjs::disable("komi")
  shinyjs::show("game")
  shinyjs::hide("inst")
}

#############################################################
# inplay: enable game controllers                           #
#############################################################
inplay <- function(){
  shinyjs::enable("play")
  shinyjs::enable("skip")
  shinyjs::enable("quit")
}

#############################################################
# waitplay: disable game controllers                       #
#############################################################
waitplay <- function(){
  shinyjs::disable("play")
  shinyjs::disable("skip")
  shinyjs::disable("quit")
}

#############################################################
# endplay: enable game selectors                            #
#############################################################
endplay <- function(){
  shinyjs::enable("start")
  shinyjs::enable("mode")
  shinyjs::enable("size")
  shinyjs::enable("komi")
  shinyjs::hide("game")
  shinyjs::show("inst")
}

#############################################################
# resetgame: reset game to NULL                             #
#############################################################
resetgame <- function(output, session){
  mmbaduk$play <<- initboard(as.numeric(size))
  output$game <- renderPlot({
    plotgame(initboard(mmbaduk$size), mmbaduk$size, NULL, plotscrs = FALSE)
  })
  
  shinyjs::enable("lastplay")
  updateTextInput(session, "lastplay", value = "")
  shinyjs::disable("lastplay")
  shinyjs::enable("currplay")
  updateTextInput(session, "currplay", value = "")
  shinyjs::disable("currplay")
  return(NULL) 
}

#############################################################
# printscores: print current scores                         #
#############################################################
printscores <- function(output){
  output$on.b <- renderText({mmbaduk$play$scores$on.black})
  output$on.w <- renderText({mmbaduk$play$scores$on.white})
  if(mmbaduk$i >1){
    output$terr.b <- renderText({mmbaduk$play$scores$terr.black})
    output$terr.w <- renderText({mmbaduk$play$scores$terr.white})
  }
  output$cap.b <- renderText({mmbaduk$play$scores$cap.black})
  output$cap.w <- renderText({mmbaduk$play$scores$cap.white})
  return(NULL)
}

#############################################################
# printinst: print game instruction                         #
#############################################################
printinst <- function(output){
  output$inst <- renderUI({
    HTML("<hr/><div style='display: table; width: 100%'>
<div style='float:left; display: table-cell; width: 50%; padding-right: 5px'>
         <h4>How to start</h4>
         <b>Select a game mode.</b>
         <ol>
         <li>Play with another person.</li>
         <li>Play with the computer as black.</li>
         <li>Play with the computer as white.</li>
         </ol>
         <b>Select a board size.</b>
         <p>Select among 5x5, 9x9, and 19x19 boards.</p>
         <b>Select a komi.</b>
         <p>Select the advantage points given to white.</p>
         <b>Start</b>
         <p>Click the button to start the selected game.</p>
         </div>
         <div style='float:right; display: table-cell; width: 50%; padding-left: 5px'>
         <h4>How to play</h4>
         <b>Select a position.</b>
         <p>Click on a desired position on the board. The selected position will be highlighted.</p>
         <b>Play stone.</b>
         <p>Click the buttom to confirm the move.</p>
         <b>Skip turn.</b>
         <p>You may skip a turn.</p>
         <b>Quit game.</b>
         <p>Click the button to end the current game. You may start a new game once a game ends.</p>
         </div>
         <div style='display: table; width: 100%'>
         <div style='float:left; display: table-cell; width: 50%; padding-right: 5px'>
         <h4>Scoring</h4>
         <p>Score is calclated as <b>number of territory + stones on board (+ komi for white)</b>. All stones on the board are considered live.</p>
         </div>
         <div style='float:right; display: table-cell; width: 50%; padding-left: 5px'>
         <h4>Contact</h4>
         <p>Please visit <a href='https://github.com/mjmoon/mmbaduk' target='_blank'>
         https://github.com/mjmoon/mmbaduk</a> and <a href='http://blog.micbon.com/' target='_blank'>http://blog.micbon.com/</a> for more information and comments or to report errors.</p>
         </div>
         </div>
          <h5><b>Note:</b> The current computer player has no intelligence. It selects a random play among available moves.</h5>
         <hr/>
         ")
    })
}

#############################################################
# getxy: return a vector of a xy coordinate for mouse inputs#
#############################################################
getxy <- function(pos){
  if(is.null(pos))
    return(NULL)
  return(c(round(pos$x), round(pos$y)))
}

#############################################################
# printpos: print position in A0 coordinate                 #
#   dep: getxy                                              #
#############################################################
printpos <- function(pos){
  return(
    paste(LETTERS[pos[1]], pos[2], sep = "")
  )
}

#############################################################
# plotpos: plot a shaded dot on a given position            #
#############################################################
plotpos <- function(pos, output){
  output$game <- renderPlot({
    plotgame(mmbaduk$play, mmbaduk$size, mmbaduk$i, plotscrs = FALSE)
    points(pos[1], pos[2], col = NULL, bg = rgb(0,0,0, 0.3), pch = 21, cex = 2.5)
  })
  return(NULL)
}