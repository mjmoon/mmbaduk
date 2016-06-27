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
initgame <- function(mode, size, komi){
  print(as.numeric(mode))
  print(as.numeric(size))
  print(as.numeric(komi))
  return(list(play = initboard(as.numeric(size)),
              size = as.numeric(size),
              mode = as.numeric(mode), 
              komi = as.numeric(komi),
              i = 1))
}

#############################################################
# playuser: parse Z0 inputs to c(x, y) coordinates          # 
#           and play if among valid moves                   #
#   dep: playmove; validmoves                               #
#############################################################
playuser <- function(z0, output){
  x <- which(LETTERS == substr(z0, 1, 1) | 
               letters == substr(z0, 1, 1))
  y <- as.numeric(substr(z0, 2, nchar(z0)))
  
  if(length(x) > 0 & !is.na(y))
    if(any(apply(validmoves(), 1, 
                 function(x, want) isTRUE(all.equal(x, want)), 
                 c(x,y)))
       ) {
      playmove(c(x,y), output)
    }
  else output$status <- renderText({"Invalid move"})
  else output$status <- renderText({"Invalid input"})
  return(NULL)
}

#############################################################
# playrandom: play a random move among valid moves          #
#   dep: playmove; validmoves                               #
#############################################################
playrandom <- function(output){
  if((mmbaduk$mode == 2 & mmbaduk$i %% 2 == 0) |
     (mmbaduk$mode == 3 & mmbaduk$i %% 2 == 1) ) {
    vmoves <- validmoves()
    if(nrow(vmoves) > 0){
      randi <- floor(runif(1,min = 1, max = nrow(vmoves)))
      on <- vmoves[randi,]
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
  shinyjs::enable("pos")
  shinyjs::enable("play")
  shinyjs::enable("skip")
  shinyjs::enable("quit")
}

#############################################################
# waitplay: disable game controllers                       #
#############################################################
waitplay <- function(){
  shinyjs::disable("pos")
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
# resetinput: empty input text box                          #
#############################################################
resetinput <- function(session){
  updateTextInput(session, "pos", value = "")
}

#############################################################
# resetscores: set scores to 0                              #
#############################################################
resetscores <- function(output){
  output$on.b <- renderText({0})
  output$on.w <- renderText({0})
  output$terr.b <- renderText({0})
  output$terr.w <- renderText({0})
  output$cap.b <- renderText({0})
  output$cap.w <- renderText({0})
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
         <li>You can play with another person.</li>
         <li>You can play with the computer as black.</li>
         <li>You can play with the computer as white.</li>
         </ol>
         <b>Select a board size.</b>
         <p>You can play on a 5x5, 9x9, or 19x19 board.</p>
         <b>Select a komi.</b>
         <p>This is the advantage point given to white for playing second.</p>
         <b>Click start</b>
         <p>The selected game will start</p>
         </div>
         <div style='float:right; display: table-cell; width: 50%; padding-left: 5px'>
         <h4>How to play</h4>
         <b>Input position.</b>
         <p>All positions should be entered using a letter for x-axis followed by a number for y-axis (e.g., A1).</p>
         <b>Play stone.</b>
         <p>Clicking the buttom will place a stone on the given position.</p>
         <b>Skip turn.</b>
         <p>You may skip a turn.</p>
         <b>Quit game.</b>
         <p>A game will end only when the buttom is clicked. You may start a new game once a game ends.</p>
         </div>
         <div style='display: table; width: 100%'>
         <div style='float:left; display: table-cell; width: 50%; padding-right: 5px'>
         <h4>Scoring</h4>
         <p>A winner is announced at the end of the game based on the current score of number of territory + stones on board (+ komi for white). Note that dead stones are not recognized (yet).</p>
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

  