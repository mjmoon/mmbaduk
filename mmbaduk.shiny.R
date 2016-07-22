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
                   i = 1,
                   p = 0)
  
  output$game <- renderPlot({
    plotgame(mmbaduk$play, mmbaduk$size, mmbaduk$i, plotscrs = FALSE)
  }, height = session$clientData$output_game_width
    )
  output$status <- renderText({
    paste("Game started")
  })
  
  resetscores(output)
  return(NULL)
}

#############################################################
# resetscores: set scores to 0                              #
#############################################################
resetscores <-function(output){
  output$on.b <- renderText({0})
  output$on.w <- renderText({0})
  output$terr.b <- renderText({0})
  output$terr.w <- renderText({0})
  output$cap.b <- renderText({0})
  output$cap.w <- renderText({0})
}


#############################################################
# playuser: play at clicked position if among valid moves   #
#   dep: playmove; validmoves                               #
#############################################################
playuser <- function(pos, output, session){
  if(checklegal((mmbaduk$i+1) %% 2 +1, findindex(pos, mmbaduk$size),  
                mmbaduk$play, mmbaduk$size)) {
    playmove(pos, output, session)
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
    smoves <- sensiblemoves()
    if(nrow(smoves) > 0){
      randi <- floor(runif(1,min = 1, max = nrow(smoves)))
      on <- smoves[randi,]
    }
    else on <- c(0,0)
    playmove(on, output, session)
  }
  return(NULL)
}

#############################################################
# playmove: play a given move and plot the resulting board  #
#           pass if the given move is c(0,0)                #
#   dep: playstone; plotgame                                #
#############################################################
playmove <- function(on, output, session){
  stone <- (mmbaduk$i+1) %% 2 +1
  if(all(on > 0)){
    
    invalidMoves <- tryCatch(
      {
        mmbaduk$play <<- playstone(stone, on, mmbaduk$play, mmbaduk$i, mmbaduk$size) 
      },
      error = function(e) e
    )
    
    if(inherits(invalidMoves, "error")) {
      playrandom(output, session)
      output$status <- renderText(invalidMoves$message)
      return(NULL)
    }
    
    output$game <- renderPlot({
      plotgame(mmbaduk$play, mmbaduk$size, mmbaduk$i, plotscrs = FALSE)
    }, height = session$clientData$output_game_width)
    printscores(output)
    
    output$status <- renderText({
      paste(ifelse(stone == 1, "Black played at", "White played at"), printpos(on))
    })
    mmbaduk$p <<- 0
    mmbaduk$i <<- mmbaduk$i + 1
    return(NULL)
  }
  if(all(on == 0)){
    output$status <- renderText({
      paste(ifelse(stone == 1, "Black", "White"), "passed")
    })
    mmbaduk$p <<- mmbaduk$p +1
    mmbaduk$i <<- mmbaduk$i + 1
    return(NULL)
  }
  return(NULL)
}

#############################################################
# quitplay: end the current game and plot the territories   #
#   dep: playterr; winner                                   #
#############################################################
quitplay <- function(output){
  output$game <- renderPlot({
    plotterr(mmbaduk$play, mmbaduk$size, plotscrs = FALSE)
  }, height = session$clientData$output_game_width)
  
  output$status <- renderText({
    paste(ifelse(winner(mmbaduk$play, mmbaduk$komi) == 1,
                 "Black", "White"), 
          "Wins")
  })
  return(NULL)
}

#############################################################
# sensiblemoves: return a matrix of sensible move           #
#                coordinates - legal and not its own eye    #
#############################################################
sensiblemoves <- function(){
  stone <- (mmbaduk$i+1) %% 2 +1
  board <- subset(mmbaduk$play$board, 
                  checklegal(stone, 1:(mmbaduk$size^2),  
                             mmbaduk$play, mmbaduk$size)
                  & !mmbaduk$play$board[, 9 + stone])
  return(cbind(board$x, board$y))
}

#############################################################
# startplay: disable game selectors                         #
#############################################################
startplay <- function(output){
  output$printinstui <- renderUI({
    list(br(), actionButton('printinst', label = label(), width = "100%"))
  })
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
  shinyjs::disable("game")
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
<div style='float:left; display: table-cell; width: 50%; padding-right: 5px; align: left'>
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
         <b>Start.</b>
         <p>Click the button to start the selected game.</p>
         </div>
         <div style='float:right; display: table-cell; width: 50%; padding-left: 5px; align: left'>
         <h4>How to play</h4>
         <b>Select a position.</b>
         <p>Click on a desired position on the board. The selected position will be highlighted.</p>
         <b>Play.</b>
         <p>Click the buttom to place the stone</p>
         <b>Pass.</b>
         <p>You may pass a turn. Game will end when both players pass.</p>
         <b>Resign.</b>
         <p>Resign to quit current game.</p>
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
  }, height = session$clientData$output_game_width)
  return(NULL)
}