#############################################################
# mmbaduk v0.1 - Baduk (Go) player by michael moon          #
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