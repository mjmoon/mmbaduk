#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# game interface (R)                                        #
#                                                           #
#############################################################
source("mmbaduk.R")

#############################################################
# getmode: get game mode from user                          #
#  1: player vs. player                                     #
#  2: player vs. computer                                   #
#   => 3. player = black | 4. player = white                #
#  3: computer vs. computer                                 #
#   => 5.                                                   #
#############################################################
getmode <- function(){
  mode <- readline(prompt = "Enter play mode:\n  1. 2 players\n  2. player vs. computer\n  3. computer vs. computer\n")
  if(mode == 1){
    cat("Playing between 2 players...\n")
    mode <- as.integer(mode)
  }
  else if(mode == 2){
    cat("Playing against the computer...\n")
    mode <- NULL
    while(is.null(mode)){
      mode <- readline(prompt = "Select your stone:\n  1. black\n  2. white\n")
      if(mode == 1){
        cat("Playing as black.")
      } else if(mode ==2){
        cat("Playing as white.")
      } else{
        message("Invalide stone selected.")
        mode <- NULL
        next()
      }
      mode <- 1 + as.integer(mode) 
    }
  }
  else if(mode == 3){
    cat("Simulating self-play...\n")
    mode <- 5
  }
  else{
    message("Invalide play mode entered.")
    mode <- getmode()
  }
  return(mode)
}

#############################################################
# getsize: get game board size (n x n)                      #
#############################################################
getsize <- function(){
  size <- NULL
  size <- readline(prompt = "Enter board size (n):")
  if(is.na(as.integer(size))){
    message("Invalid size")
    size <- getsize()
  }
  else if(as.integer(size) < 1){
    message("Invalid size")
    size <- getsize()
  }
  return(as.integer(size))
}

#############################################################
# getkomi: get komi selection from user                     #
#  1: 7.5                                                   #
#  2: 6.5                                                   #
#  3: 5.5                                                   #
#############################################################
getkomi <- function(){
  komi <- NULL
  komi <- readline(prompt = paste("Select komi (compensation points for white):",
                                  "\n  1. 7.5\n  2. 6.5\n  3. 5.5\nSelect: "))
  if(is.na(as.integer(komi)) |
     as.integer(komi) < 1 |
     as.integer(komi) > 3){
    message("Invalid selection")
    komi <- getkomi()
  }
  return(as.integer(komi))
}

#############################################################
# getrandom: get a random play among valid moves              #
#############################################################
getrandom <- function(board = NULL, i = NULL){
  board <- subset(board, 
                  board[, (i+1) %% 2 + 10]  
                  & !board[, (i+1) %% 2 + 12]
                  & kocount != 2 )
  if(nrow(board) > 0){
    randi <- floor(runif(1,min = 1, max = nrow(board)))
    return(c(board$x[randi], board$y[randi]))
  }
  return(c(0,0))
}

#############################################################
# getplay: get a play from user (x,y)                       #
#   dep: getrandom                                          #
#############################################################
getplay <- function(mode, i, board){
  if(mode == 6){
    print(i) 
    return(getrandom(board, i))
  }
  cat("Play ", i, ifelse(i%% 2 == 1, ": black's turn", ": white's turn"))
  if(mode == 2 & i%%2 == 0){
    cat("\n...\n")
    return(getrandom(board, 1))
  }
  if(mode == 3 & i%%2 == 1){
    cat("\n...\n")
    return(getrandom(board, 2))
  }
  if(mode == 5){
    cat("\n...\n")
    a <- readline("Press Return/Enter for next move.")
    if(a == "q" | a == "Q"){
      cat("Ending simulation...") 
      return(c(-1,-1))
    }
    return(getrandom(board, i))
  }
  
  x <- readline(prompt = "Enter position x:")
  if(x == "q" | x == "Q") {
    cat("Game aborted")
    return(c(-1, -1))
  }
  else if(x == "b" | x == "B") {
    cat("Last move cancelled\n")
    return(c(-1, 0))
  }
  else if(x == "p" | x == "P") {
    cat("Move passed\n")
    return(c(0, 0))
  }
  
  y <- readline(prompt = "Enter position y:")
  if(y == "q" | y == "Q") {
    cat("Game aborted")
    return(c(-1, -1))
  }
  else if(y == "b" | y == "B") {
    cat("Last move cancelled\n")
    return(c(-1, 0))
  }
  else if(y == "p" | y == "P") {
    cat("Move passed\n")
    return(c(0, 0))
  }
  else if(is.na(as.integer(x)) | is.na(as.integer(y))){
    message("Invalid position or not a number")
    getplay(mode, i, board)
  }
  else{
    return(c(as.integer(x), as.integer(y)))
  }
}

#############################################################
# checkend: return true when reached end of game            #
#           i.e., empty eye < 3 or no possible moves        #
#           without filling own eyes                        #
#############################################################
checkend <- function(board, i){
  eog <- (sum(board$on == 0) < 3 | 
            nrow(subset(board, 
                   board[, 11 - (i+1) %% 2 ]  
                   & !board[, 13 - (i+1) %% 2]
                   & kocount != 2)) == 0)
  return(eog)
}

#############################################################
# playmmbd: play mmbaduk based on user input                #
#           return a list of game states for each play      #
#   dep: initboard; getmode; getsize; getkomi; plotgame;    #
#        getplay; plotterr; winner                          #
#############################################################
playmmbd <- function(){
  plays <- list()
  i <- 1
  mode <- getmode()
  size <- getsize()
  komi <- getkomi()
  plays[[1]] <- initboard(size)
  ## ggplot2 version ##
  # board <- plotboard(size)
  # grid.draw(plotgame(plays[[1]], size, board, i))
  plotgame(plays[[1]], size, i)
  play <- plays[[1]]
  while(TRUE){
    on <- getplay(mode, i, plays[[i]][[1]])
    if(all(on == c(-1, -1))) {
      ## ggplot2 version ##
      # grid.draw(plotterr(plays[[i]], size, board))
      plotterr(plays[[i]], size)
      play[[3]]$winner <- winner(plays[[i]], komi)
      plays[[i]] <- play
      cat(paste("End of game.", ifelse(play[[3]]$winner==1, "Black wins.",
                                       "White wins.")))
      break()
    }
    if(all(on == c(-1, 0))) {
      i <- i-1
      ## ggplot2 version ##
      # grid.draw(plotgame(plays[[i]], size, board, i))
      plotgame(plays[[i]], size, i)
      next()
    }
    if(all(on == c(0, 0)) | all(!play[[1]][, 10 + (i+1) %% 2])) {
      plays[[i+1]] <- play
      i <- i+1
      next()
    }
    try(
      {
        play <- playstone((i+1) %% 2 +1, on, plays[[i]], i, size)
        plays[[i+1]] <- play
        if(mode == 2 & (i+1) %% 2 == 1)
          ## ggplot2 version ##
          # grid.draw(plotgame(play, size, board, i))
          plotgame(play, size, i)
        else if(mode == 3 & (i+1) %% 2 == 0)
          ## ggplot2 version ##
          # grid.draw(plotgame(play, size, board, i))
          plotgame(play, size, i)
        else if (mode == 1 | mode > 3)
          ## ggplot2 version ##
          # grid.draw(plotgame(play, size, board, i))
          plotgame(play, size, i)
        i = i+1
        if(checkend(play[[1]], i)){
          ## ggplot2 version ##
          # grid.draw(plotterr(play, size, board))
          plotterr(play, size)
          plays[[i]]$scores$winner <- winner(plays[[i]], komi)
          cat(paste("End of game.", ifelse(plays[[i]][[3]]$winner==1, "Black wins.",
                                           "White wins.")))
          break()
        }
      }
    )
  }
  return(plays)
}

#############################################################
# simmbd: simmulate mmbaduk and return a list               #
#   dep: initboard; getplay; plotterr; winner               #
#                                                           #
#  play:      the game state at the end of game             #
#  states:    a matrix representing the board at each play  #
#             per row                                       #
#  playlist:  a matrix representing play coordiantes        #
#############################################################
simmmbd <- function(size = NULL, komi = 7.5, loadgame = NULL){
  plays <- list()
  i <- 1
  mode <- 6
  plays[[1]] <- initboard(size, loadgame[[1]])
  ## ggplot2 version ##
  # board <- plotboard(size)
  # grid.draw(plotgame(plays[[1]], size, board, i))
  plotgame(plays[[1]], size, i)
  output.states <- NULL
  output.plays <- NULL
  
  if(!is.null(loadgame))
    if(loadgame[[3]][nrow(loadgame[[3]]),1] == 1){
      on <- c(0,0)
      plays[[i+1]] <- plays[[1]]
      i = i+1
      output.states <- plays[[1]][[1]]$on
      output.plays <- c(player = i, x = on[1], y = on[2])
    }
  
  play <- plays[[i]]
  
  while(TRUE){
    on <- getplay(mode, i, plays[[i]][[1]])
    if(all(on == c(0, 0)) | all(!play[[1]][, 10 + (i+1) %% 2])) {
      plays[[i+1]] <- play
      i <- i+1
      next()
    }
    play <- playstone((i+1) %% 2 +1, on, plays[[i]], i, size)
    plays[[i+1]] <- play
    i = i+1
    output.states <- rbind(output.states, play[[1]]$on)
    output.plays <- rbind(output.plays, 
                          c(player = i %% 2 +1, x = on[1], y = on[2]))
    
    if(checkend(play[[1]], i)){
      ## ggplot2 version ##
      # grid.draw(plotterr(play, size, board))
      plotterr(play, size)
      play[[3]]$winner <- winner(plays[[i]], komi)
      cat(paste("End of game.", ifelse(play[[3]]$winner==1, "Black wins.",
                                       "White wins.")))
      break()
    } 
  }
  return(list(play = play, states = output.states, playlist = output.plays))
}

#############################################################
# testmbd: test playing mmbaduk and                         #
#          return a list of game state for each play        #
#   dep: initboard; getmode; getsize; getkomi; plottest;    #
#        getplay; winner                                    #
#############################################################
testmmbd <- function(){
  plays <- list()
  i <- 1
  mode <- getmode()
  size <- getsize()
  komi <- getkomi()
  play <- plays[[1]] <- initboard(size)
  ## ggplot2 version ##
  # board <- plotboard(size)
  # grid.draw(plottest(plays[[1]], size, board, i))
  plottest(plays[[1]], size, i)
  while(TRUE){
    on <- getplay(mode, i, plays[[i]][[1]])
    if(all(on == c(-1, -1))) {
      ## ggplot2 version ##
      # grid.draw(plottest(plays[[i]], size, board))
      plottest(plays[[i]], size)
      plays[[i]][[3]]$winner <- winner(plays[[i]], komi)
      cat(paste("End of game.", ifelse(plays[[i]][[3]]$winner==1, "Black wins.",
                                       "White wins.")))
      break()
    }
    if(all(on == c(-1, 0))) {
      i <- i-1
      ## ggplot2 version ##
      # grid.draw(plottest(plays[[i]], size, board, i))
      plottest(plays[[i]], size, i)
      next()
    }
    if(all(on == c(0, 0)) | all(!play[[1]][, 10 + (i+1) %% 2])) {
      plays[[i+1]] <- play
      i <- i+1
      next()
    }
    play <- playstone((i+1) %% 2 +1, on, plays[[i]], i, size)
    plays[[i+1]] <- play
    if(mode == 2 & (i+1) %% 2 == 1)
      ## ggplot2 version ##
      # grid.draw(plottest(play, size, board, i))
      plottest(play, size, i)
    else if(mode == 3 & (i+1) %% 2 == 0)
      ## ggplot2 version ##
      # grid.draw(plottest(play, size, board, i))
      plottest(play, size, i)
    else if (mode == 1 | mode > 3)
      ## ggplot2 version ##
      # grid.draw(plottest(play, size, board, i))
      plottest(play, size, i)
    i = i+1

    if(checkend(play[[1]], i)){
      ## ggplot2 version ##
      # grid.draw(plottest(play, size, board))
      plottest(play, size)
      play[[3]]$winner <- winner(plays[[i]], komi)
      cat(paste("End of game.", ifelse(play[[3]]$winner==1, "Black wins.",
                                       "White wins.")))
      break()
    }
  }
  return(plays)
}

#############################################################
# replaymmbd: replay games loaded from SGF files            #
#          return a list of game state for each play        #
#   dep: initboard; getmode; getsize; getkomi;              #
#        plotgame; plotterr; getplay; winner                #
#                                                           #
#  play:      the game state at the end of game             #
#  states:    a matrix representing the board at each play  #
#             per row                                       #
#  sgfgame:   loaded sgf game                               #
#############################################################
replaymmbd <- function(game = NULL, komi = 7.5, size = 19, plotmove = FALSE){
  plays <- list()
  i <- 1
  play <- plays[[1]] <- initboard(size)
  # board <- plotboard(size)
  if(plotmove)
    ## ggplot2 version ##
    # grid.draw(plotgame(plays[[1]], size, board, i))
    plotgame(plays[[1]], size, i)
  output.states <- NULL
  for(i in 1:nrow(game)){
    on <- unlist(game[i,2:3])
    if(all(on == c(0, 0)) | all(!play[[1]][, 10 + (i+1) %% 2])) {
      plays[[i+1]] <- play
      next()
    }
    play <- playstone(unlist(game[i,1]), on, plays[[i]], i, size)
    plays[[i+1]] <- play
    output.states <- rbind(output.states, play[[1]]$on)
    
    if(plotmove){
      ## ggplot2 version ##
      # grid.draw(plotgame(play, size, board, i))
      plotgame(play, size, i)
      plt <- readline(prompt = "Press any key for next move. Press 'q' to quit plotting.\n")
      plotmove = (plt != "q" & plt != "Q")
    }
    cat("Play: ",i , "\n")
  }
  ## ggplot2 version ##
  # grid.draw(plotterr(play, size, board))
  plotterr(play, size)
  play[[3]]$winner <- winner(plays[[i]], komi)
  cat(paste("End of game.", ifelse(play[[3]]$winner==1, "Black wins.",
                                   "White wins.")))
  return(list(play = play, states = output.states, sgfgame = game))
}