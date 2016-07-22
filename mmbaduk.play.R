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
#   => 2. player = black | 3. player = white                #
#  3: computer vs. computer                                 #
#   => 4.                                                   #
#############################################################
getmode <- function(){
  mode <- readline(prompt = "Enter play mode:\n  1. 2 players\n  2. player vs. computer\n  3. computer vs. computer\n")
  if(mode == 1){
    cat("Playing between 2 players...\n")
    return(1)
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
      return(1 + as.integer(mode))
    }
  }
  else if(mode == 3){
    cat("Simulating self-play...\n")
    return(4)
  }
  else{
    message("Invalid play mode entered.")
    mode <- getmode()
  }
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
#  4: 0.5                                                   #
#############################################################
getkomi <- function(){
  komi <- NULL
  komi <- readline(prompt = paste("Select komi (compensation points for white):",
                                  "\n  1. 7.5\n  2. 6.5\n  3. 5.5\n  4. 0.5\nSelect: "))
  if(is.na(as.integer(komi)) |
     as.integer(komi) < 1 |
     as.integer(komi) > 4){
    message("Invalid selection")
    komi <- getkomi()
  }
  return(c(7.5, 6.5, 5.5, 0.5)[as.integer(komi)])
}

#############################################################
# getrandom: get a random play among valid moves              #
#############################################################
getrandom <- function(stone = NULL, play = NULL, n = NULL){
  board <- subset(play$board, 
                  checklegal(stone, 1:(n*n), play, n) &
                    !play$board[,9 + stone])
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
getplay <- function(mode, stone, play, n){
  if(mode == 5){
    cat("Play:", max(play$board$playnum) + 1, "\n")
    return(getrandom(stone, play, n))
  }
  cat(ifelse(stone == 1, "Black's turn", "White's turn"))
  if(mode == 2 & stone == 2){
    cat("\n...")
    return(getrandom(1, play, n))
  }
  if(mode == 3 & stone == 1){
    cat("\n...")
    return(getrandom(2, play, n))
  }
  if(mode == 4){
    cat("\n...")
    a <- readline("Press Return/Enter for next move.")
    if(a == "q" | a == "Q"){
      cat("Ending simulation...") 
      return(c(-1,-1))
    }
    return(getrandom(stone, play, n))
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
    getplay(mode, stone, play, n)
  }
  else{
    return(c(as.integer(x), as.integer(y)))
  }
}

#############################################################
# playmmbd: play mmbaduk based on user input                #
#           return a list of game states for each play      #
#   dep: initboard; getmode; getsize; getkomi; plotgame;    #
#        getplay; plotterr; winner                          #
#############################################################
playmmbd <- function(sim = FALSE, size = 19, komi = 7.5){
  plays <- list()
  stone <- i <- 1
  p <- 0
  if(sim){
    mode <- 5
  }
  else {
    mode <- getmode()
    size <- getsize()
    komi <- getkomi()
  }
  plays[[1]] <- initboard(size)
  play <- plays[[1]]
  plotgame(play, size, i)
  while(TRUE){
    on <- getplay(mode, stone, plays[[i]], size)
    if(all(on == c(-1, -1))) {
      plotterr(play, size)
      play$scores$winner <- winner(play, komi)
      cat(paste("End of game.", ifelse(play$scores$winner == 1, "Black wins.",
                                       "White wins.")))
      break()
    }
    if(all(on == c(-1, 0))) {
      i <- max(i-1,1)
      plotgame(plays[[i]], size, i)
      next()
    }
    if(all(on == c(0, 0))) {
      plays[[i+1]] <- play
      i = i+1
      p = p+1
      if(p == 2){
        plotterr(play, size)
        play$scores$winner <- winner(play, komi)
        plays[[i]] <- play
        cat(paste("End of game.", ifelse(play$scores$winner == 1, "Black wins.",
                                         "White wins.")))
        break()
      }
      next()
    }
    
    invalidMoves <- tryCatch(
      {
        play <- playstone(stone, on, plays[[i]], i, size)
      },
      error = function(e) e
    )
    
    if(inherits(invalidMoves, "error")) {
      print(invalidMoves$message)
      next()
    }
    
    plotgame(play, size, i)
    p = 0
    plays[[i+1]] <- play
    i = i+1
    stone = stone %% 2 +1
  }
  return(plays)
}

####### TODO: update #########

#############################################################
# simmbd: simmulate mmbaduk and return a list               #
#   dep: initboard; getplay; plotterr; winner               #
#                                                           #
#  play:      the game state at the end of game             #
#  states:    a matrix representing the board at each play  #
#             per row                                       #
#  playlist:  a matrix representing play coordiantes        #
#############################################################
# simmmbd <- function(size = NULL, komi = 7.5, loadgame = NULL){
#   plays <- list()
#   i <- 1
#   mode <- 6
#   plays[[1]] <- initboard(size, loadgame[[1]])
#   plotgame(plays[[1]], size, i)
#   output.plays <- NULL
#   output.states <- rbind(NULL, plays[[1]]$board$on)
#   
#   if(!is.null(loadgame))
#     if(loadgame[[3]][nrow(loadgame[[3]]),1] == 1){
#       on <- c(0,0)
#       plays[[i+1]] <- plays[[1]]
#       i = i+1
#       output.states <- rbind(NULL, plays[[1]]$board$on)
#       output.plays <- c(player = i, x = on[1], y = on[2])
#     }
#   
#   play <- plays[[i]]
#   
#   while(TRUE){
#     if(checkend(play$board, i)){
#       plotterr(play, size)
#       play$scores$winner <- winner(plays[[i]], komi)
#       cat(paste("End of game.", ifelse(play$scores$winner==1, "Black wins.",
#                                        "White wins.")))
#       break()
#     }
#     on <- getplay(mode, i, plays[[i]]$board)
#     if(all(on == c(0, 0)) | all(!play$board[, 10 + (i+1) %% 2])) {
#       plays[[i+1]] <- play
#       i <- i+1
#       next()
#     }
#     
#     invalidMoves <- tryCatch(
#       {
#         play <- playstone((i+1) %% 2 +1, on, plays[[i]], i, size)
#       },
#       error = function(e) e
#     )
#     
#     if(inherits(invalidMoves, "error")) {
#       print(invalidMoves$message)
#       next()
#     }
#     
#     plays[[i+1]] <- play
#     i = i+1
#     output.plays <- rbind(output.plays, 
#                           c(player = i %% 2 +1, x = on[1], y = on[2]))
#     output.states <- rbind(output.states, play$board$on)
#   }
#   return(list(play = play, states = output.states, playlist = output.plays))
# }
# 
# #############################################################
# # testmbd: test playing mmbaduk and                         #
# #          return a list of game state for each play        #
# #   dep: initboard; getmode; getsize; getkomi; plottest;    #
# #        getplay; winner                                    #
# #############################################################
# testmmbd <- function(){
#   plays <- list()
#   i <- 1
#   mode <- getmode()
#   size <- getsize()
#   komi <- getkomi()
#   play <- plays[[1]] <- initboard(size)
#   plottest(plays[[1]], size, i)
#   while(TRUE){
#     on <- getplay(mode, i, plays[[i]][[1]])
#     if(all(on == c(-1, -1))) {
#       plottest(plays[[i]], size)
#       plays[[i]][[3]]$winner <- winner(plays[[i]], komi)
#       cat(paste("End of game.", ifelse(plays[[i]][[3]]$winner==1, "Black wins.",
#                                        "White wins.")))
#       break()
#     }
#     if(all(on == c(-1, 0))) {
#       i <- i-1
#       plottest(plays[[i]], size, i)
#       next()
#     }
#     if(all(on == c(0, 0)) | all(!play[[1]][, 10 + (i+1) %% 2])) {
#       plays[[i+1]] <- play
#       i <- i+1
#       next()
#     }
#     
#     invalidMoves <- tryCatch(
#       {
#         play <- playstone((i+1) %% 2 +1, on, plays[[i]], i, size)
#       },
#       error = function(e) e
#     )
#     
#     if(inherits(invalidMoves, "error")) {
#       print(invalidMoves$message)
#       next()
#     }
#     
#     plays[[i+1]] <- play
#     if(mode == 2 & (i+1) %% 2 == 1)
#       plottest(play, size, i)
#     else if(mode == 3 & (i+1) %% 2 == 0)
#       plottest(play, size, i)
#     else if (mode == 1 | mode > 3)
#       plottest(play, size, i)
#     i = i+1
# 
#     if(checkend(play[[1]], i)){
#       plottest(play, size)
#       play[[3]]$winner <- winner(plays[[i]], komi)
#       cat(paste("End of game.", ifelse(play[[3]]$winner==1, "Black wins.",
#                                        "White wins.")))
#       break()
#     }
#   }
#   return(plays)
# }
# 
# #############################################################
# # replaymmbd: replay games loaded from SGF files            #
# #          return a list of game state for each play        #
# #   dep: initboard; getmode; getsize; getkomi;              #
# #        plotgame; plotterr; getplay; winner                #
# #                                                           #
# #  play:      the game state at the end of game             #
# #  states:    a matrix representing the board at each play  #
# #             per row                                       #
# #  sgfgame:   loaded sgf game                               #
# #############################################################
# replaymmbd <- function(game = NULL, komi = 7.5, size = 19, plotmove = FALSE){
#   plays <- list()
#   i <- 1
#   play <- plays[[1]] <- initboard(size)
#   if(plotmove)
#     plotgame(plays[[1]], size, i)
#   output.states <- NULL
#   for(i in 1:nrow(game)){
#     on <- unlist(game[i,2:3])
#     if(all(on == c(0, 0)) | all(!play[[1]][, 10 + (i+1) %% 2])) {
#       plays[[i+1]] <- play
#       next()
#     }
#     play <- playstone(unlist(game[i,1]), on, plays[[i]], i, size)
#     plays[[i+1]] <- play
#     output.states <- rbind(output.states, play[[1]]$on)
#     
#     if(plotmove){
#       plotgame(play, size, i)
#       plt <- readline(prompt = "Press any key for next move. Press 'q' to quit plotting.\n")
#       plotmove = (plt != "q" & plt != "Q")
#     }
#     cat("Play: ",i , "\n")
#   }
#   plotterr(play, size)
#   play[[3]]$winner <- winner(plays[[i]], komi)
#   cat(paste("End of game.", ifelse(play[[3]]$winner==1, "Black wins.",
#                                    "White wins.")))
#   return(list(play = play, states = output.states, sgfgame = game))
# }