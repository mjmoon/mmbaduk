#############################################################
# mmbaduk v0.1 - Baduk (Go) player by michael moon          #
#                                                           #
# game interface/plotter/player                             #
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
# getmode: get a random play among valid moves              #
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
          plays[[i]][[3]]$winner <- winner(plays[[i]], komi)
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

#############################################################
# plotboard: plot a (n x n) baduk board                     #
#############################################################
plotboard <- function(n = NULL){
  refpos <- c(ceiling((n+1)/4-1),(n+1)/2,floor((n+1)*3/4+1))
  refpts <- data.frame(x = rep(refpos,3), y = rep(refpos,each=3))
  par(mar = c(0,4,4,2))
  plot(refpts, xlim = c(1,n), ylim = c(1,n), 
       xlab = "", ylab = "", axes = FALSE, pch = 16,
       col = grey(0.5), mar = 0)
  title(main = "mmbaduk v0.1", line = 3)
  abline(h = c(1:n), v = c(1:n), lty = "dotted", col = grey(0.5))
  axis(3, tick = FALSE, at = c(1:n), labels = LETTERS[1:n])
  axis(2, tick = FALSE, at = c(1:n), labels = c(1:n), las = 2)
}

#############################################################
# plotscores: plot scores                                   #
#############################################################
plotscores <- function(scores = NULL, n = NULL){
  par(mar = c(1,0,0,0))
  plot(NULL, xlim = c(0.5,2.5), ylim = c(0,1),
       axes = FALSE, ylab = "", xlab = "")
  title(main = "Scores", line = -3.5)
  text(c(1,2), 0.7, labels = c("Black", "White"))
  text(1, c(0,0.2,0.4), cex = 0.7,
       labels = c(paste("Captured:", scores$cap.black),
                  paste("Territory:", scores$terr.black),
                  paste("On Board:", scores$on.black)))
  text(2, c(0,0.2,0.4), cex = 0.7,
       labels = c(paste("Captured:", scores$cap.white),
                  paste("Territory:", scores$terr.white),
                  paste("On Board:", scores$on.white)))
}

#############################################################
# plotgame: plot the game state                             #
#  dep:plotboard; plotscores                                #
#############################################################
plotgame <- function(play = NULL, n = NULL, i = NULL){
  blacks <- subset(play[[1]], on == 1)
  whites <- subset(play[[1]], on == 2)
  
  layout(matrix(c(1,2),2,1), heights = c(0.8, 0.15))
  plotboard(n)
  points(blacks$x, blacks$y, pch = 16, cex = 2.5)
  points(whites$x, whites$y, pch = 21, bg = "white", cex = 2.5)
  
  if(sum(play$board$playnum == i) >0)
    text(play$board$x[play$board$playnum == i], 
         play$board$y[play$board$playnum == i], 
         labels = i, col = "red", cex = 0.8)
  plotscores(play$scores)
}

#############################################################
# plotterr: plot the game state with territories            #
#  dep:plotboard; plotscores                                #
#############################################################
plotterr <- function(play = NULL, n = NULL){
  blacks <- subset(play$board, on == 1)
  whites <- subset(play$board, on == 2)
  black.terr <- subset(play$board, terr == 1)
  white.terr <- subset(play$board, terr == 2)
  
  layout(matrix(c(1,2),2,1), heights = c(0.8, 0.15))
  plotboard(n)
  points(black.terr$x, black.terr$y, pch = 16, 
         col = grey(0.7), cex = 2.2)
  points(white.terr$x, white.terr$y, pch = 21, 
         col = grey(0.7), bg = grey(0.9), cex = 2.2)
  
  points(blacks$x, blacks$y, pch = 16, cex = 2.5)
  points(whites$x, whites$y, pch = 21, bg = "white", cex = 2.5)
  
  if(length(blacks) >0)
    text(blacks$x, blacks$y, labels = blacks$playnum, 
         col = "white", cex = 0.8)
  if(length(whites) >0)
    text(whites$x, whites$y, labels = whites$playnum, 
         col = "black", cex = 0.8)
  
  plotscores(play$scores)
}

#############################################################
# plottest: plot the game state with territories,           #
#           eyes, and moves not allowed                     #
#  dep:plotboard; plotscores                                #
#############################################################
plottest <- function(play = NULL, n = NULL, i = NULL){
  blacks <- subset(play$board, on == 1)
  whites <- subset(play$board, on == 2)
  black.terr <- subset(play$board, terr == 1)
  white.terr <- subset(play$board, terr == 2)
  black.nallow <- subset(play$board, allowed.black == FALSE)
  white.nallow <- subset(play$board, allowed.white == FALSE)
  black.eye <- subset(play$board, eye.black == TRUE)
  white.eye <- subset(play$board, eye.white == TRUE)
  
  layout(matrix(c(1,2),2,1), heights = c(0.8, 0.15))
  plotboard(n)
  points(black.terr$x, black.terr$y, pch = 16, 
         col = grey(0.7), cex = 2.2)
  points(white.terr$x, white.terr$y, pch = 21, 
         col = grey(0.7), bg = grey(0.9), cex = 2.2)
  
  points(black.nallow$x, black.nallow$y, pch = 4, col = "blue")
  points(white.nallow$x, white.nallow$y, pch = 4, col = "red")
  
  points(black.eye$x, black.eye$y, pch = 3, col = "blue")
  points(white.eye$x, white.eye$y, pch = 3, col = "red")
  
  points(blacks$x, blacks$y, pch = 16, cex = 2.5)
  points(whites$x, whites$y, pch = 21, bg = "white", cex = 2.5)
  
  if(sum(play$board$playnum == i) >0)
    text(play$board$x[play$board$playnum == i], 
         play$board$y[play$board$playnum == i], 
         labels = i, col = "red", cex = 0.8)
  
  plotscores(play$scores)
}

########################## ggplot2 version ##########################
# plotboard <- function(n = NULL){
#   refpos <- c(ceiling((n+1)/4-1),(n+1)/2,floor((n+1)*3/4+1))
#   refpts <- data.frame(x = rep(refpos,3), y = rep(refpos,each=3))
#   
#   board <- ggplot(data = refpts, aes(x = x, y = y)) + 
#     labs(x = "", y = "", title = "") +
#     geom_hline(yintercept = c(1:n), color = "dark grey") +
#     geom_vline(xintercept = c(1:n), color = "dark grey") +
#     geom_point(size = 2, alpha = 0.8)
#   
#     return(board)
# }
# 
# plotgame <- function(play = NULL, n = NULL, board = NULL, i = NULL){
#   game <- board +
#     geom_point(data = subset(play[[1]], on ==1), aes(x = x, y = y), 
#                color = "black", size = 8) +
#     geom_point(data = subset(play[[1]], on ==2), aes(x = x, y = y), 
#                shape = 21, fill = "white", size = 8) +
#     geom_text(data = subset(play[[1]], playnum == i), 
#               aes(x = x, y = y, label = playnum), 
#               color = "red", fontface = "bold")
#   
#   scores <- plotscores(play, n)
#   
#   boardplt <- grid.arrange(scores[[1]], game, scores[[2]],
#                            ncol=1, nrow=3, layout_matrix = rbind(1,2,3), 
#                            heights = c(2, 8, 2))
#   return(boardplt)
# }
# 
# plotscores <- function(play = NULL, n = NULL){
#   stonesize <- 4
#   maxscale <- n*n/2
#   scrplt.black <- ggplot(data = play[[3]]) + theme_minimal() +
#     theme(axis.text.x = element_blank()) +
#     labs(x = "", y = "", title = "Black's Score") +
#     geom_point(aes(x = paste("Captured:", cap.black), y = min(maxscale,cap.black)), 
#                shape = 21, fill = "white", size = stonesize) +
#     geom_point(aes(x = paste("Territory:", terr.black), y = min(maxscale,terr.black)), 
#                alpha = 0.5, size = stonesize) +
#     geom_point(aes(x = paste("On Board:", on.black), y = min(maxscale,on.black)), 
#                size = stonesize) +
#     coord_flip(ylim = c(0,maxscale))
#   
#   scrplt.white <- ggplot(data = play[[3]]) + theme_minimal() +
#     theme(axis.text.x = element_blank()) +
#     labs(x = "", y = "", title = "White's Score") +
#     geom_point(aes(x = paste("Captured:", cap.white), y = min(maxscale,cap.white)), 
#                size = stonesize) +
#     geom_point(aes(x = paste("Territory:", terr.white), y = min(maxscale,terr.white)), 
#                color = "grey", alpha = 0.5, size = stonesize) +
#     geom_point(aes(x = paste("On Board:", on.white), y = min(maxscale,on.white)), 
#                shape = 21, fill = "white", size = stonesize) +
#     coord_flip(ylim = c(0,maxscale))
#   
#   return(list(scrplt.black, scrplt.white))
# }
# 
# plotterr <- function(play = NULL, n = NULL, board = NULL){
#   terr <- board +
#     geom_point(data = subset(play[[1]], on ==1), aes(x = x, y = y), 
#                color = "black", size = 8) +
#     geom_point(data = subset(play[[1]], on ==2), aes(x = x, y = y), 
#                shape = 21, fill = "white", size = 8) +
#     geom_point(data = subset(play[[1]], terr ==1), aes(x = x, y = y), 
#                color = "black", size = 8, alpha = 0.5) +
#     geom_point(data = subset(play[[1]], terr ==2), aes(x = x, y = y), 
#                color = "grey", size = 8, alpha = 0.5) +
#     geom_text(data = subset(play[[1]], on == 1),
#               aes(x = x, y = y, label = playnum), color = "white") +
#     geom_text(data = subset(play[[1]], on == 2),
#               aes(x = x, y = y, label = playnum), color = "black")
#   
#   scores <- plotscores(play, n)
#   
#   terrplot <- grid.arrange(scores[[1]], terr, scores[[2]],
#                            ncol=1, nrow=3, layout_matrix = rbind(1,2,3), 
#                            heights = c(2, 8, 2))
#   return(terrplot)
# }
# 
# plottest <- function(play = NULL, n = NULL, board = NULL, i = NULL){
#   test <- board +
#     geom_point(data = subset(play[[1]], terr ==1), aes(x = x, y = y), 
#                color = "black", size = 8, alpha = 0.5) +
#     geom_point(data = subset(play[[1]], terr ==2), aes(x = x, y = y), 
#                color = "grey", size = 8, alpha = 0.5) +
#     geom_point(data = subset(play[[1]], on ==1), aes(x = x, y = y), 
#                color = "black", size = 8) +
#     geom_point(data = subset(play[[1]], on ==2), aes(x = x, y = y), 
#                shape = 21, fill = "white", size = 8) +
#     geom_point(data = subset(play[[1]], allowed.black == FALSE), aes(x = x, y = y), 
#                shape = 21, color = "red", size = 6) +
#     geom_point(data = subset(play[[1]], allowed.white == FALSE), aes(x = x, y = y), 
#                shape = 21, color = "green", size = 5) +
#     geom_point(data = subset(play[[1]], eye.black == TRUE), aes(x = x, y = y), 
#                shape = 4, color = "red", size = 5) +
#     geom_point(data = subset(play[[1]], eye.white == TRUE), aes(x = x, y = y), 
#                shape = 4, color = "green", size = 5) +
#     geom_text(data = subset(play[[1]], playnum == i), 
#               aes(x = x, y = y, label = playnum), 
#               color = "red", fontface = "bold")
#   
#   scores <- plotscores(play, n)
#   
#   testplot <- grid.arrange(scores[[1]], test, scores[[2]],
#                            ncol=1, nrow=3, layout_matrix = rbind(1,2,3), 
#                            heights = c(2, 8, 2))
#   return(testplot)
# }