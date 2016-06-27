#############################################################
# mmbaduk v0.1 - Baduk (Go) player by michael moon          #
#                                                           #
# game engine                                               #
#                                                           #
#############################################################

#############################################################
# initboard: initiate game with a list                      #
#   dep: edges                                              #
#                                                           #
#  board:  a data frame with board state                    #
#          including an 4-way graph                         #
#  region: a matrix representing connected regions          #
#          for each position                                #
#  scores: a vector tracking capture, on-board, and         #
#         territory counts                                  #
#############################################################
initboard <- function(size, loadgame = NULL){
  ## ggplot2 version ##
  #   library(ggplot2)
  #   library(gridExtra)
  #   library(grid)
  
  if(!is.null(loadgame)){
    return(loadgame)
  }
  
  board <- data.frame(l = rep(0, size*size), u = rep(0, size*size),
                      r = rep(0, size*size), d = rep(0, size*size),
                      x = rep(c(1:size), each = size), y = rep(c(1:size), size),
                      on = factor(rep(0, size*size), levels = c(0,1,2)),
                      playnum = rep(0, size*size),
                      terr = rep(0, size*size),
                      allowed.black = rep(TRUE, size*size),
                      allowed.white = rep(TRUE, size*size),
                      eye.black = rep(FALSE, size*size),
                      eye.white = rep(FALSE, size*size),
                      kocount = rep(0, size*size))
  
  board[,c(1:4)] <- edges(rep(0,size*size), c(1:(size*size)), board, size)
  
  region <- matrix(rep(c(1:(size*size)), size*size), nrow = size*size,
                   byrow = TRUE)
  
  scores <- data.frame(cap.black = 0, cap.white = 0, 
                       terr.black = 0, terr.white = 0,
                       on.black = 0, on.white = 0, 
                       winner = 0)
  
  return(list(board = board, region = region, scores = scores))
}

#############################################################
# edge: update the edge with the index of a stone's given   #
#       side if connected, else 0                           #
#   dep: getneibours                                        #
#############################################################
edge <- function(stone = NULL, ind = NULL, side = NULL, board = NULL, n = NULL){
  edge <- 0
  nbrs <- getneibours(ind, n)
  nbr <- nbrs[side]
  if(nbr!=0) if(board$on[nbr] == stone)
     edge <- nbr
  return(edge)
}

#############################################################
# edges: update the edge with the index of a stone's four   #
#        sides if connected, else 0                         #
#   dep: edge                                               #
#############################################################
edges <- function(stone = NULL, ind = NULL, board = NULL, n = NULL){
  edges <- NULL
  sides <- c(1:4)
  if (length(stone) == 1) {
    for(i in sides)
      edges <- c(edges, edge(stone, ind, i, board, n))
  } else {
    for(j in 1:length(stone)){
      for(i in sides)
        edges <- c(edges, edge(stone[j], ind[j], i, board, n))
    }
    edges <- matrix(edges, nrow = length(stone), ncol = length(sides), byrow = TRUE)
  }
  return(edges)  
}

#############################################################
# region: update the region of the game                     #
#   dep: getedges                                           #
#############################################################
region <- function(stone = NULL, play = NULL, ind = NULL, n = NULL){
  conns <- getedges(play[[1]], ind)
  region <- NULL
  while(length(conns) != 0){
    region <- c(region, conns)
    temp <- NULL
    for(i in conns){
      temp <- c(temp, getedges(play[[1]],i))
    }
    conns <- unique(temp)
    conns <- conns[conns!=0 & !conns %in% region]
  }
  region <- unique(c(region, ind))
  region <- c(region, rep(0, n*n - length(region)))
  play[[2]][ind,] <- region
  
  for(i in region[region!=0]) play[[2]][i,] <- region
  
  return(play[[2]])
}

#############################################################
# getedges: return the indeces of a stone's edges           #
#   dep: edges                                              #
#############################################################
getedges <- function(board = NULL, ind = NULL){
  edges <- board[ind, c(1:4)]
  edges <- edges[edges != 0]
  return(edges)
}

#############################################################
# getneibours: return the ordered indeces of a stone's      #
#              four sides - l, u, r, d                      #
#   dep: findon                                             #
#############################################################
getneibours <- function(ind = NULL, n = NULL){
  nbrs <- rep(0,4)
  on <- findon(ind, n)
  
  if(on[1] > 1) nbrs[1] <- ind - n
  if(on[2] < n) nbrs[2] <- ind + 1
  if(on[1] < n) nbrs[3] <- ind + n
  if(on[2] > 1) nbrs[4] <- ind - 1
  return(nbrs)
}

#############################################################
# getneibours8: return the unordered indeces of a stone's   #
#               eight sides - four sides plus diagonals     #
#   dep: findon                                             #
#############################################################
getneibours8 <- function(ind = NULL, n = NULL){
  nbrs8 <- NULL
  on <- findon(ind, n)
  
  if(on[1] > 1) {
    nbrs8 <- c(nbrs8, ind - n)
    if(on[2] < n) nbrs8 <- c(nbrs8, ind - n + 1)
    if(on[2] > 1) nbrs8 <- c(nbrs8, ind - n - 1)
  }
  if(on[2] < n) nbrs8 <- c(nbrs8, ind + 1)
  if(on[1] < n) {
    nbrs8 <- c(nbrs8, ind + n)
    if(on[2] < n) nbrs8 <- c(nbrs8, ind + n + 1)
    if(on[2] > 1) nbrs8 <- c(nbrs8, ind + n - 1)
  }
  if(on[2] > 1) nbrs8 <- c(nbrs8, ind - 1)
  
  return(nbrs8)
}

#############################################################
# getregion: return the indeces of a stone's region         #
#############################################################
getregion <- function(region, ind){
  reg <- region[ind,]
  reg <- reg[reg != 0]
  return(reg)
}

#############################################################
# getneibours.reg: return the indeces of the region of      #
#                  a stone's four sides                     #
#   dep: getregion; getneibours                             #
#############################################################
getneibours.reg <- function(ind = NULL, play = NULL, n = NULL){
  nbrs.reg <- NULL
  reg <- getregion(play[[2]], ind)
  for(i in reg){
    nbrs.reg <- c(nbrs.reg, getneibours(i, n))
  }
  nbrs.reg <- unique(nbrs.reg)
  nbrs.reg <- nbrs.reg[nbrs.reg != 0 & !nbrs.reg %in% reg]
  return(nbrs.reg)
}

#############################################################
# addstone: add a stone on the game board                   #
#   dep: findindex; edges; region; capturestone; checkeye;  #
#        getneibours8; getneibours; getneibours.reg;        #
#        updateterr; updateallow;                           #
#############################################################
addstone <- function(stone = NULL, on = NULL, play = NULL, i = 0, n = NULL){
  ind <- findindex(on,n)
  play$board$on[ind] <- stone
  play$board$playnum[ind] <- i
  play$board$kocount <- sapply(play[[1]]$ko, function(x) max(0, x-1))
  
  # update edges
  play$board[ind,c(1:4)] <- edges(stone, ind, play[[1]], n)
  
  # update connected neibours
  nbrs <- getneibours(ind, n)
  nbrs <- nbrs[nbrs!=0]
  for(j in nbrs){
    play$board[j,c(1:4)] <- edges(play$board$on[j], j, play$board, n)
  }
  
  # update regions
  play$region <- region(stone, play, ind, n)
  
  # update disconnected regions
  nedges <- nbrs[!nbrs %in% getedges(play$board, ind)]
  for(e in nedges){
    play$region <- region(play$board$on[e], play, e, n)
  }
  
  # capture 
  for(j in nedges){
    play <- capturestone(stone, j, play, n)
  }
  
  # get neibours of the region
  nbrs.reg <- getneibours.reg(ind, play, n)
  
  # check restrictions
  if(sum(play$board$on[nbrs.reg] == 0) == 0){
    stop(paste(paste("Move not allowed at ", on[1]), on[2]))    
  }
  
  if(play$board$kocount[ind] > 1){
    stop(paste(paste("Move not allowed at ", on[1]), on[2]))
  }
  
  # update territory
  play$board$terr[ind] <- 0
  for(t in nedges){
    play <- updateterr(stone, t, play, n)
  }
  
  # update allowance
  play$board[ind, 10:11] <- FALSE
  for(u in nbrs.reg){
    play <- updateallow(stone, u, play, n)
  }
  
  # update eyes
  nbrs8 <- getneibours8(ind, n)
  for(k in nbrs8){
    play$board[k, 11 + stone] <- checkeye(stone, k, play[[1]], n)
  }
  play$board[ind, 12:13] <- FALSE
  
  # update scores
  play$scores$terr.black <- sum(play[[1]]$terr == 1)
  play$scores$terr.white <- sum(play[[1]]$terr == 2)
  play$scores$on.black <- sum(play[[1]]$on == 1)
  play$scores$on.white <- sum(play[[1]]$on == 2)
  
  return(play)
}

#############################################################
# checkatari: return true if the given index is in atari    #
#             - its region is surrounded by the opponent    #
#   dep: getneibours.reg                                    #
#############################################################
checkatari <- function(stone = NULL, ind = NULL, play = NULL, n = NULL){
  atari <- FALSE
  if(play$board$on[ind] != 0 & 
     all(play$board$on[getneibours.reg(ind, play, n)] == stone)){
    atari <- TRUE
  }
  return(atari)
}

#############################################################
# checkeye: return true if the given index is an eye        #
#           - it's empty surrounded by a single colour      #
#           i.e., > 7 of the eight neighbours in the middle #
#                 or all on the sides/corners               #
#   dep: getneibours; getneibours8                          #
#############################################################
checkeye <- function(stone = NULL, ind = NULL, board = NULL, n = NULL){
  eye <- FALSE
  nbrs <- getneibours(ind, n)
  nbrs <- nbrs[nbrs != 0]
  if(board$on[ind] == 0 & all(board$on[nbrs] == stone)){
    nbrs8 <- getneibours8(ind, n) 
    nbrs8 <- nbrs8[!nbrs8 %in% nbrs]
    if(all(board$on[nbrs8] == stone) | 
      sum(board$on[nbrs8] == stone) == 3){
      eye <- TRUE
    }
  }
    
  return(eye)
}

#############################################################
# capturestone: update the game by removing captured stones #
#   dep: checkatari; getregion; getneibours.reg;            #                                    #
#        updateallow                                        #
#############################################################
capturestone <- function(stone = NULL, ind = NULL, play = NULL, n = NULL){
  if(checkatari(stone, ind, play, n)){
    reg <- getregion(play$region, ind)
    play$scores[stone] <- play$scores[stone] + length(reg)
    play$board$on[reg] <- 0
    if(length(reg) == 1){
      play$board$kocount[reg] <- 2
    }
    
    play$board[reg, 10:11] <- TRUE
    reg.nbr <- getneibours.reg(ind, play, n)
    for (i in reg.nbr){
      play <- updateallow(stone, i, play, n)
    }
  }
  return(play)
}

#############################################################
# updateallow: update the list of game's allowed moves      #
#   dep: edges; region; checkatari;                         #
#        getneibours; getneibours.reg                       #
#############################################################
updateallow <- function(stone = NULL, ind = NULL, play = NULL, n = NULL){
  play_ <- play
  if(play$board$on[ind] == 0){
    allow <- play$board[ind, 10:11]
    play$board$on[ind] <- stone%%2 +1 
    play$board[ind, 1:4] <- edges(stone%%2 + 1, ind, play$board, n)
    play$region <- region(stone%%2 + 1, play, ind, n)
    
    if(checkatari(stone, ind, play, n)){
      nbrs <- getneibours(ind, n)
      nbrs <- nbrs[nbrs != 0]
      atari <- NULL
      for(i in nbrs){
        atari <- c(atari, checkatari(stone%%2 + 1, i, play, n))
      }
      if(all(!atari)){
        allow[3 - stone] <- FALSE
        allow[stone] <- TRUE
      }
      else if(all(atari)){
        allow[3 - stone] <- TRUE
        allow[stone] <- FALSE
      }
      else {
        allow[3 - stone] <- TRUE
      }
    }
    else {
      allow[3 - stone] <- TRUE
      play$board$on[ind] <- stone
      play$board[ind, 1:4] <- edges(stone, ind, play$board, n)
      play$region <- region(stone, play, ind, n)
      if(checkatari(stone%%2 + 1, ind, play, n))
        allow[stone] <- FALSE
    }
    play_$board[ind, 10:11] <- allow  
  } else {
    nbrs.reg <- getneibours.reg(ind, play_, n)
    nbrs.reg <- nbrs.reg[play$board$on[nbrs.reg] == 0]
    for(u in nbrs.reg){
      play_ <- updateallow(stone%%2 + 1, u, play_, n)
    }
  }
  return(play_)
}

#############################################################
# updateallow: update the board's territory                 #
#   dep: getregion; getneibours.reg                         #
#############################################################
updateterr <- function(stone = NULL, ind = NULL, play = NULL, n = NULL){
  if(play$board$on[ind] == 0)
    if(all(play$board$on[getneibours.reg(ind, play, n)] == stone)){
    play$board$terr[getregion(play$region, ind)] <- stone
    } else play$board$terr[getregion(play$region, ind)] <- 0
  return(play)
}

#############################################################
# isvalid: return true if the move is valid                 #
#          i.e., currently empty and within bounds          #
#   dep: findindex                                          #
#############################################################
isvalid <- function(stone = NULL, on = NULL, board = NULL, n = NULL){
  ind <- findindex(on, n)
  valid <- NULL
  valid <- c(board$on[ind] == 0, on >= 1, on <= n) 
  # emmpty + not out of bounds
  return(all(valid))
}

#############################################################
# findindex: return the index matching the given (x,y)      #
#            coordinates                                    #
#############################################################
findindex <- function(on,n){
  return((on[1]-1)*n + on[2])
}

#############################################################
# findon: return the (x,y) coordinates matching the given   #
#         index                                             #
#############################################################
findon <- function(ind, n){
  return(c(ceiling(ind/n), (ind-1) %% n + 1))
}

#############################################################
# playstone: add stone if the given move is valid           #
#   dep: isvalid; addstone                                  #
#############################################################
playstone <- function(stone = NULL, on = NULL, play = NULL, i = 0, n = NULL){
  if(isvalid(stone, on, play$board, n)){
    # add stone
    play <- addstone(stone, on, play, i, n)
    return(play)
  }
  else{
    stop("Invalid position")
  }
}

#############################################################
# winner: return the winner of the game based on current    #
#         board's territory + onboard + komi counts         #
#############################################################
winner <- function(play = NULL, komi = NULL){
  winner <- ifelse((play$scores$terr.black + sum(play[[1]]$on == 1)) 
                   > (play$scores$terr.white + sum(play[[1]]$on == 2) 
                      + komi),
                   1, 2)
  return(winner)
}

#############################################################
# plotboard: plot a (n x n) baduk board                     #
#############################################################
plotboard <- function(n = NULL){
  refpos <- c(ceiling((n+1)/4-1),(n+1)/2,floor((n+1)*3/4+1))
  refpts <- data.frame(x = rep(refpos,3), y = rep(refpos,each=3))
  par(mar = c(0,4,4,4))
  plot(refpts, xlim = c(1,n), ylim = c(1,n), 
       xlab = "", ylab = "", axes = FALSE, pch = 16,
       col = grey(0.5), mar = 0)
  # title(main = "mmbaduk v0.1", line = 3)
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
plotgame <- function(play = NULL, n = NULL, i = NULL,
                     plotscrs = TRUE){
  blacks <- subset(play[[1]], on == 1)
  whites <- subset(play[[1]], on == 2)
  
  if(plotscrs)
    layout(matrix(c(1,2),2,1), heights = c(0.8, 0.15))
  plotboard(n)
  points(blacks$x, blacks$y, pch = 16, cex = 2.5)
  points(whites$x, whites$y, pch = 21, bg = "white", cex = 2.5)
  
  if(sum(play$board$playnum == i) > 0)
    text(play$board$x[play$board$playnum == i], 
         play$board$y[play$board$playnum == i], 
         labels = i, col = "red", cex = 0.8)
  
  if(plotscrs)
    plotscores(play$scores)
}

#############################################################
# plotterr: plot the game state with territories            #
#  dep:plotboard; plotscores                                #
#############################################################
plotterr <- function(play = NULL, n = NULL, plotscrs = TRUE){
  blacks <- subset(play$board, on == 1)
  whites <- subset(play$board, on == 2)
  black.terr <- subset(play$board, terr == 1)
  white.terr <- subset(play$board, terr == 2)
  
  if(plotscrs)
    layout(matrix(c(1,2),2,1), heights = c(0.8, 0.15))
  
  plotboard(n)
  points(black.terr$x, black.terr$y, pch = 16, 
         col = grey(0.7), cex = 2.2)
  points(white.terr$x, white.terr$y, pch = 21, 
         col = grey(0.7), bg = grey(0.9), cex = 2.2)
  
  points(blacks$x, blacks$y, pch = 16, cex = 2.5)
  points(whites$x, whites$y, pch = 21, bg = "white", cex = 2.5)
  
  if(nrow(blacks) >0)
    text(blacks$x, blacks$y, labels = blacks$playnum, 
         col = "white", cex = 0.8)
  if(nrow(whites) >0)
    text(whites$x, whites$y, labels = whites$playnum, 
         col = "black", cex = 0.8)
  
  if(plotscrs)
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
