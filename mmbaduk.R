#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
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
#  chain: a matrix representing connected chains            #
#          for each position                                #
#  scores: a vector tracking capture, on-board, and         #
#         territory counts                                  #
#############################################################
initboard <- function(size, loadgame = NULL){
  if(!is.null(loadgame)){
    return(loadgame)
  }
  
  board <- data.frame(l = rep(0, size*size), #1: index for a position to the left
                      u = rep(0, size*size), #2: index for a position up
                      r = rep(0, size*size), #3: index for a position to the right
                      d = rep(0, size*size), #4: index for a position down
                      x = rep(c(1:size), each = size), #5: x coordinate
                      y = rep(c(1:size), size),        #6: y coordinate
                      on = factor(rep(0, size*size), levels = c(0,1,2)), #7: stone colour
                      playnum = rep(0, size*size),          #8: last play number
                      terr = rep(0, size*size),             #9: territory
                      eye.black = rep(FALSE, size*size),    #10: is black's eye
                      eye.white = rep(FALSE, size*size),    #11: is white's eye
                      kocount = rep(0, size*size)           #12: ko count
                      )          
  
  board[, 1:4] <- edges(rep(0, (size*size)), 1:(size*size), board, size)
  
  chain <- matrix(rep(c(1:(size*size)), size*size), nrow = size*size,
                   byrow = TRUE)
  
  scores <- data.frame(cap.black = 0, cap.white = 0, 
                       terr.black = 0, terr.white = 0,
                       on.black = 0, on.white = 0, 
                       winner = 0)
  
  return(list(board = board, chain = chain, scores = scores))
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
# winner: return the winner of the game based on current    #
#         board's territory + onboard + komi counts         #
#############################################################
winner <- function(play = NULL, komi = NULL){
  winner <- ifelse((play$scores$terr.black + play$scores$on.black) 
                   > (play$scores$terr.white + play$scores$on.white + komi),
                   1, 2)
  return(winner)
}

#############################################################
# getnbrs: return the ordered indeces of a stone's          #
#              four sides - l, u, r, d                      #
#   dep: findon                                             #
#############################################################
getnbrs <- function(ind = NULL, n = NULL, rm.0 = TRUE){
  nbrs <- rep(0,4)
  on <- findon(ind, n)
  
  if(on[1] > 1) nbrs[1] <- ind - n
  if(on[2] < n) nbrs[2] <- ind + 1
  if(on[1] < n) nbrs[3] <- ind + n
  if(on[2] > 1) nbrs[4] <- ind - 1
  
  if(rm.0) return(nbrs[nbrs != 0])
  else return(nbrs)
}

#############################################################
# getnbrs8: return the unordered indeces of a stone's       #
#               eight sides - four sides plus diagonals     #
#   dep: findon                                             #
#############################################################
getnbrs8 <- function(ind = NULL, n = NULL){
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
# edge: update the edge with the index of a stone's given   #
#       side if connected, else 0                           #
#   dep: getnbrs                                            #
#############################################################
edge <- function(stone = NULL, ind = NULL, side = NULL, board = NULL, n = NULL){
  edge <- 0
  nbrs <- getnbrs(ind, n, FALSE)
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
  if(length(ind) > 1)
    return(t(mapply(edges, stone = stone, ind = ind, 
             MoreArgs = list(board = board, n = n))))
  else
    return(sapply(c(1:4), edge, stone = stone, ind = ind, board = board, n = n))
}

#############################################################
# getedges: return the indeces of a stone's edges           #
#############################################################
getedges <- function(board = NULL, ind = NULL){
  edges <- board[ind, c(1:4)]
  edges <- edges[edges != 0]
  return(edges)
}

#############################################################
# getchain: return the indeces of a stone's chain           #
#############################################################
getchain <- function(ind, chain){
  if(length(ind) > 1)
    return(sapply(ind, getchain, chain = chain))
  else{
    chn <- chain[ind,]
    chn <- unique(chn[chn != 0])
    return(chn)
  }
}

#############################################################
# getnbrs.chain: return the indeces of the chain's neibour  #
#   dep: getchain; getnbrs                                  #
#############################################################
getnbrs.chain <- function(ind = NULL, chain = NULL, n = NULL){
  if(length(ind) > 1){
    return(sapply(ind, getnbrs.chain, chain, n))
  } else {
    chn <- getchain(ind, chain)
    nbrs.chn <- unlist(sapply(chn, getnbrs, n))
    nbrs.chn <- unique(nbrs.chn[nbrs.chn != 0 & !nbrs.chn %in% chn])
    return(nbrs.chn)
  }
}

#############################################################
# getliberty: returns the number of liberties; number of    #
#               empty adjascent points (max 8)              #
#   dep: getnbrs.chain;                                     #
#############################################################
getliberty <- function(ind = NULL, play = NULL, n = NULL){
  if(length(ind) > 1){
    return(sapply(ind, getliberty, play = play, n = n))
  } else {
    return(sum(play$board$on[getnbrs.chain(ind, play$chain, n)] == 0))
  }
}

#############################################################
# chain: update the chain of the game                       #
#   dep: getedges;                                          #
#############################################################
updatechain <- function(stone = NULL, ind = NULL, play = NULL, n = NULL){
  while(length(ind) > 0){
    conns <- getedges(play$board, ind[1])
    chain <- NULL
    while(length(conns) > 0){
      chain <- c(chain, conns)
      conns <- 
          unlist(
            sapply(conns, getedges, board = play$board)
          )
      conns <- unique(conns[!conns %in% chain])
    }
    chain <- unique(c(ind[1], chain))
    play$chain[chain, ] <- rep(c(chain, rep(0, n*n - length(chain))), each = length(chain))
    ind <- ind[!ind %in% chain]
  }
  return(play$chain)
}

#############################################################
# checkeye: return true if the given index is an eye        #
#           - it's empty surrounded by a single colour      #
#           i.e., > 7 of the eight neighbours in the middle #
#                 or all on the sides/corners               #
#   dep: getnbrs; getnbrs8;                                 #
#############################################################
checkeye <- function(stone = NULL, ind = NULL, board = NULL, n = NULL){
  if(length(ind) > 1){
    return(sapply(ind, checkeye, stone = stone, board = board, n = n))
  } else {
    eye <- FALSE
    nbrs <- getnbrs(ind, n)
    if(board$on[ind] == 0 & all(board$on[nbrs] == stone)){
      nbrs8 <- getnbrs8(ind, n) 
      nbrs8 <- nbrs8[!nbrs8 %in% nbrs]
      if(all(board$on[nbrs8] == stone) | 
         sum(board$on[nbrs8] == stone) == 3){
        eye <- TRUE
      }
    }
    return(eye)
  }
}

#############################################################
# capturestone: update the game by removing captured stones #
#   dep: getliberty; getchain;                              #
#############################################################
capturestone <- function(stone = NULL, ind = NULL, ind.p = NULL, play = NULL, n = NULL){
  for(i in ind){
    if(play$board$on[i] != 0 & getliberty(i, play, n) == 0){
      chn <- getchain(i, play$chain)
      play$scores[stone] <- play$scores[stone] + length(chn)
      play$board$on[chn] <- 0
      if(length(chn) == 1 & 
         length(getchain(ind.p, play$chain)) == 1){
        play$board$kocount[chn] <- 2
        }
      }
  }
  return(play)
}

#############################################################
# updateterr: update the board's territory                  #
#   dep: getchain; getnbrs.chain;                           #
#############################################################
updateterr <- function(stone = NULL, ind = NULL, play = NULL, n = NULL){
  while(length(ind) > 0){
    chain <- getchain(ind[1], play$chain)
    if(play$board$on[ind[1]] == 0 & 
       all(play$board$on[getnbrs.chain(ind[1], play$chain, n)] == stone)) {
      play$board$terr[chain] <- stone
    } else {
      play$board$terr[chain] <- 0
    }
    ind <- ind[!ind %in% chain]
  }
  return(play)
}

#############################################################
# checklegal: check if a move is legal                      #
#   dep: getnbrs; getliberty;                               #
#############################################################
checklegal <- function(stone = NULL, ind = NULL, play = NULL, n = NULL, kocheck = 2){
  if(length(ind) > 1) {
    return(sapply(ind, checklegal, stone = stone, play = play, n = n))
  }
  else {
    if(play$board$on[ind] != 0) return(FALSE)
    if(play$board$kocount[ind] == kocheck) return(FALSE)
    
    nbrs <- getnbrs(ind, n)
    
    # any empty neighbour ? #
    emptynbrs <- sum(play$board$on[nbrs] == 0) > 0
    
    # any same-colour neighour with liberty > 1 ? #
    libsame <- any(sapply(nbrs, 
                          function(x) ifelse(play$board$on[x] == stone, 
                                             getliberty(x, play, n), 0)
    ) > 1)
    
    # any opposite-colour neighbour with liberty == 1 ? #
    liboppo <- any(sapply(nbrs, 
                          function(x) ifelse(play$board$on[x] == stone%%2 + 1, 
                                             getliberty(x, play, n), 0)
    ) == 1)
    
    return(ifelse(any(emptynbrs, libsame, liboppo), TRUE, FALSE)) 
  }
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
#  dep:plotboard; plotscores;                               #
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
#  dep:plotboard; plotscores;                               #
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
#  dep:plotboard; plotscores;                               #
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

#############################################################
# playstone: add a stone on the game board                  #
#   dep: findindex; checklegal; getnbrs; getnbrs8; edges;   #
#        getedges; updatechain; capturestone; updateterr;   #
#        checkeye;                                          #
#############################################################
playstone <- function(stone = NULL, on = NULL, play = NULL, i = 0, n = NULL){
  ind <- findindex(on,n)
  
  if(!all(play$board$on[ind] == 0 , on >= 1, on <= n)){
    stop("Invalid position")
  }
  
  if(!checklegal(stone, ind, play, n)){
    stop("Illegal move")
  }
  
  play$board$on[ind] <- stone
  play$board$playnum[ind] <- i
  play$board$kocount <- sapply(play$board$ko, function(x) max(0, x-1))
  
  # neighbours
  nbrs <- getnbrs(ind, n)      # four-way neighbours
  nbrs8 <- getnbrs8(ind, n)    # eight-way neighbours
  
  # update edges
  play$board[c(ind, nbrs),c(1:4)] <- 
    edges(play$board$on[c(ind, nbrs)], c(ind, nbrs), play$board, n)
  
  # non-edges
  nedges <- nbrs[!nbrs %in% getedges(play$board, ind)]  
  
  # update chains
  play$chain <- updatechain(stone, c(ind, nbrs), play, n)
  
  # capture 
  play <- capturestone(stone, nedges, ind, play, n)
  
  # update territory
  play <- updateterr(stone, nedges, play, n)
  
  # update eyes
  play$board[nbrs8, 9 + stone] <- checkeye(stone, nbrs8, play$board, n)
  play$board$eye.black[ind] <- FALSE
  play$board$eye.white[ind] <- FALSE
  
  # update scores
  play$scores$terr.black <- sum(play$board$terr == 1)
  play$scores$terr.white <- sum(play$board$terr == 2)
  play$scores$on.black <- sum(play$board$on == 1)
  play$scores$on.white <- sum(play$board$on == 2)
  
  return(play)
}
