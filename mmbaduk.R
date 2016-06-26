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
  
  if(play$board$kocount[ind] > 0){
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
