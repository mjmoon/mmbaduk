#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# convolutional neural network                              #
#                                                           #
#############################################################
source("mmbaduk.play.R")

#############################################################
# countcapture: returns the number of stones that would be    #
#             captured after the given play                 #
#   dep: getneibours; getedges; getchain; checkcapture       #
#############################################################
countcapture <- function(stone = NULL, ind = NULL, play = NULL, size = NULL){
  nbrs <- getnbrs(ind, size)
  play$board$on[ind] <- stone
  play$board[ind, 1:4] <- edges(stone, ind, play$board, size)
  nedges <- nbrs[!nbrs %in% getedges(play$board, ind)]
  
  count <- length(unique(unlist(
    lapply(nedges, function(x) if(play$board$on[x] != 0 & getliberty(x, play, size) == 0) 
      getchain(x, play$chain))
  )))
  return(count)
}

#############################################################
# isatari: returns TRUE if the move puts oppponent at atari #
#############################################################
isatari <- function(stone = NULL, ind = NULL, play = NULL, size = NULL){
  nbrs <- getnbrs(ind, size)
  play$board$on[ind] <- stone
  play$board[ind, 1:4] <- edges(stone, ind, play$board, size)
  nedges <- nbrs[!nbrs %in% getedges(play$board, ind)]
  return(any(sapply(nedges, getliberty, play = play, n = size) == 1))
}

#############################################################
# isatariescape: returns TRUE if the move escapes an atari  #
#                successfully (i.e., liberty from 1 to >2)  #
#############################################################
isatariescape <- function(stone = NULL, ind = NULL, play = NULL, size = NULL){
  play$board$on[ind] <- stone
  es <- edges(stone, ind, play$board, size)
  if(length(es[es != 0]) > 0){
    anyescape <- any(sapply(es[es != 0], 
                            function(x) {
                              if(getliberty(x, play, size) == 0 &
                                 sum(getliberty(es, play, size)) > 2)
                                return(TRUE)
                              else
                                return(FALSE)
                              }
                            )
                     )
  }
  else anyescape <- FALSE
  return(anyescape)
}

#############################################################
# issensible: returns TRUE if the move is legal and does    #
#             fill its own eye                              #
#############################################################
issensible <- function(stone = NULL, ind = NULL, play = NULL, size = NULL){
  checklegal(stone, ind, play, size) & !play$board[ind, 9 + stone]
}

#############################################################
# cnninput: create input features for the cnn at the given  #
#           position                                        #
#   dep: getliberty; countcapture; isladder; issensible     #
#############################################################
cnninput <- function(i = NULL, stone = NULL, ind = NULL, play = NULL, size = NULL){
  liberties.curr <- getliberty(ind, play, size)
  turns <- i - play$board$playnum[ind]
  captuersize <- countcapture(stone, ind, play, size)
  capturesize.self <- countcapture(stone%%2 + 1, ind, play, size)
  
  onehot <- rbind(c(0,0,0,0,0,0,0,1),
                   c(0,0,0,0,0,0,1,0),
                   c(0,0,0,0,0,1,0,0),
                   c(0,0,0,0,1,0,0,0),
                   c(0,0,0,1,0,0,0,0),
                   c(0,0,1,0,0,0,0,0),
                   c(0,1,0,0,0,0,0,0),
                   c(1,0,0,0,0,0,0,0))
  
  cnninput <- rep(0, 49)
  # 1: 0's - constants 
  cnninput[2] <- 1 # 2: 1's - constants
  if(stone == 1) cnninput[3] <- 1 # 3: current player is black
  if(play$board$on[ind] == 0) cnninput[4] <- 1 # 4: stone colour - 0: empty
  if(play$board$on[ind] == 1) cnninput[5] <- 1 # 5: stone colour - 1:black
  if(play$board$on[ind] == 2) cnninput[6] <- 1 # 6: stone colour - 2:white
  # number of turns since last play
  if(turns > 7) cnninput[7:14] <-  onehot[8,]      # 7
  else if(turns > 6) cnninput[7:14] <-  onehot[7,] # 8
  else if(turns > 5) cnninput[7:14] <-  onehot[6,] # 9
  else if(turns > 4) cnninput[7:14] <-  onehot[5,] # 10
  else if(turns > 3) cnninput[7:14] <-  onehot[4,] # 11
  else if(turns > 2) cnninput[7:14] <-  onehot[3,] # 12
  else if(turns > 1) cnninput[7:14] <-  onehot[2,] # 13
  else if(turns > 0) cnninput[7:14] <-  onehot[1,] # 14
  # number of liberties
  if(liberties.curr > 7) cnninput[15:22] <- onehot[8,]      # 15
  else if(liberties.curr > 6) cnninput[15:22] <- onehot[7,] # 16
  else if(liberties.curr > 5) cnninput[15:22] <- onehot[6,] # 17
  else if(liberties.curr > 4) cnninput[15:22] <- onehot[5,] # 18
  else if(liberties.curr > 3) cnninput[15:22] <- onehot[4,] # 19
  else if(liberties.curr > 2) cnninput[15:22] <- onehot[3,] # 20
  else if(liberties.curr > 1) cnninput[15:22] <- onehot[2,] # 21
  else if(liberties.curr > 0) cnninput[15:22] <- onehot[1,] # 22
  # number of captures
  if(captuersize > 7) cnninput[23:30] <- onehot[8,]      # 23
  else if(captuersize > 6) cnninput[23:30] <- onehot[7,] # 24
  else if(captuersize > 5) cnninput[23:30] <- onehot[6,] # 25
  else if(captuersize > 4) cnninput[23:30] <- onehot[5,] # 26
  else if(captuersize > 3) cnninput[23:30] <- onehot[4,] # 27
  else if(captuersize > 2) cnninput[23:30] <- onehot[3,] # 28
  else if(captuersize > 1) cnninput[23:30] <- onehot[2,] # 29
  else if(captuersize > 0) cnninput[23:30] <- onehot[1,] # 30
  # number of self-captures
  if(capturesize.self > 7) cnninput[31:38] <- onehot[,8]      # 31
  else if(capturesize.self > 6) cnninput[31:38] <- onehot[7,] # 32
  else if(capturesize.self > 5) cnninput[31:38] <- onehot[6,] # 33
  else if(capturesize.self > 4) cnninput[31:38] <- onehot[5,] # 34
  else if(capturesize.self > 3) cnninput[31:38] <- onehot[4,] # 35
  else if(capturesize.self > 2) cnninput[31:38] <- onehot[3.] # 36
  else if(capturesize.self > 1) cnninput[31:38] <- onehot[2,] # 37
  else if(capturesize.self > 0) cnninput[31:38] <- onehot[1,] # 38
  if(isatari(stone, ind, play, size)) cnninput[39] <- 1 # 39: move puts the opponent at atari
  if(isatariescape(stone, ind, play, size)) cnninput[40] <- 1 # 40: move escapes atari
  if(issensible(stone, ind, play, size)) cnninput[41] <- 1 # 41: is sensible (legal and not own eye)
  liberties.after <- ifelse(play$board$on[ind] == 0, 
                            {
                              play$board$on[ind] <- stone
                              play$board[ind,c(1:4)] <- edges(stone, ind, play$board, size)
                              play$chain <- updatechain(stone, ind, play, size)
                              nbrs <- getnbrs(ind, size)
                              nedges <- nbrs[!nbrs %in% getedges(play$board, ind)]  
                              play <- capturestone(stone, nedges, ind, play, size)
                              getliberty(ind, play, size)
                            },
                            liberties.curr)
  # number of liberties after the play
  if(liberties.after > 7) cnninput[42:49] <- onehot[8,]      # 42
  else if(liberties.after > 6) cnninput[42:49] <- onehot[7,] # 43
  else if(liberties.after > 5) cnninput[42:49] <- onehot[6,] # 44
  else if(liberties.after > 4) cnninput[42:49] <- onehot[5,] # 45
  else if(liberties.after > 3) cnninput[42:49] <- onehot[4,] # 46
  else if(liberties.after > 2) cnninput[42:49] <- onehot[3,] # 47
  else if(liberties.after > 1) cnninput[42:49] <- onehot[2,] # 48
  else if(liberties.after > 0) cnninput[42:49] <- onehot[1,] # 49
  return(cnninput)
}

#############################################################
# cnnstate: create cnn input features for the given state   #
#   dep: cnninput                                           #
#############################################################
cnnstate <- function(i = NULL, play = NULL, size = NULL, stone = NULL){
  if(is.null(stone)) stone <- (i+1) %% 2 +1
  state <- 
    vapply(c(1:(size*size)), 
           cnninput, FUN.VALUE = vector(mode = 'numeric', length = 49),
           i = i, stone = stone, play = play, size = size, 
           USE.NAMES = F)
  return(t(state))
}

#############################################################
# randsim: return a list of cnn input features each         #
#          randomly selected from n randomly simulated      #
#          games                                            #
#   dep: plammbd; cnnstate                                  #
#############################################################
cnnrandsim <- function(niter = 3, bd.size = 19, ploton = FALSE, teston = FALSE){
  rinds <- vector(mode = "integer", length = niter)
  winners <- vector(mode = "integer", length = niter)
  states <- matrix(nrow = bd.size*bd.size*49, ncol = niter)
  for(i in 1:niter){
    cat("Simulation", i, "...")
    game <- playmmbd(size = bd.size, sim = TRUE, ploton = ploton, teston = teston)
    # last two moves are excluded (always passes)
    rind <- sample.int(length(game$boards) - 2, 1) 
    states[,i] <- cnnstate(rind, game$boards[[rind]], bd.size)
    winners[i] <- ifelse(game$winner == game$moves[rind,1], 1, -1)
    rinds[i] <- rind
    }
  return(list(state = states, res = winners, playnum = rinds))
}

