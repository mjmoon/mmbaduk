#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# read SGF games                                            #
#                                                           #
#############################################################
#############################################################
# getgames: read SGF game records                           #
#############################################################
getgames <- function(dir = NULL){
  files <- list.files(path = dir, pattern = ".sgf", recursive = TRUE)
  records <- NULL
  games <- NULL
  komi <- NULL
  winners <- NULL
  advs <- NULL
  for (i in 1:length(files)) {
    # for (i in 1:1) {
    # read individual games
    records <- as.matrix(read.table(paste(dir,files[i],sep = ""), 
                                    stringsAsFactors = FALSE))
    
    # read game plays
    rec <- records[length(records)]
    rec <- strsplit(rec, ";")[[1]][-1]
    plays <- ifelse(substr(rec, 1, 1) == "B", 1, 2)
    x <- sapply(rec, function(x) ifelse(length(which(letters == substr(x, 3,3))) > 0, 
                                        which(letters == substr(x, 3,3)), 0))
    y <- sapply(rec, function(x) ifelse(length(which(letters == substr(x, 3,3))) > 0, 
                                        which(letters == substr(x, 4,4)), 0))
    plays <- cbind(plays, unlist(x))
    plays <- cbind(plays, unlist(y))
    games[[i]] <- plays
    komi[i] <- as.numeric(
      substr(records[which(grepl("KM", records))], 4, nchar(records[which(grepl("KM", records))]) - 1)
    )
    
    if(max(0,which(grepl("RE", records))) > 0){
      winner <- substr(records[which(grepl("RE", records))],4,4)
      winners[i] <- ifelse(winner == "W", 2, ifelse(winner == "B", 1, 0))
    }
    
    
    # read advantage moves
    abi <- max(0,which(grepl("AB", records)))
    ab <- NULL
    advs[i] <- 0
    if(abi > 0){
      advs[i] <- as.integer(
        substr(records[which(grepl("HA", records))],4,4)
      )
      ab <- do.call(paste, as.list(
        records[
          abi:(abi -1 + advs[i])]
      ))
      ab <- substring(ab, 3)
      ab <- strsplit(ab, " ")[[1]]
      
      abs_ <- rep(1, length(ab))
      abs_ <- cbind(abs_, unlist(sapply(ab, function(x) which(letters == substr(x, 2,2)))))
      abs_ <- cbind(abs_, unlist(sapply(ab, function(x) which(letters == substr(x, 3,3)))))
      games[[i]] <- rbind(abs_, plays)
    }
    colnames(games[[i]]) <- c("stone", "x", "y")
  }
  return(list(game = games, winner = winners, komi = komi, advs = advs))
  
}

#############################################################
# replaymmbd: replay games loaded from SGF files            #
#          return a list of game state for each play        #
#   dep: initboard; playstone;                              #
#                                                           #
#  plays:     the game states from 1 to maxi                #
#############################################################
replaymmbd <- function(game = NULL, komi = 7.5, size = 19, maxi = NULL, plotmove = FALSE){
  plays <- list()
  if(is.null(maxi)) maxi = nrow(game)
  i <- 1
  plays[[1]] <- play <- initboard(size)
  for(i in 1:maxi){
    on <- unlist(game[i,2:3])
    if(all(on == c(0, 0))) {
      plays[[i+1]] <- play
      next()
    }
    play <- playstone(unlist(game[i,1]), on, plays[[i]], i, size)
    plays[[i+1]] <- play
    
    if(plotmove){
      plotgame(play, size, i)
      plt <- readline(prompt = "Press any key for next move. Press 'q' to quit plotting.\n")
      plotmove = (plt != "q" & plt != "Q")
    }
  }
  plotterr(play, size)
  plays[[i+1]] <- play
  return(plays)
}