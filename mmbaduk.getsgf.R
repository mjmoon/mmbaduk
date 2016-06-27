#############################################################
# mmbaduk v0.1 - Baduk (Go) player by michael moon          #
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
  rules <- NULL
  winners <- NULL
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
    rules[i] <- records[which(grepl("RU", records))]
    
    if(max(0,which(grepl("RE", records))) > 0){
      winner <- substr(records[which(grepl("RE", records))],4,4)
      winners[i] <- ifelse(winner == "W", 2, ifelse(winner == "B", 1, 0))
    }
    
    
    # read advantage moves
    abi <- max(0,which(grepl("AB", records)))
    ab <- NULL
    if(abi > 0){
      ab <- do.call(paste, as.list(
        records[
          abi:(abi -1 + as.integer(
            substr(records[which(grepl("HA", records))],4,4)
          ))]
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
  return(list(games, winners, rules))
  
}
