source("mmbaduk.play.R")
source("mmbaduk.getsgf.R")

### TEST: getsgf ###
dir <- "./kgs-19-2015-05-new/"
game <- getgames(dir)
game1 <- game[[1]][[1]]
game1.winner <- game[[2]][1]

rep.game1 <- replaymmbd(game1, plotmove = FALSE)
rep.game1 <- replaymmbd(game1, plotmove = TRUE)
### TEST: play and simmulate ###
p.game <- playmmbd()
s.game <- simmmbd(5)
t.game <- testmmbd()
### TODO: get features for CNN training ###
getfeature <- function(stone = NULL, board = NULL, move = NULL){
  features <- NULL
}
