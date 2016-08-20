#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# evaluate the value neetwork                               #
#                                                           #
#############################################################
source("mmbaduk.getsgf.R")
source("mmbaduk.cnn.R")

game06 <- getgames("./kgs-19-2016-06-new/")
game07 <- getgames("./kgs-19-2016-07-new/")

games <- c(game06$game[which(game06$komi == 7.5)], game07$game[which(game07$komi == 7.5)])
winners <- c(game06$winner[which(game06$komi == 7.5)], game07$winner[which(game07$komi == 7.5)])
advs <- c(game06$advs[which(game06$komi == 7.5)], game07$winner[which(game07$komi == 7.5)])

games <- games[which(!is.na(winners))]
winners <- winners[which(!is.na(winners))]
advs <- advs[which(!is.na(winners))]

getsgf.cnn <- function(sgfgame, advs = 0){
  ind <- sample((advs +1) : nrow(sgfgame), 1)
  play <- replaymmbd(sgfgame, maxi = ind-1)
  play <- play[[ind]]
  cnn.in <- c(cnnstate(ind, play, 19, sgfgame[ind, 1]))
  return(list(cnn.in = cnn.in, ind = ind))
}

sgf.rand <- lapply(games, getsgf.cnn)
cnn.sgf.ind <- sapply(sgf.rand, function(x) x$ind)
cnn.sgf.in <- array(sapply(sgf.rand, function(x) x$cnn.in), c(19,19,49, length(games)))

cnn.sgf.res <- ifelse((cnn.sgf.in[1,1,3,] + 1) == winners, 1, -1) 

save(cnn.sgf.in, file = "cnn.sgf.RData")

# load models #
filenames <- list.files(pattern = "*.RData")
filesloaded <- lapply(filenames, load, .GlobalEnv)

# predict #
pred.5.trn <- predict(cnn.model.5, cnn.in$state[,,,trainind], ctx = mx.cpu())
pred.15.trn <- predict(cnn.model.15, cnn.in$state[,,,trainind], ctx = mx.cpu())
pred.5.sim <- predict(cnn.model.5, cnn.in$state[,,,testind], ctx = mx.cpu())
pred.15.sim <- predict(cnn.model.15, cnn.in$state[,,,testind], ctx = mx.cpu())
pred.5.sgf <- predict(cnn.model.5, cnn.sgf.in, ctx = mx.cpu())
pred.15.sgf <- predict(cnn.model.15, cnn.sgf.in, ctx = mx.cpu())

pred.5.trn.128 <- predict(cnn.model.5.128, cnn.in$state[,,,trainind], ctx = mx.cpu())
pred.15.trn.128 <- predict(cnn.model.15.128, cnn.in$state[,,,trainind], ctx = mx.cpu())
pred.5.sim.128 <- predict(cnn.model.5.128, cnn.in$state[,,,testind], ctx = mx.cpu())
pred.15.sim.128 <- predict(cnn.model.15.128, cnn.in$state[,,,testind], ctx = mx.cpu())
pred.5.sgf.128 <- predict(cnn.model.5.128, cnn.sgf.in, ctx = mx.cpu())
pred.15.sgf.128 <- predict(cnn.model.15.128, cnn.sgf.in, ctx = mx.cpu())

save(pred.5.trn, pred.15.trn, pred.5.trn.128, pred.15.trn.128,
     pred.5.sim, pred.15.sim, pred.5.sgf, pred.15.sgf, 
     pred.5.sim.128, pred.15.sim.128, pred.5.sgf.128, pred.15.sgf.128, 
     file = "cnn.preds.RData")
load("cnn.preds.RData")
# squared errors #
sqrerrs <- data.frame(err = c(cnn.in$res[testind] - pred.5.sim)^2, model = "32, 1", data = "Test")
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[testind] - pred.15.sim)^2, model = "32, 11", data = "Test"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[testind] - pred.5.sim.128)^2, model = "128, 1", data = "Test"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[testind] - pred.15.sim.128)^2, model = "128, 11", data = "Test"))

sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[trainind] - pred.5.trn)^2, model = "32, 1", data = "Train"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[trainind] - pred.15.trn)^2, model = "32, 11", data = "Train"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[trainind] - pred.5.trn.128)^2, model = "128, 1", data = "Train"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.in$res[trainind] - pred.15.trn.128)^2, model = "128, 11", data = "Train"))

sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.sgf.res - pred.5.sgf)^2, model = "32, 1", data = "SGF"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.sgf.res - pred.15.sgf)^2, model = "32, 11", data = "SGF"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.sgf.res - pred.5.sgf.128)^2, model = "128, 1", data = "SGF"))
sqrerrs <- rbind(sqrerrs, data.frame(err = c(cnn.sgf.res - pred.15.sgf.128)^2, model = "128, 11", data = "SGF"))

mses <- aggregate(err ~ model + data, data = sqrerrs, mean)
mses$data <- factor(mses$data, levels = c("Train", "Test", "SGF"))

# plot MSEs
library(ggplot2)
png("Img/mse.png", width = 360, height = 360)
ggplot(data = mses, aes(y = err, x = model, fill = data)) + theme_minimal() +
  labs(x = "Models", y = "Mean squared errors", title = "Mean squared errors") +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(ylim = c(0.99, 1.02)) +
  scale_fill_discrete(name = "Data")
dev.off()

sgfpreds <- data.frame(preds = c(pred.5.sgf, pred.15.sgf,pred.5.sgf.128, pred.15.sgf.128), 
                      model = rep(c("32, 1", "32, 11", "128, 1", "128, 11"), each = 4),
                      ind = rep(cnn.sgf.ind, 4))

png("Img/preds.png", width = 360, height = 360)
ggplot(data = sgfpreds, aes(x = ind, y = preds, color = model)) + theme_minimal() +
  labs(x = "t", y = "Predicted value", title = "Prediction vs. sampled time") +
  geom_point(alpha = 0.8) + coord_cartesian(ylim = c(-0.1, 0)) +
  scale_color_discrete(name = "Model")
dev.off()

plot(c(pred.5.sim), col = "red")
points(c(pred.5.sgf), col = "blue")
var(sgfpreds$preds)
par(mfrow = c(1,1))
