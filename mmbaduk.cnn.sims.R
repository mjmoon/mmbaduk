#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# generate simulated data for cnn                           #
#                                                           #
#############################################################
source("mmbaduk.cnn.R")

t0 <- proc.time()
sims01 <- cnnrandsim(250)
proctime01 <- proc.time() - t0
save(sims01, proctime01, file = "./sims/sim01-250.RData")

t0 <- proc.time()
sims02 <- cnnrandsim(250)
proctime02 <- proc.time() - t0
save(sims02, proctime02, file = "./sims/sim02-250.RData")

t0 <- proc.time()
sims03 <- cnnrandsim(250)
proctime03 <- proc.time() - t0
save(sims03, proctime03, file = "./sims/sim03-250.RData")

t0 <- proc.time()
sims04 <- cnnrandsim(250)
proctime04 <- proc.time() - t0
save(sims04, proctime04, file = "./sims/sim04-250.RData")

t0 <- proc.time()
sims05 <- cnnrandsim(250)
proctime05 <- proc.time() - t0
save(sims05, proctime05, file = "./sims/sim05-250.RData")

t0 <- proc.time()
sims06 <- cnnrandsim(250)
proctime06 <- proc.time() - t0
save(sims06, proctime06, file = "./sims/sim06-250.RData")

t0 <- proc.time()
sims07 <- cnnrandsim(250)
proctime07 <- proc.time() - t0
save(sims07, proctime07, file = "./sims/sim07-250.RData")

t0 <- proc.time()
sims08 <- cnnrandsim(250)
proctime08 <- proc.time() - t0
save(sims08, proctime08, file = "./sims/sim08-250.RData")

# t0 <- proc.time()
# sims00 <- cnnrandsim(5)
# proctime00 <- proc.time() - t0
# 
# str(sims00$state)
