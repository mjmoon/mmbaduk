#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# construct the convultional neural network structure       #
#                                                           #
#############################################################
library(mxnet)
tempwd <- getwd()
setwd("./sims")
filenames <- list.files(pattern = "*.RData")
filesloaded <- lapply(filenames, load, .GlobalEnv)
setwd(tempwd)

## define input
cnn.in <- list(state = array(cbind(sims01$state, sims02$state, sims03$state, sims04$state,
                         sims05$state, sims06$state, sims07$state, sims08$state),
                         dim = c(19,19,49, 250*8)),
               res = c(sims01$res, sims02$res, sims03$res, sims04$res,
                          sims05$res, sims06$res, sims07$res, sims08$res),
               playnum = c(sims01$playnum, sims02$playnum, sims03$playnum, sims04$playnum,
                           sims05$playnum, sims06$playnum, sims07$playnum, sims08$playnum))

cnn.inL <- cnn.in
cnn.inL$res <- cnn.in$res == 1

set.seed(20150819)
trainind <- sample.int(2000, 1500)
testind <- which(!1:2000 %in% trainind)

## define model structure : nfilter = 32
nfil = 32 # number of filters; AlphaGo: 196
data <- mx.symbol.Variable('data') # input
conv1 <- mx.symbol.Convolution(
  name = "conv1",
  data = data, 
  pad = c(2,2),     ## zero-pads input 19 x 19 to 23 x 23
  kernel = c(5,5),  ## 5 x 5 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu1 <- mx.symbol.Activation(data = conv1, act_type = "relu")

conv2 <- mx.symbol.Convolution(
  name = "conv2",
  data = relu1, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  no.bias = TRUE,
  num_filter = nfil  
)
relu2 <- mx.symbol.Activation(data = conv2, act_type = "relu")

conv3 <- mx.symbol.Convolution(
  name = "conv3",
  data = relu2, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu3 <- mx.symbol.Activation(data = conv3, act_type = "relu")

conv4 <- mx.symbol.Convolution(
  name = "conv4",
  data = relu3, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu4 <- mx.symbol.Activation(data = conv4, act_type = "relu")

conv5 <- mx.symbol.Convolution(
  name = "conv5",
  data = relu4, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu5 <- mx.symbol.Activation(data = conv5, act_type = "relu")

conv6 <- mx.symbol.Convolution(
  name = "conv6",
  data = relu5, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu6 <- mx.symbol.Activation(data = conv6, act_type = "relu")

conv7 <- mx.symbol.Convolution(
  name = "conv7",
  data = relu6, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu7 <- mx.symbol.Activation(data = conv7, act_type = "relu")

conv8 <- mx.symbol.Convolution(
  name = "conv8",
  data = relu7, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu8 <- mx.symbol.Activation(data = conv8, act_type = "relu")

conv9 <- mx.symbol.Convolution(
  name = "conv9",
  data = relu8, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu9 <- mx.symbol.Activation(data = conv9, act_type = "relu")

conv10 <- mx.symbol.Convolution(
  name = "conv10",
  data = relu9, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu10 <- mx.symbol.Activation(data = conv10, act_type = "relu")

conv11 <- mx.symbol.Convolution(
  name = "conv11",
  data = relu10, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu11 <- mx.symbol.Activation(data = conv11, act_type = "relu")

conv12 <- mx.symbol.Convolution(
  name = "conv12",
  data = relu11, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu12 <- mx.symbol.Activation(data = conv12, act_type = "relu")

## define model structure : nfilter = 128
nfil = 128 # number of filters; AlphaGo: 196
data <- mx.symbol.Variable('data') # input
conv1.128 <- mx.symbol.Convolution(
  name = "conv1",
  data = data, 
  pad = c(2,2),     ## zero-pads input 19 x 19 to 23 x 23
  kernel = c(5,5),  ## 5 x 5 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu1.128 <- mx.symbol.Activation(data = conv1.128, act_type = "relu")

conv2.128 <- mx.symbol.Convolution(
  name = "conv2",
  data = relu1.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu2.128 <- mx.symbol.Activation(data = conv2.128, act_type = "relu")

conv3.128 <- mx.symbol.Convolution(
  name = "conv3",
  data = relu2.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu3.128 <- mx.symbol.Activation(data = conv3.128, act_type = "relu")

conv4.128 <- mx.symbol.Convolution(
  name = "conv4",
  data = relu3.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu4.128 <- mx.symbol.Activation(data = conv4.128, act_type = "relu")

conv5.128 <- mx.symbol.Convolution(
  name = "conv5",
  data = relu4.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu5.128 <- mx.symbol.Activation(data = conv5.128, act_type = "relu")

conv6.128 <- mx.symbol.Convolution(
  name = "conv6",
  data = relu5.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu6.128 <- mx.symbol.Activation(data = conv6.128, act_type = "relu")

conv7.128 <- mx.symbol.Convolution(
  name = "conv7",
  data = relu6.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu7.128 <- mx.symbol.Activation(data = conv7.128, act_type = "relu")

conv8.128 <- mx.symbol.Convolution(
  name = "conv8",
  data = relu7.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu8.128 <- mx.symbol.Activation(data = conv8.128, act_type = "relu")

conv9.128 <- mx.symbol.Convolution(
  name = "conv9",
  data = relu8.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu9.128 <- mx.symbol.Activation(data = conv9.128, act_type = "relu")

conv10.128 <- mx.symbol.Convolution(
  name = "conv10",
  data = relu9.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu10.128 <- mx.symbol.Activation(data = conv10.128, act_type = "relu")

conv11.128 <- mx.symbol.Convolution(
  name = "conv11",
  data = relu10.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu11.128 <- mx.symbol.Activation(data = conv11.128, act_type = "relu")

conv12.128 <- mx.symbol.Convolution(
  name = "conv12",
  data = relu11.128, 
  pad = c(1,1),     ## zero-pads input 19 x 19 to 21 x 21
  kernel = c(3,3),  ## 3 x 3 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = nfil  
)
relu12.128 <- mx.symbol.Activation(data = conv12.128, act_type = "relu")

