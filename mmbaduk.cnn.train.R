source("mmbaduk.cnn.network.R")
batchsize = 32 # batch size; AlphaGo: 32 
nepochs = 20
######### 5 Layer ########
conv.f <- mx.symbol.Convolution(
  data = relu2, 
  name = "conv.f",
  kernel = c(1,1),  ## 1 x 1 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = 1  
)
relu.f <- mx.symbol.Activation(data = conv.f, act_type = "relu")

fc.256 <- mx.symbol.FullyConnected(data = relu.f, num.hidden = 256, name = "fc256")
relu.fc <- mx.symbol.Activation(data = fc.256, act_type = "relu")
fc.1 <- mx.symbol.FullyConnected(data = relu.fc, num.hidden = 1, name = "fc1")
tanh.cnn <- mx.symbol.Activation(data = fc.1, act.type = 'tanh')
out.cnn <- mx.symbol.LinearRegressionOutput(data = tanh.cnn, name = 'tanhout')
cnn.model.5 <- mx.model.FeedForward.create(
  out.cnn, X=cnn.in$state[,,,trainind], y=cnn.in$res[trainind], 
  optimizer = 'sgd',
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

out.cnn <- mx.symbol.LogisticRegressionOutput(data = fc.1, name = "logiout")
cnn.model.5L <- mx.model.FeedForward.create(
  out.cnn, X=cnn.inL$state[,,,trainind], y=cnn.inL$res[trainind], 
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

######### 15 Layer ########
conv.f <- mx.symbol.Convolution(
  data = relu12, 
  name = "conv.f",
  kernel = c(1,1),  ## 1 x 1 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = 1  
)
relu.f <- mx.symbol.Activation(data = conv.f, act_type = "relu")

fc.256 <- mx.symbol.FullyConnected(data = relu.f, num.hidden = 256, name = "fc256")
relu.fc <- mx.symbol.Activation(data = fc.256, act_type = "relu")
fc.1 <- mx.symbol.FullyConnected(data = relu.fc, num.hidden = 1, name = "fc1")
tanh.cnn <- mx.symbol.Activation(data = fc.1, act.type = 'tanh')
out.cnn <- mx.symbol.LinearRegressionOutput(data = tanh.cnn, name = 'tanhout')
cnn.model.15 <- mx.model.FeedForward.create(
  out.cnn, X=cnn.in$state[,,,trainind], y=cnn.in$res[trainind], 
  optimizer = 'sgd',
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

out.cnn <- mx.symbol.LogisticRegressionOutput(data = fc.1, name = "logiout")
cnn.model.15L <- mx.model.FeedForward.create(
  out.cnn, X=cnn.inL$state[,,,trainind], y=cnn.inL$res[trainind], 
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

######### 5 Layer; 128 filters ########
conv.f <- mx.symbol.Convolution(
  data = relu2.128, 
  name = "conv.f",
  kernel = c(1,1),  ## 1 x 1 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = 1  
)
relu.f <- mx.symbol.Activation(data = conv.f, act_type = "relu")

fc.256 <- mx.symbol.FullyConnected(data = relu.f, num.hidden = 256, name = "fc256")
relu.fc <- mx.symbol.Activation(data = fc.256, act_type = "relu")
fc.1 <- mx.symbol.FullyConnected(data = relu.fc, num.hidden = 1, name = "fc1")
tanh.cnn <- mx.symbol.Activation(data = fc.1, act.type = 'tanh')
out.cnn <- mx.symbol.LinearRegressionOutput(data = tanh.cnn, name = 'tanhout')
cnn.model.5.128 <- mx.model.FeedForward.create(
  out.cnn, X=cnn.in$state[,,,trainind], y=cnn.in$res[trainind], 
  optimizer = 'sgd',
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

out.cnn <- mx.symbol.LogisticRegressionOutput(data = fc.1, name = "logiout")
cnn.model.5L.128 <- mx.model.FeedForward.create(
  out.cnn, X=cnn.inL$state[,,,trainind], y=cnn.inL$res[trainind], 
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

######### 15 Layer; 128 filters ########
conv.f <- mx.symbol.Convolution(
  data = relu12.128, 
  name = "conv.f",
  kernel = c(1,1),  ## 1 x 1 kernel
  stride = c(1,1),  ## 1 stride
  num_filter = 1  
)
relu.f <- mx.symbol.Activation(data = conv.f, act_type = "relu")

fc.256 <- mx.symbol.FullyConnected(data = relu.f, num.hidden = 256, name = "fc256")
relu.fc <- mx.symbol.Activation(data = fc.256, act_type = "relu")
fc.1 <- mx.symbol.FullyConnected(data = relu.fc, num.hidden = 1, name = "fc1")
tanh.cnn <- mx.symbol.Activation(data = fc.1, act.type = 'tanh')
out.cnn <- mx.symbol.LinearRegressionOutput(data = tanh.cnn, name = 'tanhout')
cnn.model.15.128 <- mx.model.FeedForward.create(
  out.cnn, X=cnn.in$state[,,,trainind], y=cnn.in$res[trainind], 
  optimizer = 'sgd',
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

out.cnn <- mx.symbol.LogisticRegressionOutput(data = fc.1, name = "logiout")
cnn.model.15L.128 <- mx.model.FeedForward.create(
  out.cnn, X=cnn.inL$state[,,,trainind], y=cnn.inL$res[trainind], 
  ctx = mx.cpu(), array.batch.size = batchsize, 
  num.round = nepochs, learning.rate = 0.01, 
  eval.metric = mx.metric.rmse
)

##### save the models #####
mx.model.save(cnn.model.5, "01", 20)
mx.model.save(cnn.model.15, "02", 20)
mx.model.save(cnn.model.5.128, "03", 20)
mx.model.save(cnn.model.15.128, "04", 20)
