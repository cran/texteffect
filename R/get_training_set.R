get_training_set<-function(X, Y, training.prop = 0.5){
  train.ind <- sample(1:nrow(X), round(training.prop*nrow(X)), replace = TRUE)
  return(train.ind)
}