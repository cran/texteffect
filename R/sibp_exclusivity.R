sibp_exclusivity<-function(sibp.fit, X, num.words = 10){
  X.train <- t(apply(X[sibp.fit$train.ind,], 1, function(x) (x - sibp.fit$meanX)/sibp.fit$sdX))

  top.words <- apply(sibp.fit$phi, 1, order, decreasing = TRUE)[1:num.words,]
  C <- array()
  for (t in 1:ncol(sibp.fit$nu)){
    C[t] <- 0
    docs.in.topic <- which(sibp.fit$nu[,t] > 0.5)
    for (m in 2:num.words){
      for (l in 1:(m-1)){
        word1 <- top.words[m,t]
        word2 <- top.words[l,t]
        if (length(docs.in.topic) > 1){
          C[t] <- C[t] + cov(X.train[docs.in.topic,word1], X.train[docs.in.topic,word2])*length(docs.in.topic)
        }
        if (length(docs.in.topic) > 1 & length(docs.in.topic) < (nrow(X.train)-1)){
          C[t] <- C[t] - cov(X.train[-docs.in.topic,word1], X.train[-docs.in.topic,word2]) * 
            (nrow(X.train) - length(docs.in.topic))
        }
      }
    }
    C[t] <- C[t]/((num.words^2 - num.words)/2)
  }
  exclusivity <- sum(C)
  return(exclusivity)
}