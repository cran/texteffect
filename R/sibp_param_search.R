sibp_param_search<-function(X, Y, K, alphas, sigmasq.ns, iters, a = 0.1, b = 0.1, 
                            sigmasq.A = 5, train.ind = train.ind, G = NULL, seed = 0){
  paramslist <- list("alphas" = alphas, "sigmasq.ns" =  sigmasq.ns, "iters" = iters)
  for (alpha in alphas){
    print(alpha)
    paramslist[[as.character(alpha)]] <- list()
    for (sigmasq.n in sigmasq.ns){
      paramslist[[as.character(alpha)]][[as.character(sigmasq.n)]] <- list()
      print(sigmasq.n)
      for (i in 1:iters){
        set.seed(seed + i)
        paramslist[[as.character(alpha)]][[as.character(sigmasq.n)]][[i]] <- 
          sibp(X = X, Y = Y, K = K, alpha = alpha, a = a, b = b, 
               sigmasq.A = sigmasq.A, sigmasq.n = sigmasq.n, 
               train.ind = train.ind, G = G, silent = TRUE)
      }
    }
  }
  return(paramslist)
}