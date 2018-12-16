# Predicts the Z on the test set
infer_Z<-function(sibp.fit, X, newX = FALSE){
  if (newX){
    X.test <- t(apply(X, 1, function(x) (x - sibp.fit$meanX)/sibp.fit$sdX))
  }
  else{
    X.test <- t(apply(X[sibp.fit$test.ind,], 1, function(x) (x - sibp.fit$meanX)/sibp.fit$sdX))
  }
  N <- nrow(X.test)

  K <- sibp.fit$K
  alpha <- sibp.fit$sigmasq.n
  sigmasq.n <- sibp.fit$sigmasq.n
  
  nu <- init_nu(N, K, alpha)
  phi <- sibp.fit$phi
  big.Phi <- sibp.fit$big.Phi
  lambda <- sibp.fit$lambda
  
  converged <- FALSE
  while(!converged)
  {
    old.nu <- nu
    pi.component <- apply(lambda, 1, function(lam) digamma(lam[1]) - digamma(lam[2]))
    
    big.Phi.tr <- sapply(1:K, function(k) sum(diag(big.Phi[[k]])))
    X.component <- -1/(2*sigmasq.n) * t(apply(apply(phi, 1, function(ph) -2*ph%*%t(X.test) + sum(ph^2)), 1, 
                                              function(x) x + big.Phi.tr))
    
    for (k in 1:K){
      X.sumexcept <- -1/(2*sigmasq.n) * as.numeric(2*phi[k,]%*%t(nu%*%phi - nu[,k,drop=FALSE]%*%phi[k,,drop=FALSE]))
      v <- pi.component[k] + X.component[,k] + X.sumexcept
      nu[,k] <- (1 + exp(-v))^(-1)
      
    }  
    if (max(abs(nu - old.nu)) < 10^(-3)) converged <- TRUE
  }
  
  return(nu)
}