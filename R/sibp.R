# Need to include group-membership vector
sibp<-function(X, Y, K, alpha, sigmasq.n, a = 0.1, b = 0.1, sigmasq.A = 5, 
               train.ind, G = NULL, silent = FALSE){
  out<-list()
  
  # Need to convert group-membership vector into group-membership matrix
  # Want it to be the case that G %*% beta selects the correct beta
  if (is.null(G)){
    G <- matrix(1, nrow = nrow(X), ncol = 1)
  }
  
  Gtrain <- G[train.ind,,drop=FALSE]

  Ytrain <- Y[train.ind]
  Xtrain <- X[train.ind,]
  
  out$train.ind <- train.ind
  out$test.ind <- setdiff(1:nrow(X), train.ind)
  
  # Standardize Y and X
  out$meanY <- mean(Ytrain)
  out$sdY <- sd(Ytrain)
  out$meanX <- apply(Xtrain, 2, mean)
  out$sdX <- apply(Xtrain, 2, sd)
  
  Ytrain <- scale(Ytrain)
  Xtrain <- scale(Xtrain)
  
  rm(X)
  rm(Y)
  
  N<-nrow(Xtrain)
  D<-ncol(Xtrain)
  
  out$K <- K
  out$L <- ncol(G)
  out$D <- D
  out$alpha <- alpha
  out$a <- a
  out$b <- b
  out$sigmasq.A <- sigmasq.A
  out$sigmasq.n <- sigmasq.n
  
  initial.params <- initial_draw(Ytrain, Xtrain, N, D, K, alpha, a, b, sigmasq.A, sigmasq.n, Gtrain)
  
  # Initialize parameters
  c <- initial.params$c
  d <- initial.params$d
  phi <- initial.params$phi
  big.Phi <- initial.params$big.Phi
  lambda <- initial.params$lambda
  m <- initial.params$m
  S <- initial.params$S
  nu <- initial.params$nu
  
  iterations<-0
  converged<-FALSE
  
  # Update until convergence
  while (!converged){
    iterations<-iterations+1
    
    new.nu <- update_Z(nu, lambda, c, d, phi, big.Phi, m, S, sigmasq.n, Ytrain, N, K, Xtrain, Gtrain)
    new.lambda <- update_pi(alpha, N, K, new.nu)
    update <- update_A(Xtrain, N, D, K, sigmasq.A, sigmasq.n, phi, new.nu)
    new.phi <- update$phi
    new.big.Phi <- update$big.Phi
    update <- update_betatau(Ytrain, K, N, a, b, new.nu, Gtrain)
    new.c <- update$c
    new.d <- update$d
    new.m <- update$m
    new.S <- update$S
    
    threshold<-10^(-3)
    
    conv.dist <- convergence.check(new.c, new.d, new.phi, new.big.Phi, new.lambda, new.m, new.S, new.nu, 
                                   c, d, phi, big.Phi, lambda, m, S, nu, threshold)
    converged <- conv.dist < 0
    
    c <- new.c
    d <- new.d
    phi <- new.phi
    big.Phi <- new.big.Phi
    lambda <- new.lambda
    m <- new.m
    S <- new.S
    nu <- new.nu
    
    if (iterations %% 10 == 0){
      if (!silent){
        print(iterations)
        print(conv.dist)
      }
    }
  }
  
  out$nu <- nu
  out$m <- m
  out$S <- S
  out$lambda <- lambda
  out$phi <- phi
  out$big.Phi <- big.Phi
  out$c <- c
  out$d <- d
  return(out)
}
