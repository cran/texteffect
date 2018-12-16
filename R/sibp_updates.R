# Initializes nu by generating a pi via stick breaking and then drawing nu from a beta(pi, 1 - pi)
init_nu<-function(N, K, alpha){
  # Initial draw of pi's via stick-breaking
  v <- rbeta(n = K, shape1 = alpha, shape2 = 1)
  pi <- rep(0, K)
  pi[1] <- v[1]
  for (k in 2:K){
    pi[k] <- v[k] * pi[k - 1]
  }
  
  return(sapply(1:K, function(k) rbeta(N, pi[k], 1 - pi[k])))
}

# Initializes all the parameters by first drawing nu and an initial phi and then drawing all of the others
# conditional on these two.

# Update: Will have to include group membership matrix
initial_draw<-function(Y, X, N, D, K, alpha, a, b, sigmasq.A, sigmasq.n, G){  
  out<-list()
  
  out$nu <- init_nu(N, K, alpha)
  
  #Initialize phi based on nu
  init.phi <- apply(X, 2, function(x) coef(lm(x ~ -1 + out$nu)))
  init.phi[is.na(init.phi)] <- rnorm(sum(is.na(init.phi)), 0, 1)
  
  update <- update_A(X, N, D, K, sigmasq.A, sigmasq.n, init.phi, out$nu)
  out$phi <- update$phi
  out$big.Phi <- update$big.Phi
  
  # Update: Will have to incorporate group-membership matrix
  update <- update_betatau(Y, K, N, a, b, out$nu, G)
  out$m <- update$m
  out$S <- update$S
  out$c <- update$c
  out$d <- update$d
  
  out$lambda <- update_pi(alpha, N, K, out$nu)
  
  return(out)
}

# Update phi and Phi
update_A<-function(X, N, D, K, sigmasq.A, sigmasq.n, phi, nu){  
  big.Phi <- lapply(apply(nu, 2, function(nuk) 1/sigmasq.A + sum(nuk)/sigmasq.n)^(-1), 
                    function(x) x*diag(D))
  for (k in 1:K){
    X.resid <- X - (nu%*%phi - nu[,k,drop=FALSE]%*%phi[k,,drop=FALSE])
    phi[k,] <- (1/sigmasq.n * colSums(nu[,k]*X.resid))*(1/sigmasq.A + sum(nu[,k,drop=FALSE])/sigmasq.n)^(-1)
  }
  
  out<-list()
  out$phi <- phi
  out$big.Phi <- big.Phi
  return(out)
}

# Update m, S, c, and d
# Update: Need to add group-membership matrix as an argument
update_betatau<-function(Y, K, N, a, b, nu, G){
  # Augment nu so it is different for each group
  nutilde <- c()
  for (l in 1:ncol(G)){
    nutilde <- cbind(nutilde, G[,l]*nu)
  }

  ztz<-t(nutilde)%*%nutilde
  diag(ztz) <- apply(nutilde, 2, sum)
  
  out <- list()
  
  out$S <- ginv(ztz + diag(ncol(nutilde)))
  out$m <- out$S %*% t(nutilde) %*% Y
  
  out$c <- a + N/2
  out$d <- c(b + (t(Y)%*%Y - t(Y) %*% nutilde %*% out$S %*% t(nutilde) %*% Y)/2)
  return(out)
}

# Update nu
# Update: Need to take group-membership matrix as an argument
update_Z<-function(nu, lambda, c, d, phi, big.Phi, m, S, sigmasq.n, Y, N, K, X, G){
  pi.component <- apply(lambda, 1, function(lam) digamma(lam[1]) - digamma(lam[2]))
  
  big.Phi.tr <- sapply(1:K, function(k) sum(diag(big.Phi[[k]])))
  X.component <- -1/(2*sigmasq.n) * t(apply(apply(phi, 1, function(ph) -2*ph%*%t(X) + sum(ph^2)), 1, 
                                            function(x) x + big.Phi.tr))
  
  # Update: Will need to include selector for appropriate m and S
  # m is LK
  mtilde <- as.matrix(G)%*%matrix(m, nrow = ncol(G), ncol = K, byrow = FALSE)
  diagStilde <- as.matrix(G)%*%matrix(diag(S), nrow = ncol(G), ncol = K, byrow = FALSE)
  Y.component <- as.numeric(-c/(2*d))*(-2*as.vector(Y)*mtilde + d*diagStilde/(c-1) + mtilde^2)
  #Y.component <- as.numeric(-c/(2*d)) * t(apply(-2*Y%*%t(m), 1, function(y) y + d*diag(S)/(c-1) + m^2))

  for (k in 1:K){
    X.sumexcept <- -1/(2*sigmasq.n) * as.numeric(2*phi[k,]%*%t(nu%*%phi - nu[,k,drop=FALSE]%*%phi[k,,drop=FALSE]))
    # Update: Will need to include selector for appropriate m and S
    Y.sumexcept <- -c/(2*d) * 2*mtilde[,k]*as.numeric(apply(nu*mtilde, 1, sum) - nu[,k]*mtilde[,k])
    #Y.sumexcept <- -c/(2*d) * 2*m[k]*as.numeric(nu%*%m - nu[,k]*m[k])
    v <- pi.component[k] + X.component[,k] + X.sumexcept + Y.component[,k]  + Y.sumexcept
    nu[,k] <- (1 + exp(-v))^(-1)
    
  }  
  return(nu)
}

#Update lambda
update_pi<-function(alpha, N, K, nu){
  lambda <- matrix(rep(0, 2*K), nrow = K, ncol = 2)
  sumnu <- apply(nu, 2, sum)
  lambda[,1] <- alpha/K + sumnu
  lambda[,2] <- 1 + N - sumnu
  
  return (lambda)
}

# Find the absolute value of the biggest parameter change, and subtract the threhsold
convergence.check<-function(new.c, new.d, new.phi, new.big.Phi, new.lambda, new.m, new.S, new.nu, 
                            old.c, old.d, old.phi, old.big.Phi, old.lambda, old.m, old.S, old.nu,
                            threshold){
  return(max(abs(c(new.c - old.c, new.d - old.d, new.phi - old.phi, unlist(new.big.Phi) - unlist(old.big.Phi),
                   new.lambda - old.lambda, new.m - old.m, new.S - old.S, new.nu - old.nu))) - threshold)
  
}