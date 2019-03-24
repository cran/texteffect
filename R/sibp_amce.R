# Get confidence intervals for the effects of the various treatments
sibp_amce<-function(sibp.fit, X, Y, G = NULL, seed = 0, level = 0.05, thresh = 0.5){
  # Want it to be the case that G %*% beta selects the correct beta
  if (is.null(G)){
    G <- matrix(1, nrow = nrow(X), ncol = 1)
  }
  
  set.seed(seed)
  
  G.test <- G[sibp.fit$test.ind,,drop=FALSE]
  Z.test <- infer_Z(sibp.fit, X)
  Y.test <- (Y[sibp.fit$test.ind] - sibp.fit$meanY)/sibp.fit$sdY
  
  Z.hard <- apply(Z.test, 2, function(z) sapply(z, function(zi) ifelse(zi >= thresh, 1, 0)))
  
  L <- sibp.fit$L
  K <- sibp.fit$K
  
  if (L == 1){
    fit <- lm(Y.test ~ Z.hard)
  } else{
    rhsmat <- c()
    for (l in 1:L){
      rhsmat<-cbind(rhsmat,Z.hard*G.test[,l])
    }
    fit <- lm(Y.test~-1+as.matrix(G.test)+rhsmat)
  }
  ci.bounds <- cbind(coef(fit)+qnorm(level/2)*summary(fit)$coefficients[,2], 
                     coef(fit)+qnorm(1-level/2)*summary(fit)$coefficients[,2])
  
  cidf <- data.frame(x = 1:((K+1)*L), 
                     effect = coef(fit), 
                     L = ci.bounds[,1], 
                     U = ci.bounds[,2])
  cidf[,-1] <- cidf[,-1]*sibp.fit$sdY
  sibp.amce <- cidf
  
  if (L == 1){
    rownames(sibp.amce) <- c("Intercept", paste0("Z",1:K))
  }
  if (L > 1){
    rnames <- c()
    for (i in 1:(K+1)){
      for (l in 1:L){
        if (i == 1){
          rnames <- c(rnames, paste0("Intercept:",colnames(G)[l]))
        }
        else{
          rnames <- c(rnames, paste0("Z",i-1,":",colnames(G)[l]))
        }
      }
    }
    rownames(sibp.amce) <- rnames
  }
  return(sibp.amce)
}

sibp_amce_plot<-function(sibp.amce, xlab = "Feature", ylab = "Outcome", subs = NULL){
  if(is.null(subs)){
    subs <- 1:nrow(sibp.amce)
  }
  x <- sibp.amce[subs,]$x
  effect <- sibp.amce[subs,]$effect
  U <- sibp.amce[subs,]$U
  L <- sibp.amce[subs,]$L
  ggplot(sibp.amce[subs,], aes(x = x, y = effect)) + geom_errorbar(aes(ymax=U, ymin=L)) + geom_point(size = 5) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), 
            axis.title.x=element_text(vjust=-0.25)) + 
      labs(x = "Feature", y = "Outcome") + geom_hline(yintercept = 0, linetype = 2)
}

