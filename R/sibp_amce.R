# Get confidence intervals for the effects of the various treatments
sibp_amce<-function(sibp.fit, X, Y, seed = 0, level = 0.05, nboot = 1000){
  X.test <- t(apply(X[sibp.fit$test.ind,], 1, function(x) (x - sibp.fit$meanX)/sibp.fit$sdX))
  Z.test <- infer_Z(sibp.fit, X.test)
  Y.test <- (Y[sibp.fit$test.ind] - sibp.fit$meanY)/sibp.fit$sdY
  
  rm(X)
  rm(Y)

  hard.assign.lm<-function(dat, ind){
    Y <- dat[ind,1]
    Z <- dat[ind,-1]
    Z.hard <- apply(Z, 2, function(z) sapply(z, function(zi) rbinom(1,1,zi)))
    return(coef(lm(Y~Z.hard)))
  }
  
  set.seed(seed)
  coef.boot <- boot(cbind(Y.test, Z.test), hard.assign.lm, R = nboot)
  incomplete <- which(apply(coef.boot$t, 2, function(x) max(is.na(x))) == 1)
  ci.bounds <- apply(coef.boot$t, 2, quantile, probs = c(level/2, 1-level/2), na.rm = TRUE)
  
  cidf <- data.frame(x=c(0,factor(1:(sibp.fit$K))), 
                     effect = coef.boot$t0, 
                     L = ci.bounds[1,], U = ci.bounds[2,])
  
  cidf[,-1] <- cidf[,-1]*sibp.fit$sdY
  rownames(cidf) <- c("Intercept", 1:sibp.fit$K)
  if (length(incomplete) > 1){
    warning(paste0("The coefficients for the following treatments were NA for some bootstrap draws, indicating that they are either very rare or very common: ", 
                   paste(rownames(cidf)[incomplete], collapse=", ")))
  }
  sibp.amce <- cidf
  return(sibp.amce)
}

sibp_amce_plot<-function(sibp.amce,  xlab = "Feature", ylab = "Outcome"){
  x <- sibp.amce[-1,]$x
  effect <- sibp.amce[-1,]$effect
  U <- sibp.amce[-1,]$U
  L <- sibp.amce[-1,]$L
  ggplot(sibp.amce[-1,], aes(x = x, y = effect)) + geom_errorbar(aes(ymax=U, ymin=L)) + geom_point(size = 5) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), 
          axis.title.x=element_text(vjust=-0.25)) + 
    labs(x = "Feature", y = "Outcome") + geom_hline(yintercept = 0, linetype = 2)    
}

