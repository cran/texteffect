sibp_rank_runs<-function(sibp.search, X, num.words = 10){
  # Calculate the exclusivity metric for each parameter configuration
  total_runs <- length(sibp.search$alphas) * length(sibp.search$sigmasq.ns) * sibp.search$iters
  df <- data.frame("alpha" = rep(0,total_runs), 
                   "sigmasq.n" = rep(0,total_runs), 
                   "iter" = rep(0, total_runs), 
                   "exclu" = rep(0, total_runs))
  ctr <- 1
  for (alpha in sibp.search$alphas){
    for (sigmasq.n in sibp.search$sigmasq.ns){
      for (i in 1:sibp.search$iters){
        df$iter[ctr] <- i
        df$alpha[ctr] <- alpha
        df$sigmasq.n[ctr] <- sigmasq.n
        df$exclu[ctr] <- sibp_exclusivity(sibp.search[[as.character(alpha)]][[as.character(sigmasq.n)]][[i]], 
                                        X, num.words)
        ctr <- ctr + 1
      }
    }
  }
  exculsivity_rank <- df[order(df$exclu, decreasing = TRUE),] 
  return(exculsivity_rank)
}