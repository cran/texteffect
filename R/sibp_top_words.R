sibp_top_words<-function(sibp.fit, words, num.words = 10, verbose = FALSE){
  if(verbose){
    print("Frequency of treatments: ")
    print(apply(sibp.fit$nu, 2, sum))
    
    print("Relation between top words and treatments")
    print(head(matrix(apply(sibp.fit$phi, 1, sort, decreasing = TRUE), 
                      nrow = sibp.fit$D, ncol=sibp.fit$K)))
  }
  top.words <- matrix(words[apply(sibp.fit$phi, 1, order, decreasing = TRUE)], 
                      nrow = sibp.fit$D, ncol=sibp.fit$K)
  return(top.words[1:num.words,])
}