\name{sibp}
\alias{sibp}
\title{Supervised Indian Buffet Process (sibp) for Discovering Treatments}
\description{
  \code{sibp} discovers latent binary treatments within a corpus, as described by Fong and Grimmer (2016).}
\usage{
	  sibp(X, Y, K, alpha, sigmasq.n, 
	  a = 0.1, b = 0.1, sigmasq.A = 5, 
	  train.ind, G = NULL, silent = FALSE )
}
\arguments{
  \item{X}{The covariates for all observations in the training set, where each row is a document and each column is the count of a word.}
  \item{Y}{The outcome for all observations in the training set.}
  \item{K}{The number of treatments to be discovered.}
  \item{alpha}{A parameter that influences how common the treatments are.  When alpha is large, the treatments are common.}
  \item{sigmasq.n}{A parameter determining the variance of the word counts conditional on the treatments.  When sigmasq.n is large, the treatments must explain most of the variation in X.}
  \item{a}{A parameter that, together with b, influences the variance of the treatment effects and the outcomes.  a = 0.1 is a reasonably diffuse choice.}
  \item{b}{A parameter that, together with a, influences the variance of the treatment effects and the outcomes.  b = 0.1 is a reasonably diffuse choice.}
  \item{sigmasq.A}{A parameter determining the variance of the effect of the treatments on word counts.  A diffuse choice, such as 5, is usually appropriate.}
  \item{train.ind}{The indices of the observations in the training set, usually obtained from get_training_set().}
  \item{G}{An optional group membership matrix.  The AMCE for a given treatment is permitted to vary as a function of the individual's group.}
  \item{silent}{If \code{TRUE}, prints how much the parameters have moved every 10 iterations of sIBP.}
}
\details{Fits a supervised Indian Buffet Process using variational inference.  Before running this function, the data should be divided into a training set and a test set.  This function should be run on the training set to discover latent treatments in the data that seem to be correlated with the outcome.

It is recommended to use link{sibp_param_search} instead of this function to search over multiple configurations of the most important parameters.  So long as only the training data is used, the analyst can freely experimient with as many parameter configurations as he likes without corrupting his causal inferences.  Once a parameter configuration is chosen, the user can then use \link{sibp_amce} on the test set to estimate the average marginal component effect (AMCE) for each treatment.}
\value{
  \item{nu}{Informally, the probability that the row document has the column treatment.  Formally, the parameter for the variational approximation of z_{i,k}, which is a Bernoulli distribution.}
  \item{m}{Informally, the effect of having each treatment on the outcome.  Formally, the mean parameter for the variational approximation of the posterior distribution of beta, which is a normal distribution.  Note that this is in the training sample, and it is inappropriate to use this posterior as the basis for causal inference.  It is instead necessary to estimate effects using the test set, see \link{sibp_amce}.}
  \item{S}{The variance parameter for the posterior distribution of beta, which is a normal distribution.}
  \item{lambda}{A matrix where the kth row contains the shape parameters for the variational approximation of the posterior distribution of pi_k, which is a beta distribution.}
  \item{phi}{Informally, the effect of the row treatment on the column word.  Formally, the mean parameter for the variational approximation of the posterior distribution of A, which is a normal distribution.}
  \item{big.Phi}{The variance parameter for the variational approximation of the posterior distribution of A, which is a normal distribution.  The kth element of the list corresponds to a treatment k.}
  \item{c}{The shape parameter for the variational approximation of the posterior distribution of tau, which is a gamma distribution.}
  \item{d}{The rate parameter for the variational approximation of the posterior distribution of tau, which is a gamma distribution.}
  \item{K}{The number of treatments.}
  \item{D}{The number of words in the vocabulary.}
  \item{alpha}{The alpha used to call this function.}
  \item{a}{The a used to call this function.}
  \item{b}{The b used to call this function.}
  \item{sigmasq.A}{The sigmasq.A used to call this function.}
  \item{sigmasq.n}{The sigmasq.n used to call this function.}
  \item{train.ind}{The indices of the observations in the training set.}
  \item{test.ind}{The indices of the observations in the test set.}
 }
\references{Fong, Christian and Justin Grimmer.  2016. ``Discovery of Treatments from Text Corpora'' Proceedings of the 54th Annual Meeting of the Association for Computational Linguistics. \url{https://aclweb.org/anthology/P/P16/P16-1151.pdf}}
\author{Christian Fong}

\seealso{\link{sibp_param_search}, \link{sibp_top_words}, \link{sibp_amce}}
\examples{
##Load the Wikipedia biography data
data(BioSample)

# Divide into training and test sets
Y <- BioSample[,1]
X <- BioSample[,-1]
set.seed(1)
train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)

# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y, K = 2, alphas = c(2,4), sigmasq.ns = c(0.8, 1), 
								 iters = 1, train.ind = train.ind)
								 
\dontrun{
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)

# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["4"]][["0.8"]][[1]], colnames(X), 10, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["1"]][[1]], colnames(X), 10, verbose = TRUE)

# Select the most interest treatments to investigate
sibp.fit <- sibp.search[["4"]][["0.8"]][[1]]

# Estimate the AMCE using the test set
amce<-sibp_amce(sibp.fit, X, Y)
# Plot 95\% confidence intervals for the AMCE of each treatment
sibp_amce_plot(amce)
}
}
