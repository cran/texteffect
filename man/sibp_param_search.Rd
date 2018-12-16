\name{sibp_param_search}
\alias{sibp_param_search}
\title{Search Parameter Configurations for Supervised Indian Buffet Process (sibp)}
\description{
  \code{sibp_param_search} runs sibp for a variety of parameter configurations, so that
  the user can then test the effects fo the most interesting treatments.}
\usage{
	  sibp_param_search(X, Y, K, alphas, sigmasq.ns, iters, 
	  a = 0.1, b = 0.1, sigmasq.A = 5, train.ind = train.ind,
	  G = NULL, seed = 0)
}
\arguments{
  \item{X}{The covariates for the full data set.  The division between the training and test set is handled inside the function.}
  \item{Y}{The outcomes for the full data set.  The division between the training and test set is handled inside the function.}
  \item{K}{The number of treatments to be discovered.}
  \item{alphas}{A vector of values of alpha to try.}
  \item{sigmasq.ns}{A vector of values of sigmasq.n to try.}
  \item{iters}{The number of starting values to attempt for each combination of alpha and sigmasq.n.}
  \item{a}{A parameter.}
  \item{b}{A parameter.}
  \item{sigmasq.A}{A parameter.}
  \item{train.ind}{The indices of the observations in the training set, usually obtained from get_training_set().}
  \item{G}{An optional group membership matrix.  The AMCE for a given treatment is permitted to vary as a function of the individual's group.}
  \item{seed}{The seed to be used, so the result can be replicated.}
}
\details{Fits a supervised Indian Buffet Process using variational inference for combinations of alpha and sigmasq.n.  alpha influences how common the treatments are (where larger alphas imply more common treatments) and sigmasq.n influences how much of the variation of the outcome must be explained by the treatments.  These parameters are the most important for determining the quality of the treatments discovered, so it is usually a good idea to experiment with many combinations.  Because the treatments discovered can be sensitive to starting values, it is also usually a good idea to try each combination of alpha and sigmasq.n several times by setting iters > 1.

Because this function uses only the training data, the user can experiment with many parameter configurations without corrupting the inferences made with the test set.  The choice of parameters is equivalent to the choice of hypotheses to test, so the analyst should choose the parameter configuration that leads to the most substantively interesting treatments.  \link{sibp_top_words} can be applied to each element of the list returned by this function to determine which parameter configurations lead to interesting treatments.  Often, it will be impractical to manually investigate every parameter configuration.  In such cases, \link{sibp_rank_runs} can be used to automatically identify some of the most promising candidates.}
\value{
  \item{paramslist}{}
 }
\references{Fong, Christian and Justin Grimmer.  2016. ``Discovery of Treatments from Text Corpora'' Proceedings of the 54th Annual Meeting of the Association for Computational Linguistics. \url{https://aclweb.org/anthology/P/P16/P16-1151.pdf}}
\author{Christian Fong}

\seealso{\link{sibp_rank_runs}, \link{sibp_top_words}, \link{sibp_amce}}
\examples{
##Load the sample of Wikipedia biography data
data(BioSample)

# Divide into training and test sets
Y <- BioSample[,1]
X <- BioSample[,-1]
set.seed(1)
train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)

# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y, K = 2, alphas = c(2,4),
                                 sigmasq.ns = c(0.8, 1), iters = 1,
							     train.ind = train.ind)
								 
\dontrun{
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)

# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["4"]][["0.8"]][[1]], colnames(X), 10, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["1"]][[1]], colnames(X), 10, verbose = TRUE)
}
}