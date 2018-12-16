\name{sibp_exclusivity}
\alias{sibp_exclusivity}
\alias{sibp_rank_runs}
\title{Calculate Exclusivity Metric}
\description{
  \code{sibp_exculsivity} calculates the coherence metric for an \code{sibp} object fit on a training set.  \code{sibp_rank_runs} runs \code{sibp_exclusivity} on each element in the list returned by \code{sibp_param_search}, and ranks the parameter configurations from most to least promising.}
\usage{
	  sibp_exclusivity(sibp.fit, X, num.words = 10)
	  sibp_rank_runs(sibp.search, X, num.words = 10)
}
\arguments{
  \item{sibp.fit}{A \code{sibp} object.}
  \item{sibp.search}{A list of \code{sibp} object fit using the training set, obtained using \code{sibp_param_search}.}
  \item{X}{The covariates for the full data set.  The division between the training and test set is handled inside the function.}
  \item{num.words}{The top words whose coherence will be evaluated.}
}
\details{The metric is formally described at the top of page 1605 of \url{https://aclweb.org/anthology/P/P16/P16-1151.pdf}.  The purpose of this metric is merely to suggest which parameter configurations might contain the most interesting treatments to test if there are too many configurations to investigate manually.  The choice of the parameter configuration should always be made on the basis of which treatments are substantively the most interesting, see \link{sibp_top_words}.}
\value{
  \item{exclusivity}{An exclusivity matrix which quantifies the degree to which the top words in a treatment appear in documents that have that treatment but not in documents that lack that treatment.}
  \item{exclusivity_rank}{A table that ranks the treatments discovered by the various runs from sibp.search from most exclusive to least exclusive.}
 }
\references{Fong, Christian and Justin Grimmer.  2016. ``Discovery of Treatments from Text Corpora'' Proceedings of the 54th Annual Meeting of the Association for Computational Linguistics. \url{https://aclweb.org/anthology/P/P16/P16-1151.pdf}}
\author{Christian Fong}


\seealso{\link{sibp_param_search}, \link{sibp_top_words}}
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
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)
}
