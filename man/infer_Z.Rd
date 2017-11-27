\name{infer_Z}
\alias{infer_Z}
\title{Infer Treatments on the Test Set}
\description{
  \code{infer_Z} uses an sibp object fitted on a training set to infer the treatments in a test set.}
\usage{
	  infer_Z(sibp.fit, X)
}
\arguments{
  \item{sibp.fit}{A \code{sibp} object.}
  \item{X}{The covariates for the data set where Z is to be inferred.  These covariates should be preprocessed by subtracting the mean of X in the training set and dividing by the standard deviation.  See the example for more details.}
}
\details{This function applies the mapping from words to treatments discovered in the training set to infer which observations have which treatments in the test set.  Usually, users will be better served by calling \code{sibp_amce}, which calls this function internally before returning estimates and confidence intervals for the average marginal component effects.}
\value{
  \item{nu}{Informally, the probability that the row document has the column treatment.  Formally, the parameter for the variatioanl approximation of z_{i,k}, which is a Bernoulli distribution.}
 }
\references{Fong, Christian and Justin Grimmer.  2016. ``Discovery of Treatments from Text Corpora'' Proceedings of the 54th Annual Meeting of the Association for Computational Linguistics. \url{https://aclweb.org/anthology/P/P16/P16-1151.pdf}}
\author{Christian Fong}


\seealso{\link{sibp}, \link{sibp_amce}}
\examples{
##Load the Wikipedia biography data
data(BioSample)

# Divide into training and test sets
Y <- BioSample[,1]
X <- BioSample[,-1]
train.ind <- get_training_set(X, Y, training.prop = 0.5)

# Fit an sIBP on the training data
sibp.fit <- sibp(X, Y, K = 2, alpha = 4, sigmasq.n = 0.8, 
				 train.ind = train.ind)

# Pre-process the test set
X.test <- t(apply(X, 1, function(x) (x - sibp.fit$meanX)/sibp.fit$sdX))

# Infer the latent treatments in the test set
infer_Z(sibp.fit, X.test)
}