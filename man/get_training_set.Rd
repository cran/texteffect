\name{get_training_set}
\alias{get_training_set}
\title{Divide Data into Training and Test Sets}
\description{
  \code{get_training_set} divides the data into a training set and test set and returns the indices of the observations in the training set.}
\usage{
	  get_training_set(X, Y, training.prop = 0.5)
}
\arguments{
  \item{X}{The covariate matrix.}
  \item{Y}{The outcome vector.}
  \item{training.prop}{The proportion of observations to be placed in the training set, rounded to the nearest whole number.}
}
\details{This package implements a two-step procedure, in which a mapping from text (X) to latent treatments is inferred on a training set, and then the relationship between the latent treatments and the outcome is estimated using a test set.  This function divides the data into a training and test set.  The user will not have to explicitly specify the training or test sets in sibp functions because this the training indices will be incorporated into the \code{sibp} object.}
\value{
  \item{train.ind}{The indices of the observation in the training set.  This is used as an argument for \code{sibp} and \code{sibp_param_search}.}
 }
\references{Fong, Christian and Justin Grimmer.  2016. ``Discovery of Treatments from Text Corpora'' Proceedings of the 54th Annual Meeting of the Association for Computational Linguistics. \url{https://aclweb.org/anthology/P/P16/P16-1151.pdf}}
\author{Christian Fong}

\seealso{\link{sibp}}
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

sibp_top_words(sibp.fit, colnames(X))
}
