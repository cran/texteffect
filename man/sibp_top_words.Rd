\name{sibp_top_words}
\alias{sibp_top_words}
\title{Report Words Most Associated with each Treatment}
\description{
  \code{sibp_top_words} returns a data frame of the words most associated with each treatment.}
\usage{
	  sibp_top_words(sibp.fit, words, num.words = 10, verbose = FALSE)
}
\arguments{
  \item{sibp.fit}{A \code{sibp} object.}
  \item{words}{The actual words, usually obtained through colnames(X).}
  \item{num.words}{The number of top words to report.}
  \item{verbose}{If set to true, reports how common each treatment is (so that the analyst can focus on the common treatments) and how closely associated each word is with each treatment.}
}
\details{The choice of the parameter configuration should always be made on the basis of which treatments are substantively the most interesting.  This function provides one natural way of discovering which words are most associated with each treatment (the mean parameter for the posterior distribution of phi, where phi is the effect of the treatment on the count of word w) and therefore helps to determine which treatments are most interesting.}
\value{
  \item{top.words}{A data frame where each column consists of the top ten words (in order) associated with a given treatment.}
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
set.seed(1)
train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)

# Fit an sIBP on the training data
sibp.fit <- sibp(X, Y, K = 2, alpha = 4, sigmasq.n = 0.8, 
				 train.ind = train.ind)

sibp_top_words(sibp.fit, colnames(X))
}
