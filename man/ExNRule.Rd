\name{ExNRule}
\alias{ExNRule}
\title{The Extended Neighbourhood Rule for k Nearest Neibour Ensemble}
\description{The function ExNRule() gives predictions and class probabilities for binary classification data using the extended neighbourhood rule (ExNRule) for k nearest neibour (kNN) ensemble.}
\usage{
ExNRule(xtrain, xtest, ytrain, k=3, r=500, p=round(sqrt(ncol(xtrain))))
}
\arguments{
\item{xtrain}{
The features space of the training dataset.
}
\item{xtest}{
The feature space of the testing data.
}
\item{ytrain}{
The response variable of the training dataset.
}
\item{k}{
The nearest observation in the extended pattern.
}
\item{r}{
The total number of base kNN models using ExNRule.
}
\item{p}{
The number of features used for each base kNN model using ExNRule.
}
}
\value{
\item{class}{Predicted classes for the test data.}
\item{class.prob}{Predicted class probabilities for the test data.}
}
\references{
Ali, Amjad, et al. "An Optimal k Nearest Neighbours Ensemble for Classification Based on Extended Neighbourhood Rule with Features subspace." arXiv preprint arXiv:2211.11278 (2022).
}
\author{
Amjad Ali, Muhammad Hamraz, Saeed Aldahmani, Zardad Khan.

Maintainer: Amjad Ali <Amjad.ali@awkum.edu.pk>
}
\examples{
data(ILPD)

X <- ILPD[,-11]
Y <- as.factor(ILPD[,11])

n <- nrow(ILPD)
train <- sample(1:n, 0.7*n, replace = FALSE)

xtrain <- X[train,]
xtest <- X[-train,]

ytrain <- Y[train]
ytest <- Y[-train]

mymod <- ExNRule(xtrain, xtest, ytrain, k=3, r=500, p=round(sqrt(ncol(xtrain))))
pred.class <- mymod$class
class.prob <- mymod$class.prob

Accuracy <- mean(pred.class==ytest)
ytestnum <- as.numeric(as.factor(ytest))-1
BrierScore <- mean((ytestnum-class.prob))

cbind(Accuracy, BrierScore)
}
\keyword{ ExNRule }
\keyword{ Bootstraping }
\keyword{ Ensemble }
\keyword{ Classification }
